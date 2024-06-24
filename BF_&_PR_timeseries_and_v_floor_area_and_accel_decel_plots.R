# make plots of sum MUA PR and mean BF vs. floor area data from IEA and China Stat Yearbook, Beijing Stat Yearbook, Shanghai Stat Yearbook
# make regional mean and 12-city mean BF and PR time series plots
# make BF and ASCAT PR time series plots for three cities in Syria
# make plots of mean BF & PR acceleration/deceleration arrows for 12 regions, 7 sub-regoins, and 12 cities

# data sources:
# 1. China Statistical Yearbook 2022; http://www.stats.gov.cn/sj/ndsj/2022/indexeh.htm; Table 14-23; visited 30 April 2023
# 2. Beijing Statistical Yearbook 2021; https://nj.tjj.beijing.gov.cn/nj/main/2021-tjnj/zk/indexeh.htm; Table 9-6; visited 10 May 2023
# 3. Advanced and Other Economy floor area 2010-2021; International Energy Agency; https://www.iea.org/reports/buildings; visited 4/19/2023
# 4. Global floor area 2000-2021; International Energy Agency; 
#       https://www.iea.org/data-and-statistics/charts/buildings-floor-area-and-cooling-degree-days-index-in-the-net-zero-scenario-2000-2030, visited 4/24/3032

library(ggplot2)
library(ggpubr)

wd = getwd()

input_dir = paste(wd,'/mua_input_files/',sep='')
output_dir = paste(wd,'/mua_output_files/',sep='')

######################################################
# read in country list
country_list_filename = paste(input_dir,'mua_country_list_economy_region.csv',sep='')

country_list = read.csv(file=country_list_filename,header = TRUE, stringsAsFactors = FALSE)
# HEADER: mua_country_code	mua_country_name_short	region_1	economy	sub_regions
# region_1(12): AustraliaNZ, OtherAsia, China, EastAsia, Europe, LatAmerCarib, MiddelEast, NorAmer,  Russia, SEAsia, India, Africa
# economies(2): advanced, other (emerging & developing); based on IEA World Energy Outlook 2022, p499-500 table footnotes
# sub_regions: keeps region_1 list, but disaggregates Africa into EastAfrica, WestAfrica, NorthAfrica, SouthAfrica, CentralAfrica,
#                                   and disaggregates OtherAsia into CentralAsia, SouthAsia (without India)


num_countries = dim(country_list)[1]

# region_list = c('AustraliaNZ', 'CentralAsia', 'China', 'EastAsia', 'Europe', 'LatAmerCarib', 'MidEastNorAfr', 'NorAmer', 
#                 'Russia', 'SEAsia', 'SouthAsia', 'SubSahAfrica')
region_list = c('Africa', 'AustraliaNZ', 'China', 'EastAsia', 'Europe', 'India', 'LatAmerCarib', 'MiddleEast', 'NorAmer', 'OtherAsia', 
                'Russia', 'SEAsia')
num_regions = length(region_list)

sub_region_list = c('EastAfrica', 'WestAfrica', 'NorthAfrica', 'SouthAfrica', 'CentralAfrica', 'CentralAsia', 'SouthAsia')
num_sub_regions = length(sub_region_list)

econ_list = c('advanced','other')
num_econ = length(econ_list)

######################################################
# read in all mua city list
mua_city_list_filename = paste(input_dir,'mua_city_list_lat_lon_clean_Jul2023.csv',sep='')
# NOTE: 'mua_city_list_lat_lon_clean.csv' has mua_objectid 1 (Winnipeg - no data) and 879 (Qinzhou - no data) removed
# NOTE:     Winnipeg & Qinzhou have data at different mua_objectid values (1506 and 1495); 
# NOTE:     Bhubaneswar has 2 mua_objectid values (156 and 1302) with data (i.e., 2 polygons)

mua_city_list = read.csv(file=mua_city_list_filename,header = TRUE, stringsAsFactors = FALSE)
# HEADER: mua_objectid, mua_aggname_short, mua_country_code, mua_country, 
#         mua_mean_lon, mua_mean_lat, mua_cell_count, mua_worldpop_thousands, mua_area_m2, 
#         region_1, economy, sub_regions

# remove any MUA rows with NA (did this manually outside of code)

num_mua_cities = dim(mua_city_list)[1]
num_city_vars = dim(mua_city_list)[2]

######################################################
# read in floor area data
floor_area_filename = paste(input_dir,'global_china_total_floor_area_data.csv',sep='')

floor_area_data = read.csv(file=floor_area_filename,header = TRUE, stringsAsFactors = FALSE)
# HEADER: year (1985-2021) 
#         IEA_FloorArea_billion_m2    (2000-2021)
#         IEA_AdvEcon_FloorArea_billion_m2  (2010, 2015, 2019-2021)
#         IEA_DevEmergEcon_FloorArea_billion_m2   (2010, 2015, 2019-2021)
#         IEA_Total_FoorArea_billion_m2   (2010, 2015, 2019-2021) (sum of previous 2)
#         China_Stat_accum_new_floor_space_compl_billion_m2  (1985, 1990, 1995, 2000, 2005-2021) 
#                     linear interpolations between 1985-90, 1990-95, 1995-2000, 2000-05)
#         Beijing_Stat_accum_new_floor_space_compl_million_m2  (1990-2020) 
#         Shanghai_Stat_accum_new_floor_space_compl_million_m2  (1985-2019) 
######################################################

# read in global MUA data
# NOTE: the working csv file of all grid data included a number of variables that were not used in the end.  These have all been replace with 'NA' 
#    and their column names with NA1 through NA57

mua_data_filename = paste(input_dir,'global_mua_grid_variables_table_wat_lt_50_NA.csv',sep = '')

mua_data_in = read.csv(file=mua_data_filename,header = TRUE, stringsAsFactors = FALSE)

# mua_data_in HEADER:
# Header: 1-6:    longitude	latitude	mua_objectid	mua_fid1	mua_country_code	mua_cellcount	
#         7-12:   mua_worldpop_thousand	mua_area_km2	mua_country	mua_aggname	grid_area_m2 water_pct		
#         13-32:  NA1, NA2, ..., NA20, 
#           33:   NA21	
#         34-41:  ERS1993_summer_mean_pr, ERS1994_summer_mean_pr, ERS1995_summer_mean_pr, ERS1996_summer_mean_pr, 
#                 ERS1997_summer_mean_pr, ERS1998_summer_mean_pr, ERS1999_summer_mean_pr, ERS2000_summer_mean_pr, 
#         42-52:  QSCAT1999_summer_mean_pr, QSCAT2000_summer_mean_pr, QSCAT2001_summer_mean_pr, QSCAT2002_summer_mean_pr, 
#                 QSCAT2003_summer_mean_pr, QSCAT2004_summer_mean_pr, QSCAT2005_summer_mean_pr, QSCAT2006_summer_mean_pr, 
#                 QSCAT2007_summer_mean_pr, QSCAT2008_summer_mean_pr, QSCAT2009_summer_mean_pr, 
#         53-67:  ASCAT2007_summer_mean_pr, ASCAT2008_summer_mean_pr, ASCAT2009_summer_mean_pr, ASCAT2010_summer_mean_pr, 
#                 ASCAT2011_summer_mean_pr, ASCAT2012_summer_mean_pr, ASCAT2013_summer_mean_pr, ASCAT2014_summer_mean_pr, 
#                 ASCAT2015_summer_mean_pr, ASCAT2016_summer_mean_pr, ASCAT2017_summer_mean_pr, ASCAT2018_summer_mean_pr,  
#                 ASCAT2019_summer_mean_pr, ASCAT2020_summer_mean_pr, ASCAT2021_summer_mean_pr
#         68-69:  NA22, NA23
#         70-73:  ers_slope, ers_slope_stderr, ers_slope_r2, ers_slope_pval, 
#         74-77:  qscat_slope, qscat_slope_stderr, qscat_slope_r2, qscat_slope_pval, 
#         78:81:  ascat_10_21_slope, ascat_10_21_slope_stderr, ascat_10_21_slope_r2, ascat_10_21_slope_pval, 
#         82:85:  NA24, NA25, NA26, NA27, 
#         86:89:  WSF3D_Vol_sum WSF3D_BF_mean WSF3D_BA_sum WSF3D_HT_mean 
#         90:93:  WSF3D_Vol_sum_3x3smooth WSF3D_BF_mean_3x3smooth WSF3D_BA_sum_3x3smooth WSF3D_HT_mean_3x3smooth 
#         94:97:  WSFEvolution_1985_mean WSFEvolution_1986_mean WSFEvolution_1987_mean WSFEvolution_1988_mean
#         98:101:  WSFEvolution_1989_mean WSFEvolution_1990_mean WSFEvolution_1991_mean WSFEvolution_1992_mean
#        102:105:  WSFEvolution_1993_mean WSFEvolution_1994_mean WSFEvolution_1995_mean WSFEvolution_1996_mean
#       106:109:  WSFEvolution_1997_mean WSFEvolution_1998_mean WSFEvolution_1999_mean WSFEvolution_2000_mean
#       110:113:  WSFEvolution_2001_mean WSFEvolution_2002_mean WSFEvolution_2003_mean WSFEvolution_2004_mean
#       114:117:  WSFEvolution_2005_mean WSFEvolution_2006_mean WSFEvolution_2007_mean WSFEvolution_2008_mean
#       118:121:  WSFEvolution_2009_mean WSFEvolution_2010_mean WSFEvolution_2011_mean WSFEvolution_2012_mean
#       122:124:  WSFEvolution_2013_mean WSFEvolution_2014_mean WSFEvolution_2015_mean
#       125:153:   NA28, ..., NA56
#       154-157:   del_wsf_bf_93_00 del_wsf_bf_99_09 NA57 del_wsf_bf_10_15
#       158-159:   region_1 economy

# add sub+_regions column (#160), ALSO update region list in the mua_all_grid file

mua_data_in$sub_regions = NA

for (icountry in 1:num_countries) {
  
  country_name = country_list[icountry,2]
  sub_region_name = country_list[icountry,5]
  region_1_name = country_list[icountry,3]
  
  mua_data_in$sub_regions[mua_data_in$mua_country == country_name] <- sub_region_name
  mua_data_in$region_1[mua_data_in$mua_country == country_name] <- region_1_name
  
}

mua_global_grid_pr = mua_data_in[,c(1:12,34:69,70, 72, 74, 76, 78, 80, 154:160)]

# Header:
#  columns 1-12: longitude latitude mua_objectid mua_fid1 mua_country_code mua_cellcount mua_worldpop_thousand mua_area_km2    mua_country mua_aggname grid_area_m2 water_pct 
#  columns 13-20: ERS1993_summer_mean_pr ERS1994_summer_mean_pr ERS1995_summer_mean_pr ERS1996_summer_mean_pr ERS1997_summer_mean_pr ERS1998_summer_mean_pr ERS1999_summer_mean_pr ERS2000_summer_mean_pr 
#  columns 21-31: QSCAT1999_summer_mean_pr QSCAT2000_summer_mean_pr QSCAT2001_summer_mean_prQSCAT2002_summer_mean_pr QSCAT2003_summer_mean_pr 
#               : QSCAT2004_summer_mean_pr QSCAT2005_summer_mean_pr QSCAT2006_summer_mean_pr QSCAT2007_summer_mean_pr QSCAT2008_summer_mean_pr QSCAT2009_summer_mean_pr 
#  columns 32-46: ASCAT2007_summer_mean_pr ASCAT2008_summer_mean_pr ASCAT2009_summer_mean_pr ASCAT2010_summer_mean_pr ASCAT2011_summer_mean_pr 
#               : ASCAT2012_summer_mean_pr ASCAT2013_summer_mean_pr ASCAT2014_summer_mean_pr ASCAT2015_summer_mean_pr ASCAT2016_summer_mean_pr
#               : ASCAT2017_summer_mean_pr ASCAT2018_summer_mean_pr ASCAT2019_summer_mean_pr ASCAT2020_summer_mean_pr ASCAT2021_summer_mean_pr 
#  columns 47-48: QSCAT_offset_A, QSCAT_offset_AE
#  columns 49-54: ers_slope  ers_slope_r2  qscat_slope  qscat_slope_r2  ascat_slope  ascat_slope_r2  
#  columns 55-58: del_wsf_bf_93_00 del_wsf_bf_99_09 del_wsf_bf_07_15 del_wsf_bf_10_15
#  columns 59-61: region_1 economy sub_regions

# note this array has 34880 grid cells
#                         
# replace any -9999 (no data) values with NA 
mua_global_grid_pr[mua_global_grid_pr == -9999]  <- NA  #  there are not any -9999 values in this array

mua_global_grid_pr = as.data.frame(mua_global_grid_pr)

mua_data_in = as.data.frame(mua_data_in)

# aggregate ERS, QSCAT, QSCAT*, and ASCAT sum(PR) for China, Beijing, Shanghai, advanced_economies, global, (other_economies = global minus advanced_economies), 12 regions

mua_global_sum_pr = as.data.frame(array(NA,c((34+23),19)))

colnames(mua_global_sum_pr) = c('year', 'sensor', 'global', 'advanced_econ', 'other_econ', 'Africa', 'AustraliaNZ', 'China', 
                                'EastAsia', 'Europe', 'India', 'LatAmerCarib', 'MiddleEast', 'NorAmer', 'OtherAsia', 
                                'Russia', 'SEAsia','Beijing','Shanghai')

# mua_global_sum_pr[,1] = c(1993:2000,1999:2009,2007:2021)
mua_global_sum_pr[,1] = c(1993:2000,1999:2009,2007:2021, 1993:2015)
mua_global_sum_pr[1:8,2] = 'ERS'
mua_global_sum_pr[9:19,2] = 'QSCAT'
mua_global_sum_pr[20:34,2] = 'ASCAT'
mua_global_sum_pr[35:57,2] = 'WSF-evo'

ers_sum = array(NA,c(1,8))
ers_sum = colSums(mua_global_grid_pr[,13:20],na.rm = T)
mua_global_sum_pr[1:8,3] = ers_sum

qscat_sum = array(NA,c(1,11))
qscat_sum = colSums(mua_global_grid_pr[,21:31],na.rm = T)
mua_global_sum_pr[9:19,3] = qscat_sum

ascat_sum = array(NA,c(1,15))
ascat_sum = colSums(mua_global_grid_pr[,32:46],na.rm = T)
mua_global_sum_pr[20:34,3] = ascat_sum

wsfevo_sum = array(NA,c(1,15))
wsfevo_sum = colSums(mua_data_in[,102:124],na.rm = T)
mua_global_sum_pr[35:57,3] = wsfevo_sum

mua_adv_econ_grid_pr = subset(mua_global_grid_pr, mua_global_grid_pr$economy == 'advanced')

num_advanced = dim(mua_adv_econ_grid_pr)[1]
num_other = 34880 - num_advanced

ers_sum = colSums(mua_adv_econ_grid_pr[,13:20],na.rm = T)
mua_global_sum_pr[1:8,4] = ers_sum

qscat_sum = colSums(mua_adv_econ_grid_pr[,21:31],na.rm = T)
mua_global_sum_pr[9:19,4] = qscat_sum

ascat_sum = colSums(mua_adv_econ_grid_pr[,32:46],na.rm = T)
mua_global_sum_pr[20:34,4] = ascat_sum

mua_wsfevo_adv_econ_grid_pr = subset(mua_data_in, mua_data_in$economy == 'advanced')
wsfevo_sum = colSums(mua_wsfevo_adv_econ_grid_pr[,102:124],na.rm = T)
mua_global_sum_pr[35:57,4] = wsfevo_sum

mua_global_sum_pr[,5] = mua_global_sum_pr[,3] - mua_global_sum_pr[,4] 

for (iregion in 1:num_regions) {   # includes China as a region
  
  mua_region_grid_pr = subset(mua_global_grid_pr, mua_global_grid_pr$region_1 == region_list[iregion])
  num_region_grids = dim(mua_region_grid_pr)[1]
  
  ers_sum = colSums(mua_region_grid_pr[,13:20],na.rm = T)
  mua_global_sum_pr[1:8,5+iregion] = ers_sum
  
  qscat_sum = colSums(mua_region_grid_pr[,21:31],na.rm = T)
  mua_global_sum_pr[9:19,5+iregion] = qscat_sum
  
  ascat_sum = colSums(mua_region_grid_pr[,32:46],na.rm = T)
  mua_global_sum_pr[20:34,5+iregion] = ascat_sum
  
  mua_wsfevo_region_grid_pr = subset(mua_data_in, mua_data_in$region_1 == region_list[iregion])
  wsfevo_sum = colSums(mua_wsfevo_region_grid_pr[,102:124],na.rm = T)
  mua_global_sum_pr[35:57,5+iregion] = wsfevo_sum / num_region_grids/ 100  # convert total grid cell percent to mean grid cell fraction
  
}

# Beijing
mua_beijing_grid_pr = subset(mua_global_grid_pr, mua_global_grid_pr$mua_aggname == 'Beijing+')
num_grids_beijing = dim(mua_beijing_grid_pr)[1]

ers_sum = colSums(mua_beijing_grid_pr[,13:20],na.rm = T)
mua_global_sum_pr[1:8,18] = ers_sum

qscat_sum = colSums(mua_beijing_grid_pr[,21:31],na.rm = T)
mua_global_sum_pr[9:19,18] = qscat_sum

ascat_sum = colSums(mua_beijing_grid_pr[,32:46],na.rm = T)
mua_global_sum_pr[20:34,18] = ascat_sum

mua_wsfevo_beijing_grid_pr = subset(mua_data_in, mua_data_in$mua_aggname == 'Beijing+')
wsfevo_sum = colSums(mua_wsfevo_beijing_grid_pr[,102:124],na.rm = T)
mua_global_sum_pr[35:57,18] = wsfevo_sum / num_grids_beijing / 100  # convert total grid cell percent to mean grid cell fraction

# Shanghai
mua_shanghai_grid_pr = subset(mua_global_grid_pr, mua_global_grid_pr$mua_aggname == 'Shanghai+')
num_grids_shanghai = dim(mua_shanghai_grid_pr)[1]

ers_sum = colSums(mua_shanghai_grid_pr[,13:20],na.rm = T)
mua_global_sum_pr[1:8,19] = ers_sum

qscat_sum = colSums(mua_shanghai_grid_pr[,21:31],na.rm = T)
mua_global_sum_pr[9:19,19] = qscat_sum

ascat_sum = colSums(mua_shanghai_grid_pr[,32:46],na.rm = T)
mua_global_sum_pr[20:34,19] = ascat_sum

mua_wsfevo_shanghai_grid_pr = subset(mua_data_in, mua_data_in$mua_aggname == 'Shanghai+')
wsfevo_sum = colSums(mua_wsfevo_shanghai_grid_pr[,102:124],na.rm = T)
mua_global_sum_pr[35:57,19] = wsfevo_sum / num_grids_shanghai / 100  # convert total grid cell percent to mean grid cell fraction


#####################################################################################################################
# plots of scatterometer PR vs. floor area (china, global, 2_economies) and vs. time (by region)

# 1. make array for global floor area vs. backscatter PR plot

floor_v_backscatter = mua_global_sum_pr[,1:3]
floor_v_bf = mua_global_sum_pr[,1:3]

floor_v_backscatter$IEA_floor_area_Bm2 = NA
floor_v_backscatter[10:19,4] = floor_area_data[16:25,2]  # QSCAT 2000-2009
floor_v_backscatter[20:34,4] = floor_area_data[23:37,2]  # ASCAT 2007-2021

floor_v_bf$IEA_floor_area_Bm2 = NA
floor_v_bf[42:57,4] = floor_area_data[16:31,2]  # wsfevo 2000-2015

#remove NA rows and plot
floor_v_backscatter = na.omit(floor_v_backscatter)

p1 = ggplot(floor_v_backscatter, aes(x=IEA_floor_area_Bm2,y=global))  +
  geom_point(aes(color = sensor)) + theme_bw() + theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6), 
    legend.title = element_text(size=4), legend.text = element_text(size=4)) + 
    theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid')) +
    ggtitle('Global urban  backscatter vs. total floor area (2000-2021)') + 
    xlab('global floor area (billion m2)') + ylab('global MUA total backscatter PR') + theme(
      plot.title = element_text(color="black", size=7),
      axis.title.x = element_text(color="black", size=8),
      axis.title.y = element_text(color="black", size=8)
    ) + theme(
      legend.key.size = unit(0.25, "cm")
    )

p1

floor_v_bf = na.omit(floor_v_bf)

p1bf = ggplot(floor_v_bf, aes(x=IEA_floor_area_Bm2,y=global/34880/100))  +   # /34880 to get grid cell mean; /100 for % to fraction
  geom_point(aes(color = sensor)) + theme_bw() + theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6), 
    legend.title = element_text(size=4), legend.text = element_text(size=4)) + 
  theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid')) +
  ggtitle('Global urban  built fraction vs. total floor area (2000-2015)') + 
  xlab('global floor area (billion m2)') + ylab('mean global MUA built frac') + theme(
    plot.title = element_text(color="black", size=7),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
  ) + theme(
    legend.key.size = unit(0.25, "cm")
  )

p1bf

# 2. make array for econ floor area vs. backscatter PR plot

econ_floor_v_backscatter = mua_global_sum_pr[,c(1,2,4,5)]
econ_floor_v_bf = mua_global_sum_pr[,c(1,2,4,5)]

econ_floor_v_backscatter$IEA_adv_econ_floor_area_Bm2 = NA  # column 5
econ_floor_v_backscatter$IEA_other_econ_floor_area_Bm2 = NA  # column 6
econ_floor_v_backscatter[23:34,5] = floor_area_data[26:37,3]  # IEA adv. econ
econ_floor_v_backscatter[23:34,6] = floor_area_data[26:37,4]  # IEA other econ
econ_floor_v_bf$IEA_adv_econ_floor_area_Bm2 = NA  # column 5
econ_floor_v_bf$IEA_other_econ_floor_area_Bm2 = NA  # column 6
econ_floor_v_bf[52:57,5] = floor_area_data[26:31,3]  # IEA adv. econ
econ_floor_v_bf[52:57,6] = floor_area_data[26:31,4]  # IEA other econ

#remove NA rows and plot
econ_floor_v_backscatter = na.omit(econ_floor_v_backscatter)

num_values = dim(econ_floor_v_backscatter)[1]

econ_floor_v_backscatter_plot = as.data.frame(array(NA,c((2*num_values),3)))
colnames(econ_floor_v_backscatter_plot) = c('economy','ASCAT_PR','IEA_floor_area_Bm2')

econ_floor_v_backscatter_plot[1:num_values,1] = 'advanced'
econ_floor_v_backscatter_plot[1:num_values,2] = econ_floor_v_backscatter[,3]
econ_floor_v_backscatter_plot[1:num_values,3] = econ_floor_v_backscatter[,5]

econ_floor_v_backscatter_plot[(1+num_values):(2*num_values),1] = 'other'
econ_floor_v_backscatter_plot[(1+num_values):(2*num_values),2] = econ_floor_v_backscatter[,4]
econ_floor_v_backscatter_plot[(1+num_values):(2*num_values),3] = econ_floor_v_backscatter[,6]

p2 = ggplot(econ_floor_v_backscatter_plot, aes(x=IEA_floor_area_Bm2,y=ASCAT_PR, color = economy)) +
  geom_point() + theme_bw() + theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6), 
    legend.title = element_text(size=4), legend.text = element_text(size=4)) + 
  theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid'))  +
  ggtitle('Urban backscatter vs. total floor area (2010, 2015, 2019-21)') + 
  xlab('global floor area (billion m2)') + ylab('global total MUA ASCAT PR') + theme(
    plot.title = element_text(color="black", size=7),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
  ) + theme(
    legend.key.size = unit(0.25, "cm")
  )

p2

# now built fraction
econ_floor_v_bf = na.omit(econ_floor_v_bf)

num_values = dim(econ_floor_v_bf)[1]

econ_floor_v_bf_plot = as.data.frame(array(NA,c((2*num_values),3)))
colnames(econ_floor_v_bf_plot) = c('economy','wsf_evo','IEA_floor_area_Bm2')

econ_floor_v_bf_plot[1:num_values,1] = 'advanced'
econ_floor_v_bf_plot[1:num_values,2] = econ_floor_v_bf[,3]
econ_floor_v_bf_plot[1:num_values,3] = econ_floor_v_bf[,5]

econ_floor_v_bf_plot[(1+num_values):(2*num_values),1] = 'other'
econ_floor_v_bf_plot[(1+num_values):(2*num_values),2] = econ_floor_v_bf[,4]
econ_floor_v_bf_plot[(1+num_values):(2*num_values),3] = econ_floor_v_bf[,6]

econ_floor_v_bf_plot[1:2,2] = econ_floor_v_bf_plot[1:2,2] / num_advanced /100  # convert from grid cell sum of percentages to mean fraction
econ_floor_v_bf_plot[3:4,2] = econ_floor_v_bf_plot[3:4,2] / num_other /100  # convert from grid cell sum of percentages to mean fraction

p2bf = ggplot(econ_floor_v_bf_plot, aes(x=IEA_floor_area_Bm2,y=wsf_evo, color = economy)) +
  geom_point() + theme_bw() + theme(
    legend.position = c(0.7, 0.98),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6), 
    legend.title = element_text(size=4), legend.text = element_text(size=4)) + 
  theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid'))  +
  ggtitle('Urban built fraction vs. total floor area (2010, 2015)') + 
  xlab('global floor area (billion m2)') + ylab('global mean MUA built frac') + theme(
    plot.title = element_text(color="black", size=7),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
  ) + theme(
    legend.key.size = unit(0.25, "cm")
  )

p2bf

# 3. make array for China floor area vs. backscatter PR plot

china_floor_v_backscatter = mua_global_sum_pr[,c(1,2,8)]

china_floor_v_bf = mua_global_sum_pr[,c(1,2,8)]

china_floor_v_backscatter$china_stat_floor_area_increase_Bm2_y = NA  # column 4
china_floor_v_backscatter[1:8,4] = floor_area_data[9:16,6]  # China annual floor area increase, ERS 1993-2000
china_floor_v_backscatter[9:19,4] = floor_area_data[15:25,6]  # China annual floor area increase, QSCAT 2000-2009
china_floor_v_backscatter[20:34,4] = floor_area_data[23:37,6]  # China annual floor area increase, ASCAT 2007-2021

china_floor_v_bf$china_stat_floor_area_increase_Bm2_y = NA  # column 4
china_floor_v_bf[35:57,4] = floor_area_data[9:31,6]  # China annual floor area increase, wsfevo 1993-2015

#remove NA rows and plot
china_floor_v_backscatter = na.omit(china_floor_v_backscatter)

p3 = ggplot(china_floor_v_backscatter, aes(x=china_stat_floor_area_increase_Bm2_y,y=China, color = sensor)) +
  geom_point() + theme_bw() + theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6), 
    legend.title = element_text(size=4), legend.text = element_text(size=4)) + 
  theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid')) + 
  ggtitle('China urban backscatter vs. total floor area increase (1993-2015)') + 
  xlab('accum China floor area since 1984 (billion m2)') + ylab('China total MUA backscatter PR') + theme(
    plot.title = element_text(color="black", size=7),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
  ) + theme(
    legend.key.size = unit(0.25, "cm")
  )

p3

#remove NA rows and plot - built fraction
china_floor_v_bf = na.omit(china_floor_v_bf)

p3bf = ggplot(china_floor_v_bf, aes(x=china_stat_floor_area_increase_Bm2_y,y=China, color = sensor)) +
  geom_point() + theme_bw() + theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6), 
    legend.title = element_text(size=4), legend.text = element_text(size=4)) + 
  theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid')) + 
  ggtitle('China urban built fraction vs. total floor area increase (1993-2021)') + 
  xlab('accumulating China total floor area since 1984 (billion m2)') + ylab('China mean MUA built frac') + theme(
    plot.title = element_text(color="black", size=7),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
  ) + theme(
    legend.key.size = unit(0.25, "cm")
  )

p3bf


# 4. make array for Beijing floor area vs. backscatter PR plot

beijing_floor_v_backscatter = mua_global_sum_pr[,c(1,2,18)]
beijing_floor_v_bf = mua_global_sum_pr[,c(1,2,18)]

beijing_floor_v_backscatter$beijing_stat_floor_area_increase_Mm2_y = NA  # column 4
beijing_floor_v_backscatter[1:8,4] = floor_area_data[9:16,7]  # Beijing annual floor area increase, ERS 1993-2000
beijing_floor_v_backscatter[9:19,4] = floor_area_data[15:25,7]  # Beijing annual floor area increase, QSCAT 2000-2009
beijing_floor_v_backscatter[20:34,4] = floor_area_data[23:37,7]  # Beijing annual floor area increase, ASCAT 2007-2021

beijing_floor_v_bf$beijing_stat_floor_area_increase_Mm2_y = NA  # column 4
beijing_floor_v_bf[35:57,4] = floor_area_data[9:31,7]  # Beijing annual floor area increase, wsfevo 1993-2015

#remove NA rows and plot
beijing_floor_v_backscatter = na.omit(beijing_floor_v_backscatter)

p4 = ggplot(beijing_floor_v_backscatter, aes(x=beijing_stat_floor_area_increase_Mm2_y,y=Beijing, color = sensor)) +
  geom_point() + theme_bw() + theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6), 
    legend.title = element_text(size=4), legend.text = element_text(size=4)) + 
  theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid')) + 
  ggtitle('Beijing+ backscatter vs. Beijing floor area increase (1993-2020)') + 
  xlab('accumulating Beijing total floor area since 1989 (million m2)') + ylab('Beijing+ total MUA backscatter PR') + theme(
    plot.title = element_text(color="black", size=7),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
  ) + theme(
    legend.key.size = unit(0.25, "cm")
  )

p4

#remove NA rows and plot - built fraction
beijing_floor_v_bf = na.omit(beijing_floor_v_bf)

p4bf = ggplot(beijing_floor_v_bf, aes(x=beijing_stat_floor_area_increase_Mm2_y,y=Beijing, color = sensor)) +
  geom_point() + theme_bw() + theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6), 
    legend.title = element_text(size=4), legend.text = element_text(size=4)) + 
  theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid')) + 
  ggtitle('Beijing+ built fraction vs. Beijing floor area increase (1993-2020)') + 
  xlab('accumulating Beijing total floor area since 1989 (million m2)') + ylab('Beijing+ MUA grid cell mean built fraction') + theme(
    plot.title = element_text(color="black", size=7),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
  ) + theme(
    legend.key.size = unit(0.25, "cm")
  )

p4bf

# 5. make array for Shanghai floor area vs. backscatter PR plot

shanghai_floor_v_backscatter = mua_global_sum_pr[,c(1,2,19)]
shanghai_floor_v_bf = mua_global_sum_pr[,c(1,2,19)]

shanghai_floor_v_backscatter$shanghai_stat_floor_area_increase_Mm2_y = NA  # column 4
shanghai_floor_v_backscatter[1:8,4] = floor_area_data[9:16,8]  # Shanghai annual floor area increase, ERS 1993-2000
shanghai_floor_v_backscatter[9:19,4] = floor_area_data[15:25,8]  # Shanghai annual floor area increase, QSCAT 2000-2009
shanghai_floor_v_backscatter[20:34,4] = floor_area_data[23:37,8]  # Shanghai annual floor area increase, ASCAT 2007-2021

shanghai_floor_v_bf$shanghai_stat_floor_area_increase_Mm2_y = NA  # column 4
shanghai_floor_v_bf[35:57,4] = floor_area_data[9:31,8]  # Shanghai annual floor area increase, wsfevo 1993-2015

#remove NA rows and plot
shanghai_floor_v_backscatter = na.omit(shanghai_floor_v_backscatter)

p5 = ggplot(shanghai_floor_v_backscatter, aes(x=shanghai_stat_floor_area_increase_Mm2_y,y=Shanghai, color = sensor)) +
  geom_point() + theme_bw() + theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6), 
    legend.title = element_text(size=4), legend.text = element_text(size=4)
    ) + 
  theme(
    legend.background = element_rect(color = 'black', fill = 'white', linetype='solid')
    ) + 
  ggtitle('Shanghai+ backscatter vs. Shanghai floor area increase (1993-2019)') + 
  xlab('accumulating Shanghai total floor area since 1977 (million m2)') + ylab('Shanghai+ total MUA backscatter PR') + 
  theme(
    plot.title = element_text(color="black", size=7),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
    ) + theme(
     legend.key.size = unit(0.25, "cm")
     )

p5

#remove NA rows and plot
shanghai_floor_v_bf = na.omit(shanghai_floor_v_bf)

p5bf = ggplot(shanghai_floor_v_bf, aes(x=shanghai_stat_floor_area_increase_Mm2_y,y=Shanghai, color = sensor)) +
  geom_point() + theme_bw() + theme(
    legend.position = c(0.02, 0.98),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6), 
    legend.title = element_text(size=4), legend.text = element_text(size=4)
  ) + 
  theme(
    legend.background = element_rect(color = 'black', fill = 'white', linetype='solid')
  ) + 
  ggtitle('Shanghai+ built fraction vs. Shanghai floor area increase (1993-2019)') + 
  xlab('accumulating Shanghai total floor area since 1977 (million m2)') + ylab('Shanghai+ MUA grid cell mean built fraction') + 
  theme(
    plot.title = element_text(color="black", size=7),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
  ) + theme(
    legend.key.size = unit(0.25, "cm")
  )

p5bf

# put Beijing and Shanghai on same plot 

beijing_shanghai_floor_v_backscatter = beijing_floor_v_backscatter
beijing_shanghai_floor_v_backscatter$city = 'Beijing+'
colnames(beijing_shanghai_floor_v_backscatter) = c('year','sensor','backscatter_PR','floor_area_increase_Mm2_y','city')
temp = shanghai_floor_v_backscatter
temp$city = 'Shanghai+'
colnames(temp) = c('year','sensor','backscatter_PR','floor_area_increase_Mm2_y','city')

beijing_shanghai_floor_v_backscatter = as.data.frame(rbind(beijing_shanghai_floor_v_backscatter, temp))

p6 = ggplot(beijing_shanghai_floor_v_backscatter, aes(x=as.numeric(floor_area_increase_Mm2_y),
                                                             y=as.numeric(backscatter_PR), color = sensor, shape = city)) +
  geom_point() + theme_bw() + theme(
    legend.position = c(0.85, 0.4),
 #   legend.justification = c("top", "left"),
    legend.box.just = "right",
    legend.margin = margin(2,2,2,2), 
    legend.title = element_text(size=4), legend.text = element_text(size=4)) +
  scale_shape_manual(values = c(25, 16)) +
  theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid')) + 
  ggtitle('Beijing+, Shanghai+ backscatter vs. accum new floor area (1993-2020') + 
  xlab('accumulating new floor area (million m2)') + ylab('total MUA backscatter PR') + 
  theme(
    plot.title = element_text(color="black", size=7),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
  ) + theme(
    legend.key.size = unit(0.25, "cm")
  )

p6

# p6 = p6 + geom_point(aes(x=as.numeric(beijing_stat_floor_area_increase_Mm2_y),
#                                                                y=as.numeric(Beijing), color = sensor, shape = 24))

# put Beijing and Shanghai on same plot  - built fraction

beijing_shanghai_floor_v_bf = beijing_floor_v_bf
beijing_shanghai_floor_v_bf$city = 'Beijing+'
colnames(beijing_shanghai_floor_v_bf) = c('year','sensor','mean_built_fraction','floor_area_increase_Mm2_y','city')
temp = shanghai_floor_v_bf
temp$city = 'Shanghai+'
colnames(temp) = c('year','sensor','mean_built_fraction','floor_area_increase_Mm2_y','city')

beijing_shanghai_floor_v_bf = as.data.frame(rbind(beijing_shanghai_floor_v_bf, temp))

p6bf = ggplot(beijing_shanghai_floor_v_bf, aes(x=as.numeric(floor_area_increase_Mm2_y),
                                                      y=as.numeric(mean_built_fraction), color = sensor, shape = city)) +
  geom_point() + theme_bw() + theme(
    legend.position = c(0.85, 0.35),
    #   legend.justification = c("top", "left"),
    legend.box.just = "right",
    legend.margin = margin(2,2,2,2), 
    legend.title = element_text(size=4), legend.text = element_text(size=4)) +
  scale_shape_manual(values = c(25, 16)) +
  theme(legend.background = element_rect(color = 'black', fill = 'white', linetype='solid')) + 
  ggtitle('Beijing+, Shanghai+ built frac vs. accum. new floor area (1993-2015)') + 
  xlab('accumulating new floor area (million m2)') + ylab(' mean MUA built frac') + 
  theme(
    plot.title = element_text(color="black", size=7),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
  ) + theme(
    legend.key.size = unit(0.25, "cm")
  )

p6bf


pdf(paste(output_dir,'backscatter_&_bf_v_floor_area_plots.pdf',sep=''))

print(
  ggarrange(p1,p1bf,p2,p2bf,p3,p3bf,p6,p6bf,   #
            #    labels = c("A", "B", "C", "D","E"),
            ncol = 2, nrow = 4)
)

dev.off()

######################################################################################################################################################
######################################################################################################################################################
########################################################################################################################

# for 12 cities, 1 per region, plot time series of ers, qscat, ascat, wsf-evo


# 12 cities, 1 per region
city_list = c('Lagos+','Sydney', 'Beijing+','Seoul+','London_*UK*', 'Delhi', 
              'Sao-Paulo', 'Tehran+','Houston+', 'Karachi', 'Moscow', 'Ho-Chi-Minh-City') 

num_cities = length(city_list)


ers_years = 1993:2000
qscat_years = 1999:2009
ascat_years = 2007:2021
wsfevo_years = 1985:2015

mua_plot_grid_timeseries = mua_data_in[c(1:12,34:67,94:124,158,159, 160)]
# Header: 1-6:    longitude	latitude	mua_objectid	mua_fid1	mua_country_code	mua_cellcount	
#         7-12:   mua_worldpop_thousand	mua_area_km2	mua_country	mua_aggname	grid_area_m2 water_pct		
#         13-20:  ERS1993_summer_mean_pr, ERS1994_summer_mean_pr, ERS1995_summer_mean_pr, ERS1996_summer_mean_pr, 
#                 ERS1997_summer_mean_pr, ERS1998_summer_mean_pr, ERS1999_summer_mean_pr, ERS2000_summer_mean_pr, 
#         21-31:  QSCAT1999_summer_mean_pr, QSCAT2000_summer_mean_pr, QSCAT2001_summer_mean_pr, QSCAT2002_summer_mean_pr, 
#                 QSCAT2003_summer_mean_pr, QSCAT2004_summer_mean_pr, QSCAT2005_summer_mean_pr, QSCAT2006_summer_mean_pr, 
#                 QSCAT2007_summer_mean_pr, QSCAT2008_summer_mean_pr, QSCAT2009_summer_mean_pr, 
#         32-46:  ASCAT2007_summer_mean_pr, ASCAT2008_summer_mean_pr, ASCAT2009_summer_mean_pr, ASCAT2010_summer_mean_pr, 
#                 ASCAT2011_summer_mean_pr, ASCAT2012_summer_mean_pr, ASCAT2013_summer_mean_pr, ASCAT2014_summer_mean_pr, 
#                 ASCAT2015_summer_mean_pr, ASCAT2016_summer_mean_pr, ASCAT2017_summer_mean_pr, ASCAT2018_summer_mean_pr,  
#                 ASCAT2019_summer_mean_pr, ASCAT2020_summer_mean_pr, ASCAT2021_summer_mean_pr
#         47:77:  WSFEvolution_1985_mean WSFEvolution_1986_mean WSFEvolution_1987_mean WSFEvolution_1988_mean
#                 WSFEvolution_1989_mean WSFEvolution_1990_mean WSFEvolution_1991_mean WSFEvolution_1992_mean
#                 WSFEvolution_1993_mean WSFEvolution_1994_mean WSFEvolution_1995_mean WSFEvolution_1996_mean
#                 WSFEvolution_1997_mean WSFEvolution_1998_mean WSFEvolution_1999_mean WSFEvolution_2000_mean
#                 WSFEvolution_2001_mean WSFEvolution_2002_mean WSFEvolution_2003_mean WSFEvolution_2004_mean
#                 WSFEvolution_2005_mean WSFEvolution_2006_mean WSFEvolution_2007_mean WSFEvolution_2008_mean
#                 WSFEvolution_2009_mean WSFEvolution_2010_mean WSFEvolution_2011_mean WSFEvolution_2012_mean
#                 WSFEvolution_2013_mean WSFEvolution_2014_mean WSFEvolution_2015_mean
#       78-80:   region_1 economy sub_regions


#  rebuild array for facet-wrap plot of 12 city time series

num_vars = 8+11+15+31 # ERS + QSCAT + ASCAT + WSF-evo

melt_city_timeseries_array = array(NA,c((num_cities*num_vars),4))
colnames(melt_city_timeseries_array) = c('city','dataset','year','value')
                                  
for (icity in 1:num_cities) {
  
  city_name = city_list[icity]
  city_mua_plot_grid_timeseries = subset(mua_plot_grid_timeseries, mua_plot_grid_timeseries$mua_aggname == city_name)
  
  ers_timeseries = colMeans(city_mua_plot_grid_timeseries[,13:20])
  qscat_timeseries = colMeans(city_mua_plot_grid_timeseries[,21:31])
  ascat_timeseries = colMeans(city_mua_plot_grid_timeseries[,32:46])
  wsfevo_timeseries = colMeans(city_mua_plot_grid_timeseries[,47:77])
  
  melt_city_timeseries_array[(((icity - 1)*num_vars)+1):(icity*num_vars),1] = city_name
  melt_city_timeseries_array[(((icity - 1)*num_vars)+1):(((icity - 1)*num_vars)+8),2] = 'ERS'
  melt_city_timeseries_array[(((icity - 1)*num_vars)+1):(((icity - 1)*num_vars)+8),3] = ers_years
  melt_city_timeseries_array[(((icity - 1)*num_vars)+1):(((icity - 1)*num_vars)+8),4] = ers_timeseries
  melt_city_timeseries_array[(((icity - 1)*num_vars)+9):(((icity - 1)*num_vars)+19),2] = 'QSCAT'
  melt_city_timeseries_array[(((icity - 1)*num_vars)+9):(((icity - 1)*num_vars)+19),3] = qscat_years
  melt_city_timeseries_array[(((icity - 1)*num_vars)+9):(((icity - 1)*num_vars)+19),4] = qscat_timeseries
  melt_city_timeseries_array[(((icity - 1)*num_vars)+20):(((icity - 1)*num_vars)+34),2] = 'ASCAT'
  melt_city_timeseries_array[(((icity - 1)*num_vars)+20):(((icity - 1)*num_vars)+34),3] = ascat_years
  melt_city_timeseries_array[(((icity - 1)*num_vars)+20):(((icity - 1)*num_vars)+34),4] = ascat_timeseries
  melt_city_timeseries_array[(((icity - 1)*num_vars)+35):(((icity - 1)*num_vars)+65),2] = 'WSF-evo'
  melt_city_timeseries_array[(((icity - 1)*num_vars)+35):(((icity - 1)*num_vars)+65),3] = wsfevo_years
  melt_city_timeseries_array[(((icity - 1)*num_vars)+35):(((icity - 1)*num_vars)+65),4] = wsfevo_timeseries/100  # convert % to fraction
  
}

melt_city_timeseries_array = as.data.frame(melt_city_timeseries_array)

melt_city_timeseries_array$city <- factor(melt_city_timeseries_array$city, levels=c('Lagos+','Sydney', 'Beijing+','Seoul+',
                                                                                    'London_*UK*', 'Delhi', 
                                                                                    'Sao-Paulo', 'Tehran+','Houston+', 
                                                                                    'Karachi', 'Moscow', 'Ho-Chi-Minh-City'))

pdf(paste(output_dir,'mua_mean_timeseries_PR_WSFevo.pdf',sep=''))

p111 = ggplot(melt_city_timeseries_array, aes(x=as.numeric(year), y=as.numeric(value), group = dataset)) +
  geom_point(aes(color=dataset)) + geom_line(aes(color=dataset)) + theme_bw() + 
  scale_x_continuous(limits = c(1990,2022)) + scale_y_continuous(limits = c(0, 0.75)) +
  
#  xlim(1990,2022) + ylim(0,1) +
  
  labs(x="year", y = "backscatter PR or built fraction") + 
  # theme(legend.position = "none") +
  # geom_segment(aes(x=del_wsf_bf_93_00, y=ers_slope, xend=del_wsf_bf_99_09, yend=qscat_slope, col = 'red'), 
  #              arrow = arrow(length=unit(0.2, 'cm'))) +
  # geom_segment(aes(x=del_wsf_bf_99_09, y=qscat_slope, xend=del_wsf_bf_10_15, yend=ascat_slope, col = 'blue'), 
  #              arrow = arrow(length=unit(0.2, 'cm'))) +
  facet_wrap(~city,  ncol=3)

print(p111)

dev.off()

########################################################################################################################
# repeat time series plots for regions

melt_region_timeseries_array = array(NA,c((num_regions*num_vars),4))
colnames(melt_region_timeseries_array) = c('region','dataset','year','value')

for (iregion in 1:num_regions) {
  
  region_name = region_list[iregion]
  region_mua_plot_grid_timeseries = subset(mua_plot_grid_timeseries, mua_plot_grid_timeseries$region_1 == region_name)
  
  ers_timeseries = colMeans(region_mua_plot_grid_timeseries[,13:20])
  qscat_timeseries = colMeans(region_mua_plot_grid_timeseries[,21:31])
  ascat_timeseries = colMeans(region_mua_plot_grid_timeseries[,32:46])
  wsfevo_timeseries = colMeans(region_mua_plot_grid_timeseries[,47:77])
  
  melt_region_timeseries_array[(((iregion - 1)*num_vars)+1):(iregion*num_vars),1] = region_name
  melt_region_timeseries_array[(((iregion - 1)*num_vars)+1):(((iregion - 1)*num_vars)+8),2] = 'ERS'
  melt_region_timeseries_array[(((iregion - 1)*num_vars)+1):(((iregion - 1)*num_vars)+8),3] = ers_years
  melt_region_timeseries_array[(((iregion - 1)*num_vars)+1):(((iregion - 1)*num_vars)+8),4] = ers_timeseries
  melt_region_timeseries_array[(((iregion - 1)*num_vars)+9):(((iregion - 1)*num_vars)+19),2] = 'QSCAT'
  melt_region_timeseries_array[(((iregion - 1)*num_vars)+9):(((iregion - 1)*num_vars)+19),3] = qscat_years
  melt_region_timeseries_array[(((iregion - 1)*num_vars)+9):(((iregion - 1)*num_vars)+19),4] = qscat_timeseries
  melt_region_timeseries_array[(((iregion - 1)*num_vars)+20):(((iregion - 1)*num_vars)+34),2] = 'ASCAT'
  melt_region_timeseries_array[(((iregion - 1)*num_vars)+20):(((iregion - 1)*num_vars)+34),3] = ascat_years
  melt_region_timeseries_array[(((iregion - 1)*num_vars)+20):(((iregion - 1)*num_vars)+34),4] = ascat_timeseries
  melt_region_timeseries_array[(((iregion - 1)*num_vars)+35):(((iregion - 1)*num_vars)+65),2] = 'WSF-evo'
  melt_region_timeseries_array[(((iregion - 1)*num_vars)+35):(((iregion - 1)*num_vars)+65),3] = wsfevo_years
  melt_region_timeseries_array[(((iregion - 1)*num_vars)+35):(((iregion - 1)*num_vars)+65),4] = wsfevo_timeseries/100  # convert % to fraction
  
}

melt_region_timeseries_array = as.data.frame(melt_region_timeseries_array)

pdf(paste(output_dir,'region_mean_timeseries_PR_WSFevo.pdf',sep=''))

p112 = ggplot(melt_region_timeseries_array, aes(x=as.numeric(year), y=as.numeric(value), group = dataset)) +
  geom_point(aes(color=dataset)) + geom_line(aes(color=dataset)) + theme_bw() + 
  scale_x_continuous(limits = c(1990,2022)) + scale_y_continuous(limits = c(0, 0.75)) +
  
  #  xlim(1990,2022) + ylim(0,1) +
  
  labs(x="year", y = "backscatter PR or built fraction") + 
  # theme(legend.position = "none") +
  # geom_segment(aes(x=del_wsf_bf_93_00, y=ers_slope, xend=del_wsf_bf_99_09, yend=qscat_slope, col = 'red'), 
  #              arrow = arrow(length=unit(0.2, 'cm'))) +
  # geom_segment(aes(x=del_wsf_bf_99_09, y=qscat_slope, xend=del_wsf_bf_10_15, yend=ascat_slope, col = 'blue'), 
  #              arrow = arrow(length=unit(0.2, 'cm'))) +
  facet_wrap(~region,  ncol=3)

print(p112)

dev.off()

################################################################################################################################################
# make time series plot for 3 syrian cities

years = 2007:2020
num_years = length(years)

syria_array_plot = as.data.frame(array(NA,c(3*num_years,4)))
colnames(syria_array_plot) = c('years','city','ASCAT_PR', 'WSFevo_BF')

damascus_subset = subset(mua_data_in, mua_data_in$mua_aggname == 'Damascus')  
aleppo_subset = subset(mua_data_in, mua_data_in$mua_aggname == 'Aleppo')  
homs_subset = subset(mua_data_in, mua_data_in$mua_aggname == 'Homs')  

for (iyear in 1:num_years) {
  
  syria_array_plot[iyear,1] = 2006+iyear
  syria_array_plot[iyear,2] = 'Damascus'
  syria_array_plot[iyear,3] = mean(damascus_subset[,(52+iyear)],na.rm = T) / mean(damascus_subset[,(53)],na.rm = T)  # ASCAT 2007-2021
  if (iyear < 10) {
    syria_array_plot[iyear,4] = mean(damascus_subset[,(115+iyear)],na.rm = T) / mean(damascus_subset[,(116)],na.rm = T)   # WSFevo 2007-2021
  }
  
  syria_array_plot[(num_years + iyear),1] = 2006+iyear
  syria_array_plot[(num_years + iyear),2] = 'Aleppo'
  syria_array_plot[(num_years + iyear),3] = mean(aleppo_subset[,(52+iyear)],na.rm = T) / mean(aleppo_subset[,(53)],na.rm = T)   # ASCAT 2007-2021
  if (iyear < 10) {
    syria_array_plot[(num_years + iyear),4] = mean(aleppo_subset[,(115+iyear)],na.rm = T) / mean(aleppo_subset[,(116)],na.rm = T)   # WSFevo 2007-2021
  }
  
  syria_array_plot[(2*num_years + iyear),1] = 2006+iyear
  syria_array_plot[(2*num_years + iyear),2] = 'Homs'
  syria_array_plot[(2*num_years + iyear),3] = mean(homs_subset[,(52+iyear)],na.rm = T) / mean(homs_subset[,(53)],na.rm = T)  # ASCAT 2007-2021
  if (iyear < 10) {
    syria_array_plot[(2*num_years + iyear),4] = mean(homs_subset[,(115+iyear)],na.rm = T) / mean(homs_subset[,(116)],na.rm = T)  # WSFevo 2007-2021
  }
  
}

color_array = c('black','red','blue')

p31 = ggplot(syria_array_plot, aes(x=years)) +
  geom_line(aes(y=ASCAT_PR, color = city), linetype=2,size=1.3) + 
  geom_line(aes(y=WSFevo_BF, color = city),  size=1.3) +
  theme_bw() + 
  theme(legend.position="bottom") +
  labs(title = paste(''),x="", y = "normalized PR or BF") + 
  coord_cartesian(ylim = c(0.85, 1.1))  

p31

# compute MUA means

pdf(paste(output_dir,'syria_city_timeseries','.pdf', sep = ''))

print(
  ggarrange(p31,   #
            #              labels = c("A", "B", "C", "D"),
            ncol = 1, nrow = 1)
)

dev.off()

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

######################################################################################################################################################
# now make plots of just the mean arrows for the 12 cities and the 12 regions and the 2 economies

# NOTE: v3: compute intercalibrated ascat and ers decacal growth rates as
#     ASCAT: rate_intercal = rate * (mean QSCAT PR) / (mean ASCAT PR) for 2007-2009
#     ERS Northern hemisphere: rate_intercal = rate * (mean QSCAT PR) / (mean ERS PR) for 1999-2001 for QSCAT and 1998-2000 for ERS 
#     ERS Southern hemisphere: rate_intercal = rate * (mean QSCAT PR) / (mean ERS PR) for 2000-2002 for QSCAT and 1998-2000 for ERS Northern hemisphere
#
for (icity in 1:num_cities) {
  
  cityname = city_list[icity]
  city_mua_plot_grid_pr = subset(mua_global_grid_pr, mua_global_grid_pr$mua_aggname == cityname)
  
  # V3: add columns for intercalibrated ERS and QSCAT slopes
  
  if (city_mua_plot_grid_pr[1,2] >= 0) {   # city in northern hemisphere
    city_mua_plot_grid_pr$ers_slope_intercal = city_mua_plot_grid_pr$ers_slope * 
      (city_mua_plot_grid_pr$QSCAT1999_summer_mean_pr + city_mua_plot_grid_pr$QSCAT2000_summer_mean_pr + city_mua_plot_grid_pr$QSCAT2001_summer_mean_pr) / 
      (city_mua_plot_grid_pr$ERS1998_summer_mean_pr + city_mua_plot_grid_pr$ERS1999_summer_mean_pr + city_mua_plot_grid_pr$ERS2000_summer_mean_pr)
  } else {   # city in southern hemisphere
    city_mua_plot_grid_pr$ers_slope_intercal = city_mua_plot_grid_pr$ers_slope * 
      (city_mua_plot_grid_pr$QSCAT2000_summer_mean_pr + city_mua_plot_grid_pr$QSCAT2001_summer_mean_pr + city_mua_plot_grid_pr$QSCAT2002_summer_mean_pr) / 
      (city_mua_plot_grid_pr$ERS1998_summer_mean_pr + city_mua_plot_grid_pr$ERS1999_summer_mean_pr + city_mua_plot_grid_pr$ERS2000_summer_mean_pr)
  }
  
  city_mua_plot_grid_pr$ascat_10_21_slope_intercal = city_mua_plot_grid_pr$ascat_10_21_slope * 
    (city_mua_plot_grid_pr$QSCAT2007_summer_mean_pr + city_mua_plot_grid_pr$QSCAT2008_summer_mean_pr + city_mua_plot_grid_pr$QSCAT2009_summer_mean_pr) / 
    (city_mua_plot_grid_pr$ASCAT2007_summer_mean_pr + city_mua_plot_grid_pr$ASCAT2008_summer_mean_pr + city_mua_plot_grid_pr$ASCAT2009_summer_mean_pr)
  
  # compute means
  
  melt_mean_city_growth_rate_arrows = as.data.frame(array(NA,c(2,6)))
  colnames(melt_mean_city_growth_rate_arrows) = c('city','del_era','x1','y1','x2','y2')
  
  melt_mean_city_growth_rate_arrows[,1] = cityname
  
  melt_mean_city_growth_rate_arrows[1,2] = '1990s -> 2000s'
  melt_mean_city_growth_rate_arrows[1,3] = mean(city_mua_plot_grid_pr$del_wsf_bf_93_00/100, na.rm = T)
  #  melt_mean_city_growth_rate_arrows[1,4] = mean(city_mua_plot_grid_pr$ers_slope, na.rm = T)
  melt_mean_city_growth_rate_arrows[1,4] = mean(city_mua_plot_grid_pr$ers_slope_intercal, na.rm = T)
  melt_mean_city_growth_rate_arrows[1,5] = mean(city_mua_plot_grid_pr$del_wsf_bf_99_09/100, na.rm = T)
  melt_mean_city_growth_rate_arrows[1,6] = mean(city_mua_plot_grid_pr$qscat_slope, na.rm = T)
  
  melt_mean_city_growth_rate_arrows[2,2] = '2000s -> 2010s'
  melt_mean_city_growth_rate_arrows[2,3] = mean(city_mua_plot_grid_pr$del_wsf_bf_99_09/100, na.rm = T)
  melt_mean_city_growth_rate_arrows[2,4] = mean(city_mua_plot_grid_pr$qscat_slope, na.rm = T)
  melt_mean_city_growth_rate_arrows[2,5] = mean(city_mua_plot_grid_pr$del_wsf_bf_10_15/100, na.rm = T)
  #  melt_mean_city_growth_rate_arrows[2,6] = mean(city_mua_plot_grid_pr$ascat_10_21_slope, na.rm = T)
  melt_mean_city_growth_rate_arrows[2,6] = mean(city_mua_plot_grid_pr$ascat_10_21_slope_intercal, na.rm = T)
  
  
  if (icity == 1) {
    melt_12_mean_city_growth_rate_arrows = melt_mean_city_growth_rate_arrows
  } else {
    melt_12_mean_city_growth_rate_arrows = rbind(melt_12_mean_city_growth_rate_arrows, melt_mean_city_growth_rate_arrows)
  }
  
}

melt_12_mean_city_growth_rate_arrows = as.data.frame(melt_12_mean_city_growth_rate_arrows)

melt_12_mean_city_growth_rate_arrows$city <- factor(melt_12_mean_city_growth_rate_arrows$city, levels=c('Lagos+','Sydney', 'Beijing+','Seoul+',
                                                                                                        'London_*UK*', 'Delhi', 
                                                                                                        'Sao-Paulo', 'Tehran+','Houston+', 
                                                                                                        'Karachi', 'Moscow', 'Ho-Chi-Minh-City'))

pdf(paste(output_dir,'mua_mean_growth_rate_PR_WSFevo.pdf',sep=''))

p301 = ggplot(melt_12_mean_city_growth_rate_arrows, aes(x=as.numeric(x1),y=as.numeric(y1),group = del_era)) +
  geom_point() + geom_point(aes(x=as.numeric(x2),y=as.numeric(y2),group = del_era)) + 
  # geom_point(aes(color=era)) + theme_bw() + 
  # scale_size_manual(values=c(0.01,0.01,0.01)) +
  scale_x_continuous(limits = c(0,0.018)) + scale_y_continuous(limits = c(-0.002, 0.015)) +
  labs(x="annual increase in WSF-evo buit fraction", y = "annual increase in backscatter PR_intercal") + 
  
  geom_segment(aes(x=as.numeric(x1), y=as.numeric(y1), xend=as.numeric(x2), yend=as.numeric(y2), color = del_era), 
               arrow = arrow(length=unit(0.25, 'cm')), lwd = 1) + theme_bw() + 
  # 
  # # geom_segment(aes(x=as.numeric(mean_x1), y=as.numeric(mean_y1), 
  #                 xend=as.numeric(mean_x2), yend=as.numeric(mean_y2), color = era),
  #             arrow = arrow(length=unit(0.5, 'cm')), lwd = 1) +
  # 
  facet_wrap(~city,  ncol=3)

print(p301)

dev.off()

# now for 12 regions
for (iregion in 1:num_regions) {
  
  region_name = region_list[iregion]
  region_mua_plot_grid_pr = subset(mua_global_grid_pr, mua_global_grid_pr$region_1 == region_name)
  
  # V3: add columns for intercalibrated ERS and QSCAT slopes
  
  if (min(region_mua_plot_grid_pr$latitude ) >= 0) {   # region in northern hemisphere
    
    region_mua_plot_grid_pr$ers_slope_intercal = region_mua_plot_grid_pr$ers_slope * 
      (region_mua_plot_grid_pr$QSCAT1999_summer_mean_pr + region_mua_plot_grid_pr$QSCAT2000_summer_mean_pr + region_mua_plot_grid_pr$QSCAT2001_summer_mean_pr) / 
      (region_mua_plot_grid_pr$ERS1998_summer_mean_pr + region_mua_plot_grid_pr$ERS1999_summer_mean_pr + region_mua_plot_grid_pr$ERS2000_summer_mean_pr)
    
  } else if (max(region_mua_plot_grid_pr$latitude ) < 0) {   # region in southern hemisphere
    
    region_mua_plot_grid_pr$ers_slope_intercal = region_mua_plot_grid_pr$ers_slope * 
      (region_mua_plot_grid_pr$QSCAT2000_summer_mean_pr + region_mua_plot_grid_pr$QSCAT2001_summer_mean_pr + region_mua_plot_grid_pr$QSCAT2002_summer_mean_pr) / 
      (region_mua_plot_grid_pr$ERS1998_summer_mean_pr + region_mua_plot_grid_pr$ERS1999_summer_mean_pr + region_mua_plot_grid_pr$ERS2000_summer_mean_pr)
    
  } else {  # region spans across northern and southern hemisphere
    
    north_subset = subset(region_mua_plot_grid_pr, region_mua_plot_grid_pr$latitude >= 0)
    south_subset = subset(region_mua_plot_grid_pr, region_mua_plot_grid_pr$latitude < 0)
    
    north_subset$ers_slope_intercal = north_subset$ers_slope * 
      (north_subset$QSCAT1999_summer_mean_pr + north_subset$QSCAT2000_summer_mean_pr + north_subset$QSCAT2001_summer_mean_pr) / 
      (north_subset$ERS1998_summer_mean_pr + north_subset$ERS1999_summer_mean_pr + north_subset$ERS2000_summer_mean_pr)
    
    south_subset$ers_slope_intercal = south_subset$ers_slope * 
      (south_subset$QSCAT1999_summer_mean_pr + south_subset$QSCAT2000_summer_mean_pr + south_subset$QSCAT2001_summer_mean_pr) / 
      (south_subset$ERS1998_summer_mean_pr + south_subset$ERS1999_summer_mean_pr + south_subset$ERS2000_summer_mean_pr)
    
    region_mua_plot_grid_pr = rbind(north_subset,south_subset)
    
  }
  
  region_mua_plot_grid_pr$ascat_10_21_slope_intercal = region_mua_plot_grid_pr$ascat_10_21_slope * 
    (region_mua_plot_grid_pr$QSCAT2007_summer_mean_pr + region_mua_plot_grid_pr$QSCAT2008_summer_mean_pr + region_mua_plot_grid_pr$QSCAT2009_summer_mean_pr) / 
    (region_mua_plot_grid_pr$ASCAT2007_summer_mean_pr + region_mua_plot_grid_pr$ASCAT2008_summer_mean_pr + region_mua_plot_grid_pr$ASCAT2009_summer_mean_pr)
  
  # compute means
  
  
  melt_region_growth_rate_arrows = as.data.frame(array(NA,c(2,6)))
  colnames(melt_region_growth_rate_arrows) = c('region','del_era','x1','y1','x2','y2')
  
  melt_region_growth_rate_arrows[,1] = region_name
  
  melt_region_growth_rate_arrows[1,2] = '1990s -> 2000s'
  melt_region_growth_rate_arrows[1,3] = mean(region_mua_plot_grid_pr$del_wsf_bf_93_00/100, na.rm = T)
  #   melt_region_growth_rate_arrows[1,4] = mean(region_mua_plot_grid_pr$ers_slope, na.rm = T)
  melt_region_growth_rate_arrows[1,4] = mean(region_mua_plot_grid_pr$ers_slope_intercal, na.rm = T)
  melt_region_growth_rate_arrows[1,5] = mean(region_mua_plot_grid_pr$del_wsf_bf_99_09/100, na.rm = T)
  melt_region_growth_rate_arrows[1,6] = mean(region_mua_plot_grid_pr$qscat_slope, na.rm = T)
  
  melt_region_growth_rate_arrows[2,2] = '2000s -> 2010s'
  melt_region_growth_rate_arrows[2,3] = mean(region_mua_plot_grid_pr$del_wsf_bf_99_09/100, na.rm = T)
  melt_region_growth_rate_arrows[2,4] = mean(region_mua_plot_grid_pr$qscat_slope, na.rm = T)
  melt_region_growth_rate_arrows[2,5] = mean(region_mua_plot_grid_pr$del_wsf_bf_10_15/100, na.rm = T)
  #   melt_region_growth_rate_arrows[2,6] = mean(region_mua_plot_grid_pr$ascat_10_21_slope, na.rm = T)
  melt_region_growth_rate_arrows[2,6] = mean(region_mua_plot_grid_pr$ascat_10_21_slope_intercal, na.rm = T)
  
  
  if (iregion == 1) {
    melt_12_region_growth_rate_arrows = melt_region_growth_rate_arrows
  } else {
    melt_12_region_growth_rate_arrows = rbind(melt_12_region_growth_rate_arrows, melt_region_growth_rate_arrows)
  }
  
}


melt_12_region_growth_rate_arrows = as.data.frame(melt_12_region_growth_rate_arrows)

pdf(paste(output_dir,'region_mean_growth_rate_PR_WSFevo.pdf',sep=''))

p401 = ggplot(melt_12_region_growth_rate_arrows, aes(x=as.numeric(x1),y=as.numeric(y1),group = del_era)) +
  geom_point() + geom_point(aes(x=as.numeric(x2),y=as.numeric(y2),group = del_era)) + 
  # geom_point(aes(color=era)) + theme_bw() + 
  # scale_size_manual(values=c(0.01,0.01,0.01)) +
  scale_x_continuous(limits = c(0,0.018)) + scale_y_continuous(limits = c(-0.001, 0.0075)) +
  labs(x="annual increase in WSF-evo buit fraction", y = "annual increase in backscatter PR_intercal") + 
  
  geom_segment(aes(x=as.numeric(x1), y=as.numeric(y1), xend=as.numeric(x2), yend=as.numeric(y2), color = del_era), 
               arrow = arrow(length=unit(0.25, 'cm')), lwd = 1) + theme_bw() + 
  # 
  # # geom_segment(aes(x=as.numeric(mean_x1), y=as.numeric(mean_y1), 
  #                 xend=as.numeric(mean_x2), yend=as.numeric(mean_y2), color = era),
  #             arrow = arrow(length=unit(0.5, 'cm')), lwd = 1) +
  # 
  facet_wrap(~region,  ncol=3)

print(p401)

dev.off()

# now for 7 sub-regions
for (iregion in 1:num_sub_regions) {
  
  subregion_name = sub_region_list[iregion]
  region_mua_plot_grid_pr = subset(mua_global_grid_pr, mua_global_grid_pr$sub_regions == subregion_name)
  
  # V3: add columns for intercalibrated ERS and QSCAT slopes
  
  if (min(region_mua_plot_grid_pr$latitude ) >= 0) {   # region in northern hemisphere
    
    region_mua_plot_grid_pr$ers_slope_intercal = region_mua_plot_grid_pr$ers_slope * 
      (region_mua_plot_grid_pr$QSCAT1999_summer_mean_pr + region_mua_plot_grid_pr$QSCAT2000_summer_mean_pr + region_mua_plot_grid_pr$QSCAT2001_summer_mean_pr) / 
      (region_mua_plot_grid_pr$ERS1998_summer_mean_pr + region_mua_plot_grid_pr$ERS1999_summer_mean_pr + region_mua_plot_grid_pr$ERS2000_summer_mean_pr)
    
  } else if (max(region_mua_plot_grid_pr$latitude ) < 0) {   # region in southern hemisphere
    
    region_mua_plot_grid_pr$ers_slope_intercal = region_mua_plot_grid_pr$ers_slope * 
      (region_mua_plot_grid_pr$QSCAT2000_summer_mean_pr + region_mua_plot_grid_pr$QSCAT2001_summer_mean_pr + region_mua_plot_grid_pr$QSCAT2002_summer_mean_pr) / 
      (region_mua_plot_grid_pr$ERS1998_summer_mean_pr + region_mua_plot_grid_pr$ERS1999_summer_mean_pr + region_mua_plot_grid_pr$ERS2000_summer_mean_pr)
    
  } else {  # region spans across northern and southern hemisphere
    
    north_subset = subset(region_mua_plot_grid_pr, region_mua_plot_grid_pr$latitude >= 0)
    south_subset = subset(region_mua_plot_grid_pr, region_mua_plot_grid_pr$latitude < 0)
    
    north_subset$ers_slope_intercal = north_subset$ers_slope * 
      (north_subset$QSCAT1999_summer_mean_pr + north_subset$QSCAT2000_summer_mean_pr + north_subset$QSCAT2001_summer_mean_pr) / 
      (north_subset$ERS1998_summer_mean_pr + north_subset$ERS1999_summer_mean_pr + north_subset$ERS2000_summer_mean_pr)
    
    south_subset$ers_slope_intercal = south_subset$ers_slope * 
      (south_subset$QSCAT1999_summer_mean_pr + south_subset$QSCAT2000_summer_mean_pr + south_subset$QSCAT2001_summer_mean_pr) / 
      (south_subset$ERS1998_summer_mean_pr + south_subset$ERS1999_summer_mean_pr + south_subset$ERS2000_summer_mean_pr)
    
    region_mua_plot_grid_pr = rbind(north_subset,south_subset)
    
  }
  
  region_mua_plot_grid_pr$ascat_10_21_slope_intercal = region_mua_plot_grid_pr$ascat_10_21_slope * 
    (region_mua_plot_grid_pr$QSCAT2007_summer_mean_pr + region_mua_plot_grid_pr$QSCAT2008_summer_mean_pr + region_mua_plot_grid_pr$QSCAT2009_summer_mean_pr) / 
    (region_mua_plot_grid_pr$ASCAT2007_summer_mean_pr + region_mua_plot_grid_pr$ASCAT2008_summer_mean_pr + region_mua_plot_grid_pr$ASCAT2009_summer_mean_pr)
  
  # compute means
  
  
  melt_region_growth_rate_arrows = as.data.frame(array(NA,c(2,6)))
  colnames(melt_region_growth_rate_arrows) = c('region','del_era','x1','y1','x2','y2')
  
  melt_region_growth_rate_arrows[,1] = subregion_name
  
  melt_region_growth_rate_arrows[1,2] = '1990s -> 2000s'
  melt_region_growth_rate_arrows[1,3] = mean(region_mua_plot_grid_pr$del_wsf_bf_93_00/100, na.rm = T)
  #   melt_region_growth_rate_arrows[1,4] = mean(region_mua_plot_grid_pr$ers_slope, na.rm = T)
  melt_region_growth_rate_arrows[1,4] = mean(region_mua_plot_grid_pr$ers_slope_intercal, na.rm = T)
  melt_region_growth_rate_arrows[1,5] = mean(region_mua_plot_grid_pr$del_wsf_bf_99_09/100, na.rm = T)
  melt_region_growth_rate_arrows[1,6] = mean(region_mua_plot_grid_pr$qscat_slope, na.rm = T)
  
  melt_region_growth_rate_arrows[2,2] = '2000s -> 2010s'
  melt_region_growth_rate_arrows[2,3] = mean(region_mua_plot_grid_pr$del_wsf_bf_99_09/100, na.rm = T)
  melt_region_growth_rate_arrows[2,4] = mean(region_mua_plot_grid_pr$qscat_slope, na.rm = T)
  melt_region_growth_rate_arrows[2,5] = mean(region_mua_plot_grid_pr$del_wsf_bf_10_15/100, na.rm = T)
  #   melt_region_growth_rate_arrows[2,6] = mean(region_mua_plot_grid_pr$ascat_10_21_slope, na.rm = T)
  melt_region_growth_rate_arrows[2,6] = mean(region_mua_plot_grid_pr$ascat_10_21_slope_intercal, na.rm = T)
  
  
  if (iregion == 1) {
    melt_7_subregion_growth_rate_arrows = melt_region_growth_rate_arrows
  } else {
    melt_7_subregion_growth_rate_arrows = rbind(melt_7_subregion_growth_rate_arrows, melt_region_growth_rate_arrows)
  }
  
}

melt_7_subregion_growth_rate_arrows = as.data.frame(melt_7_subregion_growth_rate_arrows)

melt_7_subregion_growth_rate_arrows$region <- factor(melt_7_subregion_growth_rate_arrows$region, levels=c('SouthAsia', 
                                                                                                          'CentralAsia',
                                                                                                          'NorthAfrica',
                                                                                                          'EastAfrica', 
                                                                                                          'WestAfrica',
                                                                                                          'CentralAfrica',
                                                                                                          'SouthAfrica'))

pdf(paste(output_dir,'sub_region_mean_growth_rate_PR_WSFevo.pdf',sep=''))

p501 = ggplot(melt_7_subregion_growth_rate_arrows, aes(x=as.numeric(x1),y=as.numeric(y1),group = del_era)) +
  geom_point() + geom_point(aes(x=as.numeric(x2),y=as.numeric(y2),group = del_era)) + 
  # geom_point(aes(color=era)) + theme_bw() + 
  # scale_size_manual(values=c(0.01,0.01,0.01)) +
  scale_x_continuous(limits = c(0,0.018)) + scale_y_continuous(limits = c(-0.001, 0.0075)) +
  labs(x="annual increase in WSF-evo buit fraction", y = "annual increase in backscatter PR_intercal") + 
  
  geom_segment(aes(x=as.numeric(x1), y=as.numeric(y1), xend=as.numeric(x2), yend=as.numeric(y2), color = del_era), 
               arrow = arrow(length=unit(0.25, 'cm')), lwd = 1) + theme_bw() + 
  # 
  # # geom_segment(aes(x=as.numeric(mean_x1), y=as.numeric(mean_y1), 
  #                 xend=as.numeric(mean_x2), yend=as.numeric(mean_y2), color = era),
  #             arrow = arrow(length=unit(0.5, 'cm')), lwd = 1) +
  # 
  facet_wrap(~region,  ncol=2)

print(p501)

dev.off()

