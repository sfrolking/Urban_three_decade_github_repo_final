# for all grid cells and for MUA, country, and regional aggregates:
#   compute ERS/QSCAT and QSCAT/ASCAT overlapping year backscatter PR trends
# Nov 2023

# v3: compute 3-year overlap trend intercalibration, at grid cell level, as 
#       qscat-ascat: 2007-2009
#       ers-qscat, northern hemsphere: 1998-2000, 1999-2001
#       ers-qscat, southern hemsphere: 1998-2000, 2000-2002

#     plot trend intercalibration comparison, as MUA means, for
#       qscat-ascat: 2007-2009 trends
#       ers-qscat, northern hemsphere: 1999-2000 differences
#       ers-qscat, southern hemsphere: NA -- there is only one year of overlap, so no trend

# probably don't need most of these ...
library(ggplot2)
library(ggpubr)

wd = getwd()

input_dir = paste(wd,'/mua_input_files/',sep='')

output_dir = paste(wd,'/mua_output_files/',sep='')
mua_data_filename = paste(input_dir,'global_mua_grid_variables_table_wat_lt_50','.csv',sep = '')

# output_dir = paste(wd,'/mua_output_files/',str_replace_all(date(),' ','_'),'_',sep='')

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
# read in MUA/city list

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
# read in subset city list for plotting examples

# 12 cities, 1 per region
city_subset_list = c('Lagos+','Sydney', 'Beijing+','Seoul+','London_*UK*', 'Delhi', 
                     'Sao-Paulo', 'Tehran+','Houston+', 'Karachi', 'Moscow', 'Ho-Chi-Minh-City') 

# # 12 more cities, 1 more per region
# city_subset_list_2 = c('Nairobi', 'Melbourne', 'Xian+', 'Tokyo', 'Berlin', 'Mumbai+',
# 'Buenos-Aires', 'Dubai+', 'Toronto+', 'Istanbul+', 'Saint-Petersburg','Kuala-Lumpur')

num_cities_subset = length(city_subset_list)
# num_cities_subset_2 = length(city_subset_list_2)

######################################################

# read in global MUA data
# NOTE: the working csv file of all grid data included a number of variables that were not used in the end.  These have all been replace with 'NA' 
#    and their column names with NA1 through NA57

mua_data_filename = paste(input_dir,'global_mua_grid_variables_table_wat_lt_50_NA','.csv',sep = '')

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

global_mua_grid_data_wat50 = subset(mua_data_in, mua_data_in$water_pct < 50)

# re-do MUA cell count now that water grids have been removed

mua_city_list$new_cell_count = NA

for (imua in 1:num_mua_cities) {
  
  MUA_id = mua_city_list[imua,1]
  mua_grid_subset = subset(global_mua_grid_data_wat50, global_mua_grid_data_wat50$mua_objectid == MUA_id)
  new_cell_count = dim(mua_grid_subset)[1]
  global_mua_grid_data_wat50$mua_cellcount[global_mua_grid_data_wat50$mua_objectid == MUA_id] <- new_cell_count
  mua_city_list[imua,13] = new_cell_count
}


# add sub+_regions column (#160), ALSO update region list in the mua_all_grid file

global_mua_grid_data_wat50$sub_regions = NA

for (icountry in 1:num_countries) {
  
  country_name = country_list[icountry,2]
  sub_region_name = country_list[icountry,5]
  region_1_name = country_list[icountry,3]
  
  global_mua_grid_data_wat50$sub_regions[global_mua_grid_data_wat50$mua_country == country_name] <- sub_region_name
  global_mua_grid_data_wat50$region_1[global_mua_grid_data_wat50$mua_country == country_name] <- region_1_name
  
}

# compute 3-year PR trends 'intercalibrated' for ASCAT and ERS


global_mua_grid_data_wat50$ascat_10_21_slope_intercal = global_mua_grid_data_wat50$ascat_10_21_slope *
  (global_mua_grid_data_wat50$QSCAT2007_summer_mean_pr + 
     global_mua_grid_data_wat50$QSCAT2008_summer_mean_pr + 
     global_mua_grid_data_wat50$QSCAT2009_summer_mean_pr) / 
  (global_mua_grid_data_wat50$ASCAT2007_summer_mean_pr + 
     global_mua_grid_data_wat50$ASCAT2008_summer_mean_pr + 
     global_mua_grid_data_wat50$ASCAT2009_summer_mean_pr) 

global_mua_grid_data_wat50$ers_slope_intercal = NA

num_grids = dim(global_mua_grid_data_wat50)[1]

for (igrid in 1:num_grids) {
  
  if (global_mua_grid_data_wat50[igrid,2] >= 0) {  # northern hemisphere
    global_mua_grid_data_wat50[igrid,162] = global_mua_grid_data_wat50[igrid,70] *
      (global_mua_grid_data_wat50[igrid,42] + global_mua_grid_data_wat50[igrid,43] + global_mua_grid_data_wat50[igrid,44]) /
      (global_mua_grid_data_wat50[igrid,39] + global_mua_grid_data_wat50[igrid,40] + global_mua_grid_data_wat50[igrid,41])
  } else {  # southern hemisphere
    global_mua_grid_data_wat50[igrid,162] = global_mua_grid_data_wat50[igrid,70] *
      (global_mua_grid_data_wat50[igrid,43] + global_mua_grid_data_wat50[igrid,44] + global_mua_grid_data_wat50[igrid,45]) /
      (global_mua_grid_data_wat50[igrid,39] + global_mua_grid_data_wat50[igrid,40] + global_mua_grid_data_wat50[igrid,41])
  }
  
}



# get subset for plotting

global_mua_grid_data_wat50_working = global_mua_grid_data_wat50[,c(1:12,158,160,70,74,78,154,155,157,161,162)]

#  make PR trend and BF trend arrays for ggplot

num_grids = dim(global_mua_grid_data_wat50_working)[1]

# first PR trends (intercalibrated)

pr_trend_array_ggplot = as.data.frame(array(NA,c(3*num_grids,10)))
colnames(pr_trend_array_ggplot) = c('mua_objectid','mua_aggname','mua_country','region','sub_region','pop_thous','area_km2',
                                    'pop_dens','sensor','PR_slope')

pr_trend_array_ggplot[,1] = rep(global_mua_grid_data_wat50_working[,3])
pr_trend_array_ggplot[,2] = rep(global_mua_grid_data_wat50_working[,10])
pr_trend_array_ggplot[,3] = rep(global_mua_grid_data_wat50_working[,9])
pr_trend_array_ggplot[,4] = rep(global_mua_grid_data_wat50_working[,13])
pr_trend_array_ggplot[,5] = rep(global_mua_grid_data_wat50_working[,14])
pr_trend_array_ggplot[,6] = rep(global_mua_grid_data_wat50_working[,7])
pr_trend_array_ggplot[,7] = rep(global_mua_grid_data_wat50_working[,8])
pr_trend_array_ggplot[,8] = pr_trend_array_ggplot[,6] / pr_trend_array_ggplot[,7]

pr_trend_array_ggplot[1:num_grids,9] = 'ERS'
pr_trend_array_ggplot[1:num_grids,10] = global_mua_grid_data_wat50_working[,22]

pr_trend_array_ggplot[(num_grids + 1):(2*num_grids),9] = 'QSCAT'
pr_trend_array_ggplot[(num_grids + 1):(2*num_grids),10] = global_mua_grid_data_wat50_working[,16]

pr_trend_array_ggplot[(2*num_grids + 1):(3*num_grids),9] = 'ASCAT'
pr_trend_array_ggplot[(2*num_grids + 1):(3*num_grids),10] = global_mua_grid_data_wat50_working[,21]

pr_trend_array_ggplot$pop_bin = NA
pr_trend_array_ggplot$pop_bin[pr_trend_array_ggplot$pop_thous < 1000] <- '<1M'
pr_trend_array_ggplot$pop_bin[pr_trend_array_ggplot$pop_thous < 2000 & pr_trend_array_ggplot$pop_thous >= 1000 ] <- '1-2M'
pr_trend_array_ggplot$pop_bin[pr_trend_array_ggplot$pop_thous < 5000 & pr_trend_array_ggplot$pop_thous >= 2000 ] <- '2-5M'
pr_trend_array_ggplot$pop_bin[pr_trend_array_ggplot$pop_thous < 10000 & pr_trend_array_ggplot$pop_thous >= 5000 ] <- '5-10M'
pr_trend_array_ggplot$pop_bin[pr_trend_array_ggplot$pop_thous >= 10000] <- '>10M'

# make bar plots

# subset for east to south asia
pr_trend_array_ggplot_subset = subset( pr_trend_array_ggplot, pr_trend_array_ggplot$region == 'EastAsia' | pr_trend_array_ggplot$region == 'China' |
                                         pr_trend_array_ggplot$region == 'SEAsia' | pr_trend_array_ggplot$region == 'India' )

# make 'sensor' & 'region' into lists
pr_trend_array_ggplot_subset$region <- factor(pr_trend_array_ggplot_subset$region, levels=c('EastAsia', 'China', 'SEAsia', 'India'))
pr_trend_array_ggplot_subset$sensor <- factor(pr_trend_array_ggplot_subset$sensor, levels=c('ERS', 'QSCAT', 'ASCAT'))
pr_trend_array_ggplot_subset$pop_bin <- factor(pr_trend_array_ggplot_subset$pop_bin, levels=c('<1M', '1-2M','2-5M','5-10M', '>10M'))

# second BF trends

bf_trend_array_ggplot = as.data.frame(array(NA,c(3*num_grids,10)))
colnames(bf_trend_array_ggplot) = c('mua_objectid','mua_aggname','mua_country','region','sub_region','pop_thous','area_km2',
                                    'pop_dens','decade','BF_slope')

bf_trend_array_ggplot[,1] = rep(global_mua_grid_data_wat50_working[,3])
bf_trend_array_ggplot[,2] = rep(global_mua_grid_data_wat50_working[,10])
bf_trend_array_ggplot[,3] = rep(global_mua_grid_data_wat50_working[,9])
bf_trend_array_ggplot[,4] = rep(global_mua_grid_data_wat50_working[,13])
bf_trend_array_ggplot[,5] = rep(global_mua_grid_data_wat50_working[,14])
bf_trend_array_ggplot[,6] = rep(global_mua_grid_data_wat50_working[,7])
bf_trend_array_ggplot[,7] = rep(global_mua_grid_data_wat50_working[,8])
bf_trend_array_ggplot[,8] = bf_trend_array_ggplot[,6] / bf_trend_array_ggplot[,7]

bf_trend_array_ggplot[1:num_grids,9] = '1990s'
bf_trend_array_ggplot[1:num_grids,10] = global_mua_grid_data_wat50_working[,18]

bf_trend_array_ggplot[(num_grids + 1):(2*num_grids),9] = '2000s'
bf_trend_array_ggplot[(num_grids + 1):(2*num_grids),10] = global_mua_grid_data_wat50_working[,19]

bf_trend_array_ggplot[(2*num_grids + 1):(3*num_grids),9] = '2010s'
bf_trend_array_ggplot[(2*num_grids + 1):(3*num_grids),10] = global_mua_grid_data_wat50_working[,20]

bf_trend_array_ggplot$pop_bin = NA
bf_trend_array_ggplot$pop_bin[bf_trend_array_ggplot$pop_thous < 1000] <- '<1M'
bf_trend_array_ggplot$pop_bin[bf_trend_array_ggplot$pop_thous < 2000 & bf_trend_array_ggplot$pop_thous >= 1000 ] <- '1-2M'
bf_trend_array_ggplot$pop_bin[bf_trend_array_ggplot$pop_thous < 5000 & bf_trend_array_ggplot$pop_thous >= 2000 ] <- '2-5M'
bf_trend_array_ggplot$pop_bin[bf_trend_array_ggplot$pop_thous < 10000 & bf_trend_array_ggplot$pop_thous >= 5000 ] <- '5-10M'
bf_trend_array_ggplot$pop_bin[bf_trend_array_ggplot$pop_thous >= 10000] <- '>10M'

# make bar plots

# subset for east to south asia
bf_trend_array_ggplot_subset = subset( bf_trend_array_ggplot, bf_trend_array_ggplot$region == 'EastAsia' | bf_trend_array_ggplot$region == 'China' |
                                         bf_trend_array_ggplot$region == 'SEAsia' | bf_trend_array_ggplot$region == 'India' )

# make 'sensor' & 'region' into lists
bf_trend_array_ggplot_subset$region <- factor(bf_trend_array_ggplot_subset$region, levels=c('EastAsia', 'China', 'SEAsia', 'India'))
bf_trend_array_ggplot_subset$sensor <- factor(bf_trend_array_ggplot_subset$decade, levels=c('1990s', '2000s', '2010s'))
bf_trend_array_ggplot_subset$pop_bin <- factor(bf_trend_array_ggplot_subset$pop_bin, levels=c('<1M', '1-2M','2-5M','5-10M', '>10M'))

p13 = ggplot(pr_trend_array_ggplot_subset, aes(x=sensor, y=PR_slope, fill=pop_bin)) +
  geom_boxplot(outlier.shape=16, outlier.size=0.5, outlier.alpha = 0.5, notch = T) + theme_bw() + 
  theme(legend.position="bottom") +
  labs(title = paste(''),x="", y = "PR slope") + 
  coord_cartesian(ylim = c(-0.005, 0.03)) + 
  facet_wrap(~region,  ncol=2)

p13

p14 = ggplot(bf_trend_array_ggplot_subset, aes(x=decade, y=BF_slope, fill=pop_bin)) +
  geom_boxplot(outlier.shape=16, outlier.size=0.5, outlier.alpha = 0.5, notch = T) + theme_bw() + 
  theme(legend.position="bottom") +
  labs(title = paste(''),x="", y = "BF trend (%/yr)") + 
  coord_cartesian(ylim = c(-0.0, 5)) + 
  facet_wrap(~region,  ncol=2)

p14

pdf(paste(output_dir,'regional_grid_trends_binned_by_pop','.pdf', sep = ''))

print(
  ggarrange(p13,p14,   #
            #              labels = c("A", "B", "C", "D"),
            ncol = 1, nrow = 1)
)

dev.off()

