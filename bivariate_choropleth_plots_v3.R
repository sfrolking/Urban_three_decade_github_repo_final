# constructing bvariate plotes of delta backscatter PR and delta WSFevolotion by era/decade/sensor

# read in MUA city and country list and mua gridded variables array made in 'process_mua_urban_grid_data.R'

# v2: this uses 'intercalibrated' backscatter PR trends (see notes from 11/4/2023 and email to group on 11/6/2023).
#     computing 3-year trends for 2007-2009 overlap years of QSCAT and ASCAT, there is the same change on the ground
#     so there should be a comparable value in trends between qscat and ascat. scatterplot for MUA and other aggregations
#     falls on 1:1 line if trends are 'normalized' by mean PR values over the three years.  So, intercalibration to align ASCAT
#     with QSCAT is to multiply ASCAT PR trend by {<PR>_QSCAT_2007-09 / <PR>_ASCAT_2007-09}.
#     for ERS and QSCAT, there is less overlap, 2 years (1999-2000) in northern hemisphere, 1 year (2000) in southern hemisphere.

#     since 'decadal' trends in ERS are mostly small (often also for QSCAT) a 2 year trend is subject to a lot of noise.
#     will try 'intercalibrating' with 3 year means (1998-2000 for ERS, 1999-2001 for QSCAT, 2000-2002 in southern hemisphere?)

library(ggplot2)
library(ggpubr)
library(metR)  # probably not needed
library(raster)
library(rasterVis)
library(rgdal)
library(stats)
library(tiff)

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

# Set region list and economy list

# region_list = c('AustraliaNZ', 'CentralAsia', 'China', 'EastAsia', 'Europe', 'LatAmerCarib', 'MidEastNorAfr', 'NorAmer', 
#                 'Russia', 'SEAsia', 'SouthAsia', 'SubSahAfrica')
region_list = c('Africa', 'AustraliaNZ', 'China', 'EastAsia', 'Europe', 'India', 'LatAmerCarib', 'MiddleEast', 'NorAmer', 'OtherAsia', 
                'Russia', 'SEAsia')
num_regions = length(region_list)

sub_region_list = c('EastAfrica', 'WestAfrica', 'NorthAfrica', 'SouthAfrica', 'CentralAfrica', 'CentralAsia', 'SouthAsia')
num_sub_regions = length(sub_region_list)

econ_list = c('advanced','other')
num_econ = length(econ_list)

# ######################################################
# # set subset city list for plotting examples
# 
# # 12 cities, 1 per region
# city_subset_list = c('Lagos+','Sydney', 'Beijing+','Seoul+','London_*UK*', 'Delhi', 
#                      'Sao-Paulo', 'Tehran+','Houston+', 'Karachi', 'Moscow', 'Ho-Chi-Minh-City') 
# 
# # 12 more cities, 1 more per region
# city_subset_list_2 = c('Nairobi', 'Melbourne', 'Xian+', 'Tokyo', 'Berlin', 'Mumbai+',
#                        'Buenos-Aires', 'Dubai+', 'Toronto+', 'Istanbul+', 'Saint-Petersburg','Kuala-Lumpur')
# 
# num_cities_subset = length(city_subset_list)
# num_cities_subset_2 = length(city_subset_list_2)
# 
# 
# ######################################################
# read in MAU grid data
# NOTE: the working csv file of all grid data included a number of variables that were not used in the end.  These have all been replace with 'NA' 
#    and their column names with NA1 through NA57

# mua_grid_data_filename = paste(input_dir,'global_mua_grid_variables_table_wat_lt_50','.csv',sep = '')
mua_grid_data_filename = paste(input_dir,'global_mua_grid_variables_table_wat_lt_50_NA','.csv',sep = '')

global_mua_grid_data = read.csv(file=mua_grid_data_filename,header = TRUE, stringsAsFactors = FALSE)
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
# added below
#           160: sub_regions
#       161-162: ascat_10_21_slope_intercal, ers_slope_intercal

global_mua_grid_data_wat50 = subset(global_mua_grid_data, global_mua_grid_data$water_pct < 50)
global_mua_grid_data_wat50_2 = global_mua_grid_data_wat50

# re-do MUA cell count now that water grids have been removed

for (imua in 1:num_mua_cities) {
  
  MUA_id = mua_city_list[imua,1]
  mua_grid_subset = subset(global_mua_grid_data_wat50, global_mua_grid_data_wat50$mua_objectid == MUA_id)
  new_cell_count = dim(mua_grid_subset)[1]
  global_mua_grid_data_wat50_2$mua_cellcount[global_mua_grid_data_wat50_2$mua_objectid == MUA_id] <- new_cell_count
  mua_city_list$mua_cellcount[mua_city_list$mua_objectid == MUA_id] <- new_cell_count
  
}

global_mua_grid_data_wat50 = global_mua_grid_data_wat50_2

rm(global_mua_grid_data_wat50_2)

# add sub+_regions column (#160), ALSO update region list in the mua_all_grid file. Also add new grid cell count to country list

global_mua_grid_data_wat50$sub_regions = NA

country_list$new_grid_cell_count = NA

for (icountry in 1:num_countries) {
  
  country_name = country_list[icountry,2]
  sub_region_name = country_list[icountry,5]
  region_1_name = country_list[icountry,3]
  
  global_mua_grid_data_wat50$sub_regions[global_mua_grid_data_wat50$mua_country == country_name] <- sub_region_name
  global_mua_grid_data_wat50$region_1[global_mua_grid_data_wat50$mua_country == country_name] <- region_1_name
  
}

# add 'intercalibrated' ASCAT PR trend and ERS PR trend

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

#########################################################################################################
#########################################################################################################
# MAKE THE BIVARIATE GRIDS
#########################################################################################################
#########################################################################################################

wsfevo_sextiles = unname(quantile(as.numeric(as.matrix(global_mua_grid_data_wat50[,c(154,155,157)])), probs = seq(0, 1, 1/6)))

pr_intercal_sextiles = unname(quantile(as.numeric(as.matrix(global_mua_grid_data_wat50[,c(70,74,161)])), probs = seq(0, 1, 1/6)))

ers_pr_intercal_sextiles = unname(quantile(as.numeric(as.matrix(global_mua_grid_data_wat50[,c(162)])), probs = seq(0, 1, 1/6)))

qscat_pr_sextiles = unname(quantile(as.numeric(as.matrix(global_mua_grid_data_wat50[,c(74)])), probs = seq(0, 1, 1/6)))

ascat_10_21_pr_intercal_sextiles = unname(quantile(as.numeric(as.matrix(global_mua_grid_data_wat50[,c(161)])), probs = seq(0, 1, 1/6)))

wsfevo_min = wsfevo_sextiles[1]
wsfevo_max = wsfevo_sextiles[7]
pr_min = pr_intercal_sextiles[1]
pr_max = pr_intercal_sextiles[7]
ers_pr_min = ers_pr_intercal_sextiles[1]
ers_pr_max = ers_pr_intercal_sextiles[7]
qscat_pr_min = qscat_pr_sextiles[1]
qscat_pr_max = qscat_pr_sextiles[7]
ascat_10_21_pr_min = ascat_10_21_pr_intercal_sextiles[1]
ascat_10_21_pr_max = ascat_10_21_pr_intercal_sextiles[7]
# 
# wsfevo_quintiles = unname(quantile(as.numeric(as.matrix(global_mua_grid_data_wat50[,154:157])), probs = seq(0, 1, 1/5)))
# pr_quintiles = unname(quantile(as.numeric(as.matrix(global_mua_grid_data_wat50[,c(70,74,78,82)])), probs = seq(0, 1, 1/5)))
# 
# wsfevo_terciles = unname(quantile(as.numeric(as.matrix(global_mua_grid_data_wat50[,154:157])), probs = seq(0, 1, 1/3)))
# pr_terciles = unname(quantile(as.numeric(as.matrix(global_mua_grid_data_wat50[,c(70,74,78,82)])), probs = seq(0, 1, 1/3)))

num_grids = dim(global_mua_grid_data_wat50)[1]

# determine number of grid cells in subregions

sub_region_array = as.data.frame(array(NA,c(num_sub_regions,2)))
colnames(sub_region_array) = c('sub_region','num_grids')

for (isubregion in 1:num_sub_regions) {
  subregion_name = sub_region_list[isubregion]
  subset_array = subset(global_mua_grid_data_wat50,global_mua_grid_data_wat50$sub_regions == subregion_name)
  sub_region_array[isubregion,1] = subregion_name
  sub_region_array[isubregion,2] = dim(subset_array)[1]
  
}

outfile_name = paste(output_dir,'sub_region_grid_cell_table_wat_lt_50','.csv',sep='')
write.csv(sub_region_array,file = outfile_name,row.names = F)

#########################################################################################################
################ SEXTILE #########################################################################
# make sextile indices of delta-WSFevo for all non-water grid cells in all MUAs

# make new set of bin divider values, instead of sextiles (split into bins of equal number)
# see powerpoint file 'bivariate choropleth variable histograms.xlxs' which shows histograms of variables

wsfevo_6bins = array(NA, c(1,7))    # bin breaks for wsfevo BF trends [% increase per year]
wsfevo_6bins[1,1] = wsfevo_min
wsfevo_6bins[1,2] = 0.2
wsfevo_6bins[1,3] = 0.6
wsfevo_6bins[1,4] = 1.0
wsfevo_6bins[1,5] = 1.4
wsfevo_6bins[1,6] = 2
wsfevo_6bins[1,7] = wsfevo_max

pr_6bins = array(NA, c(1,7))    # bin breaks for all sensor PR trends [increase per year]
pr_6bins[1,1] = pr_min
pr_6bins[1,2] = 0.001
pr_6bins[1,3] = 0.0025
pr_6bins[1,4] = 0.004
pr_6bins[1,5] = 0.006
pr_6bins[1,6] = 0.008
pr_6bins[1,7] = pr_max

global_mua_grid_data_wat50_sextile_array = global_mua_grid_data_wat50[,c(1,2,3,5,6,9,10,70,74,78,82,154:157,161,162)]
sextile_array = array(NA, c(num_grids,18))
colnames(sextile_array) = c('sext_ers','sext_wsf_93_00', 'sext_ers_wsf_93_00',
                            'sext_ers_intercal','sext_wsf_93_00_again', 'sext_ers_intercal_wsf_93_00',
                            'sext_qscat','sext_wsf_99_09', 'sext_qscat_wsf_99_09',
                            'sext_ascat_07_15','sext_wsf_07_15', 'sext_ascat_07_15_wsf_07_15',
                            'sext_ascat_10_21','sext_wsf_10_15', 'sext_ascat_10_21_wsf_10_15',
                            'sext_ascat_10_21_intercal','sext_wsf_10_15_again', 'sext_ascat_10_21_wsf_10_15_intercal'
)

global_mua_grid_data_wat50_sextile_array = cbind(global_mua_grid_data_wat50_sextile_array, sextile_array)
# header: 1-7:   longitude  latitude  mua_objectid  mua_country_code  mua_cellcount  mua_country  mua_aggname
#         8-11:  ers_slope  qscat_slope ascat_10_21_slope  ascat_07_15_slope 
#         12-15: del_wsf_bf_93_00  del_wsf_bf_99_09  del_wsf_bf_07_15  del_wsf_bf_10_15  
#         16-17: ascat_10_21_slope_intercal  ers_slope_intercal
#         18-20: sext_ers  sext_wsf_93_00  sext_ers_wsf_93_00
#         21-23: sext_ers_intercal  sext_wsf_93_00_again  sext_ers_intercal_wsf_93_00
#         24-26: sext_qscat  sext_wsf_99_09  sext_qscat_wsf_99_09
#         27-29: sext_ascat_07_15  sext_wsf_07_15  sext_ascat_07_15_wsf_07_15
#         30-32: sext_ascat_10_21  sext_wsf_10_15  sext_ascat_10_21_wsf_10_15
#         33-35: sext_ascat_10_21_intercal  sext_wsf_10_15_again  sext_ascat_10_21_intercal_wsf_10_15

# global_mua_grid_data_wat50_sextile_array[,c(16,17,19,20,22,23,25,26)] = 6
# V2:
global_mua_grid_data_wat50_sextile_array[,c(18,19,21,22,24,25,27,28,30,31,33,34)] = 6

# set second highest values to 5, then mid values to 4, ..., then low values to 2, then lowest values to 1, based on sextiles (so split into sixths, which may not be optimal for portrayal of results)


# use 'sextiles'6bins'

# ERS
global_mua_grid_data_wat50_sextile_array$sext_ers[global_mua_grid_data_wat50_sextile_array$ers_slope < pr_6bins[6]] <- 5
global_mua_grid_data_wat50_sextile_array$sext_ers[global_mua_grid_data_wat50_sextile_array$ers_slope < pr_6bins[5]] <- 4
global_mua_grid_data_wat50_sextile_array$sext_ers[global_mua_grid_data_wat50_sextile_array$ers_slope < pr_6bins[4]] <- 3
global_mua_grid_data_wat50_sextile_array$sext_ers[global_mua_grid_data_wat50_sextile_array$ers_slope < pr_6bins[3]] <- 2
global_mua_grid_data_wat50_sextile_array$sext_ers[global_mua_grid_data_wat50_sextile_array$ers_slope < pr_6bins[2]] <- 1
# WSFevo 93_00
global_mua_grid_data_wat50_sextile_array$sext_wsf_93_00[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_93_00 < wsfevo_6bins[6]] <- 5
global_mua_grid_data_wat50_sextile_array$sext_wsf_93_00[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_93_00 < wsfevo_6bins[5]] <- 4
global_mua_grid_data_wat50_sextile_array$sext_wsf_93_00[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_93_00 < wsfevo_6bins[4]] <- 3
global_mua_grid_data_wat50_sextile_array$sext_wsf_93_00[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_93_00 < wsfevo_6bins[3]] <- 2
global_mua_grid_data_wat50_sextile_array$sext_wsf_93_00[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_93_00 < wsfevo_6bins[2]] <- 1

# ERS_intercal
global_mua_grid_data_wat50_sextile_array$sext_ers_intercal[global_mua_grid_data_wat50_sextile_array$ers_slope_intercal < pr_6bins[6]] <- 5
global_mua_grid_data_wat50_sextile_array$sext_ers_intercal[global_mua_grid_data_wat50_sextile_array$ers_slope_intercal < pr_6bins[5]] <- 4
global_mua_grid_data_wat50_sextile_array$sext_ers_intercal[global_mua_grid_data_wat50_sextile_array$ers_slope_intercal < pr_6bins[4]] <- 3
global_mua_grid_data_wat50_sextile_array$sext_ers_intercal[global_mua_grid_data_wat50_sextile_array$ers_slope_intercal < pr_6bins[3]] <- 2
global_mua_grid_data_wat50_sextile_array$sext_ers_intercal[global_mua_grid_data_wat50_sextile_array$ers_slope_intercal < pr_6bins[2]] <- 1
# WSFevo 93_00_again
global_mua_grid_data_wat50_sextile_array$sext_wsf_93_00_again[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_93_00 < wsfevo_6bins[6]] <- 5
global_mua_grid_data_wat50_sextile_array$sext_wsf_93_00_again[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_93_00 < wsfevo_6bins[5]] <- 4
global_mua_grid_data_wat50_sextile_array$sext_wsf_93_00_again[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_93_00 < wsfevo_6bins[4]] <- 3
global_mua_grid_data_wat50_sextile_array$sext_wsf_93_00_again[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_93_00 < wsfevo_6bins[3]] <- 2
global_mua_grid_data_wat50_sextile_array$sext_wsf_93_00_again[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_93_00 < wsfevo_6bins[2]] <- 1

# QSCAT
global_mua_grid_data_wat50_sextile_array$sext_qscat[global_mua_grid_data_wat50_sextile_array$qscat_slope < pr_6bins[6]] <- 5
global_mua_grid_data_wat50_sextile_array$sext_qscat[global_mua_grid_data_wat50_sextile_array$qscat_slope < pr_6bins[5]] <- 4
global_mua_grid_data_wat50_sextile_array$sext_qscat[global_mua_grid_data_wat50_sextile_array$qscat_slope < pr_6bins[4]] <- 3
global_mua_grid_data_wat50_sextile_array$sext_qscat[global_mua_grid_data_wat50_sextile_array$qscat_slope < pr_6bins[3]] <- 2
global_mua_grid_data_wat50_sextile_array$sext_qscat[global_mua_grid_data_wat50_sextile_array$qscat_slope < pr_6bins[2]] <- 1
# WSFevo 99_09
global_mua_grid_data_wat50_sextile_array$sext_wsf_99_09[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_99_09 < wsfevo_6bins[6]] <- 5
global_mua_grid_data_wat50_sextile_array$sext_wsf_99_09[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_99_09 < wsfevo_6bins[5]] <- 4
global_mua_grid_data_wat50_sextile_array$sext_wsf_99_09[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_99_09 < wsfevo_6bins[4]] <- 3
global_mua_grid_data_wat50_sextile_array$sext_wsf_99_09[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_99_09 < wsfevo_6bins[3]] <- 2
global_mua_grid_data_wat50_sextile_array$sext_wsf_99_09[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_99_09 < wsfevo_6bins[2]] <- 1
# ASCAT 07_15
global_mua_grid_data_wat50_sextile_array$sext_ascat_07_15[global_mua_grid_data_wat50_sextile_array$ascat_07_15_slope < pr_6bins[6]] <- 5
global_mua_grid_data_wat50_sextile_array$sext_ascat_07_15[global_mua_grid_data_wat50_sextile_array$ascat_07_15_slope < pr_6bins[5]] <- 4
global_mua_grid_data_wat50_sextile_array$sext_ascat_07_15[global_mua_grid_data_wat50_sextile_array$ascat_07_15_slope < pr_6bins[4]] <- 3
global_mua_grid_data_wat50_sextile_array$sext_ascat_07_15[global_mua_grid_data_wat50_sextile_array$ascat_07_15_slope < pr_6bins[3]] <- 2
global_mua_grid_data_wat50_sextile_array$sext_ascat_07_15[global_mua_grid_data_wat50_sextile_array$ascat_07_15_slope < pr_6bins[2]] <- 1
# WSFevo 07_15
global_mua_grid_data_wat50_sextile_array$sext_wsf_07_15[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_07_15 < wsfevo_6bins[6]] <- 5
global_mua_grid_data_wat50_sextile_array$sext_wsf_07_15[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_07_15 < wsfevo_6bins[5]] <- 4
global_mua_grid_data_wat50_sextile_array$sext_wsf_07_15[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_07_15 < wsfevo_6bins[4]] <- 3
global_mua_grid_data_wat50_sextile_array$sext_wsf_07_15[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_07_15 < wsfevo_6bins[3]] <- 2
global_mua_grid_data_wat50_sextile_array$sext_wsf_07_15[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_07_15 < wsfevo_6bins[2]] <- 1
# ASCAT 10_21
global_mua_grid_data_wat50_sextile_array$sext_ascat_10_21[global_mua_grid_data_wat50_sextile_array$ascat_10_21_slope < pr_6bins[6]] <- 5
global_mua_grid_data_wat50_sextile_array$sext_ascat_10_21[global_mua_grid_data_wat50_sextile_array$ascat_10_21_slope < pr_6bins[5]] <- 4
global_mua_grid_data_wat50_sextile_array$sext_ascat_10_21[global_mua_grid_data_wat50_sextile_array$ascat_10_21_slope < pr_6bins[4]] <- 3
global_mua_grid_data_wat50_sextile_array$sext_ascat_10_21[global_mua_grid_data_wat50_sextile_array$ascat_10_21_slope < pr_6bins[3]] <- 2
global_mua_grid_data_wat50_sextile_array$sext_ascat_10_21[global_mua_grid_data_wat50_sextile_array$ascat_10_21_slope < pr_6bins[2]] <- 1
# WSFevo 10_15
global_mua_grid_data_wat50_sextile_array$sext_wsf_10_15[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_10_15 < wsfevo_6bins[6]] <- 5
global_mua_grid_data_wat50_sextile_array$sext_wsf_10_15[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_10_15 < wsfevo_6bins[5]] <- 4
global_mua_grid_data_wat50_sextile_array$sext_wsf_10_15[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_10_15 < wsfevo_6bins[4]] <- 3
global_mua_grid_data_wat50_sextile_array$sext_wsf_10_15[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_10_15 < wsfevo_6bins[3]] <- 2
global_mua_grid_data_wat50_sextile_array$sext_wsf_10_15[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_10_15 < wsfevo_6bins[2]] <- 1

# ASCAT 10_21_intercal
global_mua_grid_data_wat50_sextile_array$sext_ascat_10_21_intercal[global_mua_grid_data_wat50_sextile_array$ascat_10_21_slope_intercal < pr_6bins[6]] <- 5
global_mua_grid_data_wat50_sextile_array$sext_ascat_10_21_intercal[global_mua_grid_data_wat50_sextile_array$ascat_10_21_slope_intercal < pr_6bins[5]] <- 4
global_mua_grid_data_wat50_sextile_array$sext_ascat_10_21_intercal[global_mua_grid_data_wat50_sextile_array$ascat_10_21_slope_intercal < pr_6bins[4]] <- 3
global_mua_grid_data_wat50_sextile_array$sext_ascat_10_21_intercal[global_mua_grid_data_wat50_sextile_array$ascat_10_21_slope_intercal < pr_6bins[3]] <- 2
global_mua_grid_data_wat50_sextile_array$sext_ascat_10_21_intercal[global_mua_grid_data_wat50_sextile_array$ascat_10_21_slope_intercal < pr_6bins[2]] <- 1
# WSFevo 10_15_again
global_mua_grid_data_wat50_sextile_array$sext_wsf_10_15_again[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_10_15 < wsfevo_6bins[6]] <- 5
global_mua_grid_data_wat50_sextile_array$sext_wsf_10_15_again[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_10_15 < wsfevo_6bins[5]] <- 4
global_mua_grid_data_wat50_sextile_array$sext_wsf_10_15_again[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_10_15 < wsfevo_6bins[4]] <- 3
global_mua_grid_data_wat50_sextile_array$sext_wsf_10_15_again[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_10_15 < wsfevo_6bins[3]] <- 2
global_mua_grid_data_wat50_sextile_array$sext_wsf_10_15_again[global_mua_grid_data_wat50_sextile_array$del_wsf_bf_10_15 < wsfevo_6bins[2]] <- 1


# now build the bivariate choropleth array values

for (igrid in 1:num_grids) {
  
  #  ERS & WSFevo_93_00
  if (global_mua_grid_data_wat50_sextile_array[igrid,19] == 6){
    global_mua_grid_data_wat50_sextile_array[igrid,20] = global_mua_grid_data_wat50_sextile_array[igrid,18] 
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,19] == 5) {
    global_mua_grid_data_wat50_sextile_array[igrid,20] = global_mua_grid_data_wat50_sextile_array[igrid,18] +  6
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,19] == 4) {
    global_mua_grid_data_wat50_sextile_array[igrid,20] = global_mua_grid_data_wat50_sextile_array[igrid,18] + 12
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,19] == 3) {
    global_mua_grid_data_wat50_sextile_array[igrid,20] = global_mua_grid_data_wat50_sextile_array[igrid,18] + 18
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,19] == 2) {
    global_mua_grid_data_wat50_sextile_array[igrid,20] = global_mua_grid_data_wat50_sextile_array[igrid,18] + 24
  } else {
    global_mua_grid_data_wat50_sextile_array[igrid,20] = global_mua_grid_data_wat50_sextile_array[igrid,18] + 30
  }
  
  #  ERS_intercal & WSFevo_93_00
  if (global_mua_grid_data_wat50_sextile_array[igrid,22] == 6){
    global_mua_grid_data_wat50_sextile_array[igrid,23] = global_mua_grid_data_wat50_sextile_array[igrid,21] 
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,22] == 5) {
    global_mua_grid_data_wat50_sextile_array[igrid,23] = global_mua_grid_data_wat50_sextile_array[igrid,21] +  6
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,22] == 4) {
    global_mua_grid_data_wat50_sextile_array[igrid,23] = global_mua_grid_data_wat50_sextile_array[igrid,21] + 12
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,22] == 3) {
    global_mua_grid_data_wat50_sextile_array[igrid,23] = global_mua_grid_data_wat50_sextile_array[igrid,21] + 18
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,22] == 2) {
    global_mua_grid_data_wat50_sextile_array[igrid,23] = global_mua_grid_data_wat50_sextile_array[igrid,21] + 24
  } else {
    global_mua_grid_data_wat50_sextile_array[igrid,23] = global_mua_grid_data_wat50_sextile_array[igrid,21] + 30
  }
  
  #  QSCAT & WSFevo_99_09
  if (global_mua_grid_data_wat50_sextile_array[igrid,25] == 6){
    global_mua_grid_data_wat50_sextile_array[igrid,26] = global_mua_grid_data_wat50_sextile_array[igrid,24] 
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,25] == 5) {
    global_mua_grid_data_wat50_sextile_array[igrid,26] = global_mua_grid_data_wat50_sextile_array[igrid,24] + 6
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,25] == 4) {
    global_mua_grid_data_wat50_sextile_array[igrid,26] = global_mua_grid_data_wat50_sextile_array[igrid,24] + 12
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,25] == 3) {
    global_mua_grid_data_wat50_sextile_array[igrid,26] = global_mua_grid_data_wat50_sextile_array[igrid,24] + 18
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,25] == 2) {
    global_mua_grid_data_wat50_sextile_array[igrid,26] = global_mua_grid_data_wat50_sextile_array[igrid,24] + 24
  } else {
    global_mua_grid_data_wat50_sextile_array[igrid,26] = global_mua_grid_data_wat50_sextile_array[igrid,24] + 30
  }
  
  #  ASCAT_07_15 & WSFevo_07_15
  if (global_mua_grid_data_wat50_sextile_array[igrid,28] == 6){
    global_mua_grid_data_wat50_sextile_array[igrid,29] = global_mua_grid_data_wat50_sextile_array[igrid,27] 
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,28] == 5) {
    global_mua_grid_data_wat50_sextile_array[igrid,29] = global_mua_grid_data_wat50_sextile_array[igrid,27] + 6
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,28] == 4) {
    global_mua_grid_data_wat50_sextile_array[igrid,29] = global_mua_grid_data_wat50_sextile_array[igrid,27] + 12
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,28] == 3) {
    global_mua_grid_data_wat50_sextile_array[igrid,29] = global_mua_grid_data_wat50_sextile_array[igrid,27] + 18
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,28] == 2) {
    global_mua_grid_data_wat50_sextile_array[igrid,29] = global_mua_grid_data_wat50_sextile_array[igrid,27] + 24
  } else {
    global_mua_grid_data_wat50_sextile_array[igrid,29] = global_mua_grid_data_wat50_sextile_array[igrid,27] + 30
  }
  
  #  ASCAT_10_21 & WSFevo_10_15
  if (global_mua_grid_data_wat50_sextile_array[igrid,31] == 6){
    global_mua_grid_data_wat50_sextile_array[igrid,32] = global_mua_grid_data_wat50_sextile_array[igrid,30] 
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,31] == 5) {
    global_mua_grid_data_wat50_sextile_array[igrid,32] = global_mua_grid_data_wat50_sextile_array[igrid,30] + 6
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,31] == 4) {
    global_mua_grid_data_wat50_sextile_array[igrid,32] = global_mua_grid_data_wat50_sextile_array[igrid,30] + 12
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,31] == 3) {
    global_mua_grid_data_wat50_sextile_array[igrid,32] = global_mua_grid_data_wat50_sextile_array[igrid,30] + 18
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,31] == 2) {
    global_mua_grid_data_wat50_sextile_array[igrid,32] = global_mua_grid_data_wat50_sextile_array[igrid,30] + 24
  } else {
    global_mua_grid_data_wat50_sextile_array[igrid,32] = global_mua_grid_data_wat50_sextile_array[igrid,30] + 30
  }
  
  #  ASCAT_10_21_intercal & WSFevo_10_15
  if (global_mua_grid_data_wat50_sextile_array[igrid,34] == 6){
    global_mua_grid_data_wat50_sextile_array[igrid,35] = global_mua_grid_data_wat50_sextile_array[igrid,33] 
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,34] == 5) {
    global_mua_grid_data_wat50_sextile_array[igrid,35] = global_mua_grid_data_wat50_sextile_array[igrid,33] + 6
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,34] == 4) {
    global_mua_grid_data_wat50_sextile_array[igrid,35] = global_mua_grid_data_wat50_sextile_array[igrid,33] + 12
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,34] == 3) {
    global_mua_grid_data_wat50_sextile_array[igrid,35] = global_mua_grid_data_wat50_sextile_array[igrid,33] + 18
  } else if (global_mua_grid_data_wat50_sextile_array[igrid,34] == 2) {
    global_mua_grid_data_wat50_sextile_array[igrid,35] = global_mua_grid_data_wat50_sextile_array[igrid,33] + 24
  } else {
    global_mua_grid_data_wat50_sextile_array[igrid,35] = global_mua_grid_data_wat50_sextile_array[igrid,33] + 30
  }
  
}

# make '6bin' change raster grids

ers_intercal_wsfevo_93_00_grid = global_mua_grid_data_wat50_sextile_array[c(1,2,23)]
r_ers_intercal_wsfevo_93_00_grid = rasterFromXYZ(ers_intercal_wsfevo_93_00_grid,crs='+proj=longlat +datum=WGS84')   
geotiff_name = paste(output_dir,'ers_intercal_wsfevo_93_00_6bin_grid', sep = '')
writeRaster(r_ers_intercal_wsfevo_93_00_grid, filename = geotiff_name, format = "GTiff",overwrite = TRUE)

qscat_wsfevo_99_09_grid = global_mua_grid_data_wat50_sextile_array[c(1,2,26)]
r_qscat_wsfevo_99_09_grid = rasterFromXYZ(qscat_wsfevo_99_09_grid,crs='+proj=longlat +datum=WGS84')   
geotiff_name = paste(output_dir,'qscat_wsfevo_99_09_6bin_grid', sep = '')
writeRaster(r_qscat_wsfevo_99_09_grid, filename = geotiff_name, format = "GTiff",overwrite = TRUE)

ascat_intercal_wsfevo_10_21_grid = global_mua_grid_data_wat50_sextile_array[c(1,2,35)]
r_ascat_intercal_wsfevo_10_21_grid = rasterFromXYZ(ascat_intercal_wsfevo_10_21_grid,crs='+proj=longlat +datum=WGS84')   
geotiff_name = paste(output_dir,'ascat_intercal_wsfevo_10_21_6bin_grid', sep = '')
writeRaster(r_ascat_intercal_wsfevo_10_21_grid, filename = geotiff_name, format = "GTiff",overwrite = TRUE)

# # plot histrograms of bivariate change bins (quasi-clusters)
# hist(as.numeric(as.matrix(ers_wsfevo_93_00_grid[,3])),breaks = 36)
# hist(as.numeric(as.matrix(ers_intercal_wsfevo_93_00_grid[,3])),breaks = 36)
# hist(as.numeric(as.matrix(qscat_wsfevo_99_09_grid[,3])),breaks = 36)
# hist(as.numeric(as.matrix(ascat_wsfevo_07_15_grid[,3])),breaks = 36)
# hist(as.numeric(as.matrix(ascat_wsfevo_10_21_grid[,3])),breaks = 36)
# hist(as.numeric(as.matrix(ascat_intercal_wsfevo_10_21_grid[,3])),breaks = 36)

#########################################################################################################
# make tables of counts of grid cells in each class for each MUA
#########################################################################################################

# ###############################
# # 1. ascat_10_21 & wsfevo_10_15
# 
# mua_bivariate_index_array_10_21 = array(NA,c(num_mua_cities,(3+6+6+36)))
# colnames(mua_bivariate_index_array_10_21) = c('mua_objectid','mua_aggname', 'mua_cellcount',
#                                               'wsfevo_1_count',  'wsfevo_2_count',  'wsfevo_3_count',  'wsfevo_4_count',  'wsfevo_5_count',  'wsfevo_6_count', 
#                                               'pr_1_count',  'pr_2_count',  'pr_3_count',  'pr_4_count',  'pr_5_count',  'pr_6_count', 
#                                               'bivar_1_count',  'bivar_2_count',  'bivar_3_count',  'bivar_4_count',  'bivar_5_count',  'bivar_6_count', 
#                                               'bivar_7_count',  'bivar_8_count',  'bivar_9_count',  'bivar_10count',  'bivar_11_count',  'bivar_12_count', 
#                                               'bivar_13_count',  'bivar_14_count',  'bivar_15_count',  'bivar_16_count',  'bivar_17_count',  'bivar_18_count', 
#                                               'bivar_19_count',  'bivar_20_count',  'bivar_21_count',  'bivar_22_count',  'bivar_23_count',  'bivar_24_count', 
#                                               'bivar_25_count',  'bivar_26_count',  'bivar_27_count',  'bivar_28_count',  'bivar_29_count',  'bivar_30_count', 
#                                               'bivar_31_count',  'bivar_32_count',  'bivar_33_count',  'bivar_34_count',  'bivar_35_count',  'bivar_36_count'
# )
# 
# # global_mua_grid_data_wat50_sextile_array HEADER:
# # header: 1-7:   longitude  latitude  mua_objectid  mua_country_code  mua_cellcount  mua_country  mua_aggname
# #         8-11:  ers_slope  qscat_slope ascat_10_21_slope  ascat_07_15_slope 
# #         12-15: del_wsf_bf_93_00  del_wsf_bf_99_09  del_wsf_bf_07_15  del_wsf_bf_10_15  
# #         16-18: sext_ers  sext_wsf_93_00  sext_ers_wsf_93_00
# #         19-21: sext_qscat  sext_wsf_99_09  sext_qscat_wsf_99_09
# #         22-24: sext_ascat_07_15  sext_wsf_07_15  sext_ascat_07_15_wsf_07_15
# #         25-27: sext_ascat_10_21  sext_wsf_10_15  sext_ascat_10_21_wsf_10_15
# 
# for (imua in 1:num_mua_cities) {
#   
#   MUA_id = mua_city_list[imua,1]
#   mua_subset = subset(global_mua_grid_data_wat50_sextile_array, global_mua_grid_data_wat50_sextile_array$mua_objectid == MUA_id)
#   
#   mua_bivariate_index_array_10_21[imua,1] = MUA_id
#   mua_bivariate_index_array_10_21[imua,2] = mua_subset[1,7]  # MUA name
#   mua_bivariate_index_array_10_21[imua,3] = mua_subset[1,5]  # MUA cell count
#   
#   for (is in 1:6) {
#     mua_bivariate_index_array_10_21[imua,(3+6+is)] = sum(mua_subset$sext_ascat_10_21 == is)
#     mua_bivariate_index_array_10_21[imua,(3+is)] = sum(mua_subset$sext_wsf_10_15 == is)
#   }
#   
#   for (ib in 1:36) {
#     mua_bivariate_index_array_10_21[imua,(3+6+6+ib)] = sum(mua_subset$sext_ascat_10_21_wsf_10_15 == ib)
#   }
#   
# }

###############################
# 1a. ascat_10_21_intercal & wsfevo_10_15

mua_bivariate_index_array_10_21_intercal = array(NA,c(num_mua_cities,(3+6+6+36)))
colnames(mua_bivariate_index_array_10_21_intercal) = c('mua_objectid','mua_aggname', 'mua_cellcount',
                                                       'wsfevo_1_count',  'wsfevo_2_count',  'wsfevo_3_count',  'wsfevo_4_count',  'wsfevo_5_count',  'wsfevo_6_count', 
                                                       'pr_1_count',  'pr_2_count',  'pr_3_count',  'pr_4_count',  'pr_5_count',  'pr_6_count', 
                                                       'bivar_1_count',  'bivar_2_count',  'bivar_3_count',  'bivar_4_count',  'bivar_5_count',  'bivar_6_count', 
                                                       'bivar_7_count',  'bivar_8_count',  'bivar_9_count',  'bivar_10count',  'bivar_11_count',  'bivar_12_count', 
                                                       'bivar_13_count',  'bivar_14_count',  'bivar_15_count',  'bivar_16_count',  'bivar_17_count',  'bivar_18_count', 
                                                       'bivar_19_count',  'bivar_20_count',  'bivar_21_count',  'bivar_22_count',  'bivar_23_count',  'bivar_24_count', 
                                                       'bivar_25_count',  'bivar_26_count',  'bivar_27_count',  'bivar_28_count',  'bivar_29_count',  'bivar_30_count', 
                                                       'bivar_31_count',  'bivar_32_count',  'bivar_33_count',  'bivar_34_count',  'bivar_35_count',  'bivar_36_count'
)

# global_mua_grid_data_wat50_sextile_array HEADER:
# header: 1-7:   longitude  latitude  mua_objectid  mua_country_code  mua_cellcount  mua_country  mua_aggname
#         8-11:  ers_slope  qscat_slope ascat_10_21_slope  ascat_07_15_slope 
#         12-15: del_wsf_bf_93_00  del_wsf_bf_99_09  del_wsf_bf_07_15  del_wsf_bf_10_15  
#         16-18: sext_ers  sext_wsf_93_00  sext_ers_wsf_93_00
#         19-21: sext_qscat  sext_wsf_99_09  sext_qscat_wsf_99_09
#         22-24: sext_ascat_07_15  sext_wsf_07_15  sext_ascat_07_15_wsf_07_15
#         25-27: sext_ascat_10_21  sext_wsf_10_15  sext_ascat_10_21_wsf_10_15

for (imua in 1:num_mua_cities) {
  
  MUA_id = mua_city_list[imua,1]
  mua_subset = subset(global_mua_grid_data_wat50_sextile_array, global_mua_grid_data_wat50_sextile_array$mua_objectid == MUA_id)
  
  mua_bivariate_index_array_10_21_intercal[imua,1] = MUA_id
  mua_bivariate_index_array_10_21_intercal[imua,2] = mua_subset[1,7]  # MUA name
  mua_bivariate_index_array_10_21_intercal[imua,3] = mua_subset[1,5]  # MUA cell count
  
  for (is in 1:6) {
    mua_bivariate_index_array_10_21_intercal[imua,(3+6+is)] = sum(mua_subset$sext_ascat_10_21_intercal == is)
    mua_bivariate_index_array_10_21_intercal[imua,(3+is)] = sum(mua_subset$sext_wsf_10_15 == is)
  }
  
  for (ib in 1:36) {
    mua_bivariate_index_array_10_21_intercal[imua,(3+6+6+ib)] = sum(mua_subset$sext_ascat_10_21_wsf_10_15_intercal == ib)
  }

}

# ###############################
# # 2. ascat_07_17 & wsfevo_07_15
# 
# mua_bivariate_index_array_07_15 = array(NA,c(num_mua_cities,(3+6+6+36)))
# colnames(mua_bivariate_index_array_07_15) = c('mua_objectid','mua_aggname', 'mua_cellcount',
#                                               'wsfevo_1_count',  'wsfevo_2_count',  'wsfevo_3_count',  'wsfevo_4_count',  'wsfevo_5_count',  'wsfevo_6_count', 
#                                               'pr_1_count',  'pr_2_count',  'pr_3_count',  'pr_4_count',  'pr_5_count',  'pr_6_count', 
#                                               'bivar_1_count',  'bivar_2_count',  'bivar_3_count',  'bivar_4_count',  'bivar_5_count',  'bivar_6_count', 
#                                               'bivar_7_count',  'bivar_8_count',  'bivar_9_count',  'bivar_10count',  'bivar_11_count',  'bivar_12_count', 
#                                               'bivar_13_count',  'bivar_14_count',  'bivar_15_count',  'bivar_16_count',  'bivar_17_count',  'bivar_18_count', 
#                                               'bivar_19_count',  'bivar_20_count',  'bivar_21_count',  'bivar_22_count',  'bivar_23_count',  'bivar_24_count', 
#                                               'bivar_25_count',  'bivar_26_count',  'bivar_27_count',  'bivar_28_count',  'bivar_29_count',  'bivar_30_count', 
#                                               'bivar_31_count',  'bivar_32_count',  'bivar_33_count',  'bivar_34_count',  'bivar_35_count',  'bivar_36_count'
# )
# 
# for (imua in 1:num_mua_cities) {
#   
#   MUA_id = mua_city_list[imua,1]
#   mua_subset = subset(global_mua_grid_data_wat50_sextile_array, global_mua_grid_data_wat50_sextile_array$mua_objectid == MUA_id)
#   
#   mua_bivariate_index_array_07_15[imua,1] = MUA_id
#   mua_bivariate_index_array_07_15[imua,2] = mua_subset[1,7]  # MUA name
#   mua_bivariate_index_array_07_15[imua,3] = mua_subset[1,5]  # MUA cell count
#   
#   for (is in 1:6) {
#     mua_bivariate_index_array_07_15[imua,(3+6+is)] = sum(mua_subset$sext_ascat_07_15 == is)
#     mua_bivariate_index_array_07_15[imua,(3+is)] = sum(mua_subset$sext_wsf_07_15 == is)
#   }
#   
#   for (ib in 1:36) {
#     mua_bivariate_index_array_07_15[imua,(3+6+6+ib)] = sum(mua_subset$sext_ascat_07_15_wsf_07_15 == ib)
#   }
#   
# }

###############################
# 3. qscat_99_09 & wsfevo_99_09

mua_bivariate_index_array_99_09 = array(NA,c(num_mua_cities,(3+6+6+36)))
colnames(mua_bivariate_index_array_99_09) = c('mua_objectid', 'mua_aggname','mua_cellcount',
                                              'wsfevo_1_count',  'wsfevo_2_count',  'wsfevo_3_count',  'wsfevo_4_count',  'wsfevo_5_count',  'wsfevo_6_count', 
                                              'pr_1_count',  'pr_2_count',  'pr_3_count',  'pr_4_count',  'pr_5_count',  'pr_6_count', 
                                              'bivar_1_count',  'bivar_2_count',  'bivar_3_count',  'bivar_4_count',  'bivar_5_count',  'bivar_6_count', 
                                              'bivar_7_count',  'bivar_8_count',  'bivar_9_count',  'bivar_10count',  'bivar_11_count',  'bivar_12_count', 
                                              'bivar_13_count',  'bivar_14_count',  'bivar_15_count',  'bivar_16_count',  'bivar_17_count',  'bivar_18_count', 
                                              'bivar_19_count',  'bivar_20_count',  'bivar_21_count',  'bivar_22_count',  'bivar_23_count',  'bivar_24_count', 
                                              'bivar_25_count',  'bivar_26_count',  'bivar_27_count',  'bivar_28_count',  'bivar_29_count',  'bivar_30_count', 
                                              'bivar_31_count',  'bivar_32_count',  'bivar_33_count',  'bivar_34_count',  'bivar_35_count',  'bivar_36_count'
)

for (imua in 1:num_mua_cities) {
  
  MUA_id = mua_city_list[imua,1]
  mua_subset = subset(global_mua_grid_data_wat50_sextile_array, global_mua_grid_data_wat50_sextile_array$mua_objectid == MUA_id)
  
  mua_bivariate_index_array_99_09[imua,1] = MUA_id
  mua_bivariate_index_array_99_09[imua,2] = mua_subset[1,7]  # MUA name
  mua_bivariate_index_array_99_09[imua,3] = mua_subset[1,5]  # MUA cell count
  
  for (is in 1:6) {
    mua_bivariate_index_array_99_09[imua,(3+6+is)] = sum(mua_subset$sext_qscat == is)
    mua_bivariate_index_array_99_09[imua,(3+is)] = sum(mua_subset$sext_wsf_99_09 == is)
  }
  
  for (ib in 1:36) {
    mua_bivariate_index_array_99_09[imua,(3+6+6+ib)] = sum(mua_subset$sext_qscat_wsf_99_09 == ib)
  }
  
}

# ###############################
# # 4. ers_93_00 & wsfevo_93_00
# 
# mua_bivariate_index_array_93_00 = array(NA,c(num_mua_cities,(3+6+6+36)))
# colnames(mua_bivariate_index_array_93_00) = c('mua_objectid','mua_aggname',  'mua_cellcount', 
#                                               'wsfevo_1_count',  'wsfevo_2_count',  'wsfevo_3_count',  'wsfevo_4_count',  'wsfevo_5_count',  'wsfevo_6_count', 
#                                               'pr_1_count',  'pr_2_count',  'pr_3_count',  'pr_4_count',  'pr_5_count',  'pr_6_count', 
#                                               'bivar_1_count',  'bivar_2_count',  'bivar_3_count',  'bivar_4_count',  'bivar_5_count',  'bivar_6_count', 
#                                               'bivar_7_count',  'bivar_8_count',  'bivar_9_count',  'bivar_10count',  'bivar_11_count',  'bivar_12_count', 
#                                               'bivar_13_count',  'bivar_14_count',  'bivar_15_count',  'bivar_16_count',  'bivar_17_count',  'bivar_18_count', 
#                                               'bivar_19_count',  'bivar_20_count',  'bivar_21_count',  'bivar_22_count',  'bivar_23_count',  'bivar_24_count', 
#                                               'bivar_25_count',  'bivar_26_count',  'bivar_27_count',  'bivar_28_count',  'bivar_29_count',  'bivar_30_count', 
#                                               'bivar_31_count',  'bivar_32_count',  'bivar_33_count',  'bivar_34_count',  'bivar_35_count',  'bivar_36_count'
# )
# 
# for (imua in 1:num_mua_cities) {
#   
#   MUA_id = mua_city_list[imua,1]
#   mua_subset = subset(global_mua_grid_data_wat50_sextile_array, global_mua_grid_data_wat50_sextile_array$mua_objectid == MUA_id)
#   
#   mua_bivariate_index_array_93_00[imua,1] = MUA_id
#   mua_bivariate_index_array_93_00[imua,2] = mua_subset[1,7]  # MUA name
#   mua_bivariate_index_array_93_00[imua,3] = mua_subset[1,5]  # MUA cell count
#   
#   
#   for (is in 1:6) {
#     mua_bivariate_index_array_93_00[imua,(3+6+is)] = sum(mua_subset$sext_ers == is)
#     mua_bivariate_index_array_93_00[imua,(3+is)] = sum(mua_subset$sext_wsf_93_00 == is)
#   }
#   
#   for (ib in 1:36) {
#     mua_bivariate_index_array_93_00[imua,(3+6+6+ib)] = sum(mua_subset$sext_ers_wsf_93_00 == ib)
#   }
#   
# }

###############################
# 4a. ers_93_00_intercal & wsfevo_93_00

mua_bivariate_index_array_93_00_intercal = array(NA,c(num_mua_cities,(3+6+6+36)))
colnames(mua_bivariate_index_array_93_00_intercal) = c('mua_objectid','mua_aggname',  'mua_cellcount', 
                                                       'wsfevo_1_count',  'wsfevo_2_count',  'wsfevo_3_count',  'wsfevo_4_count',  'wsfevo_5_count',  'wsfevo_6_count', 
                                                       'pr_1_count',  'pr_2_count',  'pr_3_count',  'pr_4_count',  'pr_5_count',  'pr_6_count', 
                                                       'bivar_1_count',  'bivar_2_count',  'bivar_3_count',  'bivar_4_count',  'bivar_5_count',  'bivar_6_count', 
                                                       'bivar_7_count',  'bivar_8_count',  'bivar_9_count',  'bivar_10count',  'bivar_11_count',  'bivar_12_count', 
                                                       'bivar_13_count',  'bivar_14_count',  'bivar_15_count',  'bivar_16_count',  'bivar_17_count',  'bivar_18_count', 
                                                       'bivar_19_count',  'bivar_20_count',  'bivar_21_count',  'bivar_22_count',  'bivar_23_count',  'bivar_24_count', 
                                                       'bivar_25_count',  'bivar_26_count',  'bivar_27_count',  'bivar_28_count',  'bivar_29_count',  'bivar_30_count', 
                                                       'bivar_31_count',  'bivar_32_count',  'bivar_33_count',  'bivar_34_count',  'bivar_35_count',  'bivar_36_count'
)

for (imua in 1:num_mua_cities) {
  
  MUA_id = mua_city_list[imua,1]
  mua_subset = subset(global_mua_grid_data_wat50_sextile_array, global_mua_grid_data_wat50_sextile_array$mua_objectid == MUA_id)
  
  mua_bivariate_index_array_93_00_intercal[imua,1] = MUA_id
  mua_bivariate_index_array_93_00_intercal[imua,2] = mua_subset[1,7]  # MUA name
  mua_bivariate_index_array_93_00_intercal[imua,3] = mua_subset[1,5]  # MUA cell count
  
  
  for (is in 1:6) {
    mua_bivariate_index_array_93_00_intercal[imua,(3+6+is)] = sum(mua_subset$sext_ers_intercal == is)
    mua_bivariate_index_array_93_00_intercal[imua,(3+is)] = sum(mua_subset$sext_wsf_93_00 == is)
  }
  
  for (ib in 1:36) {
    mua_bivariate_index_array_93_00_intercal[imua,(3+6+6+ib)] = sum(mua_subset$sext_ers_intercal_wsf_93_00 == ib)
  }
  
}

outfile_name = paste(output_dir,'all_mua_bivariate_grid_stats_93_00_intercal','.csv',sep='')
write.csv(mua_bivariate_index_array_93_00_intercal,file = outfile_name,row.names = F)

outfile_name = paste(output_dir,'all_mua_bivariate_grid_stats_99_09','.csv',sep='')
write.csv(mua_bivariate_index_array_99_09,file = outfile_name,row.names = F)

outfile_name = paste(output_dir,'all_mua_bivariate_grid_stats_10_21_intercal','.csv',sep='')
write.csv(mua_bivariate_index_array_10_21_intercal,file = outfile_name,row.names = F)

#########################################################################################################
# aggregate tables of counts of grid cells in each class for each MUA into 'clusters'

# bivariate cell indices counted like reading a book (top row left to right = 1-6, bottom row left to right = 31-36)
#   so lowest in both is cell 31, highest in both is cell 6, high del-BA and low del-PR = 31, high del-PR and low del-BA = 6
#  (NOTE: in printing figures, legend is flipped about 1:1 line, so increasing PR is moving up, and increasing BF is moving right!!)

# WITH FOUR CLUSTERS:
# cluster_1 (low del-BA and low del-PR): bivariate cells 19, 20, 21, 25, 26, 27, 31, 32, 33
# cluster_2 (high del-BA and low del-PR): bivariate cells 1, 2, 3, 7, 8, 9, 13, 14, 15
# cluster_3 (low del-BA and high del-PR): bivariate cells 22, 23, 24, 28, 29, 30, 34, 35, 36
# cluster_4 (high del-BA and high del-PR): bivariate cells 4, 5, 6, 10, 11, 12, 16, 17, 18

#########################################################################################################

# ###############################
# # 1. ers_93_00 & wsfevo_93_00
# 
# mua_bivariate_index_cluster_array_93_00 = array(NA,c(num_mua_cities,(3+4)))
# colnames(mua_bivariate_index_cluster_array_93_00) = c('mua_objectid','mua_aggname', 'mua_cellcount',
#                                                       'cluster_lowBF_lowPR',  'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
# 
# mua_bivariate_index_cluster_array_93_00[,1:3] = mua_bivariate_index_array_93_00[,1:3]
# for (imua in 1:num_mua_cities) {
#   mua_bivariate_index_cluster_array_93_00[imua,4] = sum(as.matrix(as.numeric(mua_bivariate_index_array_93_00[imua,(15 + c(19, 20, 21, 25, 26, 27, 31, 32, 33))])))
#   mua_bivariate_index_cluster_array_93_00[imua,5] = sum(as.matrix(as.numeric(mua_bivariate_index_array_93_00[imua,(15 + c(1, 2, 3, 7, 8, 9, 13, 14, 15))])))
#   mua_bivariate_index_cluster_array_93_00[imua,6] = sum(as.matrix(as.numeric(mua_bivariate_index_array_93_00[imua,(15 + c(22, 23, 24, 28, 29, 30, 34, 35, 36))])))
#   mua_bivariate_index_cluster_array_93_00[imua,7] = sum(as.matrix(as.numeric(mua_bivariate_index_array_93_00[imua,(15 + c(4, 5, 6, 10, 11, 12, 16, 17, 18))])))
# }

###############################
# 1a. ers_93_00_intercal & wsfevo_93_00

mua_bivariate_index_cluster_array_93_00_intercal = array(NA,c(num_mua_cities,(3+4)))
colnames(mua_bivariate_index_cluster_array_93_00_intercal) = c('mua_objectid','mua_aggname', 'mua_cellcount',
                                                               'cluster_lowBF_lowPR',  'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')

mua_bivariate_index_cluster_array_93_00_intercal[,1:3] = mua_bivariate_index_array_93_00_intercal[,1:3]
for (imua in 1:num_mua_cities) {
  mua_bivariate_index_cluster_array_93_00_intercal[imua,4] = sum(as.matrix(as.numeric(mua_bivariate_index_array_93_00_intercal[imua,(15 + c(19, 20, 21, 25, 26, 27, 31, 32, 33))])))
  mua_bivariate_index_cluster_array_93_00_intercal[imua,5] = sum(as.matrix(as.numeric(mua_bivariate_index_array_93_00_intercal[imua,(15 + c(1, 2, 3, 7, 8, 9, 13, 14, 15))])))
  mua_bivariate_index_cluster_array_93_00_intercal[imua,6] = sum(as.matrix(as.numeric(mua_bivariate_index_array_93_00_intercal[imua,(15 + c(22, 23, 24, 28, 29, 30, 34, 35, 36))])))
  mua_bivariate_index_cluster_array_93_00_intercal[imua,7] = sum(as.matrix(as.numeric(mua_bivariate_index_array_93_00_intercal[imua,(15 + c(4, 5, 6, 10, 11, 12, 16, 17, 18))])))
}

###############################
# 2. qscat_99_09 & wsfevo_99_09

mua_bivariate_index_cluster_array_99_09 = array(NA,c(num_mua_cities,(3+4)))
colnames(mua_bivariate_index_cluster_array_99_09) = c('mua_objectid','mua_aggname', 'mua_cellcount',
                                                      'cluster_lowBF_lowPR',  'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')

mua_bivariate_index_cluster_array_99_09[,1:3] = mua_bivariate_index_array_99_09[,1:3]
for (imua in 1:num_mua_cities) {
  mua_bivariate_index_cluster_array_99_09[imua,4] = sum(as.matrix(as.numeric(mua_bivariate_index_array_99_09[imua,(15 + c(19, 20, 21, 25, 26, 27, 31, 32, 33))])))
  mua_bivariate_index_cluster_array_99_09[imua,5] = sum(as.matrix(as.numeric(mua_bivariate_index_array_99_09[imua,(15 + c(1, 2, 3, 7, 8, 9, 13, 14, 15))])))
  mua_bivariate_index_cluster_array_99_09[imua,6] = sum(as.matrix(as.numeric(mua_bivariate_index_array_99_09[imua,(15 + c(22, 23, 24, 28, 29, 30, 34, 35, 36))])))
  mua_bivariate_index_cluster_array_99_09[imua,7] = sum(as.matrix(as.numeric(mua_bivariate_index_array_99_09[imua,(15 + c(4, 5, 6, 10, 11, 12, 16, 17, 18))])))
}

# ###############################
# # 3. ascat_07_15 & wsfevo_07_15
# 
# mua_bivariate_index_cluster_array_07_15 = array(NA,c(num_mua_cities,(3+4)))
# colnames(mua_bivariate_index_cluster_array_07_15) = c('mua_objectid','mua_aggname', 'mua_cellcount',
#                                                       'cluster_lowBF_lowPR',  'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
# 
# mua_bivariate_index_cluster_array_07_15[,1:3] = mua_bivariate_index_array_07_15[,1:3]
# for (imua in 1:num_mua_cities) {
#   mua_bivariate_index_cluster_array_07_15[imua,4] = sum(as.matrix(as.numeric(mua_bivariate_index_array_07_15[imua,(15 + c(19, 20, 21, 25, 26, 27, 31, 32, 33))])))
#   mua_bivariate_index_cluster_array_07_15[imua,5] = sum(as.matrix(as.numeric(mua_bivariate_index_array_07_15[imua,(15 + c(1, 2, 3, 7, 8, 9, 13, 14, 15))])))
#   mua_bivariate_index_cluster_array_07_15[imua,6] = sum(as.matrix(as.numeric(mua_bivariate_index_array_07_15[imua,(15 + c(22, 23, 24, 28, 29, 30, 34, 35, 36))])))
#   mua_bivariate_index_cluster_array_07_15[imua,7] = sum(as.matrix(as.numeric(mua_bivariate_index_array_07_15[imua,(15 + c(4, 5, 6, 10, 11, 12, 16, 17, 18))])))
# }
# 
# ###############################
# # 4. ascat_10_21 & wsfevo_10_15
# 
# mua_bivariate_index_cluster_array_10_21 = array(NA,c(num_mua_cities,(3+4)))
# colnames(mua_bivariate_index_cluster_array_10_21) = c('mua_objectid','mua_aggname', 'mua_cellcount',
#                                                       'cluster_lowBF_lowPR',  'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
# 
# mua_bivariate_index_cluster_array_10_21[,1:3] = mua_bivariate_index_array_10_21[,1:3]
# for (imua in 1:num_mua_cities) {
#   mua_bivariate_index_cluster_array_10_21[imua,4] = sum(as.matrix(as.numeric(mua_bivariate_index_array_10_21[imua,(15 + c(19, 20, 21, 25, 26, 27, 31, 32, 33))])))
#   mua_bivariate_index_cluster_array_10_21[imua,5] = sum(as.matrix(as.numeric(mua_bivariate_index_array_10_21[imua,(15 + c(1, 2, 3, 7, 8, 9, 13, 14, 15))])))
#   mua_bivariate_index_cluster_array_10_21[imua,6] = sum(as.matrix(as.numeric(mua_bivariate_index_array_10_21[imua,(15 + c(22, 23, 24, 28, 29, 30, 34, 35, 36))])))
#   mua_bivariate_index_cluster_array_10_21[imua,7] = sum(as.matrix(as.numeric(mua_bivariate_index_array_10_21[imua,(15 + c(4, 5, 6, 10, 11, 12, 16, 17, 18))])))
# }

###############################
# 4a. ascat_10_21_intercal & wsfevo_10_15

mua_bivariate_index_cluster_array_10_21_intercal = array(NA,c(num_mua_cities,(3+4)))
colnames(mua_bivariate_index_cluster_array_10_21_intercal) = c('mua_objectid','mua_aggname', 'mua_cellcount',
                                                               'cluster_lowBF_lowPR',  'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')

mua_bivariate_index_cluster_array_10_21_intercal[,1:3] = mua_bivariate_index_array_10_21_intercal[,1:3]
for (imua in 1:num_mua_cities) {
  mua_bivariate_index_cluster_array_10_21_intercal[imua,4] = sum(as.matrix(as.numeric(mua_bivariate_index_array_10_21_intercal[imua,(15 + c(19, 20, 21, 25, 26, 27, 31, 32, 33))])))
  mua_bivariate_index_cluster_array_10_21_intercal[imua,5] = sum(as.matrix(as.numeric(mua_bivariate_index_array_10_21_intercal[imua,(15 + c(1, 2, 3, 7, 8, 9, 13, 14, 15))])))
  mua_bivariate_index_cluster_array_10_21_intercal[imua,6] = sum(as.matrix(as.numeric(mua_bivariate_index_array_10_21_intercal[imua,(15 + c(22, 23, 24, 28, 29, 30, 34, 35, 36))])))
  mua_bivariate_index_cluster_array_10_21_intercal[imua,7] = sum(as.matrix(as.numeric(mua_bivariate_index_array_10_21_intercal[imua,(15 + c(4, 5, 6, 10, 11, 12, 16, 17, 18))])))
}

outfile_name = paste(output_dir,'all_mua_bivariate_cluster_stats_93_00_intercal','.csv',sep='')
write.csv(mua_bivariate_index_cluster_array_93_00_intercal,file = outfile_name,row.names = F)

outfile_name = paste(output_dir,'all_mua_bivariate_cluster_stats_99_09','.csv',sep='')
write.csv(mua_bivariate_index_cluster_array_99_09,file = outfile_name,row.names = F)

outfile_name = paste(output_dir,'all_mua_bivariate_cluster_stats_10_21_interca_','.csv',sep='')
write.csv(mua_bivariate_index_cluster_array_10_21_intercal,file = outfile_name,row.names = F)

###########################################################################################################################################################
# make regional and economy totals of bivariate choropleth cluster stats

# v2 adds intercalibration totals

###############################
# regional totals

# region_bivariate_index_cluster_array_10_21 = as.data.frame(array(0,c(num_regions,2+4)))
# colnames(region_bivariate_index_cluster_array_10_21) = c('region','cell_count','cluster_lowBF_lowPR',  
#                                                          'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
region_bivariate_index_cluster_array_99_09 = as.data.frame(array(0,c(num_regions,2+4)))
colnames(region_bivariate_index_cluster_array_99_09) = c('region','cell_count','cluster_lowBF_lowPR',  
                                                         'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
# region_bivariate_index_cluster_array_93_00 = as.data.frame(array(0,c(num_regions,2+4)))
# colnames(region_bivariate_index_cluster_array_93_00) = c('region','cell_count','cluster_lowBF_lowPR',  
#                                                          'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
region_bivariate_index_cluster_array_10_21_intercal = as.data.frame(array(0,c(num_regions,2+4)))
colnames(region_bivariate_index_cluster_array_10_21_intercal) = c('region','cell_count','cluster_lowBF_lowPR',  
                                                                  'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
region_bivariate_index_cluster_array_93_00_intercal = as.data.frame(array(0,c(num_regions,2+4)))
colnames(region_bivariate_index_cluster_array_93_00_intercal) = c('region','cell_count','cluster_lowBF_lowPR',  
                                                                  'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')

# mua_bivariate_index_cluster_array_10_21 = as.data.frame(mua_bivariate_index_cluster_array_10_21)
# mua_bivariate_index_cluster_array_10_21 = na.omit(mua_bivariate_index_cluster_array_10_21)

mua_bivariate_index_cluster_array_99_09 = as.data.frame(mua_bivariate_index_cluster_array_99_09)
mua_bivariate_index_cluster_array_99_09 = na.omit(mua_bivariate_index_cluster_array_99_09)

# mua_bivariate_index_cluster_array_93_00 = as.data.frame(mua_bivariate_index_cluster_array_93_00)
# mua_bivariate_index_cluster_array_93_00 = na.omit(mua_bivariate_index_cluster_array_93_00)

mua_bivariate_index_cluster_array_10_21_intercal = as.data.frame(mua_bivariate_index_cluster_array_10_21_intercal)
mua_bivariate_index_cluster_array_10_21_intercal = na.omit(mua_bivariate_index_cluster_array_10_21_intercal)

mua_bivariate_index_cluster_array_93_00_intercal = as.data.frame(mua_bivariate_index_cluster_array_93_00_intercal)
mua_bivariate_index_cluster_array_93_00_intercal = na.omit(mua_bivariate_index_cluster_array_93_00_intercal)

for (iregion in 1:num_regions)  {
  
  region_name = region_list[iregion]
  # region_bivariate_index_cluster_array_10_21[iregion,1] = region_name
  region_bivariate_index_cluster_array_99_09[iregion,1] = region_name
  # region_bivariate_index_cluster_array_93_00[iregion,1] = region_name
  
  region_bivariate_index_cluster_array_10_21_intercal[iregion,1] = region_name
  region_bivariate_index_cluster_array_93_00_intercal[iregion,1] = region_name
  
  region_mua_subset = subset(mua_city_list, mua_city_list$region_1 == region_name)
  
  num_mua_in_region = dim(region_mua_subset)[1]
  
  for (imua in 1:(num_mua_in_region)) {
    region_mua_name = region_mua_subset[imua,2]
    # mua_stats_10_21 = as.data.frame(
    #   subset(mua_bivariate_index_cluster_array_10_21, mua_bivariate_index_cluster_array_10_21$mua_aggname == region_mua_name))
    mua_stats_99_09 = as.data.frame(
      subset(mua_bivariate_index_cluster_array_99_09, mua_bivariate_index_cluster_array_99_09$mua_aggname == region_mua_name))
    # mua_stats_93_00 = as.data.frame(
    #   subset(mua_bivariate_index_cluster_array_93_00, mua_bivariate_index_cluster_array_93_00$mua_aggname == region_mua_name))
    
    mua_stats_10_21_intercal = as.data.frame(
      subset(mua_bivariate_index_cluster_array_10_21_intercal, mua_bivariate_index_cluster_array_10_21_intercal$mua_aggname == region_mua_name))
    mua_stats_93_00_intercal = as.data.frame(
      subset(mua_bivariate_index_cluster_array_93_00_intercal, mua_bivariate_index_cluster_array_93_00_intercal$mua_aggname == region_mua_name))
    
    # count_10_21 = dim(mua_stats_10_21)[1]
    count_99_09 = dim(mua_stats_99_09)[1]
    # count_93_00 = dim(mua_stats_93_00)[1]
    
    count_10_21_intercal = dim(mua_stats_10_21_intercal)[1]
    count_93_00_intercal = dim(mua_stats_93_00_intercal)[1]
    
    # if (count_10_21 > 0) {
    #   for (icount in 1:count_10_21) {
    #     region_bivariate_index_cluster_array_10_21[iregion,2:6] = region_bivariate_index_cluster_array_10_21[iregion,2:6] +
    #       as.numeric(mua_stats_10_21[icount,3:7])
    #   }
    # }
    
    if (count_99_09 > 0) {
      for (icount in 1:count_99_09) {
        region_bivariate_index_cluster_array_99_09[iregion,2:6] = region_bivariate_index_cluster_array_99_09[iregion,2:6] +
          as.numeric(mua_stats_99_09[icount,3:7])
      }
    }
    
    # if (count_93_00 > 0) {
    #   for (icount in 1:count_93_00) {
    #     region_bivariate_index_cluster_array_93_00[iregion,2:6] = region_bivariate_index_cluster_array_93_00[iregion,2:6] +
    #       as.numeric(mua_stats_93_00[icount,3:7])
    #   }
    # }
    
    if (count_10_21_intercal > 0) {
      for (icount in 1:count_10_21_intercal) {
        region_bivariate_index_cluster_array_10_21_intercal[iregion,2:6] = region_bivariate_index_cluster_array_10_21_intercal[iregion,2:6] +
          as.numeric(mua_stats_10_21_intercal[icount,3:7])
      }
    }
    
    if (count_93_00_intercal > 0) {
      for (icount in 1:count_93_00_intercal) {
        region_bivariate_index_cluster_array_93_00_intercal[iregion,2:6] = region_bivariate_index_cluster_array_93_00_intercal[iregion,2:6] +
          as.numeric(mua_stats_93_00_intercal[icount,3:7])
      }
    }
    
  }
  
}

outfile_name = paste(output_dir,'region_bivariate_cluster_stats_93_00_intercal','.csv',sep='')
write.csv(region_bivariate_index_cluster_array_93_00_intercal,file = outfile_name,row.names = F)

outfile_name = paste(output_dir,'region_bivariate_cluster_stats_99_09','.csv',sep='')
write.csv(region_bivariate_index_cluster_array_99_09,file = outfile_name,row.names = F)

outfile_name = paste(output_dir,'region_bivariate_cluster_stats_10_21_intercal','.csv',sep='')
write.csv(region_bivariate_index_cluster_array_10_21_intercal,file = outfile_name,row.names = F)

###############################
# economy totals

# economy_bivariate_index_cluster_array_10_21 = as.data.frame(array(0,c(num_econ,2+4)))
# colnames(economy_bivariate_index_cluster_array_10_21) = c('economy','cell_count','cluster_lowBF_lowPR',  
#                                                           'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
economy_bivariate_index_cluster_array_99_09 = as.data.frame(array(0,c(num_econ,2+4)))
colnames(economy_bivariate_index_cluster_array_99_09) = c('economy','cell_count','cluster_lowBF_lowPR',  
                                                          'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
# economy_bivariate_index_cluster_array_93_00 = as.data.frame(array(0,c(num_econ,2+4)))
# colnames(economy_bivariate_index_cluster_array_93_00) = c('economy','cell_count','cluster_lowBF_lowPR',  
#                                                           'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
economy_bivariate_index_cluster_array_10_21_intercal = as.data.frame(array(0,c(num_econ,2+4)))
colnames(economy_bivariate_index_cluster_array_10_21_intercal) = c('economy','cell_count','cluster_lowBF_lowPR',  
                                                                   'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
economy_bivariate_index_cluster_array_93_00_intercal = as.data.frame(array(0,c(num_econ,2+4)))
colnames(economy_bivariate_index_cluster_array_93_00_intercal) = c('economy','cell_count','cluster_lowBF_lowPR',  
                                                                   'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')

# mua_bivariate_index_cluster_array_10_21 = as.data.frame(mua_bivariate_index_cluster_array_10_21)
# mua_bivariate_index_cluster_array_10_21 = na.omit(mua_bivariate_index_cluster_array_10_21)

mua_bivariate_index_cluster_array_99_09 = as.data.frame(mua_bivariate_index_cluster_array_99_09)
mua_bivariate_index_cluster_array_99_09 = na.omit(mua_bivariate_index_cluster_array_99_09)

# mua_bivariate_index_cluster_array_93_00 = as.data.frame(mua_bivariate_index_cluster_array_93_00)
# mua_bivariate_index_cluster_array_93_00 = na.omit(mua_bivariate_index_cluster_array_93_00)

mua_bivariate_index_cluster_array_10_21_intercal = as.data.frame(mua_bivariate_index_cluster_array_10_21_intercal)
mua_bivariate_index_cluster_array_10_21_intercal = na.omit(mua_bivariate_index_cluster_array_10_21_intercal)

mua_bivariate_index_cluster_array_93_00_intercal = as.data.frame(mua_bivariate_index_cluster_array_93_00_intercal)
mua_bivariate_index_cluster_array_93_00_intercal = na.omit(mua_bivariate_index_cluster_array_93_00_intercal)

for (ieconomy in 1:num_econ)  {
  
  economy_name = econ_list[ieconomy]
  # economy_bivariate_index_cluster_array_10_21[ieconomy,1] = economy_name
  economy_bivariate_index_cluster_array_99_09[ieconomy,1] = economy_name
  # economy_bivariate_index_cluster_array_93_00[ieconomy,1] = economy_name
  
  economy_bivariate_index_cluster_array_10_21_intercal[ieconomy,1] = economy_name
  economy_bivariate_index_cluster_array_93_00_intercal[ieconomy,1] = economy_name
  
  economy_mua_subset = subset(mua_city_list, mua_city_list$economy == economy_name)
  
  num_mua_in_economy = dim(economy_mua_subset)[1]
  
  for (imua in 1:(num_mua_in_economy)) {
    economy_mua_name = economy_mua_subset[imua,2]
    # mua_stats_10_21 = as.data.frame(
    #   subset(mua_bivariate_index_cluster_array_10_21, mua_bivariate_index_cluster_array_10_21$mua_aggname == economy_mua_name))
    mua_stats_99_09 = as.data.frame(
      subset(mua_bivariate_index_cluster_array_99_09, mua_bivariate_index_cluster_array_99_09$mua_aggname == economy_mua_name))
    # mua_stats_93_00 = as.data.frame(
    #   subset(mua_bivariate_index_cluster_array_93_00, mua_bivariate_index_cluster_array_93_00$mua_aggname == economy_mua_name))
    
    mua_stats_10_21_intercal = as.data.frame(
      subset(mua_bivariate_index_cluster_array_10_21_intercal, mua_bivariate_index_cluster_array_10_21_intercal$mua_aggname == economy_mua_name))
    mua_stats_93_00_intercal = as.data.frame(
      subset(mua_bivariate_index_cluster_array_93_00_intercal, mua_bivariate_index_cluster_array_93_00_intercal$mua_aggname == economy_mua_name))
    
    # count_10_21 = dim(mua_stats_10_21)[1]
    count_99_09 = dim(mua_stats_99_09)[1]
    # count_93_00 = dim(mua_stats_93_00)[1]
    
    count_10_21_intercal = dim(mua_stats_10_21_intercal)[1]
    count_93_00_intercal = dim(mua_stats_93_00_intercal)[1]
    
    # if (count_10_21 > 0) {
    #   for (icount in 1:count_10_21) {
    #     economy_bivariate_index_cluster_array_10_21[ieconomy,2:6] = economy_bivariate_index_cluster_array_10_21[ieconomy,2:6] +
    #       as.numeric(mua_stats_10_21[icount,3:7])
    #   }
    # }

    if (count_99_09 > 0) {
      for (icount in 1:count_99_09) {
        economy_bivariate_index_cluster_array_99_09[ieconomy,2:6] = economy_bivariate_index_cluster_array_99_09[ieconomy,2:6] +
          as.numeric(mua_stats_99_09[icount,3:7])
      }
    }
    
    # if (count_93_00 > 0) {
    #   for (icount in 1:count_93_00) {
    #     economy_bivariate_index_cluster_array_93_00[ieconomy,2:6] = economy_bivariate_index_cluster_array_93_00[ieconomy,2:6] +
    #       as.numeric(mua_stats_93_00[icount,3:7])
    #   }
    # }
    
    if (count_10_21_intercal > 0) {
      for (icount in 1:count_10_21_intercal) {
        economy_bivariate_index_cluster_array_10_21_intercal[ieconomy,2:6] = economy_bivariate_index_cluster_array_10_21_intercal[ieconomy,2:6] +
          as.numeric(mua_stats_10_21_intercal[icount,3:7])
      }
    }
    
    if (count_93_00_intercal > 0) {
      for (icount in 1:count_93_00_intercal) {
        economy_bivariate_index_cluster_array_93_00_intercal[ieconomy,2:6] = economy_bivariate_index_cluster_array_93_00_intercal[ieconomy,2:6] +
          as.numeric(mua_stats_93_00_intercal[icount,3:7])
      }
    }
    
  }
  
}

outfile_name = paste(output_dir,'economy_bivariate_cluster_stats_93_00_intercal','.csv',sep='')
write.csv(economy_bivariate_index_cluster_array_93_00_intercal,file = outfile_name,row.names = F)

outfile_name = paste(output_dir,'economy_bivariate_cluster_stats_99_09','.csv',sep='')
write.csv(economy_bivariate_index_cluster_array_99_09,file = outfile_name,row.names = F)

outfile_name = paste(output_dir,'economy_bivariate_cluster_stats_10_21_intercal','.csv',sep='')
write.csv(economy_bivariate_index_cluster_array_10_21_intercal,file = outfile_name,row.names = F)


###############################
# sub-region totals

# subregion_bivariate_index_cluster_array_10_21 = as.data.frame(array(0,c(num_sub_regions,2+4)))
# colnames(subregion_bivariate_index_cluster_array_10_21) = c('subregion','cell_count','cluster_lowBF_lowPR',  
#                                                             'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
subregion_bivariate_index_cluster_array_99_09 = as.data.frame(array(0,c(num_sub_regions,2+4)))
colnames(subregion_bivariate_index_cluster_array_99_09) = c('subregion','cell_count','cluster_lowBF_lowPR',  
                                                            'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
# subregion_bivariate_index_cluster_array_93_00 = as.data.frame(array(0,c(num_sub_regions,2+4)))
# colnames(subregion_bivariate_index_cluster_array_93_00) = c('subregion','cell_count','cluster_lowBF_lowPR',  
#                                                             'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
subregion_bivariate_index_cluster_array_10_21_intercal = as.data.frame(array(0,c(num_sub_regions,2+4)))
colnames(subregion_bivariate_index_cluster_array_10_21_intercal) = c('subregion','cell_count','cluster_lowBF_lowPR',  
                                                                     'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')
subregion_bivariate_index_cluster_array_93_00_intercal = as.data.frame(array(0,c(num_sub_regions,2+4)))
colnames(subregion_bivariate_index_cluster_array_93_00_intercal) = c('subregion','cell_count','cluster_lowBF_lowPR',  
                                                                     'cluster_highBF_lowPR', 'cluster_lowBF_highPR',  'cluster_highBF_highPR')

# mua_bivariate_index_cluster_array_10_21 = as.data.frame(mua_bivariate_index_cluster_array_10_21)
# mua_bivariate_index_cluster_array_10_21 = na.omit(mua_bivariate_index_cluster_array_10_21)

mua_bivariate_index_cluster_array_99_09 = as.data.frame(mua_bivariate_index_cluster_array_99_09)
mua_bivariate_index_cluster_array_99_09 = na.omit(mua_bivariate_index_cluster_array_99_09)

# mua_bivariate_index_cluster_array_93_00 = as.data.frame(mua_bivariate_index_cluster_array_93_00)
# mua_bivariate_index_cluster_array_93_00 = na.omit(mua_bivariate_index_cluster_array_93_00)

mua_bivariate_index_cluster_array_10_21_intercal = as.data.frame(mua_bivariate_index_cluster_array_10_21_intercal)
mua_bivariate_index_cluster_array_10_21_intercal = na.omit(mua_bivariate_index_cluster_array_10_21_intercal)

mua_bivariate_index_cluster_array_93_00_intercal = as.data.frame(mua_bivariate_index_cluster_array_93_00_intercal)
mua_bivariate_index_cluster_array_93_00_intercal = na.omit(mua_bivariate_index_cluster_array_93_00_intercal)

for (isubregion in 1:num_sub_regions)  {
  
  subregion_name = sub_region_list[isubregion]
  # subregion_bivariate_index_cluster_array_10_21[isubregion,1] = subregion_name
  subregion_bivariate_index_cluster_array_99_09[isubregion,1] = subregion_name
  # subregion_bivariate_index_cluster_array_93_00[isubregion,1] = subregion_name
  
  subregion_bivariate_index_cluster_array_10_21_intercal[isubregion,1] = subregion_name
  subregion_bivariate_index_cluster_array_93_00_intercal[isubregion,1] = subregion_name
  
  subregion_mua_subset = subset(mua_city_list, mua_city_list$sub_regions == subregion_name)
  
  num_mua_in_subregion = dim(subregion_mua_subset)[1]
  
  for (imua in 1:(num_mua_in_subregion)) {
    subregion_mua_name = subregion_mua_subset[imua,2]
    # mua_stats_10_21 = as.data.frame(
    #   subset(mua_bivariate_index_cluster_array_10_21, mua_bivariate_index_cluster_array_10_21$mua_aggname == subregion_mua_name))
    mua_stats_99_09 = as.data.frame(
      subset(mua_bivariate_index_cluster_array_99_09, mua_bivariate_index_cluster_array_99_09$mua_aggname == subregion_mua_name))
    # mua_stats_93_00 = as.data.frame(
    #   subset(mua_bivariate_index_cluster_array_93_00, mua_bivariate_index_cluster_array_93_00$mua_aggname == subregion_mua_name))
    
    mua_stats_10_21_intercal = as.data.frame(
      subset(mua_bivariate_index_cluster_array_10_21_intercal, mua_bivariate_index_cluster_array_10_21_intercal$mua_aggname == subregion_mua_name))
    mua_stats_93_00_intercal = as.data.frame(
      subset(mua_bivariate_index_cluster_array_93_00_intercal, mua_bivariate_index_cluster_array_93_00_intercal$mua_aggname == subregion_mua_name))
    
    # count_10_21 = dim(mua_stats_10_21)[1]
    count_99_09 = dim(mua_stats_99_09)[1]
    # count_93_00 = dim(mua_stats_93_00)[1]
    
    count_10_21_intercal = dim(mua_stats_10_21_intercal)[1]
    count_93_00_intercal = dim(mua_stats_93_00_intercal)[1]
    
    # if (count_10_21 > 0) {
    #   for (icount in 1:count_10_21) {
    #     subregion_bivariate_index_cluster_array_10_21[isubregion,2:6] = subregion_bivariate_index_cluster_array_10_21[isubregion,2:6] +
    #       as.numeric(mua_stats_10_21[icount,3:7])
    #   }
    # }
    
    if (count_99_09 > 0) {
      for (icount in 1:count_99_09) {
        subregion_bivariate_index_cluster_array_99_09[isubregion,2:6] = subregion_bivariate_index_cluster_array_99_09[isubregion,2:6] +
          as.numeric(mua_stats_99_09[icount,3:7])
      }
    }
    
    # if (count_93_00 > 0) {
    #   for (icount in 1:count_93_00) {
    #     subregion_bivariate_index_cluster_array_93_00[isubregion,2:6] = subregion_bivariate_index_cluster_array_93_00[isubregion,2:6] +
    #       as.numeric(mua_stats_93_00[icount,3:7])
    #   }
    # }
    
    if (count_10_21_intercal > 0) {
      for (icount in 1:count_10_21_intercal) {
        subregion_bivariate_index_cluster_array_10_21_intercal[isubregion,2:6] = subregion_bivariate_index_cluster_array_10_21_intercal[isubregion,2:6] +
          as.numeric(mua_stats_10_21_intercal[icount,3:7])
      }
    }
    
    if (count_93_00_intercal > 0) {
      for (icount in 1:count_93_00_intercal) {
        subregion_bivariate_index_cluster_array_93_00_intercal[isubregion,2:6] = subregion_bivariate_index_cluster_array_93_00_intercal[isubregion,2:6] +
          as.numeric(mua_stats_93_00_intercal[icount,3:7])
      }
    }
    
  }
  
}

outfile_name = paste(output_dir,'sub_region_bivariate_cluster_stats_93_00_intercal','.csv',sep='')
write.csv(subregion_bivariate_index_cluster_array_93_00_intercal,file = outfile_name,row.names = F)

outfile_name = paste(output_dir,'sub_region_bivariate_cluster_stats_99_09','.csv',sep='')
write.csv(subregion_bivariate_index_cluster_array_99_09,file = outfile_name,row.names = F)

outfile_name = paste(output_dir,'sub_region_bivariate_cluster_stats_10_21_intercal','.csv',sep='')
write.csv(subregion_bivariate_index_cluster_array_10_21_intercal,file = outfile_name,row.names = F)

# ############################################################################################################
#  build array for making regional bar plots binned by MUA population

global_mua_bin_data = mua_city_list[,1:12]
global_mua_bin_data = as.data.frame(global_mua_bin_data)

df_mua_bivariate_index_cluster_array_93_00_intercal = as.data.frame(mua_bivariate_index_cluster_array_93_00_intercal)
df_mua_bivariate_index_cluster_array_99_09 = as.data.frame(mua_bivariate_index_cluster_array_99_09)
df_mua_bivariate_index_cluster_array_10_21_intercal = as.data.frame(mua_bivariate_index_cluster_array_10_21_intercal)

global_mua_intercal_bin_data_2 <- merge(global_mua_bin_data, df_mua_bivariate_index_cluster_array_93_00_intercal)
colnames(global_mua_intercal_bin_data_2) = c('mua_objectid','mua_aggname_short','mua_country_code','mua_country','mua_mean_lon','mua_mean_lat',
                                             'mua_cellcount_orig','mua_worldpop_thousands','mua_area_m2','region_1','economy','sub_region',
                                    'mua_aggname2','mua_cellcount2','lowBF_lowPR_93_00','highBF_lowPR_93_00','lowBF_highPR_93_00','highBF_highPR_93_00') 
                                                               
global_mua_intercal_bin_data_3 <- merge(global_mua_intercal_bin_data_2, df_mua_bivariate_index_cluster_array_99_09)
colnames(global_mua_intercal_bin_data_3) = c('mua_objectid','mua_aggname_short','mua_country_code','mua_country','mua_mean_lon','mua_mean_lat',
                                             'mua_cellcount_orig','mua_worldpop_thousands','mua_area_m2','region_1','economy','sub_region',
                                             'mua_aggname2','mua_cellcount2','lowBF_lowPR_93_00','highBF_lowPR_93_00','lowBF_highPR_93_00','highBF_highPR_93_00',
                                             'mua_aggname3','mua_cellcount3','lowBF_lowPR_99_09','highBF_lowPR_99_09','lowBF_highPR_99_09','highBF_highPR_99_09') 

global_mua_intercal_bin_data_4 <- merge(global_mua_intercal_bin_data_3, df_mua_bivariate_index_cluster_array_10_21_intercal)
colnames(global_mua_intercal_bin_data_4) = c('mua_objectid','mua_aggname_short','mua_country_code','mua_country','mua_mean_lon','mua_mean_lat',
                                             'mua_cellcount_orig','mua_worldpop_thousands','mua_area_m2','region_1','economy','sub_region',
                                             'mua_aggname2','mua_cellcount2','lowBF_lowPR_93_00','highBF_lowPR_93_00','lowBF_highPR_93_00','highBF_highPR_93_00',
                                             'mua_aggname3','mua_cellcount3','lowBF_lowPR_99_09','highBF_lowPR_99_09','lowBF_highPR_99_09','highBF_highPR_99_09',
                                             'mua_aggname4','mua_cellcount4','lowBF_lowPR_10_21','highBF_lowPR_10_21','lowBF_highPR_10_21','highBF_highPR_10_21') 

temp = global_mua_intercal_bin_data_4[,colnames(global_mua_intercal_bin_data_4) != 'mua_aggname2']
temp = temp[,colnames(temp) != 'mua_aggname3']
temp = temp[,colnames(temp) != 'mua_aggname4']
temp = temp[,colnames(temp) != 'mua_cellcount2']
temp = temp[,colnames(temp) != 'mua_cellcount3']
temp = temp[,colnames(temp) != 'mua_cellcount4']

global_mua_intercal_bin_data = temp
# Header: 1-6:    mua_objectid   mua_aggname_short mua_cellcount mua_country_code mua_country mua_cell_count_orig 
#         7-12:      mua_worldpop_thousands mua_area_m2 mua_pop_per_km2  region_1  economy  sub_regions 
#         13-16:   lowBF_lowPR_93_99 highBF_lowPR_93_99 lowBF_highPR_93_99 highBF_highPR_93_99 		
#         16-20:  lowBF_lowPR_99_09  highBF_lowPR_99_09 lowBF_highPR_99_09 highBF_highPR_99_09 
#         21-24:  lowBF_lowPR_10_20 highBF_lowPR_10_20 lowBF_highPR_10_20 highBF_highPR_10_20

num_mua = dim(global_mua_intercal_bin_data)[1]

pop_ranges = c('>10M','5-10M','3-5M','2-3M','1-2M','<1M')
num_pop_ranges = length(pop_ranges)

sub_region_list =c('EastAfrica', 'WestAfrica', 'NorthAfrica', 'SouthAfrica', 'CentralAfrica', 'CentralAsia', 'SouthAsia',
                   'AustraliaNZ', 'China', 'EastAsia', 'Europe', 'India', 'LatAmerCarib', 'MiddleEast', 'NorAmer', 
                   'Russia', 'SEAsia')
num_sub_regions = length(sub_region_list)

#######################################################################################################################
# build array for plotting population ranges

# make plots for 12 regions or for 17 sub_regions (=10 regions + 2 OtherAsia and 5 Africa subregions)

for (tf_sub_regions in 0:1) {   # if 0, 12 regions; if 1, 17 sub_regions
  
  if (tf_sub_regions == 0) {
    num_reg_used = num_regions
    region_list_used = region_list
  } else {
    num_reg_used = num_sub_regions
    region_list_used = sub_region_list
  }
  
  global_mua_data_grid_pop_range_region = as.data.frame(array(NA,c((num_reg_used*num_pop_ranges),14)))
  colnames(global_mua_data_grid_pop_range_region) = c('region','pop_range', 
                                                      'lowBF_lowPR_93_99', 'highBF_lowPR_93_99', 'lowBF_highPR_93_99', 'highBF_highPR_93_99', 		
                                                      'lowBF_lowPR_99_09',  'highBF_lowPR_99_09', 'lowBF_highPR_99_09', 'highBF_highPR_99_09', 
                                                      'lowBF_lowPR_10_20', 'highBF_lowPR_10_20', 'lowBF_highPR_10_20', 'highBF_highPR_10_20')
  
  for (i in 1:num_reg_used) {
    region_name = region_list_used[i]
    
    if (tf_sub_regions == 0) {
      subset_array = subset(global_mua_intercal_bin_data, global_mua_intercal_bin_data$region_1 == region_name)
      global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+1):(i*num_pop_ranges),1] = region_name
    } else {
      subset_array = subset(global_mua_intercal_bin_data, global_mua_intercal_bin_data$sub_region == region_name)
      global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+1):(i*num_pop_ranges),1] = region_name
    }
    
    
    for (ii in 1:num_pop_ranges) {
      
      if (ii == 1) {
        subset_array_2 = subset(subset_array, subset_array$mua_worldpop_thousands > 10000)  
        global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),2] = pop_ranges[ii]
        if (is_null(dim(subset_array_2))) {
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),3:14] = 0 
        } else {
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),3] = sum(as.numeric(subset_array_2[,13]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),4] = sum(as.numeric(subset_array_2[,14]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),5] = sum(as.numeric(subset_array_2[,15]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),6] = sum(as.numeric(subset_array_2[,16]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),7] = sum(as.numeric(subset_array_2[,17]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),8] = sum(as.numeric(subset_array_2[,18]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),9] = sum(as.numeric(subset_array_2[,19]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),10] = sum(as.numeric(subset_array_2[,20]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),11] = sum(as.numeric(subset_array_2[,21]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),12] = sum(as.numeric(subset_array_2[,22]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),13] = sum(as.numeric(subset_array_2[,23]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),14] = sum(as.numeric(subset_array_2[,24]))
        }
        
      } else if (ii == 2) {
        subset_array_2 = subset(subset_array, subset_array$mua_worldpop_thousands <= 10000 & subset_array$mua_worldpop_thousands > 5000) 
        global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),2] = pop_ranges[ii]
        if (is_null(dim(subset_array_2))) {
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),3:14] = 0 
        } else {
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),3] = sum(as.numeric(subset_array_2[,13]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),4] = sum(as.numeric(subset_array_2[,14]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),5] = sum(as.numeric(subset_array_2[,15]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),6] = sum(as.numeric(subset_array_2[,16]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),7] = sum(as.numeric(subset_array_2[,17]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),8] = sum(as.numeric(subset_array_2[,18]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),9] = sum(as.numeric(subset_array_2[,19]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),10] = sum(as.numeric(subset_array_2[,20]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),11] = sum(as.numeric(subset_array_2[,21]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),12] = sum(as.numeric(subset_array_2[,22]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),13] = sum(as.numeric(subset_array_2[,23]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),14] = sum(as.numeric(subset_array_2[,24]))
        }
        
      } else if (ii == 3) {
        subset_array_2 = subset(subset_array, subset_array$mua_worldpop_thousands <= 5000 & subset_array$mua_worldpop_thousands > 3000)  
        global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),2] = pop_ranges[ii]
        if (is_null(dim(subset_array_2))) {
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),3:14] = 0 
        } else {
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),3] = sum(as.numeric(subset_array_2[,13]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),4] = sum(as.numeric(subset_array_2[,14]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),5] = sum(as.numeric(subset_array_2[,15]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),6] = sum(as.numeric(subset_array_2[,16]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),7] = sum(as.numeric(subset_array_2[,17]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),8] = sum(as.numeric(subset_array_2[,18]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),9] = sum(as.numeric(subset_array_2[,19]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),10] = sum(as.numeric(subset_array_2[,20]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),11] = sum(as.numeric(subset_array_2[,21]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),12] = sum(as.numeric(subset_array_2[,22]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),13] = sum(as.numeric(subset_array_2[,23]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),14] = sum(as.numeric(subset_array_2[,24]))
        }
        
      } else if (ii == 4) {
        subset_array_2 = subset(subset_array, subset_array$mua_worldpop_thousands <= 3000 & subset_array$mua_worldpop_thousands > 2000)  
        global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),2] = pop_ranges[ii]
        if (is_null(dim(subset_array_2))) {
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),3:14] = 0 
        } else {
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),3] = sum(as.numeric(subset_array_2[,13]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),4] = sum(as.numeric(subset_array_2[,14]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),5] = sum(as.numeric(subset_array_2[,15]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),6] = sum(as.numeric(subset_array_2[,16]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),7] = sum(as.numeric(subset_array_2[,17]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),8] = sum(as.numeric(subset_array_2[,18]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),9] = sum(as.numeric(subset_array_2[,19]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),10] = sum(as.numeric(subset_array_2[,20]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),11] = sum(as.numeric(subset_array_2[,21]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),12] = sum(as.numeric(subset_array_2[,22]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),13] = sum(as.numeric(subset_array_2[,23]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),14] = sum(as.numeric(subset_array_2[,24]))
        }
        
      } else if (ii == 5) {
        subset_array_2 = subset(subset_array, subset_array$mua_worldpop_thousands <= 2000 & subset_array$mua_worldpop_thousands > 1000)  
        global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),2] = pop_ranges[ii]
        if (is_null(dim(subset_array_2))) {
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),3:14] = 0 
        } else {
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),3] = sum(as.numeric(subset_array_2[,13]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),4] = sum(as.numeric(subset_array_2[,14]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),5] = sum(as.numeric(subset_array_2[,15]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),6] = sum(as.numeric(subset_array_2[,16]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),7] = sum(as.numeric(subset_array_2[,17]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),8] = sum(as.numeric(subset_array_2[,18]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),9] = sum(as.numeric(subset_array_2[,19]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),10] = sum(as.numeric(subset_array_2[,20]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),11] = sum(as.numeric(subset_array_2[,21]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),12] = sum(as.numeric(subset_array_2[,22]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),13] = sum(as.numeric(subset_array_2[,23]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),14] = sum(as.numeric(subset_array_2[,24]))
        }
        
      } else if (ii == 6) {
        subset_array_2 = subset(subset_array, subset_array$mua_worldpop_thousands <= 1000)  
        global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),2] = pop_ranges[ii]
        if (is_null(dim(subset_array_2))) {
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),3:14] = 0 
        } else {
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),3] = sum(as.numeric(subset_array_2[,13]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),4] = sum(as.numeric(subset_array_2[,14]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),5] = sum(as.numeric(subset_array_2[,15]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),6] = sum(as.numeric(subset_array_2[,16]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),7] = sum(as.numeric(subset_array_2[,17]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),8] = sum(as.numeric(subset_array_2[,18]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),9] = sum(as.numeric(subset_array_2[,19]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),10] = sum(as.numeric(subset_array_2[,20]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),11] = sum(as.numeric(subset_array_2[,21]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),12] = sum(as.numeric(subset_array_2[,22]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),13] = sum(as.numeric(subset_array_2[,23]))
          global_mua_data_grid_pop_range_region[((i-1)*num_pop_ranges+ii),14] = sum(as.numeric(subset_array_2[,24]))
        }
        
      }
      
    }
    
  }
  
  # build array for ggplot
  
  array_num = dim(global_mua_data_grid_pop_range_region)[1]
  global_mua_data_grid_pop_range_region_plot = as.data.frame(array(NA,c(3*4*array_num,5)))
  colnames(global_mua_data_grid_pop_range_region_plot) = c('region','pop_range','decade','delBF_delPR','count')
  
  
  global_mua_data_grid_pop_range_region_plot[(((0*array_num)+1):(1*array_num)),1:2] = global_mua_data_grid_pop_range_region[,1:2]
  global_mua_data_grid_pop_range_region_plot[(((0*array_num)+1):(1*array_num)),3] = '1990s'
  global_mua_data_grid_pop_range_region_plot[(((0*array_num)+1):(1*array_num)),4] = 'low_low'
  global_mua_data_grid_pop_range_region_plot[(((0*array_num)+1):(1*array_num)),5] = global_mua_data_grid_pop_range_region[,3]
  
  global_mua_data_grid_pop_range_region_plot[(((1*array_num)+1):(2*array_num)),1:2] = global_mua_data_grid_pop_range_region[,1:2]
  global_mua_data_grid_pop_range_region_plot[(((1*array_num)+1):(2*array_num)),3] = '1990s'
  global_mua_data_grid_pop_range_region_plot[(((1*array_num)+1):(2*array_num)),4] = 'high_low'
  global_mua_data_grid_pop_range_region_plot[(((1*array_num)+1):(2*array_num)),5] = global_mua_data_grid_pop_range_region[,4]
  
  global_mua_data_grid_pop_range_region_plot[(((2*array_num)+1):(3*array_num)),1:2] = global_mua_data_grid_pop_range_region[,1:2]
  global_mua_data_grid_pop_range_region_plot[(((2*array_num)+1):(3*array_num)),3] = '1990s'
  global_mua_data_grid_pop_range_region_plot[(((2*array_num)+1):(3*array_num)),4] = 'low_high'
  global_mua_data_grid_pop_range_region_plot[(((2*array_num)+1):(3*array_num)),5] = global_mua_data_grid_pop_range_region[,5]
  
  global_mua_data_grid_pop_range_region_plot[(((3*array_num)+1):(4*array_num)),1:2] = global_mua_data_grid_pop_range_region[,1:2]
  global_mua_data_grid_pop_range_region_plot[(((3*array_num)+1):(4*array_num)),3] = '1990s'
  global_mua_data_grid_pop_range_region_plot[(((3*array_num)+1):(4*array_num)),4] = 'high_high'
  global_mua_data_grid_pop_range_region_plot[(((3*array_num)+1):(4*array_num)),5] = global_mua_data_grid_pop_range_region[,6]
  
  global_mua_data_grid_pop_range_region_plot[(((4*array_num)+1):(5*array_num)),1:2] = global_mua_data_grid_pop_range_region[,1:2]
  global_mua_data_grid_pop_range_region_plot[(((4*array_num)+1):(5*array_num)),3] = '2000s'
  global_mua_data_grid_pop_range_region_plot[(((4*array_num)+1):(5*array_num)),4] = 'low_low'
  global_mua_data_grid_pop_range_region_plot[(((4*array_num)+1):(5*array_num)),5] = global_mua_data_grid_pop_range_region[,7]
  
  global_mua_data_grid_pop_range_region_plot[(((5*array_num)+1):(6*array_num)),1:2] = global_mua_data_grid_pop_range_region[,1:2]
  global_mua_data_grid_pop_range_region_plot[(((5*array_num)+1):(6*array_num)),3] = '2000s'
  global_mua_data_grid_pop_range_region_plot[(((5*array_num)+1):(6*array_num)),4] = 'high_low'
  global_mua_data_grid_pop_range_region_plot[(((5*array_num)+1):(6*array_num)),5] = global_mua_data_grid_pop_range_region[,8]
  
  global_mua_data_grid_pop_range_region_plot[(((6*array_num)+1):(7*array_num)),1:2] = global_mua_data_grid_pop_range_region[,1:2]
  global_mua_data_grid_pop_range_region_plot[(((6*array_num)+1):(7*array_num)),3] = '2000s'
  global_mua_data_grid_pop_range_region_plot[(((6*array_num)+1):(7*array_num)),4] = 'low_high'
  global_mua_data_grid_pop_range_region_plot[(((6*array_num)+1):(7*array_num)),5] = global_mua_data_grid_pop_range_region[,9]
  
  global_mua_data_grid_pop_range_region_plot[(((7*array_num)+1):(8*array_num)),1:2] = global_mua_data_grid_pop_range_region[,1:2]
  global_mua_data_grid_pop_range_region_plot[(((7*array_num)+1):(8*array_num)),3] = '2000s'
  global_mua_data_grid_pop_range_region_plot[(((7*array_num)+1):(8*array_num)),4] = 'high_high'
  global_mua_data_grid_pop_range_region_plot[(((7*array_num)+1):(8*array_num)),5] = global_mua_data_grid_pop_range_region[,10]
  
  global_mua_data_grid_pop_range_region_plot[(((8*array_num)+1):(9*array_num)),1:2] = global_mua_data_grid_pop_range_region[,1:2]
  global_mua_data_grid_pop_range_region_plot[(((8*array_num)+1):(9*array_num)),3] = '2010s'
  global_mua_data_grid_pop_range_region_plot[(((8*array_num)+1):(9*array_num)),4] = 'low_low'
  global_mua_data_grid_pop_range_region_plot[(((8*array_num)+1):(9*array_num)),5] = global_mua_data_grid_pop_range_region[,11]
  
  global_mua_data_grid_pop_range_region_plot[(((9*array_num)+1):(10*array_num)),1:2] = global_mua_data_grid_pop_range_region[,1:2]
  global_mua_data_grid_pop_range_region_plot[(((9*array_num)+1):(10*array_num)),3] = '2010s'
  global_mua_data_grid_pop_range_region_plot[(((9*array_num)+1):(10*array_num)),4] = 'high_low'
  global_mua_data_grid_pop_range_region_plot[(((9*array_num)+1):(10*array_num)),5] = global_mua_data_grid_pop_range_region[,12]
  
  global_mua_data_grid_pop_range_region_plot[(((10*array_num)+1):(11*array_num)),1:2] = global_mua_data_grid_pop_range_region[,1:2]
  global_mua_data_grid_pop_range_region_plot[(((10*array_num)+1):(11*array_num)),3] = '2010s'
  global_mua_data_grid_pop_range_region_plot[(((10*array_num)+1):(11*array_num)),4] = 'low_high'
  global_mua_data_grid_pop_range_region_plot[(((10*array_num)+1):(11*array_num)),5] = global_mua_data_grid_pop_range_region[,13]
  
  global_mua_data_grid_pop_range_region_plot[(((11*array_num)+1):(12*array_num)),1:2] = global_mua_data_grid_pop_range_region[,1:2]
  global_mua_data_grid_pop_range_region_plot[(((11*array_num)+1):(12*array_num)),3] = '2010s'
  global_mua_data_grid_pop_range_region_plot[(((11*array_num)+1):(12*array_num)),4] = 'high_high'
  global_mua_data_grid_pop_range_region_plot[(((11*array_num)+1):(12*array_num)),5] = global_mua_data_grid_pop_range_region[,14]
  
  # loop through sub-regions
  
  global_mua_data_grid_pop_range_region_plot$pop_range <- factor(global_mua_data_grid_pop_range_region_plot$pop_range, 
                                                                 levels=c('<1M' , '1-2M','2-3M','3-5M',
                                                                          '5-10M', '>10M'))
  
  if (tf_sub_regions == 0) {
    pdf(paste(output_dir,'MUA_region_decade_bivariate_groups_by_pop_range','.pdf',sep=''))
  } else {
    pdf(paste(output_dir,'MUA_sub_region_decade_bivariate_groups_by_pop_range','.pdf',sep=''))
  }
  
  for (i_region in 1:num_reg_used) {
    
    region_name = region_list_used[i_region]
    
    subset_plot_1 = subset(global_mua_data_grid_pop_range_region_plot, global_mua_data_grid_pop_range_region_plot$region == region_name)
    
    subset_plot_2 = subset(subset_plot_1, subset_plot_1$decade == '1990s')
    
    pr1 = ggplot(data=subset_plot_2, aes(x=pop_range, y=count, fill=delBF_delPR)) +
      #  geom_col(position="", stat="summary") +
      geom_col() +
      scale_fill_manual(values = c("#516ddd","#d46ddd" ,"#51d4ea", "#d4d4ed")) +   # blue, pink, cyan, gray
      labs(x='', y='# grid cells', title=paste(region_name, ' 1993-2000')) +
      theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme(legend.position = "none")   # angle = 45, vjust = 1, hjust=1  OR angle = 90, vjust = 0.5, hjust=1
    
    pr1
    
    subset_plot_2 = subset(subset_plot_1, subset_plot_1$decade == '2000s')
    
    pr2 = ggplot(data=subset_plot_2, aes(x=pop_range, y=count, fill=delBF_delPR)) +
      #  geom_col(position="", stat="summary") +
      geom_col() +
      scale_fill_manual(values = c("#516ddd","#d46ddd" ,"#51d4ea", "#d4d4ed")) +   # blue, pink, cyan, gray
      labs(x='', y='', title=paste(region_name, ' 1999-2009')) +
      theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme(legend.position = "none")   # angle = 45, vjust = 1, hjust=1  OR angle = 90, vjust = 0.5, hjust=1
    
    pr2
    
    subset_plot_2 = subset(subset_plot_1, subset_plot_1$decade == '2010s')
    
    pr3 = ggplot(data=subset_plot_2, aes(x=pop_range, y=count, fill=delBF_delPR)) +
      #  geom_col(position="", stat="summary") +
      geom_col()  +
      scale_fill_manual(values = c("#516ddd","#d46ddd" ,"#51d4ea", "#d4d4ed")) +   # blue, pink, cyan, gray
      labs(x='', y='', title=paste(region_name, '  2010-2020')) +
      theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + theme(legend.position = "none")   # angle = 45, vjust = 1, hjust=1  OR angle = 90, vjust = 0.5, hjust=1
    
    pr3
    
    print(
      ggarrange(pr1,pr2,pr3,  #
                #    labels = c("A", "B", "C", "D","E"),
                ncol = 3, nrow = 1)
    )
    
  }
  
  dev.off()
  
}  # end tf_sub_regions loop



















