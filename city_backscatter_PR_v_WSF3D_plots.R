# make plots of grid cell backscatter PR vs. WSF3D variables, smoothed
# May 2023

library(ggplot2)
library(ggpubr)

wd = getwd()

input_dir = paste(wd,'/mua_input_files/',sep='')
output_dir = paste(wd,'/mua_output_files/',sep='')

######################################################
# read in MUA/city list

mua_city_list_filename = paste(input_dir,'mua_city_list_lat_lon_clean_Jul2023.csv',sep='')
# NOTE: 'mua_city_list_lat_lon_clean.csv' has mua_objectid 1 (Winnipeg - no data) and 879 (Qinzhou - no data) removed
# NOTE:     Winnipeg & Qinzhou have data at different mua_objectid values; Bhubaneswar has 2 mua_objectid values with data

mua_city_list = read.csv(file=mua_city_list_filename,header = TRUE, stringsAsFactors = FALSE)
# HEADER: mua_objectid	mua_aggname_short	mua_country_code	mua_country	
#         mua_mean_lon	mua_mean_lat	mua_cell_count	mua_worldpop_millons	mua_area_km2

# remove any MUA rows with NA (did this manually outside of code)

num_mua_cities = dim(mua_city_list)[1]

######################################################
# read in subset city list for plotting examples

# 12 cities, 1 per region
city_subset_list = c('Lagos+','Sydney', 'Beijing+','Seoul+','London_*UK*', 'Delhi', 
                     'Sao-Paulo', 'Tehran+','Houston+', 'Karachi', 'Moscow', 'Ho-Chi-Minh-City') 

num_cities_subset = length(city_subset_list)

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

# make melted array for facet wrap plots

mua_data_plot = mua_data_in[,c(1:12,19,58,86:93,121,158)]
# mua_data_plot HEADER:
# Header: 1-6:    longitude	latitude	mua_objectid	mua_fid1	mua_country_code	mua_cellcount	
#         7-12:   mua_worldpop_thousand	mua_area_km2	mua_country	mua_aggname	grid_area_m2 water_pct	
#         13-14:     ASCAT2012_summer_mean_pr, ASCAT2019_summer_mean_pr,
#         15:18:  WSF3D_Vol_sum WSF3D_BF_mean WSF3D_BA_sum WSF3D_HT_mean 
#         19:22:  WSF3D_Vol_sum_3x3smooth WSF3D_BF_mean_3x3smooth WSF3D_BA_sum_3x3smooth WSF3D_HT_mean_3x3smooth 
#        23:  WSFEvolution_2012_mean
#       24:   region_1 

# first for the 12 example cities

for (imua in 1:num_cities_subset) {  # 
  
  mua_name = city_subset_list[imua]
  mua_subset_data = subset(mua_data_in, mua_data_in$mua_aggname == mua_name)
  num_grids_mua = dim(mua_subset_data)[1]
  
  mua_data_plot_2 = as.data.frame(array(NA,c(num_grids_mua,12)))
  colnames(mua_data_plot_2) = c('city', 'ASCAT_2012', 'ASCAT_2019', 'wsfevo_2012','wsf3D_vol','wsf3D_bf','wsf3D_ba','wsf3D_ht',
                               'wsf3D_vol_sm','wsf3D_bf_sm','wsf3D_ba_sm','wsf3D_ht_sm')
  mua_data_plot_2[,1] = mua_name
  mua_data_plot_2[,2] = mua_subset_data$ASCAT2012_summer_mean_pr
  mua_data_plot_2[,3] = mua_subset_data$ASCAT2019_summer_mean_pr
  mua_data_plot_2[,4] = mua_subset_data$WSFEvolution_2012_mean
  
  mua_data_plot_2[,5] = mua_subset_data$WSF3D_Vol_sum
  mua_data_plot_2[,6] = mua_subset_data$WSF3D_BF_mean
  mua_data_plot_2[,7] = mua_subset_data$WSF3D_BA_sum
  mua_data_plot_2[,8] = mua_subset_data$WSF3D_HT_mean

  mua_data_plot_2[,9] = mua_subset_data$WSF3D_Vol_sum_3x3smooth
  mua_data_plot_2[,10] = mua_subset_data$WSF3D_BF_mean_3x3smooth
  mua_data_plot_2[,11] = mua_subset_data$WSF3D_BA_sum_3x3smooth
  mua_data_plot_2[,12] = mua_subset_data$WSF3D_HT_mean_3x3smooth
  
  if (imua == 1) {
    mua_12_data_plot_2 = mua_data_plot_2
  } else {
    mua_12_data_plot_2 = rbind(mua_12_data_plot_2,mua_data_plot_2)
  }
  
}

mua_12_data_plot_2$mean_ASCAT_2012_2019 = (mua_12_data_plot_2$ASCAT_2012 + mua_12_data_plot_2$ASCAT_2019) / 2

pdf(paste(output_dir,'mua_12_ascat_v_wsf3d.pdf'))

# WSF3D Volume smoothed
p1 = ggplot(mua_12_data_plot_2, aes(x = wsf3D_vol_sm/1e9, y = mean_ASCAT_2012_2019, color = wsfevo_2012)) + 
  geom_point(size = 0.75) + theme_bw() + xlim(0,0.1310)  + ylim(0,0.9) +
  geom_smooth(method=lm, se=TRUE)
p1 = p1 +  stat_regline_equation(label.y = 0.9*0.9, aes(label = ..eq.label..),size = 2.5) +
  stat_regline_equation(label.y = 0.9*0.825, aes(label = ..rr.label..),size = 2.5)
p1 = p1 + xlab("WSF3D building volume 3x3sm km3") + ylab("mean ASCAT 2012 & 2019 summer PR")
p1 = p1 + facet_wrap(~city,ncol = 4)

plot(p1)

# WSF3D Height smoothed
p2 = ggplot(mua_12_data_plot_2, aes(x = wsf3D_ht_sm, y = ASCAT_2012, color = wsfevo_2012)) +  
  geom_point(size = 0.75) + theme_bw() + xlim(0,250)  + ylim(0,0.9) +
  geom_smooth(method=lm, se=TRUE)
p2 = p2 +  stat_regline_equation(label.y = 0.9*0.9, aes(label = ..eq.label..),size = 2.5) +
  stat_regline_equation(label.y = 0.9*0.825, aes(label = ..rr.label..),size = 2.5)
p2 = p2 + xlab("WSF3D building height 3x3sm m") + ylab("ASCAT 2012 summer PR")
p2 = p2 + facet_wrap(~city,ncol = 4)

plot(p2)

# WSF3D fraction smoothed
p3 = ggplot(mua_12_data_plot_2, aes(x = wsf3D_bf_sm/100, y = ASCAT_2019, color = wsfevo_2012)) +  # /100 for % to fraction
  geom_point(size = 0.75) + theme_bw() + xlim(0,0.4)  + ylim(0,0.9) +
  geom_smooth(method=lm, se=TRUE)
p3 = p3 +  stat_regline_equation(label.y = 0.9*0.9, aes(label = ..eq.label..),size = 2.5) +
  stat_regline_equation(label.y = 0.9*0.825, aes(label = ..rr.label..),size = 2.5)
p3 = p3 + xlab("WSF3D building fraction 3x3sm") + ylab("ASCAT 2019 summer PR")
p3 = p3 + facet_wrap(~city,ncol = 4)

plot(p3)

dev.off()


