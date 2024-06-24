# for all grid cells
#   compute ERS/QSCAT and QSCAT/ASCAT overlapping year backscatter PR trends

#     compute 3-year overlap trend intercalibration, at grid cell level, as 
#       qscat-ascat: 2007-2009
#       ers-qscat, northern hemisphere: 1998-2000, 1999-2001
#       ers-qscat, southern hemisphere: 1998-2000, 2000-2002

#     plot trend intercalibration comparison, as MUA means, for
#       qscat-ascat: 2007-2009 trends
#       ers-qscat, northern hemisphere: 1999-2000 differences
#       ers-qscat, southern hemisphere: NA -- there is only one year of overlap, so no trend

# probably don't need most of these ...
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

# 
# # add sub+_regions column (#160), ALSO update region list in the mua_all_grid file
# 
# global_mua_grid_data_wat50$sub_regions = NA
# 
# for (icountry in 1:num_countries) {
#   
#   country_name = country_list[icountry,2]
#   sub_region_name = country_list[icountry,5]
#   region_1_name = country_list[icountry,3]
#   
#   global_mua_grid_data_wat50$sub_regions[global_mua_grid_data_wat50$mua_country == country_name] <- sub_region_name
#   global_mua_grid_data_wat50$region_1[global_mua_grid_data_wat50$mua_country == country_name] <- region_1_name
#   
# }

# ******** NEW IN VERSION 3 ******** 

# compute 3-year PR trends 'intercalibrated' for ASCAT and ERS

# add columns 161-166 to grid array to hold trends

global_mua_grid_data_wat50$ers_2year_diff = NA   # 1999-2000, 
global_mua_grid_data_wat50$ers_2year_diff_intercal = NA   # 1999-2000, 
global_mua_grid_data_wat50$qscat_2year_diff = NA     # 1999-2000, northern hemisphere only

global_mua_grid_data_wat50$qscat_3year_trend = NA   #  2007-2009
global_mua_grid_data_wat50$ascat_3year_trend = NA   #  2007-2009
global_mua_grid_data_wat50$ascat_3year_trend_intercal = NA   #  2007-2009

years_q_a = 2007:2009
# years_e = 1998:2000
# years_qn = 1999:2001
# years_qs = 2000:2002

num_urban_grids = dim(global_mua_grid_data_wat50)[1]

for (igrid in 1:num_urban_grids) {
  
  # 2007-2009 QSCAT trend; inter-calibration factor = 1
  qscat_pr = global_mua_grid_data_wat50[igrid,50:52]
  qscat_lm = lm(as.numeric(qscat_pr) ~ years_q_a, na.action = na.omit)
  out = summary(qscat_lm)
  global_mua_grid_data_wat50[igrid,164] = out$coefficients[2,1] 
  
  # 2007-2009 ASCAT trend; inter-calibration factor = { <PR_Q>_2009-09 ÷ <PR_A>_2007-09 }
  ascat_pr = global_mua_grid_data_wat50[igrid,53:55]
  ascat_lm = lm(as.numeric(ascat_pr) ~ years_q_a, na.action = na.omit)
  out = summary(ascat_lm)
  global_mua_grid_data_wat50[igrid,165] = out$coefficients[2,1]   # no inter-calibration
  global_mua_grid_data_wat50[igrid,166] = out$coefficients[2,1] *   # with inter-calibration
    (global_mua_grid_data_wat50[igrid,50] + global_mua_grid_data_wat50[igrid,51] + global_mua_grid_data_wat50[igrid,52]) /
    (global_mua_grid_data_wat50[igrid,53] + global_mua_grid_data_wat50[igrid,54] + global_mua_grid_data_wat50[igrid,55])
  
  if (global_mua_grid_data_wat50[igrid,2] >= 0) {  # northern hemisphere
    
    # 1999-2000 QSCAT 2-year difference; inter-calibration factor = 1, northern hemisphere only
    global_mua_grid_data_wat50[igrid,163] = global_mua_grid_data_wat50[igrid,43] - global_mua_grid_data_wat50[igrid,42] # inter-calibration = 1
    
  }
  
  # 1998-2000 ERS 2-year difference (inter-calibration factor =  { <PR_Q>_1999-01 ÷ <PR_E>_1998-00 }
  global_mua_grid_data_wat50[igrid,161] = global_mua_grid_data_wat50[igrid,41] - global_mua_grid_data_wat50[igrid,40]
  global_mua_grid_data_wat50[igrid,162] = (global_mua_grid_data_wat50[igrid,41] - global_mua_grid_data_wat50[igrid,40]) * 
    (global_mua_grid_data_wat50[igrid,42] + global_mua_grid_data_wat50[igrid,43] + global_mua_grid_data_wat50[igrid,44]) /
    (global_mua_grid_data_wat50[igrid,39] + global_mua_grid_data_wat50[igrid,40] + global_mua_grid_data_wat50[igrid,41])
  
}


#   } else {  # southern hemisphere
  
  # if (global_mua_grid_data_wat50[igrid,2] >= 0) {  # northern hemisphere
  #   
  #   # 1999-2001 QSCAT trend; inter-calibration factor = 1
  #   qscat_pr = global_mua_grid_data_wat50[igrid,42:44]
  #   qscat_lm = lm(as.numeric(qscat_pr) ~ years_qn, na.action = na.omit)
  #   out = summary(qscat_lm)
  #   global_mua_grid_data_wat50[igrid,163] = out$coefficients[2,1] # inter-calibration = 1
  #   
  #   # 1998-2000 ERS trend (inter-calibration factor =  { <PR_Q>_1999-01 ÷ <PR_E>_1998-00 }
  #   ers_pr = global_mua_grid_data_wat50[igrid,39:41]
  #   ers_lm = lm(as.numeric(ers_pr) ~ years_e, na.action = na.omit)
  #   out = summary(ers_lm)
  #   global_mua_grid_data_wat50[igrid,161] = out$coefficients[2,1]
  #   global_mua_grid_data_wat50[igrid,162] = out$coefficients[2,1] * 
  #     (global_mua_grid_data_wat50[igrid,42] + global_mua_grid_data_wat50[igrid,43] + global_mua_grid_data_wat50[igrid,44]) /
  #     (global_mua_grid_data_wat50[igrid,39] + global_mua_grid_data_wat50[igrid,40] + global_mua_grid_data_wat50[igrid,41])
  # 
  #   # 1993-2000 ERS slope (inter-calibration factor =  { <PR_Q>_1999-01 ÷ <PR_E>_1993-00 }
  #   global_mua_grid_data_wat50[igrid,167] = global_mua_grid_data_wat50[igrid,70] * 
  #     ((global_mua_grid_data_wat50[igrid,42] + global_mua_grid_data_wat50[igrid,43] + global_mua_grid_data_wat50[igrid,44]) / 3) /
  #     ((global_mua_grid_data_wat50[igrid,34] + global_mua_grid_data_wat50[igrid,35] + global_mua_grid_data_wat50[igrid,36] +
  #         global_mua_grid_data_wat50[igrid,37] + global_mua_grid_data_wat50[igrid,38] + global_mua_grid_data_wat50[igrid,39] +
  #         global_mua_grid_data_wat50[igrid,40] + global_mua_grid_data_wat50[igrid,41]) / 8)
  #   
  # } else {  # southern hemisphere
  #   
  #   # 2000-2002 QSCAT trend; inter-calibration factor = 1
  #   qscat_pr = global_mua_grid_data_wat50[igrid,43:45]
  #   qscat_lm = lm(as.numeric(qscat_pr) ~ years_qs, na.action = na.omit)
  #   out = summary(qscat_lm)
  #   global_mua_grid_data_wat50[igrid,163] = out$coefficients[2,1] # inter-calibration = 1
  #   
  #   # 1998-2000 ERS trend (inter-calibration factor =  { <PR_Q>_2000-02 ÷ <PR_E>_1998-00 }
  #   ers_pr = global_mua_grid_data_wat50[igrid,39:41]
  #   ers_lm = lm(as.numeric(ers_pr) ~ years_e, na.action = na.omit)
  #   out = summary(ers_lm)
  #   global_mua_grid_data_wat50[igrid,161] = out$coefficients[2,1]  # no inter-calibration
  #   global_mua_grid_data_wat50[igrid,162] = out$coefficients[2,1] *  # with inter-calibration
  #     (global_mua_grid_data_wat50[igrid,43] + global_mua_grid_data_wat50[igrid,44] + global_mua_grid_data_wat50[igrid,45]) /
  #     (global_mua_grid_data_wat50[igrid,39] + global_mua_grid_data_wat50[igrid,40] + global_mua_grid_data_wat50[igrid,41])
  #   
  #   # 1993-2000 ERS slope (inter-calibration factor =  { <PR_Q>_2000-02 ÷ <PR_E>_1993-00 }
  #   global_mua_grid_data_wat50[igrid,167] = global_mua_grid_data_wat50[igrid,70] * 
  #     ((global_mua_grid_data_wat50[igrid,43] + global_mua_grid_data_wat50[igrid,44] + global_mua_grid_data_wat50[igrid,45]) / 3) /
  #     ((global_mua_grid_data_wat50[igrid,34] + global_mua_grid_data_wat50[igrid,35] + global_mua_grid_data_wat50[igrid,36] +
  #         global_mua_grid_data_wat50[igrid,37] + global_mua_grid_data_wat50[igrid,38] + global_mua_grid_data_wat50[igrid,39] +
  #         global_mua_grid_data_wat50[igrid,40] + global_mua_grid_data_wat50[igrid,41]) / 8)
  #   
  # }
  # 


# compute MUA means

mua_mean_working_data = array(NA,c(num_mua_cities,14))
colnames(mua_mean_working_data) = c('mua_objectid', 'mua_country_code',	'mua_cellcount',	'mua_country',	'mua_aggname',
                                    'region_1', 'economy','sub_regions',  
                                    'ers_2year_diff', 'ers_2year_diff_intercal', 
                                    'qscat_2year_diff', 'qscat_3year_trend', 
                                    'ascat_3year_trend', 'ascat_3year_trend_intercal')

mua_mean_working_data = as.data.frame(mua_mean_working_data)

for (icity in 1:num_mua_cities) {
  
  city_id = mua_city_list[icity,1]
  city_subset = subset(global_mua_grid_data_wat50, global_mua_grid_data_wat50$mua_objectid == city_id)
  
  mua_mean_working_data[icity,1] = city_subset[1,3]
  mua_mean_working_data[icity,2] = city_subset[1,5]
  mua_mean_working_data[icity,3] = city_subset[1,6]
  mua_mean_working_data[icity,4] = city_subset[1,9]
  mua_mean_working_data[icity,5] = city_subset[1,10]
  mua_mean_working_data[icity,6] = city_subset[1,158]
  mua_mean_working_data[icity,7] = city_subset[1,159]
  mua_mean_working_data[icity,8] = city_subset[1,160]
  mua_mean_working_data[icity,9] = mean(city_subset[,161],na.rm = T)
  mua_mean_working_data[icity,10] = mean(city_subset[,162],na.rm = T)
  mua_mean_working_data[icity,11] = mean(city_subset[,163],na.rm = T)
  mua_mean_working_data[icity,12] = mean(city_subset[,164],na.rm = T)
  mua_mean_working_data[icity,13] = mean(city_subset[,165],na.rm = T)
  mua_mean_working_data[icity,14] = mean(city_subset[,166],na.rm = T)
}

pm_qa_1 = ggplot(mua_mean_working_data, aes(y=ascat_3year_trend, x=qscat_3year_trend)) +
  geom_point(size=0.7) +
  #  scale_fill_manual(values = c("gray70","dodgerblue3" ,"darkorange3", "orchid4")) +
  scale_color_brewer(palette="Set3") +
  theme_bw()  + xlim(-0.02,0.025) + ylim(-0.02,0.025) + theme(legend.position = "right") +
  labs(title="QSCAT and ASCAT MUA mean trends for 2007-2009",
       x="rate of increase in QSCAT PR (1/y)", y = "rate of increase in ASCAT PR (1/y)") +
  theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
pm_qa_1 = pm_qa_1  + geom_abline(intercept = 0, slope = 1, linetype="dashed", color = "gray70", size=0.5)

pm_qa_1

pm_qa_2 = ggplot(mua_mean_working_data, aes(y=ascat_3year_trend_intercal, x=qscat_3year_trend)) +
  geom_point(size=0.7) +
  #  scale_fill_manual(values = c("gray70","dodgerblue3" ,"darkorange3", "orchid4")) +
  scale_color_brewer(palette="Set3") +
  theme_bw()  + xlim(-0.02,0.025) + ylim(-0.02,0.025) + theme(legend.position = "right") +
  labs(title="QSCAT and ASCAT MUA mean trends for 2007-2009",
       x="rate of increase in QSCAT PR (1/y)", y = "rate of increase in ASCAT_intercal PR (1/y)") +
  theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
pm_qa_2 = pm_qa_2  + geom_abline(intercept = 0, slope = 1, linetype="dashed", color = "gray70", size=0.5)

pm_qa_2

pm_eq_1 = ggplot(mua_mean_working_data, aes(y=ers_2year_diff, x=qscat_2year_diff)) +
  geom_point(size=0.7) +
  #  scale_fill_manual(values = c("gray70","dodgerblue3" ,"darkorange3", "orchid4")) +
  scale_color_brewer(palette="Set3") +
  theme_bw()  + xlim(-0.02,0.025) + ylim(-0.02,0.025) + theme(legend.position = "right") +
  labs(title="QSCAT and ERS MUA mean PR difference for 2000-1999",
       x="del QSCAT PR, 1999-2000", y = "del ERS PR, 1999-2000") +
  theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
pm_eq_1 = pm_eq_1  + geom_abline(intercept = 0, slope = 1, linetype="dashed", color = "gray70", size=0.5)

pm_eq_1

pm_eq_2 = ggplot(mua_mean_working_data, aes(y=ers_2year_diff_intercal, x=qscat_2year_diff)) +
  geom_point(size=0.7) +
  #  scale_fill_manual(values = c("gray70","dodgerblue3" ,"darkorange3", "orchid4")) +
  scale_color_brewer(palette="Set3") +
  theme_bw()  + xlim(-0.02,0.025) + ylim(-0.02,0.025) + theme(legend.position = "right") +
  labs(title="QSCAT and ERS_intercal MUA mean trends for 2000-1999",
       x="del QSCAT PR, 1999-2000", y = "del ERS_intercal PR, 1999-2000") +
  theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
pm_eq_2 = pm_eq_2  + geom_abline(intercept = 0, slope = 1, linetype="dashed", color = "gray70", size=0.5)

pm_eq_2


pdf(paste(output_dir,'scatterometer_trend_intercalibration_plot','.pdf',sep=''))

print(
  ggarrange(pm_qa_1, pm_qa_2, pm_eq_1, pm_eq_2,
            ncol = 2, nrow = 2)
)


dev.off()


