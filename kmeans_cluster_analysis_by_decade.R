# for all grid cells, do k-means cluster analysis by decade (1990s, 2000s, 2010s) with five clusters, 
#    using these four variables: initial state (i.e., PR_init and BF_init) and change variables (del-PR/dt, del-BF/dt)

library(ggplot2)
library(ggpubr)
library(stats)
library(factoextra)
library(cluster)
library(corrplot)
library(raster)
library(rasterVis)
library(rgdal)
library(alluvial)
library(ggalluvial)
library(dplyr)

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
# read in subset city list for plotting examples

# 12 cities, 1 per region
city_subset_list = c('Lagos+','Sydney', 'Beijing+','Seoul+','London_*UK*', 'Delhi', 
                     'Sao-Paulo', 'Tehran+','Houston+', 'Karachi', 'Moscow', 'Ho-Chi-Minh-City') 

# 12 more cities, 1 more per region
city_subset_list_2 = c('Nairobi', 'Melbourne', 'Xian+', 'Tokyo', 'Berlin', 'Mumbai+',
                       'Buenos-Aires', 'Dubai+', 'Toronto+', 'Istanbul+', 'Saint-Petersburg','Kuala-Lumpur')

num_cities_subset = length(city_subset_list)
num_cities_subset_2 = length(city_subset_list_2)

######################################################

# read in global MUA GRID data
# NOTE: the working csv file of all grid data included a number of variables that were not used in the end.  These have all been replace with 'NA' 
#    and their column names with NA1 through NA57

mua_data_filename = paste(input_dir,'global_mua_grid_variables_table_wat_lt_50.csv',sep = '')
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


# add sub+_regions column (#160), ALSO update region list in the mua_all_grid file

global_mua_grid_data_wat50$sub_regions = NA

for (icountry in 1:num_countries) {
  
  country_name = country_list[icountry,2]
  sub_region_name = country_list[icountry,5]
  region_1_name = country_list[icountry,3]
  
  global_mua_grid_data_wat50$sub_regions[global_mua_grid_data_wat50$mua_country == country_name] <- sub_region_name
  global_mua_grid_data_wat50$region_1[global_mua_grid_data_wat50$mua_country == country_name] <- region_1_name
  
}

mua_data_in = global_mua_grid_data_wat50


############################################################################################################
############################################################################################################
############################################################################################################

grid_cluster_array = mua_data_in[,c(1:12,34,43,56,70,74,78,102,109,119,124,154:155,157:160)]

# grid_cluster_array = mua_data_in_cellsGT15[,c(1:12,34,43,56,70,74,78,102,109,119,124,154:159)]

grid_cluster_array = na.omit(grid_cluster_array)

# normalize variables as z-scores using 'scale'

cluster_vars_states_rates = grid_cluster_array[,c(13:15,16:18,19:21, 23,24,25)]
cluster_vars_states_rates_scale = scale(cluster_vars_states_rates)

cluster_vars_1990s_scale = cluster_vars_states_rates_scale[,c(1,4,7,10)]
cluster_vars_2000s_scale = cluster_vars_states_rates_scale[,c(2,5,8,11)]
cluster_vars_2010s_scale = cluster_vars_states_rates_scale[,c(3,6,9,12)]

# head(cluster_vars_states_rates)
# head(cluster_vars_1990s_scale)
# head(cluster_vars_2000s_scale)
# head(cluster_vars_2010s_scale)
# 
# corrmatrix = cor(cluster_vars_scale)
# corrplot(corrmatrix, method = 'number' )
# 
# corrmatrix = cor(cluster_vars_state_93_scale)
# corrplot(corrmatrix, method = 'number' )
# 
# corrmatrix = cor(cluster_vars_state_00_scale)
# corrplot(corrmatrix, method = 'number' )
# 
# corrmatrix = cor(cluster_vars_states_rates)
# corrplot(corrmatrix, method = 'number' )

# summary(cluster_vars_states_rates)

# ################################################################################
# # determine optimal number of clusters
# 
# # use total_within_sum_of_squares method
# 
# max_clust_test = 9
# opt_cluster_array_1990s = array(NA,c((max_clust_test),2))
# colnames(opt_cluster_array_1990s) = c('number_clusters','total_within_sum_of_squares')
# opt_cluster_array_2000s = array(NA,c((max_clust_test),2))
# colnames(opt_cluster_array_2000s) = c('number_clusters','total_within_sum_of_squares')
# opt_cluster_array_2010s = array(NA,c((max_clust_test),2))
# colnames(opt_cluster_array_2010s) = c('number_clusters','total_within_sum_of_squares')
# opt_cluster_array_bf = array(NA,c((max_clust_test),2))
# colnames(opt_cluster_array_bf) = c('number_clusters','total_within_sum_of_squares')
# opt_cluster_array_pr = array(NA,c((max_clust_test),2))
# colnames(opt_cluster_array_pr) = c('number_clusters','total_within_sum_of_squares')
# 
# for (icluster in 1:max_clust_test)  {
#   
#   ##############################
#   # these helpful kmeans scripts don't run -- seems that array is too big
#   #
#   # fviz_nbclust(cluster_vars_scale, kmeans, method = "wss") +
#   #   geom_vline(xintercept = 3, linetype = 2)
#   # 
#   # fviz_nbclust(cluster_vars_scale, kmeans, method = "silhouette")
#   ##############################
#   
#   set.seed(123)
#   
#   global_grid_kmeans_array_opt = kmeans(cluster_vars_1990s_scale,icluster,nstart = icluster)
#   opt_cluster_array_1990s[icluster,1] = icluster
#   opt_cluster_array_1990s[icluster,2] = global_grid_kmeans_array_opt$tot.withinss
# 
#   global_grid_kmeans_array_opt = kmeans(cluster_vars_2000s_scale,icluster,nstart = icluster)
#   opt_cluster_array_2000s[icluster,1] = icluster
#   opt_cluster_array_2000s[icluster,2] = global_grid_kmeans_array_opt$tot.withinss
# 
#   global_grid_kmeans_array_opt = kmeans(cluster_vars_2010s_scale,icluster,nstart = icluster)
#   opt_cluster_array_2010s[icluster,1] = icluster
#   opt_cluster_array_2010s[icluster,2] = global_grid_kmeans_array_opt$tot.withinss
#   
#   rm(global_grid_kmeans_array_opt)
# }
# 
# pdf(paste(output_dir,'grid_cluster_tot_within_ss_4_vars.pdf'))
# 
# 
# plot(opt_cluster_array_1990s[,1],opt_cluster_array_1990s[,2],xlim = c(1,12),ylim = c(25000,160000),
#      xlab = 'number of clusters', ylab = 'total within Sum of Squares', main = '1990s')
# lines(opt_cluster_array_1990s[,1],opt_cluster_array_1990s[,2])
# 
# plot(opt_cluster_array_2000s[,1],opt_cluster_array_2000s[,2],xlim = c(1,12),ylim = c(25000,160000),
#      xlab = 'number of clusters', ylab = 'total within Sum of Squares', main = '2000s')
# lines(opt_cluster_array_2000s[,1],opt_cluster_array_2000s[,2])
# 
# plot(opt_cluster_array_2010s[,1],opt_cluster_array_2010s[,2],xlim = c(1,12),ylim = c(25000,160000),
#      xlab = 'number of clusters', ylab = 'total within Sum of Squares', main = '2010s')
# lines(opt_cluster_array_2010s[,1],opt_cluster_array_2010s[,2])
# 
# plot(opt_cluster_array_bf[,1],opt_cluster_array_bf[,2],xlim = c(1,12),ylim = c(25000,160000),
#      xlab = 'number of clusters', ylab = 'total within Sum of Squares', main = 'built fraction')
# lines(opt_cluster_array_bf[,1],opt_cluster_array_bf[,2])
# 
# plot(opt_cluster_array_pr[,1],opt_cluster_array_pr[,2],xlim = c(1,12),ylim = c(25000,160000),
#      xlab = 'number of clusters', ylab = 'total within Sum of Squares', main = 'backscatter')
# lines(opt_cluster_array_pr[,1],opt_cluster_array_pr[,2])
# 
# 
# dev.off()

################################################################################
# do kmeans analysis for each decade independently, using initital state (PR, BF) and decadal change rate per year (del-PR, del-BF)

# number_clusters = 4   # no obvious kink in distortion vs cluster_number plot, so choosing 4
number_clusters_1990s = 5   
# number_clusters_1990s = 4   # can switch to this to make Figure ED5d
number_clusters_2000s = 5  
number_clusters_2010s = 5  
number_clusters = max(number_clusters_1990s,number_clusters_2000s,number_clusters_2010s)  # used for plotting

set.seed(123)  # set random number seed so cluster analysis is repeatable
global_grid_kmeans_array_1990s = kmeans(cluster_vars_1990s_scale,number_clusters_1990s,nstart = 5)
set.seed(123)  # set random number seed so cluster analysis is repeatable
global_grid_kmeans_array_2000s = kmeans(cluster_vars_2000s_scale,number_clusters_2000s,nstart = 5)
set.seed(123)  # set random number seed so cluster analysis is repeatable
global_grid_kmeans_array_2010s = kmeans(cluster_vars_2010s_scale,number_clusters_2010s,nstart = 5)

global_grid_kmeans_clusters = grid_cluster_array
global_grid_kmeans_clusters$cluster_1990s_id = global_grid_kmeans_array_1990s$cluster
global_grid_kmeans_clusters$cluster_2000s_id = global_grid_kmeans_array_2000s$cluster
global_grid_kmeans_clusters$cluster_2010s_id = global_grid_kmeans_array_2010s$cluster

# scatterplots of variables by cluster

global_grid_kmeans_clusters$cluster_1990s_id <- as.factor(global_grid_kmeans_clusters$cluster_1990s_id)
global_grid_kmeans_clusters$cluster_2000s_id <- as.factor(global_grid_kmeans_clusters$cluster_2000s_id)
global_grid_kmeans_clusters$cluster_2010s_id <- as.factor(global_grid_kmeans_clusters$cluster_2010s_id)

# #############################################################################
# # make scatter plots of states vs. rates for each metric (BF and PR) for each decade 
# # 1990s change in growth rates
# p31r_1990s = ggplot(global_grid_kmeans_clusters, aes(x=del_wsf_bf_93_00/100, y=ers_slope, color=cluster_1990s_id)) +
#   geom_point(size=0.7) +
#   #  scale_fill_manual(values = c("gray70","dodgerblue3" ,"darkorange3", "orchid4")) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw()  + xlim(0,0.08) + ylim(-0.01, 0.035) + theme(legend.position = "none") +
#   labs(title="k-means on 1990s",
#        x="1990s rate of increase in WSF-evo BF (1/y)", y = "rate of increase in ERS PR (1/y)")+
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p31r_1990s
# #   
# p31s_1990s = ggplot(global_grid_kmeans_clusters, aes(x=WSFEvolution_1993_mean/100, y=ERS1993_summer_mean_pr, color=cluster_1990s_id)) +
#   geom_point(size=0.7) +
#   #  scale_fill_manual(values = c("gray70","dodgerblue3" ,"darkorange3", "orchid4")) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw()  + xlim(0,1) + ylim(0.0, 0.45) + theme(legend.position = "none") +
#   labs(title="k-means on 1990s",
#        x="1993 WSF-evo BF", y = "1993 ERS PR")+
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p31s_1990s
# 
# 
# p32r_2000s = ggplot(global_grid_kmeans_clusters, aes(x=del_wsf_bf_99_09/100, y=qscat_slope, color=cluster_2000s_id)) +
#   geom_point(size=0.7) +
#   #  scale_fill_manual(values = c("gray70","dodgerblue3" ,"darkorange3", "orchid4")) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw()  + xlim(0,0.08) + ylim(-0.01, 0.035) + theme(legend.position = "none") +
#   labs(title="k-means on 2000s",
#        x="2000s rate of increase in WSF-evo BF (1/y)", y = "rate of increase in QSCAT PR (1/y)")+
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p32r_2000s
# 
# p32s_2000s = ggplot(global_grid_kmeans_clusters, aes(x=WSFEvolution_2000_mean/100, y=QSCAT2000_summer_mean_pr, color=cluster_1990s_id)) +
#   geom_point(size=0.7) +
#   #  scale_fill_manual(values = c("gray70","dodgerblue3" ,"darkorange3", "orchid4")) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw()  + xlim(0,1) + ylim(0.0, 0.45) + theme(legend.position = "none") +
#   labs(title="k-means on 2000",
#        x="2000 WSF-evo BF", y = "2000 QSCAT PR")+
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p32s_2000s
# 
# 
# p33r_2010s = ggplot(global_grid_kmeans_clusters, aes(x=del_wsf_bf_10_15/100, y=ascat_10_21_slope, color=cluster_2010s_id)) +
#   geom_point(size=0.7) +
#   #  scale_fill_manual(values = c("gray70","dodgerblue3" ,"darkorange3", "orchid4")) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw()  + xlim(0,0.08) + ylim(-0.01, 0.035) + theme(legend.position = "none") +
#   labs(title="k-means on 2010s",
#        x="2010s rate of increase in WSF-evo BF (1/y)", y = "rate of increase in ASCAT PR (1/y)")+
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p33r_2010s
# 
# p33s_2010s = ggplot(global_grid_kmeans_clusters, aes(x=WSFEvolution_2010_mean/100, y=ASCAT2010_summer_mean_pr, color=cluster_2010s_id)) +
#   geom_point(size=0.7) +
#   #  scale_fill_manual(values = c("gray70","dodgerblue3" ,"darkorange3", "orchid4")) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw()  + xlim(0,1) + ylim(0.0, 0.45) + theme(legend.position = "none") +
#   labs(title="k-means on 2010s",
#        x="2010 WSF-evo BF", y = "2010 ASCAT PR")+
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p33s_2010s
# 
# pdf(paste(output_dir,'grid_1990s_',number_clusters_1990s,'_&_2000s_',number_clusters_2000s,
#             '_&_2010s_',number_clusters_2010s,'_BFs_&_PRs_state_&_change_scatter_plots.pdf',sep=''))
# 
# print(
#   ggarrange(p31s_1990s,p31r_1990s,
#             p32s_2000s,p32r_2000s,
#             p33s_2010s,p33r_2010s,
#             ncol = 1, nrow = 2)
# )
# 
# dev.off()
# 
# #   # Marginal density plots of x & y
# # 1990s
# p32xdens_rate90s <- ggplot(global_grid_kmeans_clusters, aes(x=del_wsf_bf_93_00/100, fill=cluster_1990s_id)) +
#   geom_density(alpha=.5) +
#   #   scale_fill_manual(values = c('yellowgreen','sienna2','violetred','slateblue','lightseagreen')) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw() + theme(legend.position = "none") +
#   labs(title="1990s change",
#        x="1990s rate of increase in WSF-evo BF (1/y)", y = "density") +
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p32xdens_rate90s
# 
# p32ydens_rate90s <- ggplot(global_grid_kmeans_clusters, aes(x=ers_slope, fill=cluster_1990s_id)) +
#   geom_density(alpha=.5) +
#   #    scale_fill_manual(values = c('yellowgreen','sienna2','violetred','slateblue','lightseagreen')) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw() + theme(legend.position = "none") +
#   labs(title="1990s change",
#        x="rate of increase in ERS PR (1/y)", y = "density") +
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p32ydens_rate90s
# 
# p32xdens_state93 <- ggplot(global_grid_kmeans_clusters, aes(x=WSFEvolution_1993_mean/100, fill=cluster_1990s_id)) +
#   geom_density(alpha=.5) +
#   #   scale_fill_manual(values = c('yellowgreen','sienna2','violetred','slateblue','lightseagreen')) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw() + theme(legend.position = "none") +
#   labs(title="1993 state",
#        x="1993 WSF-evo BF", y = "density") +
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p32xdens_state93
# 
# p32ydens_state93 <- ggplot(global_grid_kmeans_clusters, aes(x=ERS1993_summer_mean_pr, fill=cluster_1990s_id)) +
#   geom_density(alpha=.5) +
#   #    scale_fill_manual(values = c('yellowgreen','sienna2','violetred','slateblue','lightseagreen')) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw() + theme(legend.position = "none") +
#   labs(title="1993 state",
#        x="1993 ERS PR", y = "density") +
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p32ydens_state93
# 
# # 2000s
# p32xdens_rate00s <- ggplot(global_grid_kmeans_clusters, aes(x=del_wsf_bf_99_09/100, fill=cluster_2000s_id)) +
#   geom_density(alpha=.5) +
#   #   scale_fill_manual(values = c('yellowgreen','sienna2','violetred','slateblue','lightseagreen')) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw() + theme(legend.position = "none") +
#   labs(title="2000s change",
#        x="2000s rate of increase in WSF-evo BF (1/y)", y = "density") +
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p32xdens_rate00s
# 
# p32ydens_rate00s <- ggplot(global_grid_kmeans_clusters, aes(x=qscat_slope, fill=cluster_2000s_id)) +
#   geom_density(alpha=.5) +
#   #    scale_fill_manual(values = c('yellowgreen','sienna2','violetred','slateblue','lightseagreen')) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw() + theme(legend.position = "none") +
#   labs(title="2000s change",
#        x="rate of increase in QSCAT PR (1/y)", y = "density") +
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p32ydens_rate00s
# 
# p32xdens_state00 <- ggplot(global_grid_kmeans_clusters, aes(x=WSFEvolution_2000_mean/100, fill=cluster_2000s_id)) +
#   geom_density(alpha=.5) +
#   #   scale_fill_manual(values = c('yellowgreen','sienna2','violetred','slateblue','lightseagreen')) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw() + theme(legend.position = "none") +
#   labs(title="2000 state",
#        x="2000 WSF-evo BF", y = "density") +
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p32xdens_state00
# 
# p32ydens_state00 <- ggplot(global_grid_kmeans_clusters, aes(x=QSCAT2000_summer_mean_pr, fill=cluster_2000s_id)) +
#   geom_density(alpha=.5) +
#   #    scale_fill_manual(values = c('yellowgreen','sienna2','violetred','slateblue','lightseagreen')) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw() + theme(legend.position = "none") +
#   labs(title="2000 state",
#        x="2000 QSCAT PR", y = "density") +
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p32ydens_state00
# 
# # 2010s
# p32xdens_rate10s <- ggplot(global_grid_kmeans_clusters, aes(x=del_wsf_bf_10_15/100, fill=cluster_2010s_id)) +
#   geom_density(alpha=.5) +
#   #   scale_fill_manual(values = c('yellowgreen','sienna2','violetred','slateblue','lightseagreen')) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw() + theme(legend.position = "none") +
#   labs(title="2010s change",
#        x="20010s rate of increase in WSF-evo BF (1/y)", y = "density") +
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p32xdens_rate10s
# 
# p32ydens_rate10s <- ggplot(global_grid_kmeans_clusters, aes(x=ascat_10_21_slope, fill=cluster_2010s_id)) +
#   geom_density(alpha=.5) +
#   #    scale_fill_manual(values = c('yellowgreen','sienna2','violetred','slateblue','lightseagreen')) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw() + theme(legend.position = "none") +
#   labs(title="2010s change",
#        x="rate of increase in ASCAT PR (1/y)", y = "density") +
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p32ydens_rate10s
# 
# p32xdens_state10 <- ggplot(global_grid_kmeans_clusters, aes(x=WSFEvolution_2010_mean/100, fill=cluster_2010s_id)) +
#   geom_density(alpha=.5) +
#   #   scale_fill_manual(values = c('yellowgreen','sienna2','violetred','slateblue','lightseagreen')) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw() + theme(legend.position = "none") +
#   labs(title="2000 state",
#        x="2010 WSF-evo BF", y = "density") +
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p32xdens_state10
# 
# p32ydens_state10 <- ggplot(global_grid_kmeans_clusters, aes(x=ASCAT2010_summer_mean_pr, fill=cluster_2010s_id)) +
#   geom_density(alpha=.5) +
#   #    scale_fill_manual(values = c('yellowgreen','sienna2','violetred','slateblue','lightseagreen')) +
#   scale_color_brewer(palette="Dark2") +
#   theme_bw() + theme(legend.position = "none") +
#   labs(title="2010 state",
#        x="2010 ASCAT PR", y = "density") +
#   theme(axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"))
# 
# p32ydens_state10
# 
# pdf(paste(output_dir,'grid_1990s_',number_clusters_1990s,'_&_2000s_',number_clusters_2000s,
#             '_&_2010s_',number_clusters_2010s,'_BFs_&_PRs_state_&_change_density.pdf',sep=''))
# 
# 
# print(
#   ggarrange(p32xdens_rate90s, p32ydens_rate90s,p32xdens_state93,p32ydens_state93,
#             p32xdens_rate00s, p32ydens_rate00s,p32xdens_state00,p32ydens_state00,
#             p32xdens_rate10s, p32ydens_rate10s,p32xdens_state10,p32ydens_state10, #
#             #              labels = c("A", "B", "C", "D"),
#             ncol = 2, nrow = 2)
# )
# 
# dev.off()
# 


#######################################################################################################################
# box plots of variables by cluster

# first make a summary table of stats: Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 

global_grid_kmeans_clusters_stats = array(NA,c(60,10))
colnames(global_grid_kmeans_clusters_stats) = c('decade','cluster','num_grids','variable','Min','1st_Qu','Median','Mean','3rd_Qu','Max')
decade_names = c('1990s','2000s','2010s')
variable_names = c('BF_init_%','PR_init','del_BF_%','del_PR')

global_grid_kmeans_clusters_stats[1:20,1] = '1990s'
global_grid_kmeans_clusters_stats[1:20,2] = c('1','1','1','1','2','2','2','2','3','3','3','3','4','4','4','4','5','5','5','5')
global_grid_kmeans_clusters_stats[21:40,1] = '2000s'
global_grid_kmeans_clusters_stats[21:40,2] = c('1','1','1','1','2','2','2','2','3','3','3','3','4','4','4','4','5','5','5','5')
global_grid_kmeans_clusters_stats[41:60,1] = '2010s'
global_grid_kmeans_clusters_stats[41:60,2] = c('1','1','1','1','2','2','2','2','3','3','3','3','4','4','4','4','5','5','5','5')
global_grid_kmeans_clusters_stats[1:60,4] = rep(variable_names)

subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_1990s_id == 1)
global_grid_kmeans_clusters_stats[1:4,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[1,5:10] = summary(subset_array$WSFEvolution_1993_mean)
global_grid_kmeans_clusters_stats[2,5:10] = summary(subset_array$ERS1993_summer_mean_pr)
global_grid_kmeans_clusters_stats[3,5:10] = summary(subset_array$del_wsf_bf_93_00)
global_grid_kmeans_clusters_stats[4,5:10] = summary(subset_array$ers_slope)
subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_1990s_id == 2)
global_grid_kmeans_clusters_stats[5:8,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[5,5:10] = summary(subset_array$WSFEvolution_1993_mean)
global_grid_kmeans_clusters_stats[6,5:10] = summary(subset_array$ERS1993_summer_mean_pr)
global_grid_kmeans_clusters_stats[7,5:10] = summary(subset_array$del_wsf_bf_93_00)
global_grid_kmeans_clusters_stats[8,5:10] = summary(subset_array$ers_slope)
subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_1990s_id == 3)
global_grid_kmeans_clusters_stats[9:12,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[9,5:10] = summary(subset_array$WSFEvolution_1993_mean)
global_grid_kmeans_clusters_stats[10,5:10] = summary(subset_array$ERS1993_summer_mean_pr)
global_grid_kmeans_clusters_stats[11,5:10] = summary(subset_array$del_wsf_bf_93_00)
global_grid_kmeans_clusters_stats[12,5:10] = summary(subset_array$ers_slope)
subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_1990s_id == 4)
global_grid_kmeans_clusters_stats[13:16,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[13,5:10] = summary(subset_array$WSFEvolution_1993_mean)
global_grid_kmeans_clusters_stats[14,5:10] = summary(subset_array$ERS1993_summer_mean_pr)
global_grid_kmeans_clusters_stats[15,5:10] = summary(subset_array$del_wsf_bf_93_00)
global_grid_kmeans_clusters_stats[16,5:10] = summary(subset_array$ers_slope)
subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_1990s_id == 5)
global_grid_kmeans_clusters_stats[17:20,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[17,5:10] = summary(subset_array$WSFEvolution_1993_mean)
global_grid_kmeans_clusters_stats[18,5:10] = summary(subset_array$ERS1993_summer_mean_pr)
global_grid_kmeans_clusters_stats[19,5:10] = summary(subset_array$del_wsf_bf_93_00)
global_grid_kmeans_clusters_stats[20,5:10] = summary(subset_array$ers_slope)

subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_2000s_id == 1)
global_grid_kmeans_clusters_stats[21:24,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[21,5:10] = summary(subset_array$WSFEvolution_2000_mean)
global_grid_kmeans_clusters_stats[22,5:10] = summary(subset_array$QSCAT2000_summer_mean_pr)
global_grid_kmeans_clusters_stats[23,5:10] = summary(subset_array$del_wsf_bf_99_09)
global_grid_kmeans_clusters_stats[24,5:10] = summary(subset_array$qscat_slope)
subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_2000s_id == 2)
global_grid_kmeans_clusters_stats[25:28,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[25,5:10] = summary(subset_array$WSFEvolution_2000_mean)
global_grid_kmeans_clusters_stats[26,5:10] = summary(subset_array$QSCAT2000_summer_mean_pr)
global_grid_kmeans_clusters_stats[27,5:10] = summary(subset_array$del_wsf_bf_99_09)
global_grid_kmeans_clusters_stats[28,5:10] = summary(subset_array$qscat_slope)
subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_2000s_id == 3)
global_grid_kmeans_clusters_stats[29:32,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[29,5:10] = summary(subset_array$WSFEvolution_2000_mean)
global_grid_kmeans_clusters_stats[30,5:10] = summary(subset_array$QSCAT2000_summer_mean_pr)
global_grid_kmeans_clusters_stats[31,5:10] = summary(subset_array$del_wsf_bf_99_09)
global_grid_kmeans_clusters_stats[32,5:10] = summary(subset_array$qscat_slope)
subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_2000s_id == 4)
global_grid_kmeans_clusters_stats[33:36,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[33,5:10] = summary(subset_array$WSFEvolution_2000_mean)
global_grid_kmeans_clusters_stats[34,5:10] = summary(subset_array$QSCAT2000_summer_mean_pr)
global_grid_kmeans_clusters_stats[35,5:10] = summary(subset_array$del_wsf_bf_99_09)
global_grid_kmeans_clusters_stats[36,5:10] = summary(subset_array$qscat_slope)
subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_2000s_id == 5)
global_grid_kmeans_clusters_stats[37:40,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[37,5:10] = summary(subset_array$WSFEvolution_2000_mean)
global_grid_kmeans_clusters_stats[38,5:10] = summary(subset_array$QSCAT2000_summer_mean_pr)
global_grid_kmeans_clusters_stats[39,5:10] = summary(subset_array$del_wsf_bf_99_09)
global_grid_kmeans_clusters_stats[40,5:10] = summary(subset_array$qscat_slope)

subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_2010s_id == 1)
global_grid_kmeans_clusters_stats[41:44,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[41,5:10] = summary(subset_array$WSFEvolution_2010_mean)
global_grid_kmeans_clusters_stats[42,5:10] = summary(subset_array$ASCAT2010_summer_mean_pr)
global_grid_kmeans_clusters_stats[43,5:10] = summary(subset_array$del_wsf_bf_10_15)
global_grid_kmeans_clusters_stats[44,5:10] = summary(subset_array$ascat_10_21_slope)
subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_2010s_id == 2)
global_grid_kmeans_clusters_stats[45:48,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[45,5:10] = summary(subset_array$WSFEvolution_2010_mean)
global_grid_kmeans_clusters_stats[46,5:10] = summary(subset_array$ASCAT2010_summer_mean_pr)
global_grid_kmeans_clusters_stats[47,5:10] = summary(subset_array$del_wsf_bf_10_15)
global_grid_kmeans_clusters_stats[48,5:10] = summary(subset_array$ascat_10_21_slope)
subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_2010s_id == 3)
global_grid_kmeans_clusters_stats[49:52,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[49,5:10] = summary(subset_array$WSFEvolution_2010_mean)
global_grid_kmeans_clusters_stats[50,5:10] = summary(subset_array$ASCAT2010_summer_mean_pr)
global_grid_kmeans_clusters_stats[51,5:10] = summary(subset_array$del_wsf_bf_10_15)
global_grid_kmeans_clusters_stats[52,5:10] = summary(subset_array$ascat_10_21_slope)
subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_2010s_id == 4)
global_grid_kmeans_clusters_stats[53:56,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[53,5:10] = summary(subset_array$WSFEvolution_2010_mean)
global_grid_kmeans_clusters_stats[54,5:10] = summary(subset_array$ASCAT2010_summer_mean_pr)
global_grid_kmeans_clusters_stats[55,5:10] = summary(subset_array$del_wsf_bf_10_15)
global_grid_kmeans_clusters_stats[56,5:10] = summary(subset_array$ascat_10_21_slope)
subset_array = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_2010s_id == 5)
global_grid_kmeans_clusters_stats[57:60,3] = dim(subset_array)[1]
global_grid_kmeans_clusters_stats[57,5:10] = summary(subset_array$WSFEvolution_2010_mean)
global_grid_kmeans_clusters_stats[58,5:10] = summary(subset_array$ASCAT2010_summer_mean_pr)
global_grid_kmeans_clusters_stats[59,5:10] = summary(subset_array$del_wsf_bf_10_15)
global_grid_kmeans_clusters_stats[60,5:10] = summary(subset_array$ascat_10_21_slope)

kmeans_stats_filename = paste(output_dir,'grid_1990s_',number_clusters_1990s,'_&_2000s_',number_clusters_2000s,
                              '_&_2010s_',number_clusters_2010s,'kmeans_cluster_stats_array','.csv',sep='')
write.csv(global_grid_kmeans_clusters_stats,file = kmeans_stats_filename,row.names = F)


##########################################################
### FIRST k-means on the 1993 state and 1990s change

# ers slope
p3_1990s = ggplot(global_grid_kmeans_clusters, aes(x=cluster_1990s_id, y=ers_slope)) +
  geom_boxplot(outlier.shape=16, outlier.size=0.5, outlier.alpha = 0., notch = T, varwidth = T) + theme_bw() + 
  theme(legend.position="bottom") + theme(axis.text=element_text(size=8),
                                          axis.title=element_text(size=8,face="bold")) +
  labs(title = paste("k-means on 1990s state & change"),x="cluster id", y = "ERS PR trend") + 
  coord_cartesian(ylim = c(-0.005, 0.02))

p3_1990s

# 1990s del WSF-evo
p4_1990s = ggplot(global_grid_kmeans_clusters, aes(x=cluster_1990s_id, y=del_wsf_bf_93_00/100)) +
  geom_boxplot(outlier.shape=16, outlier.size=0.5, outlier.alpha = 0., notch = T, varwidth = T) + theme_bw() + 
  theme(legend.position="bottom") + theme(axis.text=element_text(size=8),
                                          axis.title=element_text(size=8,face="bold")) +
  labs(title = paste("k-means on 1990s state & change"),x="cluster id", y = "WSF BF ann.incr. 1993-00") + 
  coord_cartesian(ylim = c(-0.0, 0.035))

p4_1990s

# ERS 1993
p13_1990s = ggplot(global_grid_kmeans_clusters, aes(x=cluster_1990s_id, y=ERS1993_summer_mean_pr)) +
  geom_boxplot(outlier.shape=16, outlier.size=0.5, outlier.alpha = 0., notch = T, varwidth = T) + theme_bw() + 
  theme(legend.position="bottom") + theme(axis.text=element_text(size=8),
                                          axis.title=element_text(size=8,face="bold")) +
  labs(title = paste("k-means on 1990s state & change"),x="cluster id", y = "ERS PR 1993") + 
  coord_cartesian(ylim = c(-0.00, 0.45))

p13_1990s

# WSF-evo 1993
p14_1990s = ggplot(global_grid_kmeans_clusters, aes(x=cluster_1990s_id, y=WSFEvolution_1993_mean/100)) +
  geom_boxplot(outlier.shape=16, outlier.size=0.5, outlier.alpha = 0., notch = T, varwidth = T) + theme_bw() + 
  theme(legend.position="bottom") + theme(axis.text=element_text(size=8),
                                          axis.title=element_text(size=8,face="bold")) +
  labs(title = paste("k-means on 1990s state & change"),x="cluster id", y = "WSF BF 1993") + 
  coord_cartesian(ylim = c(-0.0, 1))

p14_1990s

##########################################################
### SECOND k-means on the 2000 state and 2000s change
# qscat slope
p1_2000s = ggplot(global_grid_kmeans_clusters, aes(x=cluster_2000s_id, y=qscat_slope)) +
  geom_boxplot(outlier.shape=16, outlier.size=0.5, outlier.alpha = 0., notch = T, varwidth = T) + theme_bw() + 
  theme(legend.position="bottom") + theme(axis.text=element_text(size=8),
                                          axis.title=element_text(size=8,face="bold")) +
  labs(title = paste("k-means on 2000s state & change"),x="cluster id", y = "QSCAT PR trend") + 
  coord_cartesian(ylim = c(-0.005, 0.02))

p1_2000s

# _2000s del WSF-evo
p5_2000s = ggplot(global_grid_kmeans_clusters, aes(x=cluster_2000s_id, y=del_wsf_bf_99_09/100)) +
  geom_boxplot(outlier.shape=16, outlier.size=0.5, outlier.alpha = 0., notch = T, varwidth = T) + theme_bw() + 
  theme(legend.position="bottom") + theme(axis.text=element_text(size=8),
                                          axis.title=element_text(size=8,face="bold")) +
  labs(title = paste("k-means on 2000s state & change"),x="cluster id", y = "WSF BF ann.incr. 1999-09") + 
  coord_cartesian(ylim = c(-0.0, 0.035))

p5_2000s

# QSCAT 2000 
p11_2000s = ggplot(global_grid_kmeans_clusters, aes(x=cluster_2000s_id, y=QSCAT2000_summer_mean_pr)) +
  geom_boxplot(outlier.shape=16, outlier.size=0.5, outlier.alpha = 0., notch = T, varwidth = T) + theme_bw() + 
  theme(legend.position="bottom") + theme(axis.text=element_text(size=8),
                                          axis.title=element_text(size=8,face="bold")) +
  labs(title = paste("k-means on 2000s state & change"),x="cluster id", y = "QSCAT PR 2000") + 
  coord_cartesian(ylim = c(-0.00, 0.45))

p11_2000s

# WSF-evo 2000
p15_2000s = ggplot(global_grid_kmeans_clusters, aes(x=cluster_2000s_id, y=WSFEvolution_2000_mean/100)) +
  geom_boxplot(outlier.shape=16, outlier.size=0.5, outlier.alpha = 0., notch = T, varwidth = T) + theme_bw() + 
  theme(legend.position="bottom") + theme(axis.text=element_text(size=8),
                                          axis.title=element_text(size=8,face="bold")) +
  labs(title = paste("k-means on 2000s state & change"),x="cluster id", y = "WSF BF 2000") + 
  coord_cartesian(ylim = c(-0.0, 1))

p15_2000s

##########################################################
### THIRD k-means on the 2010 state and 2010s change

p2_2010s = ggplot(global_grid_kmeans_clusters, aes(x=cluster_2010s_id, y=ascat_10_21_slope)) +
  geom_boxplot(outlier.shape=16, outlier.size=0.5, outlier.alpha = 0., notch = T, varwidth = T) + theme_bw() + 
  theme(legend.position="bottom") + theme(axis.text=element_text(size=8),
                                          axis.title=element_text(size=8,face="bold")) +
  labs(title = paste("k-means on 2010s state & change"),x="cluster id", y = "ASCAT PR trend") + 
  coord_cartesian(ylim = c(-0.005, 0.02))

p2_2010s

# 2010s del WSF-evo
p6_2010s = ggplot(global_grid_kmeans_clusters, aes(x=cluster_2010s_id, y=del_wsf_bf_10_15/100)) +
  geom_boxplot(outlier.shape=16, outlier.size=0.5, outlier.alpha = 0., notch = T, varwidth = T) + theme_bw() + 
  theme(legend.position="bottom") + theme(axis.text=element_text(size=8),
                                          axis.title=element_text(size=8,face="bold")) +
  labs(title = paste("k-means on 2010s state & change"),x="cluster id", y = "WSF BF ann.incr. 2010-15") + 
  coord_cartesian(ylim = c(-0.0, 0.035))

p6_2010s

# ASCAT 2010
p12_2010s = ggplot(global_grid_kmeans_clusters, aes(x=cluster_2010s_id, y=ASCAT2010_summer_mean_pr),) +
  geom_boxplot(outlier.shape=16, outlier.size=0.5, outlier.alpha = 0., notch = T, varwidth = T) + theme_bw() + 
  theme(legend.position="bottom") + theme(axis.text=element_text(size=8),
                                          axis.title=element_text(size=8,face="bold")) +
  labs(title = paste("k-means on 2010s state & change"),x="cluster id", y = "ASCAT PR 2010") + 
  coord_cartesian(ylim = c(-0.00, 0.45))

p12_2010s

# WSF-evo 2010
p16_2010s = ggplot(global_grid_kmeans_clusters, aes(x=cluster_2010s_id, y= WSFEvolution_2010_mean/100)) +
  geom_boxplot(outlier.shape=16, outlier.size=0.5, outlier.alpha = 0., notch = T, varwidth = T) + theme_bw() + 
  theme(legend.position="bottom") + theme(axis.text=element_text(size=8),
                                          axis.title=element_text(size=8,face="bold")) +
  labs(title = paste("k-means on 2010s state & change"),x="cluster id", y = "WSF BF 2010") + 
  coord_cartesian(ylim = c(-0.0, 1))

p16_2010s 


pdf(paste(output_dir,'grid_1990s_',number_clusters_1990s,'_&_2000s_',number_clusters_2000s,
            '_&_2010s_',number_clusters_2010s,'_BFs_&_PRs_state_&_change_cluster_box_plots.pdf',sep=''))

print(
  ggarrange(p13_1990s,p14_1990s,p3_1990s,p4_1990s,
            p11_2000s,p15_2000s,p1_2000s,p5_2000s,
            p12_2010s,p16_2010s,p2_2010s,p6_2010s,
            ncol = 2, nrow = 2)
)

dev.off()

#################################################################################### 
#################################################################################### 

# compute total grids in each cluster for all MUAs, countries, regions, economies, GLOBAL

# global
global_cluster_array = as.data.frame(array(0,c((number_clusters),7)))
colnames(global_cluster_array) = c('region',
                                 'cluster_1990s','num_grids_1990s','cluster_2000s','num_grids_2000s','cluster_2010s','num_grids_2010s')

for (icluster in 1:number_clusters) {
  global_cluster_array[icluster,1] = 'global'
  global_cluster_array[icluster,2] = icluster
  global_cluster_array[icluster,4] = icluster
  global_cluster_array[icluster,6] = icluster
  
  cluster_subset = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_1990s_id == icluster)
  global_cluster_array[icluster,3] = dim(cluster_subset)[1]
  cluster_subset = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_2000s_id == icluster)
  global_cluster_array[icluster,5] = dim(cluster_subset)[1]
  cluster_subset = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$cluster_2010s_id == icluster)
  global_cluster_array[icluster,7] = dim(cluster_subset)[1]
  
}

# economies
econ_cluster_array = as.data.frame(array(0,c((number_clusters*num_econ),7)))
colnames(econ_cluster_array) = c('economy',
                                 'cluster_1990s','num_grids_1990s','cluster_2000s','num_grids_2000s','cluster_2010s','num_grids_2010s')

for (iecon in 1:num_econ) {
  
  econ_name = econ_list[iecon]
  
  econ_grid_subset = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$economy == econ_name)
  
  for (icluster in 1:number_clusters) {
    econ_cluster_array[(iecon-1)*number_clusters+icluster,1] = econ_name
    econ_cluster_array[(iecon-1)*number_clusters+icluster,2] = icluster
    econ_cluster_array[(iecon-1)*number_clusters+icluster,4] = icluster
    econ_cluster_array[(iecon-1)*number_clusters+icluster,6] = icluster
    
    cluster_subset = subset(econ_grid_subset, econ_grid_subset$cluster_1990s_id == icluster)
    econ_cluster_array[(iecon-1)*number_clusters+icluster,3] = dim(cluster_subset)[1]
    cluster_subset = subset(econ_grid_subset, econ_grid_subset$cluster_2000s_id == icluster)
    econ_cluster_array[(iecon-1)*number_clusters+icluster,5] = dim(cluster_subset)[1]
    cluster_subset = subset(econ_grid_subset, econ_grid_subset$cluster_2010s_id == icluster)
    econ_cluster_array[(iecon-1)*number_clusters+icluster,7] = dim(cluster_subset)[1]
    
  }
}

# regions
region_cluster_array = as.data.frame(array(0,c((number_clusters*num_regions),7)))
colnames(region_cluster_array) = c('region',
                                   'cluster_1990s','num_grids_1990s','cluster_2000s','num_grids_2000s','cluster_2010s','num_grids_2010s')

for (ireg in 1:num_regions) {
  
  region_name = region_list[ireg]
  
  region_grid_subset = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$region_1 == region_name)
  
  for (icluster in 1:number_clusters) {
    region_cluster_array[(ireg-1)*number_clusters+icluster,1] = region_name
    region_cluster_array[(ireg-1)*number_clusters+icluster,2] = icluster
    region_cluster_array[(ireg-1)*number_clusters+icluster,4] = icluster
    region_cluster_array[(ireg-1)*number_clusters+icluster,6] = icluster
    
    cluster_subset = subset(region_grid_subset, region_grid_subset$cluster_1990s_id == icluster)
    region_cluster_array[(ireg-1)*number_clusters+icluster,3] = dim(cluster_subset)[1]
    cluster_subset = subset(region_grid_subset, region_grid_subset$cluster_2000s_id == icluster)
    region_cluster_array[(ireg-1)*number_clusters+icluster,5] = dim(cluster_subset)[1]
    cluster_subset = subset(region_grid_subset, region_grid_subset$cluster_2010s_id == icluster)
    region_cluster_array[(ireg-1)*number_clusters+icluster,7] = dim(cluster_subset)[1]
    
  }
}

# country
country_cluster_array = as.data.frame(array(0,c((number_clusters*num_countries),8)))
colnames(country_cluster_array) = c('country','region',
                                    'cluster_1990s','num_grids_1990s','cluster_2000s','num_grids_2000s','cluster_2010s','num_grids_2010s')

for (icountry in 1:num_countries) {
  
  country_name = country_list[icountry,2]
  region_name = country_list[icountry,3]
  
  country_grid_subset = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$mua_country == country_name)
  
  for (icluster in 1:number_clusters) {
    country_cluster_array[(icountry-1)*number_clusters+icluster,1] = country_name
    country_cluster_array[(icountry-1)*number_clusters+icluster,2] = region_name
    country_cluster_array[(icountry-1)*number_clusters+icluster,3] = icluster
    country_cluster_array[(icountry-1)*number_clusters+icluster,5] = icluster
    country_cluster_array[(icountry-1)*number_clusters+icluster,7] = icluster
    
    cluster_subset = subset(country_grid_subset, country_grid_subset$cluster_1990s_id == icluster)
    country_cluster_array[(icountry-1)*number_clusters+icluster,4] = dim(cluster_subset)[1]
    cluster_subset = subset(country_grid_subset, country_grid_subset$cluster_2000s_id == icluster)
    country_cluster_array[(icountry-1)*number_clusters+icluster,6] = dim(cluster_subset)[1]
    cluster_subset = subset(country_grid_subset, country_grid_subset$cluster_2010s_id == icluster)
    country_cluster_array[(icountry-1)*number_clusters+icluster,8] = dim(cluster_subset)[1]
    
  }
}

# MUA
mua_cluster_array = as.data.frame(array(0,c((number_clusters*num_mua_cities),9)))
colnames(mua_cluster_array) = c('mua','country','region',
                                'cluster_1990s','num_grids_1990s','cluster_2000s','num_grids_2000s','cluster_2010s','num_grids_2010s')

for (imua in 1:num_mua_cities) {
  
  mua_name = mua_city_list[imua,2]
  mua_ID = mua_city_list[imua,1]
  country_name = mua_city_list[imua,4]
  region_name = mua_city_list[imua,10]
  
  
  mua_grid_subset = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$mua_objectid == mua_ID)
  
  for (icluster in 1:number_clusters) {
    mua_cluster_array[(imua-1)*number_clusters+icluster,1] = mua_name
    mua_cluster_array[(imua-1)*number_clusters+icluster,2] = country_name
    mua_cluster_array[(imua-1)*number_clusters+icluster,3] = region_name
    
    mua_cluster_array[(imua-1)*number_clusters+icluster,4] = icluster
    mua_cluster_array[(imua-1)*number_clusters+icluster,6] = icluster
    mua_cluster_array[(imua-1)*number_clusters+icluster,8] = icluster
    
    cluster_subset = subset(mua_grid_subset, mua_grid_subset$cluster_1990s_id == icluster)
    mua_cluster_array[(imua-1)*number_clusters+icluster,5] = dim(cluster_subset)[1]
    cluster_subset = subset(mua_grid_subset, mua_grid_subset$cluster_2000s_id == icluster)
    mua_cluster_array[(imua-1)*number_clusters+icluster,7] = dim(cluster_subset)[1]
    cluster_subset = subset(mua_grid_subset, mua_grid_subset$cluster_2010s_id == icluster)
    mua_cluster_array[(imua-1)*number_clusters+icluster,9] = dim(cluster_subset)[1]
    
  }
}


############################################################################################################
# make a geotiff file of the cluster id numbers

# make k-means cluster ID raster grid for 1990s clustering
cluster_1990s_raster_grid = global_grid_kmeans_clusters[,c(1,2,29)]
# header: longitude latitude  cluster_5_id
r_cluster_1990s_raster_grid = rasterFromXYZ(cluster_1990s_raster_grid,crs='+proj=longlat +datum=WGS84')
geotiff_name = paste(output_dir,'kmeans_cluster_',number_clusters_1990s,'_1990s_raster_grid', sep = '')
writeRaster(r_cluster_1990s_raster_grid, filename = geotiff_name, format = "GTiff",overwrite = TRUE)

# make k-means cluster ID raster grid for 2000s clustering
cluster_2000s_raster_grid = global_grid_kmeans_clusters[,c(1,2,30)]
# header: longitude latitude  cluster_5_id
r_cluster_2000s_raster_grid = rasterFromXYZ(cluster_2000s_raster_grid,crs='+proj=longlat +datum=WGS84')
geotiff_name = paste(output_dir,'kmeans_cluster_',number_clusters_2000s,'_2000s_raster_grid', sep = '')
writeRaster(r_cluster_2000s_raster_grid, filename = geotiff_name, format = "GTiff",overwrite = TRUE)

# make k-means cluster ID raster grid for 2010s clustering
cluster_2010s_raster_grid = global_grid_kmeans_clusters[,c(1,2,31)]
# header: longitude latitude  cluster_5_id
r_cluster_2010s_raster_grid = rasterFromXYZ(cluster_2010s_raster_grid,crs='+proj=longlat +datum=WGS84')
geotiff_name = paste(output_dir,'kmeans_cluster_',number_clusters_2010s,'_2010s_raster_grid', sep = '')
writeRaster(r_cluster_2010s_raster_grid, filename = geotiff_name, format = "GTiff",overwrite = TRUE)

########################################################################################################

# make alluvial plots of transitions between clusters

global_grid_kmeans_clusters$cluster_1990s_id = as.factor(global_grid_kmeans_clusters$cluster_1990s_id)
global_grid_kmeans_clusters$cluster_2000s_id = as.factor(global_grid_kmeans_clusters$cluster_2000s_id)
global_grid_kmeans_clusters$cluster_2010s_id = as.factor(global_grid_kmeans_clusters$cluster_2010s_id)

pdf(paste(output_dir,'grid_1990s_',number_clusters_1990s,'_&_2000s_',number_clusters_2000s,
          '_&_2010s_',number_clusters_2010s,'_kMeans_cluster_alluvial.pdf',sep=''))

for (iregion in 0:(num_regions+num_sub_regions)) {
  
  if (iregion == 0) {
    global_grid_kmeans_clusters_alluvial = global_grid_kmeans_clusters
  } else if (iregion < (num_regions + 1)) {
    global_grid_kmeans_clusters_alluvial = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$region_1 == region_list[iregion])
  } else {
    global_grid_kmeans_clusters_alluvial = subset(global_grid_kmeans_clusters, global_grid_kmeans_clusters$sub_regions == sub_region_list[(iregion - num_regions)])
  }
  
  # Build alluvial array
  
  alluvial_array = as.data.frame(array(NA,c((5*5*5),4)))
  colnames(alluvial_array) = c('cluster_1990s', 'cluster_2000s', 'cluster_2010s','freq')
  index = 0
  for (c90s in 1:number_clusters_1990s) {
    
    sub1 = subset(global_grid_kmeans_clusters_alluvial, global_grid_kmeans_clusters_alluvial$cluster_1990s_id == c90s)
    
    for (c00s in 1:number_clusters_2000s) {
      
      sub12 = subset(sub1, sub1$cluster_2000s_id == c00s)
      
      for (c10s in 1:number_clusters_2010s) {
        
        index = index+1
        
        sub123 = subset(sub12, sub12$cluster_2010s_id == c10s)
        alluvial_array[index,1] = c90s
        alluvial_array[index,2] = c00s
        alluvial_array[index,3] = c10s
        alluvial_array[index,4] = dim(sub123)[1]
        
      }
    }
  }
  
  # naming the clusters
  # NOTE: since k-means clustering was done with a set seed for the random number, it is repeatable each time the code runs.  
  #    BUT: the ordering of the clusters (e.g., budding, stable, outward, ...) is not consistent between the three decades (see box and whisker plots), and
  #           maybe not between different runs of the code.
  #    SO: this code hardwires the cluster names based on the number of grid cells in each cluster in each decade in the global analysis (see next ~140 lines),
  #           which is consistent between different runs of the code.
  
  alluvial_array$cluster_name_1990s = NA
  alluvial_array$cluster_name_2000s = NA
  alluvial_array$cluster_name_2010s = NA
  
  if (iregion == 0) {
    
    kmeans_cluster_names = as.data.frame(array(NA,c((number_clusters_1990s+number_clusters_2000s+number_clusters_2010s),4)))
    colnames(kmeans_cluster_names) = c('cluster_name','cluster_id_1990s','cluster_id_2000s','cluster_id_2010s')
    index = 1
    # cluster names:
    #  1:  budding (1990s, 2000s, 2010s)
    #  1*: budding* (1990s)
    #  2:  outward (1990s, 2000s)
    #  3:  slow up & out (2010s)
    #  4:  fast up & out (2000s, 2010s)
    #  5:  upward (1990s, 2000s, 2010s)
    #  6:  stabilized (1990s, 2000s, 2010s)
    
    for (icl in 1:number_clusters_1990s) {
      subcl = subset(alluvial_array,alluvial_array$cluster_1990s == icl)
      nclust = sum(subcl$freq)
      if (abs(nclust - 11234) < 10) {
        alluvial_array$cluster_name_1990s[alluvial_array$cluster_1990s == icl] <- '1_90s'  # budding
        kmeans_cluster_names[index,1] = '1_90s'  # budding
        kmeans_cluster_names[index,2] = icl
        index = index + 1
      } else if (abs(nclust - 11274) < 10) {
        alluvial_array$cluster_name_1990s[alluvial_array$cluster_1990s == icl] <- '1*_90s'  # budding-2
        kmeans_cluster_names[index,1] = '1*_90s'  # budding-2
        kmeans_cluster_names[index,2] = icl
        index = index + 1
      } else if (abs(nclust - 3723) < 10) {
        alluvial_array$cluster_name_1990s[alluvial_array$cluster_1990s == icl] <- '2_90s'   # outward
        kmeans_cluster_names[index,1] =  '2_90s'   # outward
        kmeans_cluster_names[index,2] = icl
        index = index + 1
      } else if (abs(nclust - 2307) < 10) {
        alluvial_array$cluster_name_1990s[alluvial_array$cluster_1990s == icl] <- '5_90s'   # upward
        kmeans_cluster_names[index,1] = '5_90s'   # upward
        kmeans_cluster_names[index,2] = icl
        index = index + 1
      } else if (abs(nclust - 6342) < 10) {
        alluvial_array$cluster_name_1990s[alluvial_array$cluster_1990s == icl] <- '6_90s'  # stabilized
        kmeans_cluster_names[index,1] = '6_90s'  # stabilized
        kmeans_cluster_names[index,2] = icl
        index = index + 1
      }
    }
    
    for (icl in 1:number_clusters_2000s) {
      subcl = subset(alluvial_array,alluvial_array$cluster_2000s == icl)
      nclust = sum(subcl$freq)
      if (abs(nclust - 19270) < 10) {
        alluvial_array$cluster_name_2000s[alluvial_array$cluster_2000s == icl] <- '1_00s'  # budding
        kmeans_cluster_names[index,1] = '1_00s'  # budding
        kmeans_cluster_names[index,3] = icl
        index = index + 1
      } else if (abs(nclust - 4570) < 10) {
        alluvial_array$cluster_name_2000s[alluvial_array$cluster_2000s == icl] <- '2_00s'  # outward
        kmeans_cluster_names[index,1] = '2_00s'  # outward
        kmeans_cluster_names[index,3] = icl
        index = index + 1
      } else if (abs(nclust - 1572) < 10) {
        alluvial_array$cluster_name_2000s[alluvial_array$cluster_2000s == icl] <- '5_00s'  # mature up
        kmeans_cluster_names[index,1] =  '5_00s'  # mature up
        kmeans_cluster_names[index,3] = icl
        index = index + 1
      } else if (abs(nclust - 1230) < 10) {
        alluvial_array$cluster_name_2000s[alluvial_array$cluster_2000s == icl] <- '4_00s'  # fast up and out
        kmeans_cluster_names[index,1] = '4_00s'  # fast up and out
        kmeans_cluster_names[index,3] = icl
        index = index + 1
      } else if (abs(nclust - 8238) < 10) {
        alluvial_array$cluster_name_2000s[alluvial_array$cluster_2000s == icl] <- '6_00s'  # stabilized
        kmeans_cluster_names[index,1] = '6_00s'  # stabilized
        kmeans_cluster_names[index,3] = icl
        index = index + 1
      }
    }
    
    for (icl in 1:number_clusters_2010s) {
      subcl = subset(alluvial_array,alluvial_array$cluster_2010s == icl)
      nclust = sum(subcl$freq)
      if (abs(nclust - 16569) < 10) {
        alluvial_array$cluster_name_2010s[alluvial_array$cluster_2010s == icl] <- '1_10s'  # budding
        kmeans_cluster_names[index,1] = '1_10s'  # budding
        kmeans_cluster_names[index,4] = icl
        index = index + 1
      } else if (abs(nclust - 2562) < 10) {
        alluvial_array$cluster_name_2010s[alluvial_array$cluster_2010s == icl] <- '5_10s'  # mature up
        kmeans_cluster_names[index,1] = '5_10s'  # upward
        kmeans_cluster_names[index,4] = icl
        index = index + 1
      } else if (abs(nclust - 5732) < 10) {
        alluvial_array$cluster_name_2010s[alluvial_array$cluster_2010s == icl] <- '3_10s'  # slow up & out
        kmeans_cluster_names[index,1] = '3_10s'  # slow up & out
        kmeans_cluster_names[index,4] = icl
        index = index + 1
      } else if (abs(nclust - 1577) < 10) {
        alluvial_array$cluster_name_2010s[alluvial_array$cluster_2010s == icl] <- '4_10s'  # fast up and out
        kmeans_cluster_names[index,1] = '4_10s'  # fast up and out
        kmeans_cluster_names[index,4] = icl
        index = index + 1
      } else if (abs(nclust - 8440) < 10) {
        alluvial_array$cluster_name_2010s[alluvial_array$cluster_2010s == icl] <- '6_10s'  # stabilized
        kmeans_cluster_names[index,1] = '6_10s'  # stabilized
        kmeans_cluster_names[index,4] = icl
        index = index + 1
      }
    }
    
  } else {  # naming clusters in regions and sub-regions
    
    for (icl in 1:number_clusters_1990s) {
      temp_array = subset(kmeans_cluster_names, kmeans_cluster_names$cluster_id_1990s == icl)
      cluster_name = temp_array[1,1]
      alluvial_array$cluster_name_1990s[alluvial_array$cluster_1990s == icl] <- cluster_name
    }
    
    for (icl in 1:number_clusters_2000s) {
      temp_array = subset(kmeans_cluster_names, kmeans_cluster_names$cluster_id_2000s == icl)
      cluster_name = temp_array[1,1]
      alluvial_array$cluster_name_2000s[alluvial_array$cluster_2000s == icl] <- cluster_name
    }
    
    for (icl in 1:number_clusters_2010s) {
      temp_array = subset(kmeans_cluster_names, kmeans_cluster_names$cluster_id_2010s == icl)
      cluster_name = temp_array[1,1]
      alluvial_array$cluster_name_2010s[alluvial_array$cluster_2010s == icl] <- cluster_name
    }
    
  }
  
  alluvial_array$cluster_name_1990s <- factor(alluvial_array$cluster_name_1990s, levels=c('6_90s', '5_90s', '2_90s', '1*_90s', '1_90s'))
  
  alluvial_array$cluster_name_2000s <- factor(alluvial_array$cluster_name_2000s, levels=c('6_00s', '5_00s', '4_00s','2_00s',  '1_00s'))
  
  alluvial_array$cluster_name_2010s <- factor(alluvial_array$cluster_name_2010s, levels=c('6_10s', '5_10s', '4_10s', '3_10s', '1_10s'))
  
  pa1 = ggplot(data = alluvial_array,
               aes(axis1 = cluster_name_1990s,   # First variable on the X-axis
                   axis2 = cluster_name_2000s,   # Second variable on the X-axis
                   axis3 = cluster_name_2010s,   # Third variable on the X-axis
                   y = freq)) +
    geom_alluvium(aes(fill = cluster_name_2000s)) +
    #  scale_fill_brewer(type = "qual", palette = "Accent") +
    scale_color_brewer(palette = "Accent") +
    #  geom_stratum(alpha = .25, width = 1/8, reverse = FALSE) +
    geom_stratum() +
    geom_text(stat = "stratum",size = 2,
              aes(label = after_stat(stratum))) +
    scale_x_continuous(breaks = 1:3, labels = c("1990s", "2000s", "2010s")) +
    # coord_flip() +
    theme_bw() 
  
  if (iregion == 0) {
    pa1 = pa1 + ggtitle(paste('Global',": All MUA grid cells; k-means cluster trajectories",sep=''))
  } else if (iregion < (num_regions+1)) {
    pa1 = pa1 + ggtitle(paste(region_list[iregion],": All MUA grid cells; k-means cluster trajectories",sep=''))
  } else {
    pa1 = pa1 + ggtitle(paste(sub_region_list[(iregion-num_regions)],": All MUA grid cells; k-means cluster trajectories",sep=''))
  }
  
  print(pa1)
  
  # if (iregion == 0) {
  #   outfile_name = paste(output_dir,'grid_1990s_',number_clusters_1990s,'_&_2000s_',number_clusters_2000s,
  #                        '_&_2010s_',number_clusters_2010s,'_kMeans_cluster_global_transition_array.csv',sep='')
  # } else if (iregion < (num_regions+1)) {
  #   outfile_name = paste(output_dir,'grid_1990s_',number_clusters_1990s,'_&_2000s_',number_clusters_2000s,
  #                        '_&_2010s_',number_clusters_2010s,'_kMeans_cluster_',region_list[iregion],'_transition_array.csv',sep='')
  # } else {
  #   outfile_name = paste(output_dir,'grid_1990s_',number_clusters_1990s,'_&_2000s_',number_clusters_2000s,
  #                        '_&_2010s_',number_clusters_2010s,'_kMeans_cluster_',sub_region_list[(iregion-num_regions)],'_transition_array.csv',sep='')
  # }
  # 
  
  if (iregion == 0) {
    alluvial_array_save = alluvial_array
  }
  
}

dev.off()
  
# #######################################################################################################################
# outfile_name = paste(output_dir,'grid_1990s_',number_clusters_1990s,'_&_2000s_',number_clusters_2000s,
#                       '_&_2010s_',number_clusters_2010s,'_kMeans_cluster_global_transition_array.csv',sep='')
# write.csv(alluvial_array_save,file = outfile_name,row.names = F)

# compute number of grid cells per country, region, sub-region
# start by number of grid cells per country  -- Adding this to country_list array

country_list_2 = country_list
country_list_2$num_grid = NA

for (icountry in 1:num_countries) {
  
  CountryName = country_list[icountry,2]
  mua_subset = subset(mua_city_list, mua_city_list$mua_country == CountryName)
  
  country_list_2[icountry,6] = sum(mua_subset$new_cell_count)
}

country_list = country_list_2

# next number of grid cells per region

region_array = array(NA,c(num_regions,2))
colnames(region_array) = c('region','num_grids')

for (iregion in 1:num_regions) {
  RegionName = region_list[iregion]
  country_subset = subset(country_list_2, country_list_2$region_1 == RegionName)
  
  region_array[iregion,1] = RegionName
  region_array[iregion,2] = sum(country_subset$num_grid)
  
}

# next number of grid cells per sub-region

sub_region_array = array(NA,c(num_sub_regions,2))
colnames(sub_region_array) = c('region','num_grids')

for (iregion in 1:num_sub_regions) {
  SubRegionName = sub_region_list[iregion]
  country_subset = subset(country_list_2, country_list_2$sub_regions == SubRegionName)
  
  sub_region_array[iregion,1] = SubRegionName
  sub_region_array[iregion,2] = sum(country_subset$num_grid)
  
}

# add number grid cells per country 

country_array = country_list[,c(2,6)]
colnames(country_array) = c('region','num_grids')

# concatenate lists
region_and_sub_region_array = rbind(region_array,sub_region_array,country_array)

outfile_name = paste(output_dir,'grid_1990s_',number_clusters_1990s,'_&_2000s_',number_clusters_2000s,
                       '_&_2010s_',number_clusters_2010s,'_region_and_sub_region_and_country_num_grids.csv',sep='')

write.csv(region_and_sub_region_array,file = outfile_name,row.names = F)
