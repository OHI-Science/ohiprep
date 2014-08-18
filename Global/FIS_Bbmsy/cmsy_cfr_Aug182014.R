########################################################
###### checking B_Bmsy with different runs #############

# upload results from Hexagon runs with uniform/constrained prior, with/without 0s padding
dir_Hex_data = 'Global/FIS_Bbmsy/Hex outputs'
file_1<- 'cmsy_ohi_results_table_originalPrio_added0s.RData'
load(file.path(dir_Hex_data, file_1 )) ; head(cmsy.ohi.orig.with0.df)
opw0 <- cmsy.ohi.orig.with0.df
file_2<- 'cmsy_ohi_results_table_originalPrio_no0s.RData' 
load(file.path(dir_Hex_data, file_2 )) ; head(cmsy.ohi.orig.no0.df)
opno0 <- cmsy.ohi.orig.no0.df
file_3 <- 'cmsy_ohi_results_table_unifPrio_no0s.RData' 
load(file.path(dir_Hex_data, file_3 )) ; head(cmsy.ohi.unif.no0.df)
upno0 <- cmsy.ohi.unif.no0.df

# upload old results with unif and 0s
upw0 <- read.csv("Global/FIS_Bbmsy/raw/cmsy.ohi.df_Jul292014.csv", na.strings=''); head(upw0)

# upload average catch (or: src('Global/NCEAS-Fisheries_2014a/cnk_fis_meancatch_datamaker.R'))
mc <- read.csv('Global/NCEAS-Fisheries_2014a/data/fnk_fis_meancatch_lyr.csv', na.strings='', stringsAsFactors=F); head(mc)

