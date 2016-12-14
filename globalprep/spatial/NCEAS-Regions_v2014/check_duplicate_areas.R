library(dplyr)

setwd('G:/ohiprep/Global/NCEAS-Regions_v2014')
d = read.csv('data/sp_inland25km_gcs_data.csv')
head(d)
d[duplicated(d[,c('sp_id')]),]
# sp_type sp_id  sp_name sp_key area_km2
#    land   223 Svalbard    NOR 64360.58