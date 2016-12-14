# setup ----
library(reshape2)
library(plyr)
library(dplyr)

#  from get paths configuration based on host machine name
source('src/R/common.R')             # set dir_neptune_data
dir13 = file.path(dir_neptune_data, 'model/GL-NCEAS-OceanRegions_v2013a')
dir14 = 'Global/NCEAS-Regions_v2014' # product

# data paths
z13 = read.csv(file.path(dir13, 'manual_output/eez_rgn_2013master.csv'))
z14 = read.csv(file.path(dir14 , 'manual_output/sp_rgn_manual.csv'))

z = merge(
  z13 %.%
    group_by(rgn_nam_2013) %.%
    summarize(
      rgn_type_2013 = first(as.character(rgn_typ)),
      rgn_id_2013   = first(rgn_id_2013),
      rgn_key_2013  = first(rgn_key_2013)) %.%
    rename(c('rgn_nam_2013'='rgn_name_2013')),  
  z14 %.%
    filter(rgn_type %in% c('eez','fao')) %.%
    group_by(rgn_name) %.%
    summarize(
      rgn_type_2014 = first(as.character(rgn_type)),
      rgn_id_2014   = first(rgn_id),
      rgn_key_2014  = first(rgn_key)) %.%
    rename(c('rgn_name'='rgn_name_2014')) %.%
    mutate(
      rgn_name_2013 = rgn_name_2014),  
  by='rgn_name_2013', all=T)

filter(z, is.na(rgn_name_2013))
filter(z, is.na(rgn_name_2014) & rgn_type_2013=='eez')
filter(z, !is.na(rgn_name_2014) & rgn_id_2013!=rgn_id_2014)

# TODO: eez -> offshore, land -> inland. fao -> highseas.