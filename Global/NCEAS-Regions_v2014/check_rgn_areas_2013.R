source('src/R/common.R')
library(dplyr)

dir_rgn13 = file.path(dir_neptune_data, 'model/GL-NCEAS-OceanRegions_v2013a/data')

merge(
  read.csv(file.path(dir_rgn13, 'rgn_areas.csv'  ), na.strings=''),
  read.csv(file.path(dir_rgn13, 'rgn_details.csv'), na.strings=''),
  by='rgn_id') %.%
  filter(rgn_typ=='eez') %.%
  select(rgn_nam, rgn_id, area_km2) %.%  
  mutate(eez_area_rank = rank(desc(area_km2))) %.%
  subset(rgn_nam=='China')

# compare with 2014 data (rgns still under construction)

read.csv('Global/NCEAS-Regions_v2014/data/sp_data_gcs.csv', na.strings='') %.%
  filter(rgn_name=='China') %.%
  select(-ends_with('12')) # exclude Nature 2012 fields
# sp_type sp_id sp_name sp_key area_km2 rgn_type rgn_id rgn_name rgn_key
#     eez   209   China    CHN        0      eez    209    China     CHN
#    land   209   China    CHN        0     land    209    China     CHN


# bbest trying again: land ok, offshore
for (csv in list.files('Global/NCEAS-Regions_v2014/data', '.*\\.csv$', full.names=T)){
  cat(sprintf('\n%s\n', basename(csv)))
  
  d = read.csv(csv, na.strings='')

  if ('sp_name' %in% names(d)){
    print(d %>% filter(sp_name=='China'))
  } else if ('rgn_name' %in% names(d)){
    print(d %>% filter(rgn_name=='China'))
  } else {
    print('  WARNING: sp_name or rgn_name not found!')
  }
}

# rgn_gcs_data.csv
#   rgn_type rgn_id rgn_name rgn_key area_km2
# 1     land    209    China     CHN 355085.1
# 
# rgn_inland1km_gcs_data.csv
# rgn_type rgn_id rgn_name rgn_key area_km2
# 1     land    209    China     CHN 20882.33
# 
# rgn_inland25km_gcs_data.csv
# rgn_type rgn_id rgn_name rgn_key area_km2
# 1     land    209    China     CHN 211937.4
# 
# rgn_inland50km_gcs_data.csv
# rgn_type rgn_id rgn_name rgn_key area_km2
# 1     land    209    China     CHN 355084.8
# 
# rgn_offshore1km_gcs_data.csv
# rgn_type rgn_id rgn_name rgn_key area_km2
# 1      eez    209    China     CHN 25451.72
# 
# rgn_offshore3nm_gcs_data.csv
# rgn_type rgn_id rgn_name rgn_key area_km2
# 1      eez    209    China     CHN 92113.56
# 
# sp_gcs_data.csv
# sp_type sp_id sp_name sp_key area_km2 rgn_type rgn_id rgn_name rgn_key cntry_id12 rgn_id12 rgn_name12
# 1    land   209   China    CHN 355085.1     land    209    China     CHN        CHN      159      China
# 
# sp_inland1km_gcs_data.csv
# sp_type sp_id sp_name sp_key area_km2
# 1    land   209   China    CHN 20882.33
# 
# sp_inland25km_gcs_data.csv
# sp_type sp_id sp_name sp_key area_km2
# 1    land   209   China    CHN 211937.4
# 
# sp_inland50km_gcs_data.csv
# sp_type sp_id sp_name sp_key area_km2
# 1    land   209   China    CHN 355084.8
# 
# sp_offshore1km_gcs_data.csv
# sp_type sp_id sp_name sp_key area_km2
# 1     eez   209   China    CHN 25451.72
# 
# sp_offshore3nm_gcs_data.csv
# sp_type sp_id sp_name sp_key area_km2
# 1     eez   209   China    CHN 92113.56