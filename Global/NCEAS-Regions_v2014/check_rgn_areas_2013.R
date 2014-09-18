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


# quick check for Mel
library(foreign)
library(dplyr)

dbf = '/Volumes/data_edit/git-annex/Global/NCEAS-Regions_v2014/data/rgn_gcs.dbf'
d = read.dbf(dbf)
head(d)
#   rgn_type rgn_id         rgn_name rgn_key Shape_Leng Shape_Area  area_km2
# 1      eez      1    Cocos Islands     CCK   23.00573   38.80783  467246.2
# 2      eez      2 Christmas Island     CXR   19.88828   27.16158  328000.0
# 3      eez      3   Norfolk Island     NFK   23.13732   39.97401  431137.9
# 4      eez      4 Macquarie Island     AUS   31.73381   66.69859  476361.3
# 5      eez      5    New Caledonia     NCL   85.87783  119.36465 1369631.0
# 6      eez      6          Vanuatu     VUT   59.10036   52.42502  618645.3
d %>% filter(rgn_type=='land') %>% head
#   rgn_type rgn_id         rgn_name rgn_key Shape_Leng  Shape_Area    area_km2
# 1     land      1    Cocos Islands     CCK  0.9141698 0.001481390    17.83758
# 2     land      2 Christmas Island     CXR  0.6953346 0.011687770   141.52596
# 3     land      3   Norfolk Island     NFK  0.4835012 0.003948591    42.62639
# 4     land      4 Macquarie Island     AUS  1.3694896 0.018038750   129.72684
# 5     land      5    New Caledonia     NCL 34.7424748 1.657875826 19046.42566
# 6     land      6          Vanuatu     VUT 29.5320472 1.057110499 12504.07198

d %>% filter(rgn_name=='China') %>% head
#   rgn_type rgn_id rgn_name rgn_key Shape_Leng Shape_Area    area_km2
# 1      eez    209    China     CHN   350.8939   81.33713   875,238.7
# 2     land    209    China     CHN   502.0519  951.25586 9,376,105.9

# Compare with China eez on Wikipedia:
#   http://en.wikipedia.org/wiki/Exclusive_economic_zone#People.27s_Republic_of_China
#   China's eez: 877,019 km2
# Compare with China land on Wikipedia:
#   http://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_area
#   China land: 9,326,410 km2