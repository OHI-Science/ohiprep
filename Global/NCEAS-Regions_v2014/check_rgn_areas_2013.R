source('src/R/common.R')
library

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
