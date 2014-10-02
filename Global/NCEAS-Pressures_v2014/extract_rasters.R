library(raster)
library(foreign)
library(dplyr)

# paths
source('src/R/common.R')
dir_product = 'Global/NCEAS-Pressures_v2014'
dir_data = file.path(dir_product, 'data')
dir.create(dir_data)

# TODO: MF for reals. BB for now just using existing.

# cc_acid
dbf = file.path(dir_neptune_data, 'model/GL-NCEAS-Pressures_v2013a/tmp/rgn_masked_impacts_acid.tif.dbf')
read.dbf(dbf) %>% 
  select(
    rgn_id = VALUE,
    mean   = MEAN) %>%
  write.csv(file.path(dir_data, 'cc_acid_raw.csv'), row.names=F, na='')