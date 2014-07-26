##########################################################
### Extracting data from WDPA-MPA_v2014 raster for EEZ
###  and other regions
###  MRF: July 25 2014
##########################################################

library(sp)
library(raster)

source('~/ohiprep/src/R/common.R')

## read in spatial data:
mpa <- raster((file.path(dir_neptune_data, "git-annex/Global/WDPA-MPA_v2014/tmp/wdpa_designated_mol.tif")))
plot(mpa, add=TRUE)

regions <- readOGR(dsn=file.path(dir_neptune_data, "git-annex/Global/NCEAS-Regions_v2014/data/"), layer="rgn_offshore_mol") 
plot(regions, add=TRUE)
#rasterize(regions, mpa, field="ECO_CODE", filename="C:\\Users\\frazier\\Desktop\\pressures\\MEOW\\MEOW_raster", overwrite=TRUE)
regions <- regions[regions@data$rgn_type =="eez", ]
regions <- regions[regions@data$rgn_id !=213, ]

v <- extract(mpa, regions[regions@data$rgn_id == 163,], progress="text")

