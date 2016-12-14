##########################################################
### Extracting data from WDPA-MPA_v2014 raster for EEZ
###  and other regions
###  MRF: July 25 2014
##########################################################

library(sp)
library(raster)

source('ohiprep/src/R/common.R')

## read in spatial data:
mpa <- raster((file.path(dir_neptune_data, "git-annex/Global/WDPA-MPA_v2014/tmp/wdpa_designated_mol.tif")))
plot(mpa)
mpa_points <- rasterToPoints(mpa, spatial=TRUE, progress="text")

regions <- readOGR(dsn=file.path(dir_neptune_data, "git-annex/Global/NCEAS-Regions_v2014/data"), layer="rgn_offshore_mol") 
proj4string(regions) <- CRS(proj4string(mpa))
rasterize(regions, mpa, )

mpa_data <- over(mpa_points, regions)

