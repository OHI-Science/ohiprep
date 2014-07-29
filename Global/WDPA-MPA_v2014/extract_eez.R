##########################################################
### Extracting data from WDPA-MPA_v2014 raster for EEZ
###  and other regions
###  MRF: July 25 2014
##########################################################

library(sp)
library(raster)
library(dplyr)

source('~/ohiprep/src/R/common.R')

## read in spatial data:
mpa <- raster((file.path(dir_neptune_data, "git-annex/Global/WDPA-MPA_v2014/tmp/wdpa_designated_mol.tif")))
plot(mpa)

regions <- readOGR(dsn=file.path(dir_neptune_data, "git-annex/Global/NCEAS-Regions_v2014/data/"), layer="rgn_offshore_mol") 
proj4string(regions) <- CRS(proj4string(mpa))
regions <- regions[regions@data$rgn_type =="eez", ]
regions <- regions[regions@data$rgn_id !=213, ]
plot(regions, add=TRUE)
rasterize(regions, mpa, field="rgn_id", filename=file.path(dir_neptune_data, "model/GL-WDPA-MPA_v2013/tmp/eez_clipRaster"), overwrite=TRUE, progress="text")
regions_raster <- raster(file.path(dir_neptune_data, "model/GL-WDPA-MPA_v2013/tmp/eez_clipRaster"))
 
mask(mpa, regions_raster,  
                        filename=file.path(dir_neptune_data, "model/GL-WDPA-MPA_v2013/tmp/MPA_masked"), 
                        overwrite=TRUE, progress="text")
mpa_masked <- raster(file.path(dir_neptune_data, "model/GL-WDPA-MPA_v2013/tmp/MPA_masked"))
mpa_points <- rasterToPoints(mpa_masked , spatial=TRUE, progress="text")

mpa_data <- extract(regions_raster, mpa_points, progress="text")
mpa_data <- data.frame(mpa_data)
mpa_data$year  <- mpa_points@data$wdpa_designated_mol

mpa_region_summary <- mpa_data %.%
  select(rgn_id=mpa_data, year) %.%
  group_by(rgn_id, year) %.%
  summarize(area_km2=length(year))

write.csv(data.frame(mpa_region_summary), 'Global/WDPA-MPA_v2014/data/lsp_protarea_eez.csv', row.names=FALSE)

##################################
#checking on things
plot(regions[regions@data$rgn_id == 89, ], add=TRUE)
plot(regions[regions@data$rgn_id == 162, ], add=TRUE)
plot(regions[regions@data$rgn_id == 163, ], add=TRUE)
plot(regions[regions@data$rgn_id == 232, ], add=TRUE)
plot(regions[regions@data$rgn_id == 16, ], add=TRUE)

 # comparing to Ben's data for offshore 3nm
regions <- readOGR(dsn=file.path(dir_neptune_data, "git-annex/Global/NCEAS-Regions_v2014/data/"), layer="rgn_offshore3nm_mol") 
#proj4string(regions) <- CRS(proj4string(mpa))
regions <- regions[regions@data$rgn_type =="eez", ]
regions <- regions[regions@data$rgn_id !=213, ]
plot(regions, add=TRUE)
rasterize(regions, mpa, field="rgn_id", filename=file.path(dir_neptune_data, "model/GL-WDPA-MPA_v2013/tmp/offshore_3nm_clipRaster"), overwrite=TRUE, progress="text")
regions_raster <- raster(file.path(dir_neptune_data, "model/GL-WDPA-MPA_v2013/tmp/offshore_3nm_clipRaster"))

mask(mpa, regions_raster,  
     filename=file.path(dir_neptune_data, "model/GL-WDPA-MPA_v2013/tmp/MPA_masked_3nm_offshore"), 
     overwrite=TRUE, progress="text")
mpa_masked <- raster(file.path(dir_neptune_data, "model/GL-WDPA-MPA_v2013/tmp/MPA_masked_3nm_offshore"))
mpa_points <- rasterToPoints(mpa_masked , spatial=TRUE, progress="text")

mpa_data <- extract(regions_raster, mpa_points, progress="text")
mpa_data <- data.frame(mpa_data)
mpa_data$year  <- mpa_points@data$wdpa_designated_mol

mpa_region_summary <- mpa_data %.%
  select(rgn_id=mpa_data, year) %.%
  group_by(rgn_id, year) %.%
  summarize(area_km2=length(year))
plot(regions[regions$rgn_id ==3,])
plot(mpa_points, add=TRUE)
