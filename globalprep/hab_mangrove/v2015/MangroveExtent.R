#######################################################
## extracting new mangrove data
## Data downloaded 6/5/2015 and is 
## located here: Neptune/git-annex/globalprep/Mangrove/v2015 
## Dan tried extracting with ARC but the files are at 30m2
## resolution and were huge.  So he ended up converting to 
## Mollweide and 500 m2 resolution.
#######################################################

library(raster)
library(rgdal)
library(dplyr)
library(tidyr)

tmpdir <- '~/big/R_raster_tmp'
rasterOptions(tmpdir=tmpdir)

source('src/R/common.R')

#-------------------------------------
## Closer look at mangrove data
#------------------------------------
#mangrove <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/mangrove_2012_mw/mangrove_2012_mw.tif'))
# units are m2 of mangrove

#save as km2 (mostly to convert to numeric so I do not run into integer overflow issues when I extract the data)
# calc(mangrove, fun=function(x){x/1000000},
#      filename = file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/mangrove_2012_mw/mangrove_2012_mw_km2.tif'),
#                           progress='text')
mangrove <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/mangrove_2012_mw/mangrove_2012_mw_km2.tif'))
## some checks of the data:
cellStats(mangrove, stat="sum", progress='text')
## observed mangrove area = 82386.49 km2 compared to text values of 81,849 km2. 
## this is a relatively small error, and this suggests the transformations were performed correctly
## (note: error is to be expected with crs conversions such as this)

# closer look of the little speckles:
s <- select(mangrove)
plot(s)
t <- select(s)
plot(t)

#-------------------------------------
## Creating polygons to extract relevant regions
#------------------------------------

###############
## eez and land:
regions <- readOGR(dsn="/var/data/ohi/git-annex/globalprep/spatial/v2015/data", "regions_mol")
# # check that the data line up:
# #plot(regions, add=TRUE)

rasterize(regions, mangrove, field='rgn_id', 
          filename=file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/eez_and_land_500m'),
          progress='text')

# for some reason, these regions will not rasterize when they are with everything else
# although, come to find out this doesn't matter becasue these do not have mangrove habitat
regions_extras <-  regions[regions@data$rgn_id %in% c(1, 86, 88, 105, 107, 159), ]
rasterize(regions_extras, mangrove, field='rgn_id', 
          filename=file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/eez_and_land_extras_500m'),
          progress='text')

# combine these:
regions_extras_raster <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/eez_and_land_extras_500m'))
regions_eez_raster <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/eez_and_land_500m'))
raster::cover(regions_extras_raster, regions_eez_raster, 
              filename = file.path(dir_neptune_data, "git-annex/globalprep/Mangrove/v2015/tmp/eez_and_land_all_500m.tif"))


########################
## eez only
eez <- regions[regions$ant_typ == "eez", ]
data <- raster::extract(mangrove, eez, na.rm=TRUE, normalizeWeights=FALSE, fun=sum, df=TRUE, progress="text")
data2 <- cbind(data, eez@data) %>%
  dplyr::select(eez=mangrove_2012_mw_km2, rgn_id, rgn_name)
write.csv(data2, "globalprep/hab_mangrove/v2015/tmp/eez_km2_v2.csv", row.names=FALSE)


# rasterize(regions_eez, mangrove, field='rgn_id', 
#           filename=file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_eezOnly_500m.tif'),
#           progress='text')

# regions_extras <-  regions_eez[regions_eez@data$rgn_id %in% c(1, 86, 88, 105, 107, 159), ]
# rasterize(regions_extras, mangrove, field='rgn_id', 
#           filename=file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_eez_extras_500m'),
#           progress='text')

# regions_extras_raster <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_eez_extras_500m'))
# regions_eez_raster <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_eezOnly_500m.tif'))
# raster::cover(regions_extras_raster, regions_eez_raster, 
#               filename = file.path(dir_neptune_data, "git-annex/globalprep/Mangrove/v2015/tmp/eez_all_500m.tif"))

#############################
## inland
inland_poly <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", "rgn_inland_mol")
rasterize(inland_poly, mangrove, field='rgn_id', 
          filename=file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_inland_500'),
          progress='text')

##############################
## inland 1km

inland_1km_poly <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", "rgn_inland1km_mol")
# rasterize(inland_1km_poly, mangrove, field='rgn_id', 
#           filename=file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_inland_1km_500'),
#           progress='text')
## for some reason, this raster does not look correct in some regions, so going with this method:
data <- raster::extract(mangrove, inland_1km_poly, na.rm=TRUE, normalizeWeights=FALSE, fun=sum, df=TRUE, progress="text")
data2 <- cbind(data, inland_1km_poly@data) %>%
  dplyr::select(inland_1k=mangrove_2012_mw_km2, rgn_id, rgn_name)
write.csv(data2, "globalprep/hab_mangrove/v2015/tmp/inland_1km_km2.csv", row.names=FALSE)

###############################################
#### convert to tifs so I can check them in Arc:
inland <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_inland_500'))
writeRaster(inland, file.path(dir_neptune_data, "git-annex/globalprep/Mangrove/v2015/regions_inland_500_v2.tif"), progress="text")

inland_1km <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_inland_1km_500'))
writeRaster(inland_1km, file.path(dir_neptune_data, "git-annex/globalprep/Mangrove/v2015/regions_inland_1km_500_v2.tif"), progress="text",
            overwrite =TRUE)

#-------------------------------------
## Extracting mangrove data
#------------------------------------
inland_1km <- raster(file.path(dir_neptune_data, "git-annex/globalprep/Mangrove/v2015/regions_inland_1km_500_v2.tif"))
inland_1km_sum <- zonal(mangrove,  inland_1km, fun='sum', progress="text")
inland_1km_sum <- data.frame(inland_1km_sum)
write.csv(inland_1km_sum, "globalprep/hab_mangrove/v2015/tmp/inland_1km_km2.csv", row.names=FALSE)

inland <- raster(file.path(dir_neptune_data, "git-annex/globalprep/Mangrove/v2015/regions_inland_500_v2.tif"))
inland_sum <- zonal(mangrove,  inland, fun='sum', progress="text")
inland_sum <- data.frame(inland_sum)
write.csv(inland_sum, "globalprep/hab_mangrove/v2015/tmp/inland_km2.csv", row.names=FALSE)

eez_land <- raster(file.path(dir_neptune_data, "git-annex/globalprep/Mangrove/v2015/tmp/eez_and_land_500m.tif"))
eez_sum <- zonal(mangrove,  eez_land, fun='sum', progress="text")
eez_sum <- data.frame(eez_sum)
write.csv(eez_sum, "globalprep/hab_mangrove/v2015/tmp/eez_and_land_km2.csv", row.names=FALSE)

eez <- raster(file.path(dir_neptune_data, "git-annex/globalprep/Mangrove/v2015/tmp/eez_all_500m.tif"))
eez_sum <- zonal(mangrove,  eez, fun='sum', progress="text")
eez_sum <- data.frame(eez_sum)
write.csv(eez_sum, "globalprep/hab_mangrove/v2015/tmp/eez_km2.csv", row.names=FALSE)

#-------------------------------------
## Checking and saving data 
#------------------------------------


eez <- read.csv("globalprep/hab_mangrove/v2015/tmp/eez_km2.csv") %>%
  dplyr::select(rgn_id=zone, eez=sum)

eez_land <- read.csv("globalprep/hab_mangrove/v2015/tmp/eez_and_land_km2.csv") %>%
  dplyr::select(rgn_id = zone, eez_land=sum)

land <- read.csv("globalprep/hab_mangrove/v2015/tmp/inland_km2.csv") %>%
  dplyr::select(rgn_id = zone, land=sum)

land_1km <- read.csv("globalprep/hab_mangrove/v2015/tmp/inland_1km_km2.csv") %>%
  dplyr::select(rgn_id=zone, land_1km=sum)

data <- eez_land %>%
  left_join(land) %>%
  left_join(land_1km) %>%
  left_join(eez)