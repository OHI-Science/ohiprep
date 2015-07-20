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


mangrove <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/mangrove_2012_mw/mangrove_2012_mw.tif'))
# units are m2 of mangrove

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

 
 regions <- readOGR(dsn="/var/data/ohi/git-annex/globalprep/spatial/v2015/data", "regions_mol")
# # check that the data line up:
# #plot(regions, add=TRUE)

 regions_eez <- regions[regions$rgn_typ == "eez", ]
rasterize(regions, mangrove, field='rgn_id', 
          filename=file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_eez_500m'),
          progress='text')

# for some reason, these regions will not rasterize when they are with everything else
regions_eez_extras <-  regions_eez[regions_eez@data$rgn_id %in% c(1, 86, 88, 105, 107, 159), ]
rasterize(regions_eez_extras, mangrove, field='rgn_id', 
          filename=file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_eez_extras_500m'),
          progress='text')

# combine these:
regions_eez_extras_raster <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_eez_extras_500m'))
plot(regions_eez_extras_raster)
regions_eez_raster <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_eez_500m'))
raster::cover(regions_eez_extras_raster, regions_eez_raster, 
              filename = file.path(dir_neptune_data, "git-annex/globalprep/Mangrove/v2015/tmp/regions_eez_all_500m.tif"))

# eez <- raster(file.path(rast_loc, "sp_mol_raster_1km/sp_mol_raster_1km.tif"))  # raster data
# 
# resample(eez, mangrove, method="ngb", 
#          filename=file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_eez'),
#          progress='text')

###

# inland <- raster(file.path(rast_loc, 'rgn_inland_mol.tif'))
# resample(inland, mangrove, method="ngb", 
#          filename=file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_inland'),
#          progress='text')
inland_poly <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", "rgn_inland_mol")
rasterize(inland_poly, mangrove, field='rgn_id', 
          filename=file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_inland_500'),
          progress='text')

###

# inland_1km <- raster(file.path(rast_loc, 'rgn_inland1km_mol.tif'))
# resample(inland_1km, mangrove, method="ngb", 
#          filename=file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_inland_1km'),
#          progress='text')

inland_1km_poly <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", "rgn_inland1km_mol")
rasterize(inland_1km_poly, mangrove, field='rgn_id', 
          filename=file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_inland_1km_500'),
          progress='text')

#### convert to tifs so I can check them in Arc:
inland <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_inland_500'))
writeRaster(inland, file.path(dir_neptune_data, "git-annex/globalprep/Mangrove/v2015/regions_inland_500_v2.tif"), progress="text")

inland_1km <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Mangrove/v2015/tmp/regions_inland_1km_500'))
writeRaster(inland, file.path(dir_neptune_data, "git-annex/globalprep/Mangrove/v2015/regions_inland_1km_500_v2.tif"), progress="text")