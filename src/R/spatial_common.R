
## spatial libraries

library(sp)
library(rgdal)
library(sf)
library(raster)


## set the mazu and neptune data_edit share based on operating system
dir_M             <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
                       'Darwin'  = '/Volumes/ohi',    ### connect (cmd-K) to smb://mazu/ohi
                       'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

## Shapefiles

## OHI regions in vector format
regions <- st_read(dsn = file.path(dir_M, "git-annex/globalprep/spatial/v2017"), layer = "regions_2017_update")

## Rasters

## load ocean raster for masking spatial raster data
ocean <- raster(file.path(dir_M, 'model/GL-NCEAS-Halpern2008/tmp/ocean.tif'))

## rasterized OHI regions for zonal statistics
zones <- raster(file.path(dir_M, "git-annex/globalprep/spatial/v2017/regions_eez_with_fao_ant.tif"))