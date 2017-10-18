
## spatial libraries

library(sp)
library(rgdal)
library(sf)
library(raster)
devtools::install_github("krlmlr/rprojroot")
library(rprojroot)

## set the mazu and neptune data_edit share based on operating system
dir_M             <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
                       'Darwin'  = '/Volumes/ohi',    ### connect (cmd-K) to smb://mazu/ohi
                       'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

ohiprep_root <- rprojroot::find_rstudio_root_file()

## OHI region files

### Shapefile:
## OHI eez, antarctica, and high seas regions
## This is the most up-to-date file with some topology issues corrected.
## rgn_type and type_w_ant indicate whether the region is "eez", "land", 
regions <- st_read(dsn = file.path(dir_M, "git-annex/globalprep/spatial/v2017"), layer = "regions_2017_update")

### Raster:
## rasterized OHI regions for zonal statistics
## This was created using the git-annex/globalprpe/spatial/v2017/regions_2017_update file and
## the fasterize package (which corrected some small errors in the previous raster that was created 
## using the rasterize package)
zones <- raster(file.path(dir_M, "git-annex/globalprep/spatial/v2017/regions_eez_with_fao_ant.tif"))

### Dataframes
## csv file with region names and ID variables that match the OHI region file
ohiprep_root <- rprojroot::find_rstudio_root_file()
rgns_global <- read.csv(file.path(ohiprep_root, "globalprep/spatial/v2017/output/regionData.csv"))
UNgeorgn_id <- read.csv(file.path(ohiprep_root, "globalprep/spatial/v2017/output/georegions.csv")) 
UNgeorgn_nm <- read.csv(file.path(ohiprep_root, "globalprep/spatial/v2017/output/georegion_labels.csv")) 
rgn_syns <- read.csv(file.path(ohiprep_root, "globalprep/spatial/v2017/output/rgn_eez_v2013a_synonyms.csv")) 
low_pop <- read.csv(file.path(ohiprep_root, "globalprep/spatial/v2017/output/rgn_uninhabited_islands.csv"))


ohi_global_root <- sub("/[^/]*$","", ohiprep_root) 
territory <- read.csv(file.path(ohi_global_root, "ohi-global/eez/spatial/rgns_list.csv"))

### Ocean raster

## load ocean raster for masking spatial raster data
## this was the ocean layer created for the original cumulative human impacts
## This should be used when layers are going to be used for cumulative human impacts
ocean <- raster(file.path(dir_M, 'model/GL-NCEAS-Halpern2008/tmp/ocean.tif'))


