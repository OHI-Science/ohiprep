### Figuring out CHI Fis

library(raster)
library(sp)
library(rgdal)

source("src/R/common.R")

dd_2008 <- raster(file.path(dir_M, 
  "marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2008_interim/old_layers/demersal_destructive_fishing/moll_nontrans_unclipped_1km_nonull/demersal_destructive_fishing.tif"))
dd_2013 <- raster(file.path(dir_M, 
                            "marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2013_interim/new_layers/demersal_destructive_fishing/moll_nontrans_unclipped_1km/demersal_destructive_fishing.tif"))
hist(dd_2008)
