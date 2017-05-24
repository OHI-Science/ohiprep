####################################
## Extracting cumulative CHI data
## for OHI regions
## Post-score analysis
####################################

source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(dplyr)


chi <- raster(file.path(dir_M, "marine_threats/impact_layers_2013_redo",  
        'global_impact_model_2013/normalized_by_one_time_period/averaged_by_num_ecosystems/all_layers/global_cumul_impact_2013_all_layers.tif'))
zones <- raster(file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/data/sp_mol_raster_1km/sp_mol_raster_1km.tif"))  # raster data
rgn_data <- read.csv(file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/data/sp_mol_raster_1km", 'regionData.csv'))    # data for sp_id's used in raster

regions_stats <- zonal(chi,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$sp_id) #should be none
setdiff(rgn_data$sp_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone")

write.csv(data, "globalprep/supplementary_information/v2016/CHI_data.csv", row.names=FALSE)
