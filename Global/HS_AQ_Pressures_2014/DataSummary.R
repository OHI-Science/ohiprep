#######################################################################
### NCEAS/OHI
### This is for the pressures summary for 2013 HS/AQ regions
### Melanie Frazier March 17 2014
#######################################################################
#testing

## Downloading packages
library(rgdal)
library(raster) 
library(sp)
library(maptools)
library(rgeos)
library(plotKML)
library(dplyr)

setwd('/var/data/ohi/model/GL-HS-AQ-PressuresSummary_v2013')

# Region mollwide file: 
# (from /var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data - but converted to Mol)
regions_mol <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_mol")
regions@data[regions@data$sp_type=="ccamlr",]
table(regions@data$sp_type)
table(regions@data$rgn_type)
regions_mol <- regions_mol[regions_mol@data$sp_type %in% c("eez-ccamlr", "fao", "eez"), ]
regions_mol@data$sp_id <- as.numeric(1:dim(regions_mol@data)[1])  

# Region GCS file:
regions_gcs <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_gcs")
regions_gcs <- regions_gcs[regions_gcs@data$sp_type %in% c("eez-ccamlr", "fao", "eez"), ]
regions_gcs@data$sp_id <- as.numeric(1:dim(regions_gcs@data)[1])  



############################
## SST ----
############################

# take a look at SST raster that I need to extract data from:
SST <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/sst_05_10-82_86i_mol.tif")

plot(SST)
plot(regions_mol, add=TRUE)

#convert to raster
rasterize(regions_mol, SST, 
          field="sp_id", 
          filename="sp_mol_raster_SST", overwrite=TRUE, 
          progress="text")


regions_raster <- raster("sp_mol_raster_SST")

regions_stats <- zonal(SST,  regions_raster, progress="text")

data <- merge(regions_mol@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"SST_ZonalMean.csv", 
          row.names=FALSE)  

#SST_oldData <- read.dbf("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/rgn_fao_mol_sst_05_10-82_86i_mol.dbf")


############################
## ACID ----
############################
acid <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/masked_impacts_acid.tif")

plot(acid)
plot(regions_mol, add=TRUE)

#convert to raster
rasterize(regions_mol, acid, 
          field="sp_id", 
          filename="sp_mol_raster_acid", overwrite=TRUE, 
          progress="text")


regions_raster <- raster("sp_mol_raster_acid")

regions_stats <- zonal(acid,  regions_raster, progress="text")

data <- merge(regions_mol@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"acid_ZonalMean.csv", 
          row.names=FALSE)  

#acid_oldData <- read.dbf("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/rgn_fao_mol_sst_05_10-82_86i_mol.dbf")

############################
## UV ----
############################
uv <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/impact_layers_2013_redo/impact_layers/work/uv/uv_baseline_anomaly/omi_aura_uv_anomaly_2008m01-2012m12_trans.tif")

plot(uv)
plot(regions_gcs, add=TRUE)

## extracting with polygons might be better due to low resolution of data.
v <- extract(uv, regions_gcs, weights=TRUE, progress="text", df=TRUE)
v2 <- v %.%
  group_by(ID) %.%
  summarize(mean = weighted.mean(omi_aura_uv_anomaly_2008m01.2012m12_trans, weight, na.rm=TRUE))
v2 <- merge(regions_gcs@data, v2, all.y=TRUE, by.x="sp_id", by.y="ID") 
write.csv(v2,"tmp//uv_ZonalMean.csv", 
          row.names=FALSE)  

#uv_oldData <- read.dbf("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/rgn_fao_mol_sst_05_10-82_86i_mol.dbf")

############################
## Commercial High Bycatch
############################
source('src/R/common.R') # defines dir_neptune_data
hb_dem <- raster(file.path(dir_neptune_data, "model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/data/fp_com_hb_dem_2013_rescaled.tif"))
hb_dem_nd <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/fp_com_hb_dem_nd_2013_rescaled.tif")
hb_pel <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/fp_com_hb_pel_2013_rescaled.tif")

plot(hb_dem)
plot(regions_mol, add=TRUE)

#convert to raster
rasterize(regions_mol, hb_dem, 
          field="sp_id", 
          filename="sp_mol_raster_ComHBC", overwrite=TRUE, 
          progress="text")


regions_raster <- raster("sp_mol_raster_ComHBC")

CommercialHBC <- stack(hb_dem, hb_dem_nd, hb_pel)

regions_stats <- zonal(CommercialHBC,  regions_raster, progress="text")

data <- merge(regions_mol@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"CommHBC_ZonalMean.csv", 
          row.names=FALSE)  


############################
## Commercial Low Bycatch
############################
lb_dem <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/fp_com_lb_dem_nd_2013_rescaled.tif")
lb_pel <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/fp_com_lb_pel_2013_rescaled.tif")

plot(lb_dem)
plot(regions_mol, add=TRUE)

#can use the same region raster as the high by-catch
regions_raster <- raster("sp_mol_raster_ComHBC")

CommercialHBC <- stack(lb_dem, lb_pel)

regions_stats <- zonal(CommercialHBC,  regions_raster, progress="text")

data <- merge(regions_mol@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"CommLBC_ZonalMean.csv", 
          row.names=FALSE)  


############################
## SLR
############################

# might be one worth extracting using polygons...
slr <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/impact_layers_2013_redo/impact_layers/work/sea_level_rise/final_products/slr_oct1992_dec2012_gcs_wgs84_normalized.tif")
slr <- rotate(slr) # scales the data to start at -180 rather than 0

plot(slr)
plot(regions_gcs, add=TRUE)
hist(slr)

#convert to raster
rasterize(regions_gcs, slr, 
          field="sp_id", 
          filename="sp_gcs_raster_slr", overwrite=TRUE, 
          progress="text")

regions_raster <- raster("sp_gcs_raster_slr")                                                                                                                                                                                                                                                                                                                                                                            

regions_stats <- zonal(slr,  regions_raster, progress="text")

data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"slr_ZonalMean.csv", 
          row.names=FALSE)                               



############################
## Shipping
############################
shipping <- raster("/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2013_interim/new_layers/shipping/moll_nontrans_unclipped_1km_transoneperiod_clipped/shipping.tif")
shipping

plot(shipping)
plot(regions_mol, add=TRUE)


#convert to raster
rasterize(regions_mol, shipping, 
          field="sp_id", 
          filename="sp_mol_raster_shipping", overwrite=TRUE, 
          progress="text")

 regions_raster <- raster("sp_mol_raster_shipping")                                                                                                                                                                                                                                                                                                                                                                            

regions_stats <- zonal(shipping,  regions_raster, progress="text")
data <- merge(regions_mol@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 


write.csv(data,"shipping_ZonalMean.csv", 
          row.names=FALSE)  











