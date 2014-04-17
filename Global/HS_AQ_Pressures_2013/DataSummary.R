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
library(plyr)

setwd('/var/data/ohi/model/GL-HS-AQ-PressuresSummary_v2013')

# Region mollwide file: 
# (from /var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data - but converted to Mol)
regions <- readOGR(dsn=".", layer="sp_mol")
regions@data[regions@data$sp_type=="ccamlr",]
table(regions@data$sp_type)
table(regions@data$rgn_type)
regions <- regions[regions@data$sp_type %in% c("ccamlr", "fao", "eez"), ]
regions@data$sp_id <- as.numeric(1:dim(regions@data)[1])  

# Region GCS file:
regions_gcs <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_gcs")
regions_gcs <- regions_gcs[regions_gcs@data$sp_type %in% c("ccamlr", "fao", "eez"), ]
regions_gcs@data$sp_id <- as.numeric(1:dim(regions_gcs@data)[1])  



############################
## SST ----
############################

# take a look at SST raster that I need to extract data from:
SST <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/sst_05_10-82_86i_mol.tif")

v <- extract(SST, regions, weights=TRUE, progress="text")
ldply(v, fun=)

sapply(v, function(x) if (!is.null(x)) {sum(apply(x, 1, prod)) / sum(x[,2])} else NA  )


SST_oldData <- read.dbf("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/rgn_fao_mol_sst_05_10-82_86i_mol.dbf")

plot(SST)
plot(regions, add=TRUE)

#convert to raster
rasterize(regions, SST, 
          field="sp_id", 
          filename="sp_mol_raster_SST", overwrite=TRUE, 
          progress="text")


regions_raster <- raster("sp_mol_raster_SST")

regions_stats <- zonal(SST,  regions_raster, progress="text")

data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"SST_ZonalMean.csv", 
          row.names=FALSE)  
#NOTE: Need to standardize - but need to ask ben about a discrepancy between
# the model_rasters_output.csv and the data results in the final table (cc_sst_2013.csv)



############################
## ACID ----
############################
acid <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/masked_impacts_acid.tif")
#acid_oldData <- read.dbf("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/rgn_fao_mol_sst_05_10-82_86i_mol.dbf")

plot(acid)
plot(regions, add=TRUE)

#convert to raster
rasterize(regions, acid, 
          field="sp_id", 
          filename="sp_mol_raster_acid", overwrite=TRUE, 
          progress="text")


regions_ras hter <- raster("sp_mol_raster_acid")

regions_stats <- zonal(acid,  regions_raster, progress="text")

data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"acid_ZonalMean.csv", 
          row.names=FALSE)  


############################
## UV ----
############################
uv <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/impact_layers_2013_redo/impact_layers/work/uv/uv_baseline_anomaly/omi_aura_uv_anomaly_2008m01-2012m12_trans.tif")
#uv_oldData <- read.dbf("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/rgn_fao_mol_sst_05_10-82_86i_mol.dbf")

plot(uv)
plot(regions_gcs, add=TRUE)

#convert to raster
rasterize(regions_gcs, uv, 
          field="sp_id", 
          filename="sp_gcs_raster_uv", overwrite=TRUE, 
          progress="text")


regions_raster <- raster("sp_gcs_raster_uv")

regions_stats <- zonal(uv,  regions_raster, progress="text")

data <- merge(regions_gcs@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"uv_ZonalMean.csv", 
          row.names=FALSE)  

############################
## Commercial High Bycatch
############################
hb_dem <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/fp_com_hb_dem_2013_rescaled.tif")
hb_dem_nd <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/fp_com_hb_dem_nd_2013_rescaled.tif")
hb_pel <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/fp_com_hb_pel_2013_rescaled.tif")

plot(hb_dem)
plot(regions, add=TRUE)

#convert to raster
rasterize(regions, hb_dem, 
          field="sp_id", 
          filename="sp_mol_raster_ComHBC", overwrite=TRUE, 
          progress="text")


regions_raster <- raster("sp_mol_raster_ComHBC")

CommercialHBC <- stack(hb_dem, hb_dem_nd, hb_pel)

regions_stats <- zonal(CommercialHBC,  regions_raster, progress="text")

data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"CommHBC_ZonalMean.csv", 
          row.names=FALSE)  


############################
## Commercial Low Bycatch
############################
lb_dem <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/fp_com_lb_dem_nd_2013_rescaled.tif")
lb_pel <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/fp_com_lb_pel_2013_rescaled.tif")

plot(lb_dem)
plot(regions, add=TRUE)

#can use the same region raster as the high by-catch
regions_raster <- raster("sp_mol_raster_ComHBC")

CommercialHBC <- stack(lb_dem, lb_pel)

regions_stats <- zonal(CommercialHBC,  regions_raster, progress="text")

data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"CommLBC_ZonalMean.csv", 
          row.names=FALSE)  


############################
## SLR
############################
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

test <- extract(slr, regions_gcs[25, ], fun=mean, na.rm=TRUE, weights=TRUE)
test <- extract(slr, regions_gcs[25, ], fun=mean, na.rm=TRUE, weights=TRUE)



############################
## HD subtidal Hard Bottom:poison data
############################
subtidal_hb_1 <- raster("/var/data/ohi/stable/GL-WRI-ReefsAtRisk/data/gl_thr_poison.tif")
hist(subtidal_hb_1) #looks like 1 values should be 0
#subtidal_hb_2 <- raster("/var/data/ohi/stable/GL-WRI-ReefsAtRisk/data/gl_thr_blast.tif")

plot(subtidal_hb_1)
plot(regions, add=TRUE)

#convert to raster
rasterize(regions, subtidal_hb_1, 
          field="sp_id", 
          filename="sp_mol_raster_subtidal_hb_1", overwrite=TRUE, 
          progress="text")


regions_raster <- raster("sp_mol_raster_subtidal_hb_1")

#CommercialHBC <- stack(hb_dem, hb_dem_nd, hb_pel)
#regions_stats <- zonal(CommercialHBC,  regions_raster, progress="text")
regions_stats <- zonal(subtidal_hb_1,  regions_raster, progress="text")
data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"subtidal_hb_1.csv", 
          row.names=FALSE)  


############################
## HD subtidal Hard Bottom:blast data
############################
subtidal_hb_2 <- raster("/var/data/ohi/stable/GL-WRI-ReefsAtRisk/data/gl_thr_blast.tif")

plot(subtidal_hb_2)
plot(regions, add=TRUE)

regions_raster <- raster("sp_mol_raster_subtidal_hb_1") # can use the same region raster

regions_stats <- zonal(subtidal_hb_2,  regions_raster, progress="text")
data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

data$mean <- data$mean-1

write.csv(data,"subtidal_hb_2.csv", 
          row.names=FALSE)  



############################
## Shipping
############################
shipping <- raster("/var/data/ohi/model/GL-NCEAS-Halpern2008/data/masked_impacts_shipping.tif")
shipping

plot(shipping)
plot(regions, add=TRUE)


#convert to raster
rasterize(regions, shipping, 
          field="sp_id", 
          filename="sp_mol_raster_shipping", overwrite=TRUE, 
          progress="text")

regions_raster <- raster("sp_mol_raster_shipping")                                                                                                                                                                                                                                                                                                                                                                            

regions_stats <- zonal(shipping,  regions_raster, progress="text")
data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

data$mean <- data$mean-1

write.csv(data,"shipping.csv", 
          row.names=FALSE)  




#################################
## Checking against old regions
#################################

regions_mol_old <- readOGR(dsn="/var/data/ohi/model/GL-NCEAS-OceanRegions_v2013a/data", layer="rgn_ocean_cntry_mol")
regions_mol_old <- regions_mol_old[regions_mol_old$rgn_typ %in% "eez", ]

# take a look at SST raster that I need to extract data from:
SST <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/sst_05_10-82_86i_mol.tif")
SST_oldData <- read.dbf("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/rgn_fao_mol_sst_05_10-82_86i_mol.dbf")

plot(SST)
plot(regions, add=TRUE)

#convert to raster
rasterize(regions_mol_old, SST, 
          field="rgn_id", 
          filename="sp_mol_raster_SST_old", overwrite=TRUE, 
          progress="text")


regions_raster <- raster("sp_mol_raster_SST_old")

data <- zonal(SST,  regions_raster, progress="text")

write.csv(data,"SST_ZonalMean_old.csv", 
          row.names=FALSE)  

#test <- extract(SST, regions_mol_old, fun=mean, na.rm=TRUE, weights=TRUE, progress="text")
# results: na.rm did not seem to generally work.  However, the results did not 
# seem overall better for the few regions that worked. 











