 #######################################################################
### NCEAS/OHI
### This is for the pressures summary for 2013 HS/AQ regions
### Melanie Frazier March 17 2014
#######################################################################

## Downloading packages
library(rgdal)
library(raster) 
library(sp)
library(maptools)
library(rgeos)
library(plotKML)
library(dplyr)
library(stringr)

setwd('/var/data/ohi/model/GL-HS-pressure_HD_subtidal_SB_v2013')

# Region mollwide file: 
# (from /var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data - but converted to Mol)
regions <- readOGR(dsn="/var/data/ohi/model/GL-HS-AQ-PressuresSummary_v2013", layer="sp_mol")
regions <- regions[regions@data$sp_type %in% c("ccamlr", "fao", "eez"), ]
regions@data$sp_id <- as.numeric(1:dim(regions@data)[1])  


# Region GCS file:
regions_gcs <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_gcs")
regions_gcs <- regions_gcs[regions_gcs@data$sp_type %in% c("ccamlr", "fao", "eez"), ]
regions_gcs@data$sp_id <- as.numeric(1:dim(regions_gcs@data)[1])  



############################
## Soft Bottom Data ----
############################

# Subtidal Softbottom (0-60 m):
SubtidalSB <- raster("/var/data/ohi/model/GL-NCEAS-Halpern2008/data/masked_ecosystems_s_t_s_bottom.tif")
#plot(SubtidalSB)
#plot(regions, add=TRUE)

 
# SoftShelf (60-200 m):
SoftShelf <- raster("/var/data/ohi/model/GL-NCEAS-Halpern2008/data/masked_ecosystems_soft_shelf.tif")
#SoftShelf
#plot(SoftShelf)

# SoftSlope (200-2000 m)
SoftSlope <- raster("/var/data/ohi/model/GL-NCEAS-Halpern2008/data/masked_ecosystems_soft_slope.tif")
#SoftSlope
#plot(SoftSlope)

# SoftSlope (>2000 m)
Deep <- raster("/var/data/ohi/model/GL-NCEAS-Halpern2008/data/masked_ecosystems_d_s_bottom.tif")
#Deep
#plot(Deep)

SB <- stack(SubtidalSB, SoftShelf, SoftSlope, Deep)
SB
plot(SB)

# check to make sure this includes all layers:
SBsum <- sum(SB, na.rm=TRUE)
SBsum
plot(SBsum)

 v <- extract(SBsum, regions, weights=TRUE, progress="text", df=TRUE)
 v2 <- v %.%
   group_by(ID) %.%
   summarize(masked_ecosystems_s_t_s_bottom = sum(masked_ecosystems_s_t_s_bottom*weight))
 
 v2 <- merge(regions@data, v2, all.y=TRUE, by.x="sp_id", by.y="ID") 
 write.csv(v2,"tmp//SoftBottom_ZonalSum_masked_ecosystems_s_t_s_bottom.csv", 
           row.names=FALSE)  
 
 
 
#convert regions to raster
rasterize(regions, SoftSlope, 
          field="sp_id", 
          filename="sp_mol_raster_SoftBottom", overwrite=TRUE, 
          progress="text")

regions_raster <- raster("sp_mol_raster_SoftBottom")

regions_stats <- zonal(SB,  regions_raster, fun="sum", progress="text")

data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"tmp//SoftBottom_ZonalSum.csv", 
          row.names=FALSE)  

############################
## Fisheries Gear Data ----
############################

# pel_lb:
pel_lb <- raster("/var/data/ohi/git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data/catch_pel_lb_gcs.tif")
pel_lb
plot(pel_lb)
plot(regions_gcs, add=TRUE)

 v <- extract(pel_lb, regions_gcs, weights=TRUE, progress="text", df=TRUE)
 v2 <- v %.%
    group_by(ID) %.%
   summarize(catch_pel_lb_gcs=sum(catch_pel_lb_gcs*weight)) 
 v2 <- merge(regions_gcs@data, v2, all.y=TRUE, by.x="sp_id", by.y="ID") 
 write.csv(v2,"tmp//Gear_ZonalSum_catch_pel_lb_gcs.csv", 
           row.names=FALSE)  
 
 
 
# pel_hb:
pel_hb <- raster("/var/data/ohi/git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data/catch_pel_hb_gcs.tif")
pel_hb
plot(pel_hb)

 v <- extract(pel_hb, regions_gcs, weights=TRUE, progress="text", df=TRUE)
 v2 <- v %.%
   group_by(ID) %.%
   summarize(catch_pel_hb_gcs=sum(catch_pel_hb_gcs*weight)) 
 v2 <- merge(regions_gcs@data, v2, all.y=TRUE, by.x="sp_id", by.y="ID") 
 write.csv(v2,"tmp//Gear_ZonalSum_catch_pel_hb_gcs.csv", 
           row.names=FALSE)  
 
# dem_d:
dem_d <- raster("/var/data/ohi/git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data/catch_dem_d_gcs.tif")
dem_d
plot(dem_d)

 v <- extract(dem_d, regions_gcs, weights=TRUE, progress="text", df=TRUE)
 v2 <- v %.%
   group_by(ID) %.%
   summarize(catch_dem_d_gcs=sum(catch_dem_d_gcs*weight)) 
 v2 <- merge(regions_gcs@data, v2, all.y=TRUE, by.x="sp_id", by.y="ID") 
 write.csv(v2,"tmp//Gear_ZonalSum_catch_dem_d_gcs.csv", 
           row.names=FALSE)  
 
 
# dem_nd_lb:
dem_nd_lb <- raster("/var/data/ohi/git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data/catch_dem_nd_lb_gcs.tif")
dem_nd_lb
plot(dem_nd_lb)

 v <- extract(dem_nd_lb, regions_gcs, weights=TRUE, progress="text", df=TRUE)
 v2 <- v %.%
   group_by(ID) %.%
   summarize(catch_dem_nd_lb_gcs = sum(catch_dem_nd_lb_gcs*weight))
 
 v2 <- merge(regions_gcs@data, v2, all.y=TRUE, by.x="sp_id", by.y="ID") 
 write.csv(v2,"tmp//Gear_ZonalSum_catch_dem_nd_lb_gcs.csv", 
           row.names=FALSE)  
 
# dem_nd_hb:
dem_nd_hb <- raster("/var/data/ohi/git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data/catch_dem_nd_hb_gcs.tif")
dem_nd_hb
plot(dem_nd_hb)

 v <- extract(dem_nd_hb, regions_gcs, weights=TRUE, progress="text", df=TRUE)
 v2 <- v %.%
   group_by(ID) %.%
   summarize(catch_dem_nd_hb_gcs = sum(catch_dem_nd_hb_gcs*weight))
 
 v2 <- merge(regions_gcs@data, v2, all.y=TRUE, by.x="sp_id", by.y="ID") 
 write.csv(v2,"tmp//Gear_ZonalSum_catch_dem_nd_hb_gcs.csv", 
           row.names=FALSE)  
 
 


 
 ##################################################
 ## Transfering these data for the website ----
 ##################################################
 dir("/var/cache/halpern-et-al/var/www/ebm-site/GlobalMarine/impacts/raw_data/tif")
 # pel_lb:
 pel_lb <- raster("/var/data/ohi/git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data/fishprod_pel_lb_gcs.tif")
 plot(pel_lb)
 writeRaster(pel_lb, filename="/var/cache/halpern-et-al/var/www/ebm-site/GlobalMarine/impacts/raw_data/tif/fishprod_pel_lb_gcs.tif", format="GTiff")
 
 # pel_hb:
 pel_hb <- raster("/var/data/ohi/git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data/fishprod_pel_hb_gcs.tif")
 plot(pel_hb)
 writeRaster(pel_hb, filename="/var/cache/halpern-et-al/var/www/ebm-site/GlobalMarine/impacts/raw_data/tif/fishprod_pel_hb_gcs.tif", format="GTiff")
 
 # dem_d:
 dem_d <- raster("/var/data/ohi/git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data/fishprod_dem_d_gcs.tif")
 plot(dem_d)
 writeRaster(dem_d, filename="/var/cache/halpern-et-al/var/www/ebm-site/GlobalMarine/impacts/raw_data/tif/fishprod_dem_d_gcs.tif", format="GTiff")
 
 # dem_nd_lb:
 dem_nd_lb <- raster("/var/data/ohi/git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data/fishprod_dem_nd_lb_gcs.tif")
 plot(dem_nd_lb)
 writeRaster(dem_nd_lb, filename="/var/cache/halpern-et-al/var/www/ebm-site/GlobalMarine/impacts/raw_data/tif/fishprod_dem_nd_lb_gcs.tif", format="GTiff")
 
 # dem_nd_hb:
 dem_nd_hb <- raster("/var/data/ohi/git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data/fishprod_dem_nd_hb_gcs.tif")
 plot(dem_nd_hb)
 writeRaster(dem_nd_hb, filename="/var/cache/halpern-et-al/var/www/ebm-site/GlobalMarine/impacts/raw_data/tif/fishprod_dem_nd_hb_gcs.tif", format="GTiff")
 