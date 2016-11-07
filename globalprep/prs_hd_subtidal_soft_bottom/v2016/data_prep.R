######################################
## Prepare rasters to calcuate 
## habitat destruction in subtidal soft-bottom
##
## MRF: 9/19/2016
######################################

## this is only relevant to eez regions (not high seas and not Antarctica)


source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(sp)
library(dplyr)

##################################################################################################
### Prepare the raster data by multiplying the fisheries data by the subtidal soft-bottom habitat
##################################################################################################

subtidal <- raster(file.path(dir_M, "marine_threats/data/completed/ecosystems/sub_tidal_soft_bottom/tif/s_t_s_bottom.tif"))
regions <- readOGR(dsn=file.path(dir_M, "git-annex/globalprep/spatial/d2014/data"), layer="regions_mol")
# plot(subtidal)
# plot(regions, add=TRUE)
projection(subtidal) <- CRS(proj4string(regions))

fun=function(x){ifelse(x==0, NA, 1)}

calc(subtidal, 
     fun, 
     progress='text',
     filename=file.path(dir_M, "git-annex/globalprep/prs_hd_subtidal_soft_bottom/v2016/int/prs_hd_subtidal_soft_bottom_NAs.tif"), overwrite=TRUE)

subtidal <- raster(file.path(dir_M, "git-annex/globalprep/prs_hd_subtidal_soft_bottom/v2016/int/prs_hd_subtidal_soft_bottom_NAs.tif"))

# raster/zonal data
dem_d <- file.path(dir_M, "git-annex/chi_rate/stressors/fishing/int/dem_d")

for(year in 2006:2010){ #year = 2006
  tmp <- raster(file.path(dem_d, sprintf("dem_d_moll_1km_%s.tif", year)))
  
  data_stack <- stack(tmp, subtidal)
  
  overlay(data_stack, 
      fun=function(x, y){x*y}, 
       progress='text',
       filename=file.path(dir_M, sprintf("git-annex/globalprep/prs_hd_subtidal_soft_bottom/v2016/int/dem_d_subtidal_sb_%s.tif", year)), overwrite=TRUE)
    }



##################################################################################################
### Take the natural log of the data and calculate the 99.99th quantile for each year
###  (the max value will be the reference point)
##################################################################################################

quantiles <- data.frame(fisData = paste0("dem_d_subtidal_sb_", 2006:2010), quantile_9999_ln=NA)
files <- paste0("dem_d_subtidal_sb_", 2006:2010, ".tif")
rast_locs <- file.path(dir_M, "git-annex/globalprep/prs_hd_subtidal_soft_bottom/v2016/int")

for(hd in files){ # hd='dem_d_subtidal_sb_2006.tif'
  tmp <- raster(file.path(rast_locs, hd))
  saveName <- gsub(".tif", "", hd)
  calc(tmp, function(x){log(x+1)}, progress="text", filename=file.path(rast_locs, sprintf("%s_log.tif", saveName)), overwrite=TRUE)
  tmp <- raster(file.path(rast_locs, sprintf("%s_log.tif", saveName)))
  quantiles$quantile_9999_ln[quantiles$fisData == saveName] <- quantile(tmp, .9999, na.rm=TRUE)
}

write.csv(quantiles, 'globalprep/prs_hd_subtidal_soft_bottom/v2016/int/ref_point_9999quantiles.csv', row.names=FALSE)

############################################################################
## standardize rasters by highest 99.99th quantile
############################################################################
## the ref value is 11.148

files <- paste0("dem_d_subtidal_sb_", 2006:2010, "_log.tif")
rast_locs <- file.path(dir_M, "git-annex/globalprep/prs_hd_subtidal_soft_bottom/v2016/int")
ref_value <- 11.14759

for(hd in files){ # hd='dem_d_subtidal_sb_2006_log.tif'
  tmp <- raster(file.path(rast_locs, hd))
  saveName <- gsub(".tif", "", hd)
  calc(tmp, function(x){x/ref_value}, progress="text", filename=file.path(rast_locs, sprintf("%s_rescaled.tif", saveName)), overwrite=TRUE)
}

############################################################################
## Extract mean of rasters for each OHI region
############################################################################

zones <- raster(file.path(dir_M, "git-annex/globalprep/spatial/d2014/data/rgn_mol_raster_1km/sp_mol_raster_1km.tif"))  # raster data

rgn_data <- read.csv(file.path(dir_M, "git-annex/globalprep/spatial/d2014/data/rgn_mol_raster_1km/regionData.csv"))    # data for sp_id's used in raster

# save location
save_loc <- "globalprep/prs_hd_subtidal_soft_bottom/v2016/output"

#### HD data ----
# read in data

rasts <- paste0("dem_d_subtidal_sb_", 2006:2010, "_log_rescaled.tif")

pressure_stack <- stack()

for(raster in rasts){ #raster="dem_d_subtidal_sb_2007_log_rescaled.tif"
  tmp <- raster(file.path(rast_locs, raster))
  pressure_stack <- stack(pressure_stack, tmp)
}

## some exploring:
# plot(pressure_stack[[5]], col=rev(heat.colors(255)))
# click(pressure_stack[[5]])

# extract data for each region:
regions_stats <- zonal(pressure_stack,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$ant_id) 
setdiff(rgn_data$ant_id, regions_stats2$zone) 

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="rgn_id", by.y="zone") %>%
  gather("year", "pressure_score", starts_with("dem")) 

hd_data <- data %>%
  mutate(year=gsub('dem_d_subtidal_sb_', '', year)) %>%
  mutate(year=gsub('_log_rescaled', '', year)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(rgn_typ == "eez") %>%
  dplyr::select(rgn_id, rgn_nam, year, pressure_score)

write.csv(hd_data, file.path(save_loc, "hd_sb_subtidal.csv"), row.names=FALSE)

## save toolbox data for different years/regions

# function to extract data more easily
saveData <- function(newYear){ 
  
  assessYear <- newYear + 6
  criteria_year <- ~year == newYear

    hd  <- hd_data %>%
      filter_(criteria_year) %>%
      dplyr::select(rgn_id, pressure_score) %>%
      arrange(rgn_id)
  
  write.csv(hd, file.path(save_loc, sprintf('hd_sb_subtidal_%s.csv', assessYear)), row.names=FALSE)
}


### extract data 
for(newYear in (max(hd_data$year) - 4):(max(hd_data$year))){
  saveData(newYear)
}


### try visualizing the data using googleVis plot
library(googleVis)
plotData <- hd_data %>%
  dplyr::select(rgn_nam, year, pressure_score)

Motion=gvisMotionChart(plotData, 
                       idvar="rgn_nam", 
                       timevar="year")
plot(Motion)

print(Motion, file=file.path(save_loc, 'hd.html'))


