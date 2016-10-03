######################################
## SST extract for regions
##
## MRF: 9/29/2016
######################################

source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(sp)
library(dplyr)

# raster/zonal data
sst_loc <- file.path(dir_M, "git-annex/globalprep/prs_sst/v2016/output")

zones <- raster(file.path(dir_M, "git-annex/globalprep/spatial/d2014/data/rgn_mol_raster_1km/sp_mol_raster_1km.tif"))  # raster data

rgn_data <- read.csv(file.path(dir_M, "git-annex/globalprep/spatial/d2014/data/rgn_mol_raster_1km/regionData.csv"))    # data for sp_id's used in raster

# save location
save_loc <- "globalprep/prs_sst/v2016"


###########################################
# apply ice mask
###########################################
# read in raster files
rasts <- list.files(sst_loc)
rasts <- grep("sst", rasts, value=TRUE)

ice_mask <- raster(file.path(dir_M, "git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/ice_mask_resampled"))

for(sst in rasts){ # sst = "sst_1986-1990_1985-1989.tif"
  rast <- raster(file.path(sst_loc, sst))
  overlay(rast, ice_mask, fun=function(x,y) x*y, progress='text',
          filename=file.path(dir_M, 
                             sprintf('git-annex/globalprep/prs_sst/v2016/output/sea_ice_mask/sea_ice_mask_%s', sst)),
          overwrite=TRUE)
}



######################
## Extract data for each region using sst data (with sea ice regions cropped out)

# read in raster files
rasts <- list.files(file.path(sst_loc, "sea_ice_mask"))

pressure_stack <- stack()

for(raster in rasts){ #raster="sea_ice_mask_sst_1986-1990_1985-1989.tif"
  tmp <- raster(file.path(sst_loc, "sea_ice_mask", raster))
  pressure_stack <- stack(pressure_stack, tmp)
}

## some exploring:
plot(pressure_stack[[5]], col=rev(heat.colors(255)))
click(pressure_stack[[5]])

# extract data for each region:
regions_stats <- zonal(pressure_stack,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$ant_id) # antarctica regions are in there, makes sense....no land
setdiff(rgn_data$ant_id, regions_stats2$zone) # 213 is in there, that makes sense (Antarctica)

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="rgn_id", by.y="zone") %>%
  gather("year", "pressure_score", starts_with("slr")) 

write.csv(data, "tmp_data.csv", row.names=FALSE)

sst_data <- data %>%
  mutate(year=gsub('slr_', '', year)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(rgn_typ == "eez") %>%
  dplyr::select(rgn_id, rgn_nam, year, pressure_score)

write.csv(slr_data, file.path(save_loc, "int/slr.csv"), row.names=FALSE)

## save toolbox data for different years/regions

# function to extract data more easily
saveData <- function(newYear){
  
  assessYear <- newYear + 1
  criteria_year <- ~year == newYear
  
  slr  <- slr_data %>%
    filter_(criteria_year) %>%
    dplyr::select(rgn_id, pressure_score) %>%
    arrange(rgn_id)
  
  write.csv(slr, file.path(save_loc, sprintf('output/slr_%s.csv', assessYear)), row.names=FALSE)
}


### extract data 
for(newYear in (max(slr_data$year) - 4):(max(slr_data$year))){
  saveData(newYear)
}


### try visualizing the data using googleVis plot
library(googleVis)
plotData <- slr_data %>%
  dplyr::select(rgn_nam, year, pressure_score)

Motion=gvisMotionChart(plotData, 
                       idvar="rgn_nam", 
                       timevar="year")
plot(Motion)

print(Motion, file=file.path(save_loc, 'slr.html'))


