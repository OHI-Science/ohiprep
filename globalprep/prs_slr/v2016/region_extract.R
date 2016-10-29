######################################
## SLR extract for regions
##
## MRF: 7/12/2016
######################################

## this is only relevant to eez regions (not high seas and not Antarctica)


source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(sp)
library(dplyr)

# raster/zonal data
slr_loc <- file.path(dir_M, "git-annex/globalprep/prs_slr/v2016/output")

zones <- raster(file.path(dir_M, "git-annex/globalprep/spatial/d2014/data/rgn_mol_raster_1km/sp_mol_raster_1km.tif"))  # raster data

rgn_data <- read.csv(file.path(dir_M, "git-annex/globalprep/spatial/d2014/data/rgn_mol_raster_1km/regionData.csv"))    # data for sp_id's used in raster

# save location
save_loc <- "globalprep/prs_slr/v2016"

# read in raster files
rasts <- list.files(slr_loc)

pressure_stack <- stack()
for(raster in rasts){ #raster="slr_1993.tif"
  tmp <- raster(file.path(slr_loc, raster))
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
  gather("year", "pressure_score", starts_with("sea")) 

sst_data <- data %>%
  mutate(year=substr(year, 23,26)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(rgn_typ == "eez") %>%
  dplyr::select(rgn_id, rgn_nam, year, pressure_score)

write.csv(slr_data, file.path(save_loc, "int/sst.csv"), row.names=FALSE)

## save toolbox data for different years/regions

# function to extract data more easily
saveData <- function(newYear){
  
  assessYear <- newYear + 4
  criteria_year <- ~year == newYear

    sst  <- sst_data %>%
      filter_(criteria_year) %>%
      dplyr::select(rgn_id, pressure_score) %>%
      arrange(rgn_id)
  
  write.csv(sst, file.path(save_loc, sprintf('output/sst_%s.csv', assessYear)), row.names=FALSE)
}


### extract data 
for(newYear in (max(sst_data$year) - 4):(max(sst_data$year))){
  saveData(newYear)
}


### try visualizing the data using googleVis plot
library(googleVis)
plotData <- sst_data %>%
  dplyr::select(rgn_nam, year, pressure_score)

Motion=gvisMotionChart(plotData, 
                       idvar="rgn_nam", 
                       timevar="year")
plot(Motion)

print(Motion, file=file.path(save_loc, 'sst.html'))


