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

#### Acid ----
# read in acid data (should be 10 layers, with values 0 to 1)

rasts <- list.files(slr_loc)

pressure_stack <- stack()
for(raster in rasts){ #raster="oa_interpolated_cells.tif"
  tmp <- raster(file.path(slr_loc, raster))
  pressure_stack <- stack(pressure_stack, tmp)
}

## some exploring:
plot(pressure_stack[[5]], col=rev(heat.colors(255)))
click(pressure_stack[[5]])

# extract data for each region:
regions_stats <- zonal(pressure_stack,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$rgn_id) # antarctica regions are in there, makes sense....no land
setdiff(rgn_data$rgn_id, regions_stats2$zone) # 213 is in there, that makes sense (Antarctica)

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="rgn_id", by.y="zone") %>%
  gather("year", "pressure_score", starts_with("slr")) 

slr_data <- data %>%
  mutate(year=gsub('slr_moll_1km_rescaled_', '', year)) %>%
  mutate(year = substring(year, 6, 9)) %>%
  mutate(year = as.numeric(year)) %>%
  filter(rgn_typ == "eez") %>%
  dplyr::select(rgn_id, rgn_nam, year, pressure_score)

write.csv(slr_data, file.path(save_loc, "int/slr.csv"), row.names=FALSE)

## save toolbox data for different years/regions

# function to extract data more easily
saveData <- function(newYear){
  
  assessYear <- newYear + 2
  criteria_year <- ~year == newYear

    slr  <- slr_data %>%
      filter_(criteria_year) %>%
      dplyr::select(rgn_id, pressure_score) %>%
      arrange(rgn_id)
  
  write.csv(slr, file.path(save_loc, sprintf('output/acid_%s.csv', assessYear)), row.names=FALSE)
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


