######################################
## SLR extract for regions
##
## JCA: 8/2/17
######################################

## this is only relevant to eez regions (not high seas and not Antarctica)


source('src/R/common.R')

library(raster)
library(rgdal)
library(sp)
library(dplyr)

# raster/zonal data
slr_loc <- file.path(dir_M, "git-annex/globalprep/prs_slr/v2017/output")

zones <- raster(file.path(dir_M, "git-annex/globalprep/spatial/d2014/data/rgn_mol_raster_1km/sp_mol_raster_1km.tif"))  # raster data

rgn_data <- read.csv(file.path(dir_M, "git-annex/globalprep/spatial/d2014/data/rgn_mol_raster_1km/regionData.csv"))    # data for sp_id's used in raster

# save location
save_loc <- "globalprep/prs_slr/v2017"

# read in raster files
rasts <- list.files(slr_loc, full.names = TRUE)

rast_slr <- raster(rasts)

# extract data for each region:
regions_stats <- zonal(rast_slr,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$ant_id) # antarctica regions are in there, makes sense....no land
setdiff(rgn_data$ant_id, regions_stats2$zone) # 213 is in there, that makes sense (Antarctica)


# join with older data
detach("package:rgdal", unload=TRUE)
detach("package:raster", unload=TRUE)
detach("package:sp", unload=TRUE)
old <- read.csv('globalprep/prs_slr/v2016/output/slr_updated.csv')
new <- regions_stats2 %>%
  dplyr::mutate(year = 2017) %>%
  dplyr::select(rgn_id = zone, year, pressure_score = mean) %>%
  filter(!is.na(pressure_score)) %>% #high seas and antarctica data
  filter(rgn_id <= 255)
new <- rbind(new, old)
summary(new)

write.csv(new, "globalprep/prs_slr/v2017/output/slr.csv", row.names = FALSE)

## visualize data
library(googleVis)

data <- merge(rgn_data, new, all.y = TRUE, by="rgn_id")


plotData <- data %>%
  dplyr::select(rgn_nam, year, pressure_score) %>%
  dplyr::arrange(rgn_nam, year) %>%
  data.frame()

Motion=gvisMotionChart(plotData, 
                       idvar="rgn_nam", 
                       timevar="year")
plot(Motion)

print(Motion, file=file.path(save_loc, 'slr.html'))


