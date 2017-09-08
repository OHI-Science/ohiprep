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
library(sf)

# raster/zonal data
slr_loc <- file.path(dir_M, "git-annex/globalprep/prs_slr/v2017/output")

zones <- raster(file.path(dir_M, "git-annex/globalprep/spatial/v2017/regions_eez_with_fao_ant.tif"))  # raster data

rgn_data <- read_sf(file.path(dir_M, 'git-annex/globalprep/spatial/v2017'), 'regions_2017_update') %>%
  st_set_geometry(NULL) %>%
  dplyr::filter(rgn_type == "eez") %>%
  dplyr::select(rgn_id = rgn_ant_id, rgn_name)


# save location
save_loc <- "globalprep/prs_slr/v2017"

# read in raster files
rasts <- list.files(slr_loc, full.names = TRUE)
rasts <- rasts[!grepl(".aux", rasts)]

stack_slr <- stack(rasts)

# extract data for each region:
regions_stats <- zonal(stack_slr,  zones, fun="mean", na.rm=TRUE, progress="text")


regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$rgn_id) # High Seas regions are in there, makes sense....no land
setdiff(rgn_data$rgn_id, regions_stats2$zone)

regions_stats2 <- regions_stats2 %>%
  rename(rgn_id = zone) %>%
  filter(rgn_id <=250) %>%
  gather("year", "pressure_score", -1) %>%
  mutate(year = as.numeric(as.character(substring(year, 5, 8))))

write.csv(regions_stats2, "globalprep/prs_slr/v2017/output/slr.csv", row.names = FALSE)

## visualize data
library(googleVis)

plotData <- regions_stats2%>%
  left_join(rgn_data)%>%
  dplyr::select(rgn_name, year, pressure_score) %>%
  dplyr::arrange(rgn_name, year) %>%
  data.frame()

Motion=gvisMotionChart(plotData, 
                       idvar="rgn_name", 
                       timevar="year")
plot(Motion)

print(Motion, file=file.path(save_loc, 'slr.html'))


