#########################################
## Extracting the OA data for the 
## regions
## MRF: June 1 2016
#########################################

source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(dplyr)
library(sf)

# raster/zonal data
oa_loc <- file.path(dir_M, "git-annex/globalprep/prs_oa/v2017/output")

zones <- raster(file.path(dir_M, "git-annex/globalprep/spatial/v2017/regions_eez_with_fao_ant.tif"))  # raster data

rgn_data <- read_sf(file.path(dir_M, "git-annex/globalprep/spatial/v2017"), "regions_2017_update")    # data for sp_id's used in raster
rgn_data <- rgn_data %>%
  dplyr::filter(rgn_type %in% c("eez", "fao")) %>%
  dplyr::select(rgn_name, rgn_type, rgn_ant_id) %>%
  st_set_geometry(NULL) %>%
  data.frame()


# save location
save_loc <- "globalprep/prs_oa/v2017/output"

#### Acid ----
# read in acidification data

rasts <- c(list.files(file.path(dir_M, "git-annex/globalprep/prs_oa/v2017/output"),full.names=T))

plot(raster(rasts[60]), col=cols,box=F,axes=F, main = 'Rescaled Ωaragonite layer for 2016')
pressure_stack <- stack(rasts)

## some exploring:
plot(pressure_stack[[6]], col=rev(heat.colors(255)))
click(pressure_stack[[6]])

# extract data for each region:
regions_stats <- zonal(pressure_stack,  zones, fun="mean", na.rm=TRUE, progress="text")
write.csv(regions_stats, "globalprep/prs_oa/v2017/int/region_stats.csv", row.names=FALSE)

regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$rgn_ant_id) #should be none
setdiff(rgn_data$rgn_ant_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats2, all.y=TRUE, by.x="rgn_ant_id", by.y="zone") %>%
  tidyr::gather("year", "pressure_score", starts_with("oa")) %>%
  filter(year != "oa_interpolated_cells")

oa_data <- data %>%
    mutate(year=as.numeric(gsub('oa_prs_layer_', '', year)))

write.csv(oa_data, "globalprep/prs_oa/v2017/int/acid.csv", row.names=FALSE)

final <- read.csv("globalprep/prs_oa/v2017/int/acid.csv") %>%
  dplyr::filter(rgn_ant_id <= 250) %>%
  dplyr::select(rgn_id = rgn_ant_id, year, pressure_score)

write.csv(final, "globalprep/prs_oa/v2017/output/acid.csv", row.names=FALSE)

### try visualizing the data using googleVis plot
library(googleVis)
plotData <- oa_data %>%
  filter(rgn_type == "eez",
         rgn_ant_id <= 250) %>%
  dplyr::select(rgn_name, year, pressure_score)

Motion=gvisMotionChart(plotData, 
                       idvar="rgn_name", 
                       timevar="year")
plot(Motion)

print(Motion, file= 'globalprep/prs_oa/v2017/int/acid.html')


##### May come back to this, but, gapfilling should be the same as last year!
##### This might be useful for getting potential error.

### get estimate of gap-filling
# rasts <- raster('/var/data/ohi/git-annex/globalprep/Pressures_acid/v2015/output/oa_interpolated_cells.tif')
# reclassify(rasts, c(-Inf, Inf, 1), filename='/var/data/ohi/git-annex/globalprep/Pressures_acid/v2015/output/oa_interpolated_cells_yes_no.tif', progress="text")
interp <- raster('/var/data/ohi/git-annex/globalprep/Pressures_acid/v2015/output/oa_interpolated_cells_yes_no.tif')

# extract data for each region:
regions_stats <- zonal(interp,  zones, fun="sum", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$sp_id) #should be none
setdiff(rgn_data$sp_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") %>%
  dplyr::select(sp_id, rgn_id, sp_type, rgn_name, interpolated=sum) 

write.csv(data, file.path(save_loc, "tmp/acid_interpolated_cells.csv"), row.names=FALSE)


## Get all cell counts for each region
# rc_count <- freq(zones, progress="text")
# rc_count <- data.frame(rc_count)
# write.csv(rc_count, file.path(save_loc, "sp_id_areas.csv"), row.names=FALSE)

rc_count2  <- read.csv(file.path(save_loc, "sp_id_areas.csv")) %>%
  dplyr::select(sp_id=value, cellNum=count) %>%
  left_join(data, by='sp_id') %>%
  filter(!is.na(sp_id)) %>%
  mutate(prop_gap_filled = interpolated/cellNum)
write.csv(rc_count2, file.path(save_loc, "tmp/acid_prop_interpolated.csv"), row.names=FALSE)

final_gap <- rc_count2 %>%
  dplyr::filter(sp_type == "eez") %>%
  dplyr::select(rgn_id, gap_filled = prop_gap_filled) %>%
  mutate(gap_filled = round(gap_filled, 2)) %>%
  arrange(rgn_id)

write.csv(final_gap, file.path(save_loc, "data/acid_gap_fill_attr.csv"), row.names=FALSE)
final_gap <- read.csv(file.path(save_loc, "data/acid_gap_fill_attr.csv"))


library(ggplot2)

ggplot(final_gap, aes(gap_filled)) +
  geom_histogram() +
  theme_bw() +
  labs(title="Acid: Proportion gap-filled")

sum(final_gap$gap_filled > 0.9)

