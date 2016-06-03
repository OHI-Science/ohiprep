#########################################
## Extracting the OA data for the 
## regions
## MRF: June 1 2016
#########################################

source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(dplyr)

# raster/zonal data
oa_loc <- file.path(dir_M, "git-annex/globalprep/prs_oa/v2016/output")

zones <- raster(file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/data/sp_mol_raster_1km/sp_mol_raster_1km.tif"))  # raster data
rgn_data <- read.csv(file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/data/sp_mol_raster_1km", 'regionData.csv'))    # data for sp_id's used in raster

# save location
save_loc <- "globalprep/prs_oa/v2016/output"

#### Acid ----
# read in acid data (should be 10 layers, with values 0 to 1)

rasts <- list.files(oa_loc)
rasts <- rasts[-which(rasts=="oa_interpolated_cells.tif")] # cut out the gapfilling raster

pressure_stack <- stack()
for(raster in rasts){ #raster="oa_interpolated_cells.tif"
  tmp <- raster(file.path(oa_loc, raster))
  pressure_stack <- stack(pressure_stack, tmp)
}

## some exploring:
plot(pressure_stack[[5]], col=rev(heat.colors(255)))
click(pressure_stack[[5]])

# extract data for each region:
regions_stats <- zonal(pressure_stack,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$sp_id) #should be none
setdiff(rgn_data$sp_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") %>%
  gather("year", "pressure_score", starts_with("oa")) 

oa_data <- data %>%
    mutate(year=as.numeric(gsub('oa_prs_layer_', '', year)))

write.csv(oa_data, file.path(save_loc, "acid.csv"), row.names=FALSE)

## save toolbox data for different years/regions

# function to extract data more easily
saveData <- function(regionType, newYear){
  
  criteria_year <- ~year == newYear
  criteria_rgn <- ~sp_type == regionType
  
  if(regionType == 'eez-ccamlr'){
    acid  <- oa_data %>%
      filter_(criteria_rgn) %>%
      filter_(criteria_year) %>%
      dplyr::select(sp_id, pressure_score) %>%
      arrange(sp_id)
  } else{
    acid  <- oa_data %>%
      filter_(criteria_rgn) %>%
      filter_(criteria_year) %>%
      dplyr::select(rgn_id, pressure_score) %>%
      arrange(rgn_id)
  }
  
  write.csv(acid, file.path(save_loc, sprintf('acid_%s_%s.csv', regionType, newYear)), row.names=FALSE)
}


### extract data 
for(newYear in 2011:2015){
  saveData("eez", newYear)
}

for(newYear in 2011:2015){
  saveData("eez-ccamlr", newYear)
}

for(newYear in 2011:2015){
  saveData("fao", newYear)
}



### try visualizing the data using googleVis plot
library(googleVis)
plotData <- oa_data %>%
  filter(sp_type == "eez") %>%
  dplyr::select(rgn_name, year, pressure_score)

Motion=gvisMotionChart(plotData, 
                       idvar="rgn_name", 
                       timevar="year")
plot(Motion)

print(Motion, file=file.path(save_loc, 'acid.html'))






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

