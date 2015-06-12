### zonal extraction and summary of pressure data
### MRF: Feb 25 2015


source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(dplyr)

# raster/zonal data
rast_loc <- file.path(dir_neptune_data, "git-annex/Global/NCEAS-Regions_v2014/data/sp_mol_raster_1km")
zones <- raster(file.path(rast_loc, "sp_mol_raster_1km.tif"))  # raster data
rgn_data <- read.csv(file.path(rast_loc, 'regionData.csv'))    # data for sp_id's used in raster

# save location
save_loc <- "globalprep/PressuresRegionExtract"

#### Acid ----
# read in acid data (should be 10 layers, with values 0 to 1)

rasts <- paste0('/var/data/ohi/git-annex/globalprep/Pressures_acid/v2015/output/annual_oa_rescaled_1km_int_clip_', c(2005:2014), '.tif')

pressure_stack <- stack()
for(i in 1:length(rasts)){ #i=1
  tmp <- raster(rasts[i])
  pressure_stack <- stack(pressure_stack, tmp)
}

# extract data for each region:
regions_stats <- zonal(pressure_stack,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$sp_id) #should be none
setdiff(rgn_data$sp_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") %>%
  gather("year", "pressure_score", starts_with("annual")) %>%
  mutate(year=as.numeric(gsub('annual_oa_rescaled_1km_int_clip_', '', year)))

write.csv(data, file.path(save_loc, "tmp/acid.csv"), row.names=FALSE)

## save toolbox data for different years/regions

# function to extract data more easily
saveData <- function(regionType, newYear){

criteria_year <- ~year == newYear
criteria_rgn <- ~sp_type == regionType

if(regionType == 'eez-ccamlr'){
  acid  <- data %>%
    filter_(criteria_rgn) %>%
    filter_(criteria_year) %>%
    dplyr::select(rgn_id=sp_id, pressure_score) %>%
    arrange(rgn_id)
} else{
acid  <- data %>%
  filter_(criteria_rgn) %>%
  filter_(criteria_year) %>%
  dplyr::select(rgn_id, pressure_score) %>%
  arrange(rgn_id)
}

write.csv(acid, file.path(save_loc, sprintf('data/acid_%s_%s.csv', regionType, newYear)), row.names=FALSE)
}


### extract data 
for(newYear in 2011:2014){
  saveData("eez", newYear)
}

for(newYear in 2011:2014){
  saveData("eez-ccamlr", newYear)
}

for(newYear in 2011:2014){
  saveData("fao", newYear)
}



### try visualizing the data using googleVis plot
library(googleVis)
plotData <- data %>%
  filter(sp_type == "eez") %>%
  dplyr::select(rgn_name, year, pressure_score)

library(googleVis)

Motion=gvisMotionChart(plotData, 
                       idvar="rgn_name", 
                       timevar="year")
plot(Motion)

print(Motion, file=file.path(save_loc, 'acid.html'))


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


#########################################
#### SLR ----
#########################################

# https://github.com/OHI-Science/issues/issues/374
# the following raster is log transformed and then the 99.99th quantile was used to establish the standardization value.
# The outcome was that most regions had a pressure score of around 0.7 - which seemed high for this pressure.  This 
# suggested that we should probably avoid log transforming these particular data.
# rast <- raster('/var/data/ohi/git-annex/globalprep/AVISO-SeaLevelRise_v2015/output/slr_final.tif')
 rast <- raster('/var/data/ohi/git-annex/globalprep/AVISO-SeaLevelRise_v2015/output/slr_nonlog_final.tif')

# extract data for each region:
regions_stats <- zonal(rast,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$sp_id) #should be none
setdiff(rgn_data$sp_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone")

## save data for toolbox
eez <- data %>%
  filter(sp_type=="eez") %>%
  dplyr::select(rgn_id, pressure_score=mean)
  
  write.csv(eez, file.path(save_loc, 'data/slr_eez_2015.csv'), row.names=FALSE)
eez <- read.csv(file.path(save_loc, 'data/slr_eez_2015.csv'))

# fao <- data %>%  ## probably not a pressure in high seas
#   filter(sp_type=="fao") %>%
#   dplyr::select(rgn_id, pressure_score=mean)

# write.csv(fao, file.path(save_loc, 'data/slr_fao_2015.csv'), row.names=FALSE)

## should go through and eliminate the regions that do not have land
antarctica <- data %>%
  filter(sp_type=="eez-ccamlr") %>%
  dplyr::select(rgn_id = sp_id, pressure_score=mean)

write.csv(antarctica, file.path(save_loc, 'data/slr_ccamlr_2015.csv'), row.names=FALSE)


## plot the data to make sure range of values for regions is reasonable
library(ggplot2)
ggplot(eez, aes(pressure_score)) +
  geom_histogram(fill="gray", color="black") + 
  theme_bw() + 
  labs(title="Region scores for SLR")
quantile(eez$pressure_score)


## extract data to show proportion of gap-filling
# rasts <- raster('/var/data/ohi/git-annex/globalprep/AVISO-SeaLevelRise_v2015/output/slr_interpolated_cells.tif')
# reclassify(rasts, c(-Inf, Inf, 1), filename='/var/data/ohi/git-annex/globalprep/AVISO-SeaLevelRise_v2015/output/slr_interpolated_cells_yes_no.tif', progress="text")
interp <- raster('/var/data/ohi/git-annex/globalprep/AVISO-SeaLevelRise_v2015/output/slr_interpolated_cells_yes_no.tif')

# extract data for each region:
regions_stats <- zonal(interp,  zones, fun="sum", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$sp_id) #should be none
setdiff(rgn_data$sp_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") %>%
  dplyr::select(sp_id, rgn_id, sp_type, rgn_name, interpolated=sum) 

write.csv(data, file.path(save_loc, "tmp/slr_interpolated_cells.csv"), row.names=FALSE)


rc_count2  <- read.csv(file.path(save_loc, "sp_id_areas.csv")) %>%
  dplyr::select(sp_id=value, cellNum=count) %>%
  left_join(data, by='sp_id') %>%
  filter(!is.na(sp_id)) %>%
  mutate(prop_gap_filled = interpolated/cellNum)
write.csv(rc_count2, file.path(save_loc, "tmp/slr_prop_interpolated.csv"), row.names=FALSE)

final_gap <- rc_count2 %>%
  dplyr::filter(sp_type == "eez") %>%
  dplyr::select(rgn_id, gap_filled = prop_gap_filled) %>%
  mutate(gap_filled = round(gap_filled, 2)) %>%
  arrange(rgn_id)

write.csv(final_gap, file.path(save_loc, "data/slr_gap_fill_attr.csv"), row.names=FALSE)
final_gap <- read.csv(file.path(save_loc, "data/slr_gap_fill_attr.csv"))
#library(ggplot2)

ggplot(final_gap, aes(gap_filled)) +
  geom_histogram() +
  theme_bw() +
  labs(title="SLR: Proportion area gap-filled")

sum(final_gap$gap_filled > 0.5)


#########################################
## Trash ----
#########################################
# some issues dealing with the preparation of these data: 
# https://github.com/OHI-Science/issues/issues/306#issuecomment-72252954
# also want to apply ice mask so as to eliminate these regions



## creating data with ice mask
# trash <- raster('/var/data/ohi/git-annex/globalprep/FiveGyres_MarinePlastics_CW/v2015/output/weight_rescale.tif')
# ice_mask_resampled <- raster("/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/ice_mask_resampled")
# s <- stack(ice_mask_resampled, trash)
# overlay(s, fun=function(x,y) x*y,
#         filename="/var/data/ohi/git-annex/globalprep/FiveGyres_MarinePlastics_CW/v2015/output/weight_rescale_icemask.tif",
#         progress="text", overwrite=TRUE)

rast <- raster("/var/data/ohi/git-annex/globalprep/FiveGyres_MarinePlastics_CW/v2015/output/weight_rescale_icemask.tif")
# extract data for each region:
regions_stats <- zonal(rast,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$sp_id) #should be none
setdiff(rgn_data$sp_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone")

## save data for toolbox
eez <- data %>%
  filter(sp_type=="eez") %>%
  dplyr::select(rgn_id, pressure_score=mean)

#write.csv(eez, file.path(save_loc, 'data/trash_eez_2015.csv'), row.names=FALSE)
eez <- read.csv(file.path(save_loc, 'data/trash_eez_2015.csv'))

fao <- data %>%  ## probably not a pressure in high seas
  filter(sp_type=="fao") %>%
  dplyr::select(rgn_id, pressure_score=mean)

# write.csv(fao, file.path(save_loc, 'data/trash_fao_2015.csv'), row.names=FALSE)

antarctica <- data %>%
  filter(sp_type=="eez-ccamlr") %>%
  dplyr::select(rgn_id = sp_id, pressure_score=mean)

#write.csv(antarctica, file.path(save_loc, 'data/trash_ccamlr_2015.csv'), row.names=FALSE)


## plot the data to make sure range of values for regions is reasonable
library(ggplot2)
ggplot(eez, aes(pressure_score)) +
  geom_histogram(fill="gray", color="black") + 
  theme_bw() + 
  labs(title="Region scores for trash")
quantile(eez$pressure_score)

data %>%
  filter(sp_type=="eez") %>%
arrange(mean)


#########################################
#### UV ----
#########################################
# https://github.com/OHI-Science/issues/issues/377
# 2 raster choices: logged and non-logged.  Going with the logged version mainly out of tradition
rast <- raster('/var/data/ohi/git-annex/globalprep/Pressures_UV/output/uv_anomaly_diff_moll_1km_log_resc.tif')

# extract data for each region:
regions_stats <- zonal(rast,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$sp_id) #should be none
setdiff(rgn_data$sp_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone")

## save data for toolbox
eez <- data %>%
  filter(sp_type=="eez") %>%
  dplyr::select(rgn_id, pressure_score=mean)

#write.csv(eez, file.path(save_loc, 'data/uv_eez_2015.csv'), row.names=FALSE)
eez <- read.csv(file.path(save_loc, 'data/uv_eez_2015.csv'))

fao <- data %>% 
  filter(sp_type=="fao") %>%
  dplyr::select(rgn_id, pressure_score=mean)

# write.csv(fao, file.path(save_loc, 'data/uv_fao_2015.csv'), row.names=FALSE)

antarctica <- data %>%
  filter(sp_type=="eez-ccamlr") %>%
  dplyr::select(rgn_id = sp_id, pressure_score=mean)

#write.csv(antarctica, file.path(save_loc, 'data/uv_ccamlr_2015.csv'), row.names=FALSE)


## plot the data to make sure range of values for regions is reasonable
library(ggplot2)
ggplot(eez, aes(pressure_score)) +
  geom_histogram(fill="gray", color="black") + 
  theme_bw() + 
  labs(title="Region scores for UV")
quantile(eez$pressure_score)


data %>%
  filter(sp_type=="eez") %>%
  arrange(mean)



#########################################
#### Exploring fertilizer and pesticide plume data ----
#########################################
dir(file.path(dir_halpern2008, "mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/201112/step8"))
rast_locs <- file.path(dir_halpern2008, "mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/201112/step8")

# Fertilizer
fert_older <- raster(file.path(rast_locs, "global_plumes_fert_2007_2010_trans.tif"))
fert_older <- setMinMax(fert_older)
fert_older

fert_newer <- raster(file.path(rast_locs, "global_plumes_fert_2011_2012_trans.tif"))
fert_newer <- setMinMax(fert_newer)
fert_newer

fert_dif <- raster(file.path(rast_locs, "global_plumes_fert_2011_2012_raw_minus_2007_2010_raw.tif"))
fert_dif <- setMinMax(fert_dif)
fert_dif


# Pesticide
pest_older <- raster(file.path(rast_locs, "global_plumes_pest_2007_2010_trans.tif"))
setMinMax(pest_older)
pest_older

pest_newer <- raster(file.path(rast_locs, "global_plumes_pest_2011_2012_trans.tif"))
setMinMax(pest_newer)
pest_newer

pest_dif <- raster(file.path(rast_locs, "global_plumes_pest_2011_2012_raw_minus_2007_2010_raw.tif"))
pest_dif <- setMinMax(pest_dif)
pest_dif
