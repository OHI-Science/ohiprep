### zonal extraction and summary of pressure data
### MRF: Feb 25 2015

########## NOTE: For future versions make rgn_id into sp_id for CCAMLR regions!!!


tmpdir <- '~/big/R_raster_tmp'
rasterOptions(tmpdir=tmpdir)

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
#### SST ----
#########################################
# https://github.com/OHI-Science/issues/issues/499

# load and check relevant rasters
rast_2012 <- raster(file.path(dir_neptune_data, 
                              'git-annex/globalprep/Pressures_SST/v2015/output/sst_2005_2009-1985_1989_rescaled_v2.tif'))
rast_2012
rast_2013 <- raster(file.path(dir_neptune_data, 
                              'git-annex/globalprep/Pressures_SST/v2015/output/sst_2006_2010-1985_1989_rescaled_v2.tif'))
rast_2013
rast_2014 <- raster(file.path(dir_neptune_data, 
                              'git-annex/globalprep/Pressures_SST/v2015/output/sst_2007_2011-1985_1989_rescaled_v2.tif'))
rast_2014
rast_2015 <- raster(file.path(dir_neptune_data, 
                              'git-annex/globalprep/Pressures_SST/v2015/output/sst_2008_2012-1985_1989_rescaled_v2.tif'))
rast_2015


# apply ice mask
ice_mask <- raster("/var/data/ohi/git-annex/Global/NCEAS-Pressures-Summaries_frazier2013/ice_mask_resampled")

for(i in 2012:2015){ #i=2012
rast <- get(paste0("rast_", i))
overlay(rast, ice_mask, fun=function(x,y) x*y, progress='text',
                         filename=file.path(dir_neptune_data, 
                        sprintf('git-annex/globalprep/Pressures_SST/v2015/output/sst_stack_%s_rescaled_icemask', i)),
        overwrite=TRUE)
}



# extract data
sst_2012_ice <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Pressures_SST/v2015/output/sst_stack_2012_rescaled_icemask'))
names(sst_2012_ice) <- "sst_2012"
sst_2013_ice <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Pressures_SST/v2015/output/sst_stack_2013_rescaled_icemask'))
names(sst_2013_ice) <- "sst_2013"
plot(sst_2013_ice)
sst_2014_ice <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Pressures_SST/v2015/output/sst_stack_2014_rescaled_icemask'))
names(sst_2014_ice) <- "sst_2014"
sst_2015_ice <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/Pressures_SST/v2015/output/sst_stack_2015_rescaled_icemask'))
names(sst_2015_ice) <- "sst_2015"


sst_stack <- stack(sst_2012_ice, sst_2013_ice, sst_2014_ice, sst_2015_ice)

# extract data
regions_stats <- zonal(sst_stack,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$sp_id) #should be none
setdiff(rgn_data$sp_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone")
#write.csv(data, file.path(save_loc, "tmp/sst.csv"), row.names=FALSE)
data <- read.csv(file.path(save_loc, "tmp/sst.csv"))

## save data for toolbox
for(years in c(2012:2015)){ #years="2012"
  scenario <- sprintf("sst_%s", years)
  
  eez <- filter(data, sp_type == "eez")
  eez <- eez[, c('rgn_id', scenario)]
  names(eez)[names(eez) == scenario] <- "pressure_score"
  write.csv(eez, sprintf('globalprep/PressuresRegionExtract/data/sst_eez_%s.csv', years), row.names=FALSE)
  
  ant <- filter(data, sp_type == "eez-ccamlr")
  ant <- ant[, c('sp_id', scenario)]
  names(ant)[names(ant) == scenario] <- "pressure_score"
  names(ant)[names(ant) == 'sp_id'] <- "rgn_id"
  write.csv(ant, sprintf('globalprep/PressuresRegionExtract/data/sst_eez-ccamlr_%s', years), row.names=FALSE)
  
  fao <- filter(data, sp_type == "fao")
  fao <- fao[, c('rgn_id', scenario)]
  names(fao)[names(fao) == scenario] <- "pressure_score"
  write.csv(fao, sprintf('globalprep/PressuresRegionExtract/data/sst_fao_%s', years), row.names=FALSE)
}


## plot the data to make sure range of values for regions is reasonable

# 1. compare with last years data

old_sst <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-Pressures_v2013a/data/cc_sst_2013_NEW.csv"))
compare <- old_sst %>%
  dplyr::select(rgn_id, old_pressure_score=pressure_score) %>%
  left_join(data) %>%
  filter(!(is.na(rgn_name))) %>%
  filter(sp_type=="eez") %>%
  dplyr::select(rgn_id, rgn_name, old_pressure_score, sst_2012, sst_2013, sst_2014, sst_2015)

#filtered out these, but wanted to make sure they didn't reflect underlying issues:  
# compare[is.na(compare$sp_id), ] # ones that don't match new data are antarctica high seas regions (268, 271, 278), an NA high seas region (265), and conflict areas (255)   
# compare[is.na(compare$old_pressure_score), ] # often Bosnia/Herzegovina falls out of raster analyses due to very small eez region

library(ggplot2)

ggplot(compare, aes(x=old_pressure_score, y=sst_2013)) +
  geom_point(shape=19) + 
  theme_bw() + 
  geom_abline(intercept=0, slope=1) + 
  labs(title="SST comparison")

ggplot(compare, aes(x=sst_2013)) +
  geom_histogram(fill="gray", color="black") + 
  theme_bw() + 
  labs(title="SST 2013")
quantile(compare$sst_2013)

library(tidyr)
compare_plot <- gather(compare, "year", "pressure_score", 3:7) %>%
  filter(year != "old_pressure_score") %>%
  mutate(year = as.numeric(gsub("sst_", "", year))) %>%
  dplyr::select(rgn_name, year, pressure_score)

library(googleVis)

Motion=gvisMotionChart(compare_plot, 
                       idvar="rgn_name", 
                       timevar="year")
plot(Motion)

print(Motion, file=file.path(save_loc, 'sst.html'))


#### Fisheries ----
# read in fisheries pressure data (should be 8 layers, with values 0 to 1)

#check an example:
tmp <- raster('/var/data/ohi/git-annex/globalprep/Pressures_fishing/v2015/output/catch_03_07_npp_hb_rescaled.tif')

files <- list.files('/var/data/ohi/git-annex/globalprep/Pressures_fishing/v2015/output')
rescaled_files <- grep("_rescaled", files, value=TRUE)

pressure_stack <- stack()
for(rast in rescaled_files){ # rast = 'catch_03_07_npp_hb_rescaled.tif'
  tmp <- raster(file.path('/var/data/ohi/git-annex/globalprep/Pressures_fishing/v2015/output', rast))
  pressure_stack <- stack(pressure_stack, tmp)
}

# extract data for each region:
regions_stats <- zonal(pressure_stack,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$sp_id) #should be none
setdiff(rgn_data$sp_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 
write.csv(data, file.path(save_loc, "tmp/fisheries.csv"), row.names=FALSE)

data[is.na(data$catch_03_07_npp_hb_rescaled), ]

data <- read.csv(file.path(save_loc, "tmp/fisheries.csv"))
data_long <- data %>%
  gather("layer", "pressure_score", starts_with("catch")) %>%
  mutate(layer = gsub('_rescaled', '', layer)) 
#  mutate(pressure_score = ifelse(is.na(pressure_score), 0, pressure_score))

## record gap-filled regions:
convert_year <- data.frame(layer =c('catch_06_10_npp_hb', 'catch_05_09_npp_hb', 'catch_04_08_npp_hb', 'catch_03_07_npp_hb',
                                    'catch_06_10_npp_lb', 'catch_05_09_npp_lb', 'catch_04_08_npp_lb', 'catch_03_07_npp_lb'),
                           year = 2015:2012)


gap_record <- data_long %>% 
  left_join(convert_year) %>%
  mutate(gap_filled = ifelse(is.na(pressure_score), "gap-filled", "no"))
write.csv(gap_record, file.path(save_loc, "data/fisheries_gap_filling.csv"), row.names=FALSE)

### gap-fill some eez regions:

regions <- read.csv("src/LookupTables/rgn_georegions_wide_2013b.csv") %>%
  dplyr::select(-rgn_nam)

## fill in a couple missing values:
# replace Bosnia with Croatio values
croatia <- data_long[data_long$rgn_id == 187,] %>%
  dplyr::select(layer, pressure_score2=pressure_score) %>%
  mutate(rgn_id = 232)

data_gapfill <- data_long %>%
  left_join(croatia) %>%
  mutate(pressure_score = ifelse(is.na(pressure_score), pressure_score2, pressure_score)) %>%
  dplyr::select(-pressure_score2)

# replace arctic and Bouvet Island with zeros 
data_gapfill$pressure_score[data_gapfill$rgn_id %in% c(105, 260)] <- 0

# regional gap-filling for remaining: Bulgaria (71), Romania (72), Georgia (74), Ukraine (75), Jordan (215)  
eez_gap_fill <- data_gapfill %>%
  filter(sp_type == "eez") %>%
  left_join(regions, by="rgn_id") %>%
  group_by(layer, r2) %>%
  mutate(mean_pressure_score = mean(pressure_score, na.rm=TRUE)) %>%
  mutate(pressure_score = ifelse(is.na(pressure_score), mean_pressure_score, pressure_score)) %>%
  ungroup() %>%
  dplyr::select(sp_id, sp_type, rgn_id, rgn_name, layer, pressure_score)

## the two r2 regions that need gap-filled data:
data.frame(eez_gap_fill[eez_gap_fill$r2 %in% c("151"), ])
data.frame(eez_gap_fill[eez_gap_fill$r2 %in% c(145), ])

### replacing previous eez data with gapfilled eez data:
pressure_data <- data_gapfill %>%
  filter(sp_type != "eez") %>%
  bind_rows(eez_gap_fill)


layerType <- unique(pressure_data$layer)

for(layer in layerType){ #layer="catch_03_07_npp_hb"
  pressureData <- pressure_data[pressure_data$layer %in% layer, ]

  # eez data
    data <- pressureData %>%
    filter(sp_type == 'eez') %>%
    dplyr::select(rgn_id = rgn_id, pressure_score) %>%
    arrange(rgn_id)
  write.csv(data, file.path(save_loc, sprintf('data/%s_eez.csv', layer)), row.names=FALSE)
  
  # hs data
  data <- pressureData %>%
    filter(sp_type == 'fao') %>%
    dplyr::select(rgn_id = rgn_id, pressure_score) %>%
    arrange(rgn_id)
  write.csv(data, file.path(save_loc, sprintf('data/%s_fao.csv', layer)), row.names=FALSE)
  
  # antarctica data
  data <- pressureData %>%
    filter(sp_type == 'eez-ccamlr') %>%
    dplyr::select(rgn_id = sp_id, pressure_score) %>%
    arrange(rgn_id)
  write.csv(data, file.path(save_loc, sprintf('data/%s_ccamlr.csv', layer)), row.names=FALSE)

}


### visualizing the data using googleVis plot
library(googleVis)


high_bycatch <- pressure_data %>%
  filter(sp_type == "eez") %>%
  filter(layer %in% grep("_hb", layerType, value=TRUE)) %>%
  left_join(convert_year) %>%
  dplyr::select(rgn_name, year, pressure_score)


Motion=gvisMotionChart(high_bycatch, 
                       idvar="rgn_name", 
                       timevar="year")
plot(Motion)

print(Motion, file=file.path(save_loc, 'high_bycatch.html'))


low_bycatch <- pressure_data %>%
  filter(sp_type == "eez") %>%
  filter(layer %in% grep("_lb", layerType, value=TRUE)) %>%
  left_join(convert_year) %>%
  dplyr::select(rgn_name, year, pressure_score)
Motion=gvisMotionChart(high_bycatch, 
                       idvar="rgn_name", 
                       timevar="year")
plot(Motion)

print(Motion, file=file.path(save_loc, 'low_bycatch.html'))


#########################################
#### Land-based fertilizer and pesticide plume data prep ----
#########################################

rast_locs <- file.path(dir_halpern2008, "mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/raw_global_results")

## peak at raster to see what is up:
check <- raster(file.path(rast_locs, 'global_plumes_fert_2012_raw.tif'))
## darn: different extents and such...need to make these the same

quantiles <- data.frame(plumeData <- list.files(rast_locs), quantile_9999_ln=NA)
files <- grep(".tif", list.files(rast_locs), value=TRUE)

for(plume in files){ #plume='global_plumes_fert_2007_raw.tif'
  tmp <- raster(file.path(rast_locs, plume))
  saveName <- gsub(".tif", "", plume)
  calc(tmp, function(x){log(x+1)}, progress="text", filename=file.path(rast_locs, sprintf("Frazier/%s_log.tif", saveName)), overwrite=TRUE)
  tmp <- raster(file.path(rast_locs, sprintf("Frazier/%s_log.tif", saveName)))
  quantiles$quantile_9999_ln[plumeData == plume] <- quantile(tmp, .9999)
  extend(tmp, zones, filename=file.path(rast_locs, sprintf("Frazier/%s_log_extend.tif", saveName)), progress="text", overwrite=TRUE)
  tmp <- raster(file.path(rast_locs, sprintf("Frazier/%s_log_extend.tif", saveName)))
  unlink(file.path(rast_locs, sprintf('Frazier/%s_log.tif', saveName)))
}


#############################
## fertilizer ----
#############################

## scaling coefficient for fertlizer = 5.594088 (file with these values: ohiprep/globalprep/PressuresRegionExtract/land_based_quantiles.csv)
fert_scalar <- 5.594088

list_fert <- files <- grep("_fert", list.files(file.path(rast_locs, "Frazier")), value=TRUE)

for(fert in list_fert){ #fert="global_plumes_fert_2007_raw_log_extend.tif"
  tmp <- raster(file.path(rast_locs, "Frazier", fert))
  saveName <- gsub('.tif', '', fert)
  calc(tmp, fun=function(x){ifelse(x>fert_scalar, 1, x/fert_scalar)},
       progress='text',
       filename=file.path(rast_locs, sprintf("Frazier/%s_scaled.tif", saveName)), overwrite=TRUE)
}

list_fert <- files <- grep("_fert", list.files(file.path(rast_locs, "Frazier")), value=TRUE)
list_fert_scaled <- grep("_scaled", list_fert, value=TRUE)


pressure_stack <- stack()
for(rast in list_fert_scaled){
  tmp <- raster(file.path(rast_locs, "Frazier", rast))
  pressure_stack <- stack(pressure_stack, tmp)
}


# extract data for each eez region:
regions_stats <- zonal(pressure_stack,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$sp_id) #should be none
setdiff(rgn_data$sp_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 
write.csv(data, file.path(save_loc, "tmp/nutrients_plume_data.csv"), row.names=FALSE)

data <- read.csv(file.path(save_loc, "tmp/nutrients_plume_data.csv"))
data <- gather(data, "year", "pressure_score", starts_with("global"))
data <- data %>%
  mutate(year = gsub("global_plumes_fert_", "", year)) %>%
  mutate(year = gsub("_raw_log_extend_scaled", "", year)) %>%
  mutate(year = as.numeric(as.character(year))) %>% 
  filter(sp_type == "eez") %>%   # this doesn't really apply to high seas regions and Antarctica is all zeros
  dplyr::select(rgn_id, rgn_name, year, pressure_score)

# calculate pressure data for each year
## trend should be calculated on 3nm (not eez)
for(scenario_year in 2012:2015){ #scenario_year=2015
  #calculate/save pressure score data
  score_data <- data %>%
    filter(year == (scenario_year-3)) %>%
    dplyr::select(rgn_id, pressure_score)
  write.csv(score_data, file.path(save_loc, sprintf('data/cw_fertilizers_score_%s.csv', scenario_year)), row.names=FALSE)
}

## extract at 3 nm (in addition to a pressure, this will be used for CW and the CW trend)
# (going to try using the polygon, rather than converting to raster)
offshore_3nm_poly <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", "rgn_offshore3nm_mol")
offshore_3nm_poly <- offshore_3nm_poly[offshore_3nm_poly@data$rgn_type == "eez", ]

# this is here in case I decide to use this method instead of using the polygons to extract the data:
# tmp <- raster(file.path(rast_locs, "Frazier/global_plumes_fert_2007_raw_log_extend.tif"))
# rasterize(inland_3nm_poly, tmp, field='rgn_id', 
#           filename=file.path(rast_locs, "Frazier/inland_3nm.tif"), overwrite=TRUE,
#           progress='text')

data <- raster::extract(pressure_stack, offshore_3nm_poly, na.rm=TRUE, normalizeWeights=FALSE, fun=mean, df=TRUE, progress="text")
data2 <- cbind(data, offshore_3nm_poly@data) 
write.csv(data2, file.path(save_loc, "tmp/nutrients_plume_data_offshore_3nm.csv"), row.names=FALSE)

data <- read.csv(file.path(save_loc, "tmp/nutrients_plume_data_offshore_3nm.csv")) 
data <- gather(data, "year", "pressure_score", starts_with("global"))
data <- data %>%
  mutate(year = gsub("global_plumes_fert_", "", year)) %>%
  mutate(year = gsub("_raw_log_extend_scaled", "", year)) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  dplyr::select(rgn_id, rgn_name, year, pressure_score) %>%
  filter(!is.na(pressure_score))#NA is Antarctica - this is fine


# calculate pressure data for each year
## trend should be calculated on 3nm (not eez)
for(scenario_year in 2012:2015){ #scenario_year=2015
    #calculate/save trend data
    trend_data <- data %>%
      filter(year %in% (scenario_year-7):(scenario_year-3)) %>%
      group_by(rgn_id) %>%
      do(mdl = lm(pressure_score ~ year, data = .)) %>%
      summarize(rgn_id,
                trend = coef(mdl)['year'] * 5) %>%
      ungroup()
    write.csv(trend_data, file.path(save_loc, sprintf('data/cw_fertilizers_trend_%s.csv', scenario_year)), row.names=FALSE)
  
  #calculate/save pressure score data
  score_data <- data %>%
    filter(year == (scenario_year-3)) %>%
    dplyr::select(rgn_id, pressure_score)
  write.csv(score_data, file.path(save_loc, sprintf('data/cw_fertilizers_score_3nm_%s.csv', scenario_year)), row.names=FALSE)
}

#############################
## pesticides ----
#############################

## scaling coefficient for pesticides = 1.91788700716876 (file with these values: ohiprep/globalprep/PressuresRegionExtract/land_based_quantiles.csv)
pest_scalar <- 1.91788700716876

list_pest <- grep("_pest", list.files(file.path(rast_locs, "Frazier")), value=TRUE)

for(pest in list_pest){ #pest="global_plumes_pest_2007_raw_log_extend.tif"
  tmp <- raster(file.path(rast_locs, "Frazier", pest))
  saveName <- gsub('.tif', '', pest)
  calc(tmp, fun=function(x){ifelse(x>pest_scalar, 1, x/pest_scalar)},
       progress='text',
       filename=file.path(rast_locs, sprintf("Frazier/%s_scaled.tif", saveName)), overwrite=TRUE)
}

##################
## to get the chemical pressure: pesticides + ocean pollution + inorganic pollution

## need to make the op and ip rasters have the same extent:
pest_rast <- raster(file.path(rast_locs, "Frazier", "global_plumes_pest_2007_raw_log_extend_scaled.tif"))

# only one ocean pollution raster for both time periods (so only normalized by one time period)
library(spatial.tools)
op <- raster('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2013_final/normalized_by_one_time_period/ocean_pollution.tif')
op_extend <- modify_raster_margins(op, extent_delta=c(1,0,1,0))
extent(op_extend) = extent(pest_rast)
writeRaster(op_extend, file.path(rast_locs, "Frazier/ocean_pollution_extend.tif"), overwrite=TRUE)

# two rasters for inorganic pollution (2003-2006 and 2007-2010)
# I used the 2007-2010 raster (normalized by both time periods):
# ip_07_10 <- raster('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2013_final/normalized_by_two_time_periods/inorganic.tif')
# extend(ip_07_10, pest_rast, filename=file.path(rast_locs, "Frazier/inorganic_pollution_07_10_extend.tif"), progress='text')
ip_07_10_extend <- raster(file.path(rast_locs, "Frazier/inorganic_pollution_07_10_extend.tif"))

# but, it might be better to use the earlier raster for some time periods, if so, here is the link:
#ip_03_06 <- raster('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2008_final/normalized_by_two_time_periods/inorganic.tif')


for(pest_year in 2007:2012){ #pest_year = 2007 
  pest_rast <- raster(file.path(rast_locs, "Frazier", sprintf("global_plumes_pest_%s_raw_log_extend_scaled.tif", pest_year)))
  chem_stack <- stack(pest_rast, op_extend, ip_07_10_extend)
  calc(chem_stack, 
       sum, na.rm=TRUE,
       progress='text',
       filename=file.path(rast_locs, sprintf("Frazier/chemical_pollution_%s.tif", pest_year)), overwrite=TRUE)
}

## take a look at the distribution of scores 
raster <- raster(file.path(rast_locs, "Frazier/chemical_pollution_2007.tif"))
quantile(raster, c(0.25, 0.50, 0.75, 0.9, 0.99, 0.999, 0.9999))
raster <- raster(file.path(rast_locs, "Frazier/chemical_pollution_2012.tif"))
quantile(raster, c(0.25, 0.50, 0.75, 0.9, 0.99, 0.999, 0.9999))

for(chem_year in 2007:2012){ #chem_year=2012
  tmp <- raster(file.path(rast_locs, sprintf("Frazier/chemical_pollution_%s.tif", chem_year)))
  calc(tmp, fun=function(x){ifelse(x>1, 1, x)},
       progress='text',
       filename=file.path(rast_locs, sprintf("Frazier/chemical_pollution_%s_scaled.tif", chem_year)), overwrite=TRUE)
}

## delete intermediate files due to lack of space on neptune:
for(delete_year in 2007:2012){
  unlink(file.path(rast_locs, sprintf("Frazier/chemical_pollution_%s.tif", delete_year)))
}

list_chem <- files <- grep("chemical_pollution", list.files(file.path(rast_locs, "Frazier")), value=TRUE)
list_chem_scaled <- grep("_scaled", list_chem, value=TRUE)


pressure_stack <- stack()
for(rast in list_chem_scaled){
  tmp <- raster(file.path(rast_locs, "Frazier", rast))
  pressure_stack <- stack(pressure_stack, tmp)
}


# extract data for each eez region:
regions_stats <- zonal(pressure_stack,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$sp_id) #should be none
setdiff(rgn_data$sp_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 
write.csv(data, file.path(save_loc, "tmp/chemical_data.csv"), row.names=FALSE)


data <- read.csv(file.path(save_loc, "tmp/chemical_data.csv"))
data <- gather(data, "year", "pressure_score", starts_with("chemical"))
data <- data %>%
  mutate(year = gsub("chemical_pollution_", "", year)) %>%
  mutate(year = gsub("_scaled", "", year)) %>%
  mutate(year = as.numeric(as.character(year))) %>% 
  filter(sp_type == "eez") %>%   # this doesn't really apply to high seas regions and Antarctica is all zeros
  dplyr::select(rgn_id, rgn_name, year, pressure_score)

# calculate pressure data for each year
## trend should be calculated on 3nm (not eez)
for(scenario_year in 2012:2015){ #scenario_year=2015
  #calculate/save pressure score data
  score_data <- data %>%
    filter(year == (scenario_year-3)) %>%
    dplyr::select(rgn_id, pressure_score)
  write.csv(score_data, file.path(save_loc, sprintf('data/cw_chemical_score_%s.csv', scenario_year)), row.names=FALSE)
}

## extract at 3 nm (in addition to a pressure, this will be used for CW and the CW trend)
# (going to try using the polygon, rather than converting to raster)
offshore_3nm_poly <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", "rgn_offshore3nm_mol")
offshore_3nm_poly <- offshore_3nm_poly[offshore_3nm_poly@data$rgn_type == "eez", ]

# this is here in case I decide to use this method instead of using the polygons to extract the data:
# tmp <- raster(file.path(rast_locs, "Frazier/global_plumes_fert_2007_raw_log_extend.tif"))
# rasterize(inland_3nm_poly, tmp, field='rgn_id', 
#           filename=file.path(rast_locs, "Frazier/inland_3nm.tif"), overwrite=TRUE,
#           progress='text')

data <- raster::extract(pressure_stack, offshore_3nm_poly, na.rm=TRUE, normalizeWeights=FALSE, fun=mean, df=TRUE, progress="text")
data2 <- cbind(data, offshore_3nm_poly@data) 
write.csv(data2, file.path(save_loc, "tmp/chemical_data_offshore_3nm.csv"), row.names=FALSE)

data <- read.csv(file.path(save_loc, "tmp/chemical_data_offshore_3nm.csv")) 
data <- gather(data, "year", "pressure_score", starts_with("chemical"))
data <- data %>%
  mutate(year = gsub("chemical_pollution_", "", year)) %>%
  mutate(year = gsub("_scaled", "", year)) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  dplyr::select(rgn_id, rgn_name, year, pressure_score) %>%
  filter(!is.na(pressure_score))#NA is Antarctica - this is fine


# calculate pressure data for each year
## trend should be calculated on 3nm (not eez)
for(scenario_year in 2012:2015){ #scenario_year=2015
  #calculate/save trend data
  trend_data <- data %>%
    filter(year %in% (scenario_year-7):(scenario_year-3)) %>%
    group_by(rgn_id) %>%
    do(mdl = lm(pressure_score ~ year, data = .)) %>%
    summarize(rgn_id,
              trend = coef(mdl)['year'] * 5) %>%
    ungroup()
  write.csv(trend_data, file.path(save_loc, sprintf('data/cw_chemical_trend_%s.csv', scenario_year)), row.names=FALSE)
  
  #calculate/save pressure score data
  score_data <- data %>%
    filter(year == (scenario_year-3)) %>%
    dplyr::select(rgn_id, pressure_score)
  write.csv(score_data, file.path(save_loc, sprintf('data/cw_chemical_score_3nm_%s.csv', scenario_year)), row.names=FALSE)
}


## Visualizing the data using GoogleVis
### visualizing the data using googleVis plot
plume_files <- grep("cw_", list.files(file.path(save_loc, 'data')), value=TRUE)

plume_types <- c('cw_chemical_score',
                 'cw_chemical_score_3nm',
                 'cw_chemical_trend',
                 'cw_fertilizers_score',
                 'cw_fertilizers_score_3nm',
                 'cw_fertilizers_trend')

rgns <- read.csv(file.path(save_loc, "data/cw_chemical_score_2015.csv")) %>%
  dplyr::select(rgn_id)

allData <- expand.grid(rgn_id = rgns$rgn_id, year=2012:2015) 

for(plume in plume_types) { #plume = 'cw_chemical_score'
data <- data.frame()  
    for(year in 2012:2015){#year = 2012
    tmp <- read.csv(file.path(save_loc, 'data', paste0(plume, sprintf("_%s.csv", year))))
    tmp$year <- year
    names(tmp)[which(names(tmp)=="pressure_score" | names(tmp)=="trend")] <- plume
    data <- rbind(data, tmp)
  }
allData <- left_join(allData, data, by=c('rgn_id', 'year'))  
}

regions <- rgn_data %>%
  dplyr::select(rgn_id, rgn_name)
  
allData <- left_join(allData, regions, by="rgn_id") %>%
  dplyr::select(-rgn_id)

library(googleVis)

Motion=gvisMotionChart(allData, 
                       idvar="rgn_name", 
                       timevar="year")
plot(Motion)

print(Motion, file=file.path(save_loc, 'plumes.html'))

