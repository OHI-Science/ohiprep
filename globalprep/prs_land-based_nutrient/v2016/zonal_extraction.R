#####################################################
## Exctracting nutrient pressure data for OHI 2016
## MRF: June 1 2016
#####################################################

source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(dplyr)

# raster/zonal data
zones <- file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/data/sp_mol_raster_1km/sp_mol_raster_1km.tif")
rgn_data <- read.csv(file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/data/sp_mol_raster_1km/regionData.csv"))    # data for sp_id's used in raster

# save location
save_loc_rasts <- file.path(dir_M, "git-annex/globalprep/prs_land-based_nutrient/v2016/int")
save_loc_data <- "globalprep/prs_land-based_nutrient/v2016"

rast_locs <- file.path(dir_halpern2008, 
                       "mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/step7/output/2013/global_plumes_fert_2013_raw.tif")

## reference points
ref_point <- read.csv("globalprep/reference_points_pressures.csv") %>%
  dplyr::filter(pressure == "Fertilizer plume data") %>%
  dplyr::select(ref_point) %>%
  data.frame()

## peak at raster to see what is up:
check <- raster(rast_locs)
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
