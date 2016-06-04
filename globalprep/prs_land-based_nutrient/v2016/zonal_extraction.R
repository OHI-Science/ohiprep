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
zones <- raster(zones)
rgn_data <- read.csv(file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/data/sp_mol_raster_1km/regionData.csv"))    # data for sp_id's used in raster

# save location
save_loc_rasts <- file.path(dir_M, "git-annex/globalprep/prs_land-based_nutrient/v2016/int")
save_loc_data <- "globalprep/prs_land-based_nutrient/v2016"


raster_2016_loc <- file.path(dir_M, 
                       "marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/step7/output/2013/global_plumes_fert_2013_raw.tif")
raster_2016 <- raster(raster_2016_loc)
raster_2016

## reference point
## scaling coefficient for fertlizer = 5.594088 (file with these values: ohiprep/globalprep/PressuresRegionExtract/land_based_quantiles.csv)
## This was derived using all 99.99th quantile from all years of data in 2015 (fao data from 2007 to 2012)
ref_point <- read.csv("globalprep/reference_points_pressures.csv") %>%
  dplyr::filter(pressure == "Fertilizer plume data") %>%
  dplyr::select(ref_point) %>%
  data.frame()
ref_point <- as.numeric(as.character(ref_point$ref_point))


## log raster and then rescale and then make the same standard size (these are always different sizes...very annoying)
  calc(raster_2016, function(x){log(x+1)}, progress="text", filename=file.path(save_loc_rasts, "global_plumes_fert_2013_log.tif"), overwrite=TRUE)
  tmp <- raster(file.path(save_loc_rasts, "global_plumes_fert_2013_log.tif"))
  calc(tmp, fun=function(x){ifelse(x>ref_point, 1, x/ref_point)},
       progress='text',
       filename=file.path(save_loc_rasts, "global_plumes_fert_2013_log_scaled.tif"), overwrite=TRUE)
  tmp <- raster(file.path(save_loc_rasts, "global_plumes_fert_2013_log_scaled.tif"))
  extend(tmp, zones, filename=file.path(save_loc_rasts, "global_plumes_fert_2013_log_scaled_extend.tif"), progress="text", overwrite=TRUE)
  tmp <- raster(file.path(save_loc_rasts, "global_plumes_fert_2013_log_scaled_extend.tif"))
  

## extract by zone
regions_stats <- zonal(tmp,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$sp_id) #should be none
setdiff(rgn_data$sp_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 
write.csv(data, file.path(save_loc_data, "int/nutrients_plume_data.csv"), row.names=FALSE)

data <- read.csv(file.path(save_loc_data, "int/nutrients_plume_data.csv"))

# only relevant to eez data:
eez <- data %>%
  dplyr::filter(sp_type=="eez") %>%
  dplyr::select(rgn_id, pressure_score=mean) %>%
  dplyr::arrange(rgn_id)
write.csv(eez, file.path(save_loc_data, "output/cw_fertilizers_score_2016.csv"))
  

## extract at 3 nm (in addition to a pressure, this will be used for CW and the CW trend)
# (going to try using the polygon, rather than converting to raster)
offshore_3nm_poly <- readOGR(dsn = file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/data"), "rgn_offshore3nm_mol")
offshore_3nm_poly <- offshore_3nm_poly[offshore_3nm_poly@data$rgn_type == "eez", ]

# this is here in case I decide to use this method instead of using the polygons to extract the data:
# tmp <- raster(file.path(rast_locs, "Frazier/global_plumes_fert_2007_raw_log_extend.tif"))
# rasterize(inland_3nm_poly, tmp, field='rgn_id', 
#           filename=file.path(rast_locs, "Frazier/inland_3nm.tif"), overwrite=TRUE,
#           progress='text')

data <- raster::extract(tmp, offshore_3nm_poly, na.rm=TRUE, normalizeWeights=FALSE, fun=mean, df=TRUE, progress="text")
data2 <- cbind(data, offshore_3nm_poly@data) 
write.csv(data2, file.path(save_loc_data, "int/nutrients_plume_data_offshore_3nm.csv"), row.names=FALSE)

data <- read.csv(file.path(save_loc_data, "int/nutrients_plume_data_offshore_3nm.csv")) 

# save final data
final <- data %>%
  dplyr::select(rgn_id, pressure_score) %>%
  arrange(rgn_id) %>%
  filter(!is.na(pressure_score))#NA is Antarctica - this is fine
write.csv(final, file.path(save_loc_data, "output/cw_fertilizers_score_3nm_2016.csv"), row.names=FALSE)

### TREND calculations
data <- data %>%
  mutate(year = 2013) %>%
  dplyr::select(rgn_id, rgn_name, year, pressure_score=global_plumes_fert_2013_log_scaled_extend) %>%
  filter(!is.na(pressure_score))#NA is Antarctica - this is fine

## get previous years data
old_data <- read.csv("globalprep/prs_land-based_nutrient/v2015/int/nutrients_plume_data.csv")
old_data <- gather(old_data, "year", "pressure_score", starts_with("global"))
old_data <- old_data %>%
  mutate(year = gsub("global_plumes_fert_", "", year)) %>%
  mutate(year = gsub("_raw_log_extend_scaled", "", year)) %>%
  mutate(year = as.numeric(as.character(year))) %>% 
  filter(sp_type == "eez") %>%   # this doesn't really apply to high seas regions and Antarctica is all zeros
  dplyr::select(rgn_id, rgn_name, year, pressure_score)

trend_data <- rbind(data, old_data)

## trend should be calculated on 3nm (not eez)
scenario_year <- 2016
  trend_data <- trend_data %>%
    filter(year %in% (scenario_year-7):(scenario_year-3)) %>%
    group_by(rgn_id) %>%
    do(mdl = lm(pressure_score ~ year, data = .)) %>%
    summarize(rgn_id,
              trend = coef(mdl)['year'] * 5) %>%
    ungroup()
  write.csv(trend_data, file.path(save_loc_data, sprintf('output/cw_fertilizers_trend_%s.csv', scenario_year)), row.names=FALSE)
  
  library(googleVis)
  plotData <- data %>%
    dplyr::select(rgn_name, year, pressure_score)
p <- gvisMotionChart(plotData, idvar = 'rgn_name', timevar="year")
    plot(p)
    
    print(p, file=file.path(save_loc_data, 'fertilizer.html'))
    