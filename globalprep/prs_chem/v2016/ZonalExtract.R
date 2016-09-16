## zonal extraction and summary of pressure data
### MRF: June 3 2016
## MRF: Sep 16 2016 update: revising trend to proportion
##      (rather than absolute...saved with "new" suffix)



source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(dplyr)

# raster/zonal data
zones <- file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/data/sp_mol_raster_1km/sp_mol_raster_1km.tif")
zones <- raster(zones)
rgn_data <- read.csv(file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/data/sp_mol_raster_1km/regionData.csv"))    # data for sp_id's used in raster

# save location
save_loc_rasts <- file.path(dir_M, "git-annex/globalprep/prs_chem/v2016/int")
save_loc_data <- "globalprep/prs_chem/v2016"


## ocean pollution (shipping and ports) and land-based inorganic pollution layers
# only one ocean pollution raster for both time periods (so only normalized by one time period)
 library(spatial.tools)
 op <- raster(file.path(dir_M, 
    'marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2013_final/normalized_by_one_time_period/ocean_pollution.tif'))
 op_extend <- modify_raster_margins(op, extent_delta=c(1,0,1,0))
 extent(op_extend) = extent(zones)
writeRaster(op_extend, file.path(save_loc_rasts, "ocean_pollution_extend.tif"), overwrite=TRUE)
op <- raster(file.path(save_loc_rasts, "ocean_pollution_extend.tif"))


# two rasters for inorganic pollution (2003-2006 and 2007-2010)
# I used the 2007-2010 raster (normalized by both time periods):
# ip_07_10 <- raster('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2013_final/normalized_by_two_time_periods/inorganic.tif')
# extend(ip_07_10, pest_rast, filename=file.path(rast_locs, "Frazier/inorganic_pollution_07_10_extend.tif"), progress='text')
ip <- raster(file.path(dir_M, 
                       "marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/raw_global_results/Frazier/inorganic_pollution_07_10_extend.tif"))

# get the 2016 (based on FAO 2013 data) land-based organic pollution raster:
raster_2016_loc <- file.path(dir_M, 
                             "marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/step7/output/2013/global_plumes_pest_2013_raw.tif")
raster_2016 <- raster(raster_2016_loc)
raster_2016

## reference point
## scaling coefficient for fertlizer = 1.91788700716876 (file with these values: ohiprep/globalprep/PressuresRegionExtract/land_based_quantiles.csv)
## This was derived using all 99.99th quantile from all years of data in 2015 (fao data from 2007 to 2012)
ref_point <- read.csv("globalprep/reference_points_pressures.csv") %>%
  dplyr::filter(pressure == "Pesticide plume data") %>%
  dplyr::select(ref_point) %>%
  data.frame()
ref_point <- as.numeric(as.character(ref_point$ref_point))


## log raster and then rescale and then make the same standard size (these are always different sizes...very annoying)
calc(raster_2016, function(x){log(x+1)}, progress="text", filename=file.path(save_loc_rasts, "global_plumes_pest_2013_log.tif"), overwrite=TRUE)
tmp <- raster(file.path(save_loc_rasts, "global_plumes_pest_2013_log.tif"))
calc(tmp, fun=function(x){ifelse(x>ref_point, 1, x/ref_point)},
     progress='text',
     filename=file.path(save_loc_rasts, "global_plumes_pest_2013_log_scaled.tif"), overwrite=TRUE)
tmp <- raster(file.path(save_loc_rasts, "global_plumes_pest_2013_log_scaled.tif"))
extend(tmp, zones, filename=file.path(save_loc_rasts, "global_plumes_pest_2013_log_scaled_extend.tif"), progress="text", overwrite=TRUE)
pest <- raster(file.path(save_loc_rasts, "global_plumes_pest_2013_log_scaled_extend.tif"))

##################
## to get the chemical pressure: pesticides + ocean pollution + inorganic pollution

chem_stack <- stack(pest, op, ip)
  calc(chem_stack, 
       sum, na.rm=TRUE,
       progress='text',
       filename=file.path(save_loc_rasts, "chemical_pollution_2013.tif"), overwrite=TRUE)
chem <- raster(file.path(save_loc_rasts, "chemical_pollution_2013.tif"))
plot(chem)
chem
  
calc(chem, fun=function(x){ifelse(x>1, 1, x)},
       progress='text',
       filename=file.path(save_loc_rasts, "chemical_pollution_2013_scaled.tif"), overwrite=TRUE)

chem_scaled <- raster(file.path(save_loc_rasts, "chemical_pollution_2013_scaled.tif"))

# extract data for each eez region:
regions_stats <- zonal(chem_scaled,  zones, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$sp_id) #should be none
setdiff(rgn_data$sp_id, regions_stats2$zone) #should be none

data <- merge(rgn_data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 
write.csv(data, file.path(save_loc_data, "int/chemical_data.csv"), row.names=FALSE)


data <- read.csv(file.path(save_loc_data, "int/chemical_data.csv"))

ccamlr_data <- data %>%
  filter(sp_type == "eez-ccamlr") %>%
  dplyr::select(sp_id, pressure_score=mean)
write.csv(ccamlr_data, file.path(save_loc_data, sprintf('output/aq_chemical_score_2016.csv')), row.names=FALSE)

hs_data <- data %>%
  filter(sp_type == "fao") %>%
  dplyr::select(rgn_id, pressure_score=mean)
write.csv(hs_data, file.path(save_loc_data, sprintf('output/hs_chemical_score_2016.csv')), row.names=FALSE)

eez_data <- data %>%
  filter(sp_type == "eez") %>%
  dplyr::select(rgn_id, pressure_score = mean) %>%
  filter(rgn_id != 213) %>%
  arrange(rgn_id)
write.csv(eez_data, file.path(save_loc_data, sprintf('output/cw_chemical_score_2016.csv')), row.names=FALSE)


## extract at 3 nm (in addition to a pressure, this will be used for CW and the CW trend)
# (going to try using the polygon, rather than converting to raster)
offshore_3nm_poly <- readOGR(dsn = file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/data"), "rgn_offshore3nm_mol")
offshore_3nm_poly <- offshore_3nm_poly[offshore_3nm_poly@data$rgn_type == "eez", ]

# this is here in case I decide to use this method instead of using the polygons to extract the data:
# tmp <- raster(file.path(rast_locs, "Frazier/global_plumes_fert_2007_raw_log_extend.tif"))
# rasterize(inland_3nm_poly, tmp, field='rgn_id', 
#           filename=file.path(rast_locs, "Frazier/inland_3nm.tif"), overwrite=TRUE,
#           progress='text')

data <- raster::extract(chem_scaled, offshore_3nm_poly, na.rm=TRUE, normalizeWeights=FALSE, fun=mean, df=TRUE, progress="text")
data2 <- cbind(data, offshore_3nm_poly@data) 
write.csv(data2, file.path(save_loc_data, "int/chemical_data_offshore_3nm.csv"), row.names=FALSE)

data <- read.csv(file.path(save_loc_data, "int/chemical_data_offshore_3nm.csv")) 

status_data <- data %>%
  filter(rgn_type == "eez") %>%
  filter(rgn_id != 213) %>%
  dplyr::select(rgn_id, pressure_score = chemical_pollution_2013_scaled)
write.csv(status_data, file.path(save_loc_data, "output/cw_chemical_score_3nm_2016.csv"), row.names=FALSE)

### TREND calculations
new_data <- data %>%
  mutate(year = 2013) %>%
  dplyr::select(rgn_id, rgn_name, year, pressure_score=chemical_pollution_2013_scaled) %>%
  filter(!is.na(pressure_score))#NA is Antarctica - this is fine

## get previous years data
old_data <- read.csv("globalprep/prs_chem/v2015/int/chemical_data_offshore_3nm.csv")
old_data <- gather(old_data, "year", "pressure_score", starts_with("chemical"))
old_data <- old_data %>%
  mutate(year = gsub("chemical_pollution_", "", year)) %>%
  mutate(year = gsub("_scaled", "", year)) %>%
  mutate(year = as.numeric(as.character(year))) %>% 
  filter(rgn_type == "eez") %>%   # this doesn't really apply to high seas regions and Antarctica is all zeros
  dplyr::select(rgn_id, rgn_name, year, pressure_score)

trend_data <- rbind(new_data, old_data)
summary(trend_data)

## trend should be calculated on 3nm (not eez)
for(scenario_year in 2012:2016){ #scenario_year=2012
  
  ## NOTE: trends for 2012 are calculated with 3 years of data
  ##      trends for 2013 are calculaed with 4 years of data, the other years are calculated with 5 years data
  trend_years <- (scenario_year-7):(scenario_year-3)
  adj_trend_year <- ifelse(scenario_year %in% 2016:2014, (scenario_year-7), 2007)
  
  trends <- trend_data %>%
    filter(!is.na(pressure_score)) %>%
    group_by(rgn_id) %>%
    do(mdl = lm(pressure_score ~ year, data=., subset=year %in% trend_years),
       adjust_trend = .$pressure_score[.$year == adj_trend_year]) %>%
    summarize(rgn_id, trend = ifelse(coef(mdl)['year']==0, 0, coef(mdl)['year']/adjust_trend * 5)) %>%
    ungroup() %>%
    mutate(trend = ifelse(trend>1, 1, trend)) %>%
    mutate(trend = ifelse(trend<(-1), (-1), trend)) %>%
    mutate(trend = round(trend, 4)) %>%
    dplyr::select(rgn_id, trend) 
  
  write.csv(trends, file.path(save_loc_data, sprintf('output/cw_chemical_trend_%s_new.csv', scenario_year)), row.names=FALSE)
  }

### compare old and new trend data
old <- read.csv('globalprep/prs_chem/v2015/output/cw_chemical_trend_2012.csv') %>%
  dplyr::select(rgn_id, old_trend = trend)
new <- read.csv(file.path(save_loc_data, 'output/cw_chemical_trend_2012_new.csv')) %>%
  left_join(old, by="rgn_id")
new
plot(new$old_trend, new$trend)
abline(0, 1, col="red")
tmp <- filter(trend_data, rgn_id==90) %>%
  arrange(year)


library(googleVis)
plotData <- new_data %>%
  dplyr::select(rgn_name, year, pressure_score)
p <- gvisMotionChart(plotData, idvar = 'rgn_name', timevar="year")
plot(p)

print(p, file=file.path(save_loc_data, 'chemical.html'))
