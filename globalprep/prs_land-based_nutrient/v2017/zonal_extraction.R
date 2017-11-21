#####################################################
## Exctracting nutrient pressure data for OHI 2017
## MRF: Found additional years of data for this variable
## (older data), which will improve trend estimates for
## earlier assessment years
#####################################################

### Check:  Why are these slightly different than the files located here:
### D:\marine_threats\impact_layers_2013_redo\impact_layers\work\land_based\before_2007\step7\output
### Need to figure this out before moving forward and using these files
### The advantage to these files is that more years are included.
### Will not update with these data for OHI 2017 (continue using data in v2016 for now)


source('src/R/common.R')
source('src/R/spatial_common.R')

library(raster)
library(rgdal)
library(dplyr)
library(stringr)

library(parallel)
library(foreach)
library(doParallel)



# save location
save_loc_rasts <- file.path(dir_M, "git-annex/globalprep/prs_land-based_nutrient/v2017/int")
save_loc_data <- "globalprep/prs_land-based_nutrient/v2017"


rast_loc <- file.path(dir_M, 
                       "marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/raw_global_results")

files <- list.files(rast_loc, pattern = "fert", full=TRUE)


## mask ocean and extend raster to make consistent with standard OHI global region file
## considered the 'raw' data
registerDoParallel(10)

foreach(file = files)%dopar% { #file = files[1]
  
  year <- str_sub(file,-12,-9)
  
  raster(file) %>%
    raster::extend(ocean) %>%
    mask(ocean, 
         filename = file.path(dir_M, 
                              sprintf("git-annex/globalprep/prs_land-based_nutrient/v2017/int/%s", basename(file))),
         overwrite = TRUE, progress = "text")
}


### log the data

files <- list.files(file.path(dir_M, "git-annex/globalprep/prs_land-based_nutrient/v2017/int"), 
         pattern = "raw", full=TRUE)

foreach(file = files) %dopar% { #file = files[9]
  name <- basename(file)
  name <- sub('\\.tif$', '', name)
  name <- gsub("_raw", "", name)
  tmp <- raster(file)
  calc(tmp, function(x){log(x+1)}, 
       filename = file.path(dir_M, 
                            sprintf("git-annex/globalprep/prs_land-based_nutrient/v2017/int/%s_log.tif", name)), 
       overwrite=TRUE)
}

### Collect quantile data

files <- list.files(file.path(dir_M, 'git-annex/globalprep/prs_land-based_nutrient/v2017/int'), 
                    full.names = TRUE, pattern = "log.tif")

quantiles <- data.frame(plumeData = basename(files), quantile_9999_ln=NA)

for(file in files) { #file = files[9]
  
  tmp <- raster(file)
  quantiles$quantile_9999_ln[quantiles$plumeData == basename(file)] <- quantile(tmp, .9999)
  
}

write.csv(quantiles, 
          "globalprep/prs_land-based_nutrient/v2017/int/quantiles.csv",
          row.names = FALSE)

## reference point
## scaling coefficient for fertlizer = 5.594088 (file with these values: ohiprep/globalprep/PressuresRegionExtract/land_based_quantiles.csv)
## This was derived using all 99.99th quantile from all years of data for 2015 OHI assessment (fao data from 2002 to 2012)

ref_point <- read.csv("globalprep/supplementary_information/v2016/reference_points_pressures.csv") %>%
  dplyr::filter(pressure == "Fertilizer plume data") %>%
  dplyr::select(ref_point) %>%
  data.frame()
ref_point <- as.numeric(as.character(ref_point$ref_point))


files <- list.files(file.path(dir_M, 'git-annex/globalprep/prs_land-based_nutrient/v2017/int'), 
                    full.names = TRUE, pattern = "log.tif")

for (file in files) { #file = files[9]
  year <- str_sub(file,-12,-9)
  
  tmp <- raster(file)
  
  calc(tmp, fun=function(x){ifelse(x>ref_point_fert, 1, x/ref_point_fert)},
       filename = file.path(dir_M, 
  sprintf("git-annex/globalprep/prs_land-based_nutrient/v2017/int/nutrient_%s_rescaled_mol.tif", year)), 
       overwrite=TRUE)
}


files <- list.files(file.path(dir_M, "git-annex/globalprep/prs_land-based_nutrient/v2017/int"), pattern = "rescaled", 
           full=TRUE)

tmp <- stack(files[1])

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
write.csv(eez, file.path(save_loc_data, "output/cw_fertilizers_score_2016.csv"), row.names=FALSE)
  

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
old_data <- read.csv("globalprep/prs_land-based_nutrient/v2015/int/nutrients_plume_data_offshore_3nm.csv")
old_data <- gather(old_data, "year", "pressure_score", starts_with("global"))
old_data <- old_data %>%
  mutate(year = gsub("global_plumes_fert_", "", year)) %>%
  mutate(year = gsub("_raw_log_extend_scaled", "", year)) %>%
  mutate(year = as.numeric(as.character(year))) %>% 
  filter(rgn_type == "eez") %>%   # this doesn't really apply to high seas regions and Antarctica is all zeros
  dplyr::select(rgn_id, rgn_name, year, pressure_score)

trend_data <- rbind(data, old_data)

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
  
  write.csv(trends, file.path(save_loc_data, sprintf('output/cw_fertilizers_trend_%s_new.csv', scenario_year)), row.names=FALSE)
}
  ## compare new and old trend values:
  old <- read.csv('globalprep/prs_land-based_nutrient/v2015/output/cw_fertilizers_trend_2012.csv') %>%
    dplyr::select(rgn_id, old_trend = trend)
new <- read.csv(file.path(save_loc_data, 'output/cw_fertilizers_trend_2012_new.csv')) %>%
  left_join(old, by="rgn_id")
new
plot(new$old_trend, new$trend)
abline(0, 1, col="red")
tmp <- filter(trend_data, rgn_id==219) %>%
  arrange(year)
  
  library(googleVis)
  plotData <- data %>%
    dplyr::select(rgn_name, year, pressure_score)
p <- gvisMotionChart(plotData, idvar = 'rgn_name', timevar="year")
    plot(p)
    
    print(p, file=file.path(save_loc_data, 'fertilizer.html'))
    