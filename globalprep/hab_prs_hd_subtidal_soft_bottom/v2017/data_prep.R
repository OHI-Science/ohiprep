######################################
## Prepare rasters to calcuate 
## habitat destruction in subtidal soft-bottom
##
## MRF: 9/19/2016
######################################

## this is only relevant to eez regions (not high seas and not Antarctica)


source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(sp)
library(dplyr)
library(stringr)
setwd("/home/frazier/ohiprep")

##################################################################################################
### Habitat: subtidal and shelf
##################################################################################################

# The subtidal and shelf data have not changed from the 2016 assessment.
# Code for extracting these data found: ohiprep/globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/data_prep.R

subtidal <- read.csv("globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output/habitat_extent_softbottom.csv")

######################################################
## Extract demersal destructive fishing by region
#######################################################

### convert density of fish (tonnes/km2) to catch total (tonnes)

fish_crs <- raster(file.path(dir_M, 
                             "git-annex/impact_acceleration/stressors/comm_fish/int/catch_annual_rasters/dem_dest/dem_dest_2003.tif"))

raster_area <- area(fish_crs)

catch_rast_loc <- file.path(dir_M, "git-annex/impact_acceleration/stressors/comm_fish/int/catch_annual_rasters/dem_dest")

for(catch_year in 2003:2014){ # catch_year = 2003
  
  cat(catch_year)
  
  catch <- raster(file.path(catch_rast_loc, sprintf("dem_dest_%s.tif", catch_year)))
  
  catch_count  <- overlay(catch, raster_area, fun = function(x,y){x*y}, progress = 'text',
                           filename = file.path(dir_M,
                          sprintf("git-annex/globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/int/catch_count_%s.tif", catch_year))) 
}


#######################################
## extract count data for each OHI region
#######################################

### make crs the same for ohi region data

ohi_regions <- readOGR(dsn = file.path(dir_M, "git-annex/globalprep/spatial/v2017"), layer="regions_2017_update")  
ohi_regions_wgs <- spTransform(ohi_regions, proj4string(fish_crs))


catch_count_rasts <- list.files(file.path(dir_M, "git-annex/globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/int"), 
                             full.names = TRUE) 

for(catch_count in catch_count_rasts){ # catch_count = catch_count_rasts[1]

cat(catch_count)
catch_year <- str_sub(catch_count,-8,-5)
  
catch <- raster(catch_count)

data        <- raster::extract(catch, ohi_regions, weights = TRUE, normalizeWeights = FALSE, progress = 'text') 
names(data) <- paste(ohi_regions@data$type_w_ant, ohi_regions@data$rgn_ant_id, sep="_") 
catch_dem            <- plyr::ldply(data, rbind)

# the following code keeps the raster information when the raster lands partially on the land polygons
catch_dem <- catch_dem %>%
  tidyr::separate(.id, c("rgn_type", "rgn_id"), sep="_") %>%
  dplyr::mutate(tonnes = value*weight) %>%
  dplyr::group_by(rgn_type, rgn_id) %>%
  dplyr::summarize(tonnes = sum(tonnes, na.rm=TRUE)) %>%
  dplyr::group_by(rgn_id) %>%
  dplyr::summarize(tonnes = sum(tonnes, na.rm=TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(as.numeric(as.character(rgn_id))) %>%
  dplyr::mutate(year = catch_year)

write.csv(catch_dem, sprintf("globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/int/dem_catch_%s.csv", catch_year),
          row.names=FALSE)
}


############################################
## Calculate softbottom habitat destruction
############################################

files <- list.files("globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/int", pattern = "dem_catch", full.names = TRUE) 

dem_catch_all <- data.frame()

for(file in files) { # file = "dem_catch_2003.csv"
  tmp <- read.csv(file)
  dem_catch_all <- rbind(dem_catch_all, tmp)
}

################################
### compare to old
### new data, and units are probably different
files_old <- list.files("globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/int", pattern = "dem_catch", full.names = TRUE) 

dem_catch_all_old <- data.frame()

for(file in files_old) { # file = "dem_catch_2003.csv"
  tmp <- read.csv(file)
  dem_catch_all_old <- rbind(dem_catch_all_old, tmp)
}
dem_catch_all_old <- dem_catch_all_old %>%
  rename(tonnes_old = tonnes)


compare <- left_join(dem_catch_all_old, dem_catch_all, by=c('year', 'rgn_id'))
plot(log(compare$tonnes_old), log(compare$tonnes))
abline(0, 1, col="red")
plot(compare$tonnes_old, compare$tonnes)
abline(0, 1, col="red")
###################################


area <- read.csv("globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output/habitat_extent_softbottom.csv")

data_density <- dem_catch_all %>%
  left_join(area, by="rgn_id") %>%
  filter(rgn_id <= 250) %>%
  mutate(density = tonnes/km2)

## find max density across all years
max <- data_density %>%
  group_by(year) %>%
  summarize(maxDensity = max(density, na.rm=TRUE)) %>%
  data.frame()

write.csv(max, 
          "globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/int/reference_point_max.csv", 
          row.names=FALSE)

ref_point <- read.csv("globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/int/reference_point_max.csv")
ref_point_max <- max(ref_point$maxDensity) ## 2939.976

## rescale the density: 
## density is rescaled twice to reduce skew

data_rescaled <- data_density %>%
  mutate(density_rescaled_max = log(density + 1)/log(ref_point_max + 1)) %>% #pressure-type measure
  mutate(inv_dens_rescaled_max = 1 - density_rescaled_max)  # health-type measure

## Find the second rescaling point
ref_median <- data_rescaled %>%
  group_by(year) %>%
  summarize(ref_median = median(inv_dens_rescaled_max, na.rm=TRUE)) %>%
  data.frame()

write.csv(ref_median, 
          "globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/int/reference_point_median.csv", 
          row.names=FALSE)

ref_point_median <- min(ref_median$ref_median) # 0.8999136

## Rescale for second time:  
  data_rescaled_2 <- data_rescaled %>%
    mutate(density_rescaled_median = inv_dens_rescaled_max/ref_point_median) %>%
    mutate(density_rescaled_median_capped = ifelse(density_rescaled_median > 1, 
                                                   1, 
                                                   density_rescaled_median))
  
  hist(data_rescaled_2$density_rescaled_median_capped[data_rescaled_2$year==2010])
  filter(data_rescaled_2, rgn_id==163)
  filter(data_rescaled_2, year==2010)
  

  ## check against old data
old <- read.csv("globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output/habitat_health_softbottom_v2010.csv") %>%
  dplyr::select(rgn_id, old_health=health)
test <- data_rescaled_2 %>%
  filter(year==2010) %>%
  dplyr::select(rgn_id, health = density_rescaled_median_capped) %>%
  left_join(old)
plot(health ~old_health, data=test)
## Not a great correlation, but different years data and fisheries data is very different
## some new NA values, but it doesn't seem like these have soft-bottom habitat
## (at least according to our raster data)

### Get relevant data

save_dir <- "globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/output"

condition_pressure <- data_rescaled_2 %>%
  mutate(habitat = "soft_bottom") %>%
  mutate(pressure = 1 - density_rescaled_median_capped) %>%
  dplyr::select(rgn_id, year, habitat, health=density_rescaled_median_capped, pressure) %>%
  filter(!is.na(pressure))

# get habitat trends
stop_year <- max(condition_pressure$year)

trend <- data.frame()

for (status_year in (stop_year-4):stop_year){ #status_year = 2010
  trend_years <- status_year:(status_year - 4)
  first_trend_year <- min(trend_years)
  
  trend_new <- condition_pressure %>%
    filter(year %in% trend_years) %>%
    group_by(rgn_id) %>%
    do(mdl = lm(health ~ year, data=.),
       adjust_trend = .$health[.$year == first_trend_year]) %>%
    summarize(rgn_id = rgn_id,
              trend = round(coef(mdl)['year']/adjust_trend * 5, 4)) %>%
    ungroup() %>%
    mutate(trend = ifelse(trend > 1, 1, trend)) %>%
    mutate(trend = ifelse(trend < (-1), (-1), trend))
  
  trend_new <- trend_new %>%
    mutate(habitat = "soft_bottom") %>%
    mutate(year = status_year) %>%
    select(rgn_id, year, habitat, trend)
  
  trend <- rbind(trend, trend_new)
}
  
write.csv(trend, file.path(save_dir, "habitat_trend_softbottom.csv"), row.names=FALSE)
 
  health <- condition_pressure %>%
    mutate(habitat = "soft_bottom") %>%
    select(rgn_id, year, habitat, health)
  write.csv(health, file.path(save_dir, "habitat_health_softbottom.csv"), row.names=FALSE)
  
  pressure <- condition_pressure %>%
    select(rgn_id, year, pressure_score = pressure)
  write.csv(pressure, file.path(save_dir, "hd_sb_subtidal.csv"), row.names=FALSE)
  

