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

##################################################################################################
### Prepare the habitat raster data by combining subtidal and shelf
##################################################################################################

subtidal <- raster(file.path(dir_M, "marine_threats/data/completed/ecosystems/sub_tidal_soft_bottom/tif/s_t_s_bottom.tif"))
shelf <- raster(file.path(dir_M, "marine_threats/data/completed/ecosystems/soft_shelf/tif/soft_shelf.tif"))

regions <- readOGR(dsn=file.path(dir_M, "git-annex/globalprep/spatial/d2014/data"), layer="regions_mol")
# plot(subtidal)
# plot(regions, add=TRUE)
projection(subtidal) <- CRS(proj4string(regions))
projection(shelf) <- CRS(proj4string(regions))

## combine the two soft-bottom habitat layers:
sb_stack <- stack(subtidal, shelf)
overlay(sb_stack, 
        fun=function(x, y){x+y}, 
        progress='text',
        filename=file.path(dir_M, "git-annex/globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/int/all_soft_bottom.tif"), overwrite=TRUE)

sb <- raster(file.path(dir_M, "git-annex/globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/int/all_soft_bottom.tif"))

## convert 0 values to NA
fun=function(x){ifelse(x==0, NA, 1)}
calc(sb, 
     fun, 
     progress='text',
     filename=file.path(dir_M, "git-annex/globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/int/all_soft_bottom_NAs.tif"), overwrite=TRUE)

#######################################
### extract habitat area data
#######################################

subtidal <- raster(file.path(dir_M, "git-annex/globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/int/all_soft_bottom_NAs.tif"))

zones <- raster(file.path(dir_M, "git-annex/globalprep/spatial/d2014/data/rgn_mol_raster_1km/sp_mol_raster_1km.tif"))  # raster data
rgn_data <- read.csv(file.path(dir_M, "git-annex/globalprep/spatial/d2014/data/rgn_mol_raster_1km/regionData.csv"))    # data for sp_id's used in raster

# save location
save_loc <- "globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output"

regions_stats <- zonal(subtidal,  zones, fun="sum", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, rgn_data$ant_id) 
setdiff(rgn_data$ant_id, regions_stats2$zone) 

extent <- regions_stats2 %>%
  mutate(km2 = sum*(.9344789^2)) %>%
  dplyr::select(rgn_id=zone, km2) %>%
  filter(rgn_id <= 250) %>%
  filter(km2 > 0)
write.csv(extent, file.path(save_loc, "habitat_extent_softbottom.csv"), row.names=FALSE)

#forgot to add habitat variable...adding now:
extent <- read.csv(file.path(save_loc, "habitat_extent_softbottom.csv")) %>%
  mutate(habitat = "soft_bottom") %>%
  select(rgn_id, habitat, km2)
write.csv(extent, file.path(save_loc, "habitat_extent_softbottom.csv"), row.names=FALSE)

# compare to old values to make sure something didn't go horribly wrong:
old <- read.csv("globalprep/hab_soft_bottom/v2012/data/habitat_extent_softbottom.csv") %>%
  dplyr::select(rgn_id, km2_old = km2)

extent_compare <- extent %>%
  left_join(old)
plot(km2 ~ km2_old, data=extent_compare)
abline(0,1, col="red")
plot(log(km2) ~ log(km2_old), data=extent_compare)
abline(0,1, col="red")
# looks good

######################################################
## Extract demersal destructive fishing by region
#######################################################

ohi_regions <- readOGR(dsn = file.path(dir_M, "git-annex/globalprep/spatial/d2014/data"), layer="regions_gcs")  

catch_rast_loc <- "../impact_acceleration/stressors/comm_fish/annual_catch"
setwd("/home/frazier/ohiprep")

for(catch_year in 2003:2010){ # catch_year = 2003

cat(catch_year)

catch <- raster(file.path(catch_rast_loc, sprintf("annual_catch_dem_d_%s.tif", catch_year)))

data        <- raster::extract(catch, ohi_regions, weights = TRUE, normalizeWeights = FALSE, progress = 'text') 
names(data) <- paste(ohi_regions@data$ant_typ, ohi_regions@data$ant_id, sep="_") 
catch_dem            <- plyr::ldply(data, rbind)

catch_dem <- catch_dem %>%
  separate(.id, c("rgn_type", "rgn_id"), sep="_") %>%
  mutate(tonnes = value*weight) %>%
  group_by(rgn_type, rgn_id) %>%
  summarize(tonnes = sum(tonnes, na.rm=TRUE)) %>%
  group_by(rgn_id) %>%
  summarize(tonnes = sum(tonnes, na.rm=TRUE)) %>%
  ungroup() %>%
  arrange(as.numeric(as.character(rgn_id))) %>%
  mutate(year = catch_year)

write.csv(catch_dem, sprintf("globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/int/dem_catch_%s.csv", catch_year),
          row.names=FALSE)
}


############################################
## Calculate softbottom habitat destruction
############################################

files <- sprintf("dem_catch_%s.csv", 2003:2010)

dem_catch_all <- data.frame()

for(file in files) { # file = "dem_catch_2003.csv"
  tmp <- read.csv(file.path("globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/int",
                  file))
  dem_catch_all <- rbind(dem_catch_all, tmp)
}


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
          "globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/int/reference_point_max.csv", 
          row.names=FALSE)

ref_point <- read.csv("globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/int/reference_point_max.csv")
ref_point_max <- max(ref_point$maxDensity) ## 1159.452

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
          "globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/int/reference_point_median.csv", 
          row.names=FALSE)

ref_point_median <- min(ref_median$ref_median) # 0.9359877

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
old <- read.csv("globalprep/hab_soft_bottom/v2012/data/habitat_health_softbottom.csv") %>%
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
condition_pressure <- data_rescaled_2 %>%
  mutate(habitat = "soft_bottom") %>%
  mutate(pressure = 1 - density_rescaled_median_capped) %>%
  dplyr::select(rgn_id, year, habitat, health=density_rescaled_median_capped, pressure) %>%
  filter(!is.na(pressure))

save_dir <- "globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output"
stop_year <- max(condition_pressure$year)

for (status_year in (stop_year-4):stop_year){ #status_year = 2010
  trend_years <- status_year:(status_year - 4)
  first_trend_year <- min(trend_years)
  
  trend_data <- condition_pressure[condition_pressure$year %in% trend_years, ]
  
  trend <- trend_data %>%
    group_by(rgn_id) %>%
    do(mdl = lm(health ~ year, data=.),
       adjust_trend = .$health[.$year == first_trend_year]) %>%
    summarize(rgn_id = rgn_id,
              trend = round(coef(mdl)['year']/adjust_trend * 5, 4)) %>%
    ungroup() %>%
    mutate(trend = ifelse(trend > 1, 1, trend)) %>%
    mutate(trend = ifelse(trend < (-1), (-1), trend))
  
  trend <- trend %>%
    mutate(habitat = "soft_bottom") %>%
    select(rgn_id, habitat, trend)
  write.csv(trend, file.path(save_dir, sprintf("habitat_trend_softbottom_v%s.csv", status_year)), row.names=FALSE)
 
  health <- condition_pressure[condition_pressure$year %in% status_year, ] %>%
    mutate(habitat = "soft_bottom") %>%
    select(rgn_id, habitat, health)
  write.csv(health, file.path(save_dir, sprintf("habitat_health_softbottom_v%s.csv", status_year)), row.names=FALSE)
  
  pressure <- condition_pressure[condition_pressure$year %in% status_year, ] %>%
    select(rgn_id, pressure_score = pressure)
  write.csv(pressure, file.path(save_dir, sprintf("hd_sb_subtidal_v%s.csv", status_year)), row.names=FALSE)
  
}

