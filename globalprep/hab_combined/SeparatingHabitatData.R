###############################################
## Separating the Habitat data so it
## can be updated more easily in the future
## MRF, 5/21/2015
## This is only for archive purposes - we should never need to do this again!
#############################################

source("src/R/common.R")
pathCoral <- "globalprep/hab_coral/v2012/data"
pathMangrove <- "globalprep/hab_mangrove/v2012/data"
pathRockyreef <- "globalprep/hab_rockyreef/v2012/data"
pathSaltmarsh <- "globalprep/hab_saltmarsh/v2012/data"
pathSeagrass <- "globalprep/hab_seagrass/v2012/data"
pathSoftbottom <- "globalprep/hab_soft_bottom/v2012/data"


## Extent data ----
extent2013 <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-Habitats_v2013a/data/hab_habitat_extent_NewSeaIce.csv")) %>%
  select(rgn_id, habitat, km2)
table(extent2013$habitat)

extent2012 <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-Habitats_v2013a/data/hab_habitat_extent_NewSeaIce_2012.csv")) %>%
  select(rgn_id, habitat, km2_2012=km2)

compareExtent <- left_join(extent2013, extent2012) %>%
  filter(km2_2012 != km2)
compareExtent
### only difference is the sea ice.  In future analyses the extent will be based on the reference period and will stay constant across years
### Also, these data (except sea ice) are all based on the original 2012 analysis.
### I will split these data by habitat and save to the appropriate folder (except sea ice which will probably be analyzed every year).  

extent2013_coral <- extent2013 %>%
  filter(habitat == "coral")
summary(extent2013_coral)
write.csv(extent2013_coral, file.path(pathCoral, "habitat_extent_coral.csv"), row.names=FALSE)

extent2013_mangrove <- extent2013 %>%
  filter(habitat %in% c("mangrove", "mangrove_inland1km", "mangrove_offshore1km"))
summary(extent2013_mangrove)
write.csv(extent2013_mangrove, file.path(pathMangrove, "habitat_extent_mangrove.csv"), row.names=FALSE)

extent2013_rocky_reef <- extent2013 %>%
  filter(habitat == "rocky_reef")
summary(extent2013_rocky_reef)
write.csv(extent2013_rocky_reef, file.path(pathRockyreef, "habitat_extent_rocky_reef.csv"), row.names=FALSE)

extent2013_saltmarsh <- extent2013 %>%
  filter(habitat == "saltmarsh")
summary(extent2013_saltmarsh)
write.csv(extent2013_saltmarsh, file.path(pathSaltmarsh, "habitat_extent_saltmarsh.csv"), row.names=FALSE)

extent2013_seagrass <- extent2013 %>%
  filter(habitat == "seagrass")
summary(extent2013_seagrass)
write.csv(extent2013_seagrass, file.path(pathSeagrass, "habitat_extent_seagrass.csv"), row.names=FALSE)

extent2013_softbottom <- extent2013 %>%
  filter(habitat == "soft_bottom")
summary(extent2013_softbottom)
write.csv(extent2013_softbottom, file.path(pathSoftbottom, "habitat_extent_softbottom.csv"), row.names=FALSE)


## Health data ----
health2013 <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-Habitats_v2013a/data/hab_habitat_health.csv")) %>%
  select(rgn_id, habitat, health)
table(health2013$habitat)

health2012 <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-Habitats_v2013a/data/hab_habitat_health_2012.csv")) %>%
  select(rgn_id, habitat, health_2012=health)

compareHealth <- left_join(health2013, health2012) %>%
  filter(health_2012 != health)
compareHealth
### only difference is the sea ice.  
### These data (except sea ice) are based on the original 2012 analysis.
### I will split these data by habitat and save to the appropriate folder (except sea ice which will probably be analyzed every year).  

health2013_coral <- health2013 %>%
  filter(habitat == "coral")
summary(health2013_coral)
write.csv(health2013_coral, file.path(pathCoral, "habitat_health_coral.csv"), row.names=FALSE)

health2013_mangrove <- health2013 %>%
  filter(habitat == "mangrove")
summary(health2013_mangrove)
write.csv(health2013_mangrove, file.path(pathMangrove, "habitat_health_mangrove.csv"), row.names=FALSE)

health2013_saltmarsh <- health2013 %>%
  filter(habitat == "saltmarsh")
summary(health2013_saltmarsh)
write.csv(health2013_saltmarsh, file.path(pathSaltmarsh, "habitat_health_saltmarsh.csv"), row.names=FALSE)

health2013_seagrass <- health2013 %>%
  filter(habitat == "seagrass")
summary(health2013_seagrass)
write.csv(health2013_seagrass, file.path(pathSeagrass, "habitat_health_seagrass.csv"), row.names=FALSE)

health2013_softbottom <- health2013 %>%
  filter(habitat == "soft_bottom")
summary(health2013_softbottom)
write.csv(health2013_softbottom, file.path(pathSoftbottom, "habitat_health_softbottom.csv"), row.names=FALSE)


## Trend data ----
trend2013 <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-Habitats_v2013a/data/hab_habitat_trend.csv")) %>%
  select(rgn_id, habitat, trend)
table(trend2013$habitat)

trend2012 <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-Habitats_v2013a/data/hab_habitat_trend_2012.csv")) %>%
  select(rgn_id, habitat, trend_2012=trend)

compareTrend <- left_join(trend2013, trend2012) %>%
  filter(trend_2012 != trend)
compareTrend

### only difference is the sea ice.  
### These data (except sea ice) are based on the original 2012 analysis.
### I will split these data by habitat and save to the appropriate folder (except sea ice which will probably be analyzed every year).  

trend2013_coral <- trend2013 %>%
  filter(habitat == "coral")
summary(trend2013_coral)
write.csv(trend2013_coral, file.path(pathCoral, "habitat_trend_coral.csv"), row.names=FALSE)

trend2013_mangrove <- trend2013 %>%
  filter(habitat == "mangrove")
summary(trend2013_mangrove)
write.csv(trend2013_mangrove, file.path(pathMangrove, "habitat_trend_mangrove.csv"), row.names=FALSE)

trend2013_saltmarsh <- trend2013 %>%
  filter(habitat == "saltmarsh")
summary(trend2013_saltmarsh)
write.csv(trend2013_saltmarsh, file.path(pathSaltmarsh, "habitat_trend_saltmarsh.csv"), row.names=FALSE)

trend2013_seagrass <- trend2013 %>%
  filter(habitat == "seagrass")
summary(trend2013_seagrass)
write.csv(trend2013_seagrass, file.path(pathSeagrass, "habitat_trend_seagrass.csv"), row.names=FALSE)

trend2013_softbottom <- trend2013 %>%
  filter(habitat == "soft_bottom")
summary(trend2013_softbottom)
write.csv(trend2013_softbottom, file.path(pathSoftbottom, "habitat_trend_softbottom.csv"), row.names=FALSE)
