###############################################
## Combining the most current data for each habitat (2015 analysis)
## Currently, only the sea ice is being updated regularly
## MRF, 5/21/2015
#############################################

source("src/R/common.R")
pathCoral <- "globalprep/hab_coral/v2012/data"
pathMangrove <- "globalprep/hab_mangrove/v2012/data"
pathRockyreef <- "globalprep/hab_rockyreef/v2012/data"
pathSaltmarsh <- "globalprep/hab_saltmarsh/v2012/data"
pathSeagrass <- "globalprep/hab_seagrass/v2012/data"
pathSoftbottom <- "globalprep/hab_soft_bottom/v2012/data"
pathSeaice <- "globalprep/NSIDC_SeaIce/v2015/data"

## Extent data ----
coral_extent <- read.csv(file.path(pathCoral, "habitat_extent_coral.csv"))
mangrove_extent <- read.csv(file.path(pathMangrove, "habitat_extent_mangrove.csv"))
rockyreef_extent <- read.csv(file.path(pathRockyreef, "habitat_extent_rocky_reef.csv"))
saltmarsh_extent <- read.csv(file.path(pathSaltmarsh, "habitat_extent_saltmarsh.csv"))
seagrass_extent <- read.csv(file.path(pathSeagrass, "habitat_extent_seagrass.csv"))
softbottom_extent <- read.csv(file.path(pathSoftbottom, "habitat_extent_softbottom.csv"))
seaice_extent <- read.csv(file.path(pathSeaice, "hab_ice_extent_eez.csv"))

habitat_extent_v2015 <- rbind(coral_extent, mangrove_extent, rockyreef_extent, saltmarsh_extent, seagrass_extent, softbottom_extent, seaice_extent)
table(habitat_extent_v2015$habitat)
write.csv(habitat_extent_v2015, "globalprep/hab_combined/v2015/data/habitat_extent_v2015", row.names=FALSE)
## habitat extent uses the same data for all years of analysis

## Health data ----
# everything stays the same except for sea ice (which is updated every year)
coral_health <- read.csv(file.path(pathCoral, "habitat_health_coral.csv"))
mangrove_health <- read.csv(file.path(pathMangrove, "habitat_health_mangrove.csv"))
saltmarsh_health <- read.csv(file.path(pathSaltmarsh, "habitat_health_saltmarsh.csv"))
seagrass_health <- read.csv(file.path(pathSeagrass, "habitat_health_seagrass.csv"))
softbottom_health <- read.csv(file.path(pathSoftbottom, "habitat_health_softbottom.csv"))

allbutseaice_health <- rbind(coral_health, mangrove_health, saltmarsh_health, seagrass_health, softbottom_health)
table(allbutseaice_health$habitat)


 # health 2015
seaice_health_2015 <- read.csv(file.path(pathSeaice, "hab_ice_health_eez_2014.csv"))
habitat_health_2015 <- rbind(allbutseaice_health, seaice_health_2015)
summary(habitat_health_2015)
write.csv(habitat_health_2015, "globalprep/hab_combined/v2015/data/habitat_health_2015", row.names=FALSE)

# health 2014
seaice_health_2014 <- read.csv(file.path(pathSeaice, "hab_ice_health_eez_2013.csv"))
habitat_health_2014 <- rbind(allbutseaice_health, seaice_health_2014)
summary(habitat_health_2014)
write.csv(habitat_health_2014, "globalprep/hab_combined/v2015/data/habitat_health_2014", row.names=FALSE)

 # health 2013
seaice_health_2013 <- read.csv(file.path(pathSeaice, "hab_ice_health_eez_2012.csv"))
habitat_health_2013 <- rbind(allbutseaice_health, seaice_health_2013)
summary(habitat_health_2013)
write.csv(habitat_health_2013, "globalprep/hab_combined/v2015/data/habitat_health_2013", row.names=FALSE)

 # health 2012
seaice_health_2012 <- read.csv(file.path(pathSeaice, "hab_ice_health_eez_2011.csv"))
habitat_health_2012 <- rbind(allbutseaice_health, seaice_health_2012)
summary(habitat_health_2012)
write.csv(habitat_health_2012, "globalprep/hab_combined/v2015/data/habitat_health_2012", row.names=FALSE)


## Trend data ----
coral_trend <- read.csv(file.path(pathCoral, "habitat_trend_coral.csv"))
mangrove_trend <- read.csv(file.path(pathMangrove, "habitat_trend_mangrove.csv"))
saltmarsh_trend <- read.csv(file.path(pathSaltmarsh, "habitat_trend_saltmarsh.csv"))
seagrass_trend <- read.csv(file.path(pathSeagrass, "habitat_trend_seagrass.csv"))
softbottom_trend <- read.csv(file.path(pathSoftbottom, "habitat_trend_softbottom.csv"))

allbutseaice_trend <- rbind(coral_trend, mangrove_trend, saltmarsh_trend, seagrass_trend, softbottom_trend)
table(allbutseaice_trend$habitat)


# trend 2015
seaice_trend <- function(ScenarioYear, dataYear){
seaice_trend <- read.csv(file.path(pathSeaice, sprintf("hab_ice_trend_eez_%s.csv", dataYear)))
habitat_trend <- rbind(allbutseaice_trend, seaice_trend)
summary(habitat_trend)
write.csv(habitat_trend, sprintf("globalprep/hab_combined/v2015/data/habitat_trend_%s", ScenarioYear), row.names=FALSE)
}

seaice_trend(ScenarioYear=2015, dataYear=2014)
seaice_trend(ScenarioYear=2014, dataYear=2013)
seaice_trend(ScenarioYear=2013, dataYear=2012)
seaice_trend(ScenarioYear=2012, dataYear=2011)
