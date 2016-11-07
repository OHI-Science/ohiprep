###############################################
## Combining the most current data for each habitat (2016 analysis)
## Currently, only the sea ice is updated regularly
#############################################

source("src/R/common.R")
pathCoral <- "globalprep/hab_coral/v2012/data"
pathMangrove <- "globalprep/hab_mangrove/v2015/data"
pathRockyreef <- "globalprep/hab_rockyreef/v2012/data"
pathSaltmarsh <- "globalprep/hab_saltmarsh/v2012/data"
pathSeagrass <- "globalprep/hab_seagrass/v2012/data"
pathSoftbottom <- "globalprep/hab_soft_bottom/v2012/data"
pathSeaice <- "globalprep/hab_seaice/v2016/output"

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
write.csv(habitat_extent_v2015, "globalprep/hab_combined/v2016/output/habitat_extent_v2015.csv", row.names=FALSE)
## habitat extent uses the same data for all years of analysis

## Health data ----
# everything stays the same except for sea ice (which is updated every year)
coral_health <- read.csv(file.path(pathCoral, "habitat_health_coral.csv"))
mangrove_health <- read.csv(file.path(pathMangrove, "habitat_health_mangrove.csv")) %>%
  select(rgn_id, habitat, health)
saltmarsh_health <- read.csv(file.path(pathSaltmarsh, "habitat_health_saltmarsh.csv"))
seagrass_health <- read.csv(file.path(pathSeagrass, "habitat_health_seagrass.csv"))
softbottom_health <- read.csv(file.path(pathSoftbottom, "habitat_health_softbottom.csv"))


allbutseaice_health <- rbind(coral_health, mangrove_health, saltmarsh_health, seagrass_health, softbottom_health)
table(allbutseaice_health$habitat)


# health function
habitat_summary <- function(ScenarioYear, dataYear_seaice){
  seaice_trend <- read.csv(file.path(pathSeaice, sprintf("hab_ice_health_eez_%s.csv", dataYear_seaice)))
  habitat_trend <- rbind(allbutseaice_health, seaice_trend)
  write.csv(habitat_trend, sprintf("globalprep/hab_combined/v2016/output/habitat_health_%s.csv", ScenarioYear), row.names=FALSE)
}

habitat_summary(ScenarioYear=2016, dataYear_seaice=2015)
habitat_summary(ScenarioYear=2015, dataYear_seaice=2014)
habitat_summary(ScenarioYear=2014, dataYear_seaice=2013)
habitat_summary(ScenarioYear=2013, dataYear_seaice=2012)
habitat_summary(ScenarioYear=2012, dataYear_seaice=2011)


## Trend data ----
coral_trend <- read.csv(file.path(pathCoral, "habitat_trend_coral.csv"))
saltmarsh_trend <- read.csv(file.path(pathSaltmarsh, "habitat_trend_saltmarsh.csv"))
seagrass_trend <- read.csv(file.path(pathSeagrass, "habitat_trend_seagrass.csv"))
softbottom_trend <- read.csv(file.path(pathSoftbottom, "habitat_trend_softbottom.csv"))

allbutseaice_trend <- rbind(coral_trend, saltmarsh_trend, seagrass_trend, softbottom_trend)
table(allbutseaice_trend$habitat)


# trend 2015
trend_summary <- function(ScenarioYear, dataYear_seaice, dataYear_mangrove){
seaice_trend <- read.csv(file.path(pathSeaice, sprintf("hab_ice_trend_eez_%s.csv", dataYear_seaice)))
mangrove_trend <- read.csv(file.path(pathMangrove, sprintf('habitat_trend_mangrove_v%s.csv', dataYear_mangrove)))
habitat_trend <- rbind(allbutseaice_trend, seaice_trend, mangrove_trend)
summary(habitat_trend)
write.csv(habitat_trend, sprintf("globalprep/hab_combined/v2016/output/habitat_trend_%s.csv", ScenarioYear), row.names=FALSE)
}

trend_summary(ScenarioYear=2016, dataYear_seaice=2015, dataYear_mangrove=2015)
trend_summary(ScenarioYear=2015, dataYear_seaice=2014, dataYear_mangrove=2015)
trend_summary(ScenarioYear=2014, dataYear_seaice=2013, dataYear_mangrove=2014)
trend_summary(ScenarioYear=2013, dataYear_seaice=2012, dataYear_mangrove=2013)
trend_summary(ScenarioYear=2012, dataYear_seaice=2011, dataYear_mangrove=2012)
