#####################################
## calculate pathogen pressure scores
#####################################

library(dplyr)

## calculate status
data <- read.csv("Antarctica/AQ_CW_pressure_pathogen/intermediate/pathogen_data.csv") %>%
  mutate(station_score = population * treatment) %>%
  group_by(sp_id) %>%
  summarize(pressure_score = 1- sum(station_score)/sum(population*3))

regions <- read.csv("Antarctica/Other/ccamlr2sp_id.csv") %>%
  left_join(data) %>%
  mutate(pressure_score = ifelse(is.na(pressure_score), 0, pressure_score)) %>%
  select(sp_id, pressure_score)

write.csv(regions, "Antarctica/AQ_CW_pressure_pathogen/data/po_pathogens.csv", row.names=FALSE)

## calculate trends
data <- read.csv("Antarctica/AQ_CW_pressure_pathogen/intermediate/pathogen_data.csv") %>%
  mutate(station_score = population * treatment) %>%
  group_by(sp_id, year) %>%
  summarize(pressure_score = 1- sum(station_score)/sum(population*3))

## Currently, no stations have been built in the past 5 years along the coast.  
## Will assume a trend of zero. If this changes, trend will actually need to be calculated.
if(!is.na(sum(data$year>(2015-4)))){
  stop("Will need to calculate trend, previously assumed to be zero")
}

trend <- regions %>%
  mutate(pressure_score = 0) %>%
  select(sp_id, trend=pressure_score)

write.csv(trend, "Antarctica/AQ_CW_pressure_pathogen/data/cw_pathogen_trend.csv", row.names=FALSE)
