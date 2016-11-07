#####################################
## calculate pathogen pressure scores
#####################################

library(dplyr)

## convert treatment types to pressure scores

treat_pressure <- data.frame(treatment = c(0, 1, 2, 3),
                             pressure = c(1, 0.7, 0.4, 0.1))

### NOTE: the "year" variable refers to the year that the station was created....not the yearly pollution
## calculate status
data <- read.csv("Antarctica/AQ_CW_pressure_pathogen/intermediate/pathogen_data.csv") %>%
  left_join(treat_pressure, by='treatment') %>%
  mutate(station_score = population * pressure) %>%
  group_by(sp_id) %>%
  summarize(pressure_rgn = sum(station_score)) %>%
  ungroup()

# standardize by area:
area <- read.csv("Antarctica/AQ_CW_pressure_pathogen/intermediate/area_3nm_offshore.csv")

data <- data %>%
  left_join(area, by="sp_id") %>%
  mutate(pressure_unstand = pressure_rgn/area_km2) %>%
  mutate(pressure_score = pressure_unstand/(max(pressure_unstand) *1.1)) %>%
  select(sp_id, pressure_score)

# fill in missing regions
regions <- read.csv("Antarctica/Other/ccamlr2sp_id.csv") %>%
  left_join(data, by="sp_id") %>%
  mutate(pressure_score = ifelse(is.na(pressure_score), 0, pressure_score)) %>%
  select(sp_id, pressure_score)

write.csv(regions, "Antarctica/AQ_CW_pressure_pathogen/data/po_pathogens.csv", row.names=FALSE)

## calculate trends
## Currently, no stations have been built in the past 5 years along the coast.  
## Will assume a trend of zero. If this changes, trend will actually need to be calculated.
if(!is.na(sum(data$year>(2015-4)))){
  stop("Will need to calculate trend, previously assumed to be zero")
}

trend <- regions %>%
  mutate(pressure_score = 0) %>%
  select(sp_id, trend=pressure_score)

write.csv(trend, "Antarctica/AQ_CW_pressure_pathogen/data/cw_pathogen_trend.csv", row.names=FALSE)
