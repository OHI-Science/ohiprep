#### 2017 update for new ohi-global framework

library(dplyr)

#extent:
extent <- read.csv("globalprep/hab_mangrove/v2015/data/habitat_extent_mangrove.csv")
extent <- extent %>%
  mutate(year = 2015) %>%
  select(rgn_id, habitat, year, km2)

write.csv(extent, "globalprep/hab_mangrove/v2015/data/habitat_extent_mangrove_updated.csv", row.names=FALSE)

#health:
health <- read.csv("globalprep/hab_mangrove/v2015/data/habitat_health_mangrove.csv")
health <- health %>%
  mutate(year = 2012) %>%
  select(rgn_id, habitat, year, health)

write.csv(health, "globalprep/hab_mangrove/v2015/data/habitat_health_mangrove_updated.csv", row.names=FALSE)

#trend:
data <- data.frame()

for (year in 2012:2015){ # year = 2012
  trend <- read.csv(sprintf("globalprep/hab_mangrove/v2015/data/habitat_trend_mangrove_v%s.csv", year))
  
  trend <- trend %>%
    mutate(year = year) %>%
    select(rgn_id, habitat, year, trend)
  
  data <- rbind(data, trend)
  
}

write.csv(data, "globalprep/hab_mangrove/v2015/data/habitat_trend_mangrove_updated.csv", row.names=FALSE)
