#### 2017 update for new ohi-global framework

library(dplyr)

#extent:
extent <- read.csv("globalprep/hab_seagrass/v2012/data/habitat_extent_seagrass.csv")
extent <- extent %>%
  mutate(year = 2012) %>%
  select(rgn_id, habitat, year, km2)

write.csv(extent, "globalprep/hab_seagrass/v2012/data/habitat_extent_seagrass_updated.csv", row.names=FALSE)

#health:
health <- read.csv("globalprep/hab_seagrass/v2012/data/habitat_health_seagrass.csv")
health <- health %>%
  mutate(year = 2012) %>%
  select(rgn_id, habitat, year, health)

write.csv(health, "globalprep/hab_seagrass/v2012/data/habitat_health_seagrass_updated.csv", row.names=FALSE)

#trend:
trend <- read.csv("globalprep/hab_seagrass/v2012/data/habitat_trend_seagrass.csv")
trend <- trend %>%
  mutate(year = 2012) %>%
  select(rgn_id, habitat, year, trend)

write.csv(trend, "globalprep/hab_seagrass/v2012/data/habitat_trend_seagrass_updated.csv", row.names=FALSE)
