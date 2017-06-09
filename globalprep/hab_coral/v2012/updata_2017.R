#### 2017 update for new ohi-global framework

library(dplyr)

#extent:
extent <- read.csv("globalprep/hab_coral/v2012/data/habitat_extent_coral.csv")
extent <- extent %>%
  mutate(year = 2012) %>%
  select(rgn_id, habitat, year, km2)

write.csv(extent, "globalprep/hab_coral/v2012/data/habitat_extent_coral_updated.csv", row.names=FALSE)

#health:
health <- read.csv("globalprep/hab_coral/v2012/data/habitat_health_coral.csv")
health <- health %>%
  mutate(year = 2012) %>%
  select(rgn_id, habitat, year, health)

write.csv(health, "globalprep/hab_coral/v2012/data/habitat_health_coral_updated.csv", row.names=FALSE)

#trend:
trend <- read.csv("globalprep/hab_coral/v2012/data/habitat_trend_coral.csv")
trend <- trend %>%
  mutate(year = 2012) %>%
  select(rgn_id, habitat, year, trend)

write.csv(trend, "globalprep/hab_coral/v2012/data/habitat_trend_coral_updated.csv", row.names=FALSE)
