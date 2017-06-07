#### 2017 update for new ohi-global framework

library(dplyr)

#extent:
extent <- read.csv("globalprep/hab_saltmarsh/v2012/data/habitat_extent_saltmarsh.csv")
extent <- extent %>%
  mutate(year = 2012) %>%
  select(rgn_id, habitat, year, km2)

write.csv(extent, "globalprep/hab_saltmarsh/v2012/data/habitat_extent_saltmarsh_updated.csv", row.names=FALSE)

#health:
health <- read.csv("globalprep/hab_saltmarsh/v2012/data/habitat_health_saltmarsh.csv")
health <- health %>%
  mutate(year = 2012) %>%
  select(rgn_id, habitat, year, health)

write.csv(health, "globalprep/hab_saltmarsh/v2012/data/habitat_health_saltmarsh_updated.csv", row.names=FALSE)

#trend:
trend <- read.csv("globalprep/hab_saltmarsh/v2012/data/habitat_trend_saltmarsh.csv")
trend <- trend %>%
  mutate(year = 2012) %>%
  select(rgn_id, habitat, year, trend)

write.csv(trend, "globalprep/hab_saltmarsh/v2012/data/habitat_trend_saltmarsh_updated.csv", row.names=FALSE)
