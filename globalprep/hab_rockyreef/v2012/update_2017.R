#### 2017 update for new ohi-global framework

library(dplyr)

data <- read.csv("globalprep/hab_rockyreef/v2012/data/habitat_extent_rocky_reef.csv") %>%
  mutate(year = 2012) %>%
  select(rgn_id, habitat, year, km2)

write.csv(data, "globalprep/hab_rockyreef/v2012/data/habitat_extent_rocky_reef_updated.csv", row.names=FALSE)
