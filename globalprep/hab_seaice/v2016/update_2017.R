#### 2017 update for new ohi-global framework

library(dplyr)

# trend
trend <- data.frame()

for (year in 2011:2015){ # year = 2012
  trend_new <- read.csv(sprintf("globalprep/hab_seaice/v2016/output/hab_ice_trend_eez_%s.csv", year))
  
  trend_new <- trend_new %>%
    mutate(year = year) %>%
    select(rgn_id, habitat, year, trend)
  
  trend <- rbind(trend, trend_new)
  
}


write.csv(trend, "globalprep/hab_seaice/v2016/output/hab_ice_trend_eez_updated.csv", row.names=FALSE)


# health
health <- data.frame()

for (year in 2011:2015){ # year = 2012
  health_new <- read.csv(sprintf("globalprep/hab_seaice/v2016/output/hab_ice_health_eez_%s.csv", year))
  
  health_new <- health_new %>%
    mutate(year = year) %>%
    select(rgn_id, habitat, year, health)
  
  health <- rbind(health, health_new)
  
}


write.csv(health, "globalprep/hab_seaice/v2016/output/hab_ice_health_eez_updated.csv", row.names=FALSE)

# extent
extent <- read.csv("globalprep/hab_seaice/v2016/output/hab_ice_extent_eez.csv")
extent <- extent %>%
  mutate(year = 2016) %>%
  select(rgn_id, habitat, year, km2)

write.csv(extent, "globalprep/hab_seaice/v2016/output/hab_ice_extent_eez_updated.csv", row.names=FALSE)

