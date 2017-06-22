#### 2017 update for new ohi-global framework

library(dplyr)

# health
data <- data.frame()

for (year in 2006:2010){ # year = 2010
  new_data <- read.csv(sprintf("globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output/habitat_health_softbottom_v%s.csv", year))
  
  new_data <- new_data %>%
    mutate(year = year) %>%
    select(rgn_id, habitat, year, health)
  
  data <- rbind(data, new_data)
  
}

write.csv(data, "globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output/habitat_health_softbottom_updated.csv", row.names=FALSE)

# trend
data <- data.frame()

for (year in 2006:2010){ # year = 2010
  new_data <- read.csv(sprintf("globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output/habitat_trend_softbottom_v%s.csv", year))
  
  new_data <- new_data %>%
    mutate(year = year) %>%
    select(rgn_id, habitat, year, trend)
  
  data <- rbind(data, new_data)
  
}

write.csv(data, "globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output/habitat_trend_softbottom_updated.csv", row.names=FALSE)


# pressure
data <- data.frame()

for (year in 2006:2010){ # year = 2010
  new_data <- read.csv(sprintf("globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output/hd_sb_subtidal_v%s.csv", year))
  
  new_data <- new_data %>%
    mutate(year = year) %>%
    select(rgn_id, year, pressure_score)
  
  data <- rbind(data, new_data)
  
}

write.csv(data, "globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output/hd_sb_subtidal_updated.csv", row.names=FALSE)


# extent

  data <- read.csv(sprintf("globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output/habitat_extent_softbottom.csv", year)) %>%
    mutate(year = 2012) %>%
    select(rgn_id, habitat, year, km2)
  
  write.csv(data, "globalprep/hab_prs_hd_subtidal_soft_bottom/v2016/output/habitat_extent_softbottom_updated.csv", row.names=FALSE) 
  