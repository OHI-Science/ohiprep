#### 2017 update for new ohi-global framework

library(dplyr)


## artisanal low bycatch
data <- data.frame()

for (year in 2013:2016){ # year = 2012
  new_data <- read.csv(sprintf("globalprep/prs_fish/v2016/output/artisanal_fish_lb_%s.csv", year))
  
  new_data <- new_data %>%
    mutate(year = year) %>%
    select(rgn_id, year, pressure_score)
  
  data <- rbind(data, new_data)
  
}

write.csv(data, "globalprep/prs_fish/v2016/output/artisanal_fish_lb_updated.csv", row.names=FALSE)


## commercial low bycatch
data <- data.frame()

for (year in 2013:2016){ # year = 2012
  new_data <- read.csv(sprintf("globalprep/prs_fish/v2016/output/comm_fish_lb_%s.csv", year))
  
  new_data <- new_data %>%
    mutate(year = year) %>%
    select(rgn_id, year, pressure_score)
  
  data <- rbind(data, new_data)
  
}

write.csv(data, "globalprep/prs_fish/v2016/output/comm_fish_lb_updated.csv", row.names=FALSE)


## commercial high bycatch
data <- data.frame()

for (year in 2013:2016){ # year = 2012
  new_data <- read.csv(sprintf("globalprep/prs_fish/v2016/output/comm_fish_hb_%s.csv", year))
  
  new_data <- new_data %>%
    mutate(year = year) %>%
    select(rgn_id, year, pressure_score)
  
  data <- rbind(data, new_data)
  
}

write.csv(data, "globalprep/prs_fish/v2016/output/comm_fish_hb_updated.csv", row.names=FALSE)
