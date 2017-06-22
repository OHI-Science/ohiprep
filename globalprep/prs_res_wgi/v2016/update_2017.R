#### 2017 update for new ohi-global framework

library(dplyr)


## pressure
data <- data.frame()

for (year in 2012:2016){ # year = 2012
  new_data <- read.csv(sprintf("globalprep/prs_res_wgi/v2016/output/wgi_prs_%s.csv", year))
  
  new_data <- new_data %>%
    mutate(year = year) %>%
    select(rgn_id, year, pressure_score)
  
  data <- rbind(data, new_data)
  
}

write.csv(data, "globalprep/prs_res_wgi/v2016/output/wgi_prs_updated.csv", row.names=FALSE)

## resilience
data <- data.frame()

for (year in 2012:2016){ # year = 2012
  new_data <- read.csv(sprintf("globalprep/prs_res_wgi/v2016/output/wgi_res_%s.csv", year))
  
  new_data <- new_data %>%
    mutate(year = year) %>%
    select(rgn_id, year, resilience_score)
  
  data <- rbind(data, new_data)
  
}

write.csv(data, "globalprep/prs_res_wgi/v2016/output/wgi_res_updated.csv", row.names=FALSE)
