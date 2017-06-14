#### 2017 update for new ohi-global framework

library(dplyr)


data <- data.frame()

for (year in 2012:2016){ # year = 2012
  new_data <- read.csv(sprintf("globalprep/mar_prs_population/v2016/output/prs_pop_density_%s.csv", year))
  
  new_data <- new_data %>%
    mutate(year = year) %>%
    select(rgn_id, year, pressure_score)
  
  data <- rbind(data, new_data)
  
}

write.csv(data, "globalprep/mar_prs_population/v2016/output/prs_pop_density_updated.csv", row.names=FALSE)

