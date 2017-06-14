#### 2017 update for new ohi-global framework

library(dplyr)


data <- data.frame()

for (year in 2012:2016){ # year = 2012
  new_data <- read.csv(sprintf("globalprep/prs_targetedharvest/v2016/output/fao_targeted_%s.csv", year))
  
  new_data <- new_data %>%
    mutate(year = year) %>%
    select(rgn_id, year, pressure_score)
  
  data <- rbind(data, new_data)
  
}

write.csv(data, "globalprep/prs_targetedharvest/v2016/output/fao_targeted_updated.csv", row.names=FALSE)

