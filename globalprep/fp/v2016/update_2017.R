#### 2017 update for new ohi-global framework

library(dplyr)


## Genetic escapes
data <- data.frame()

for (year in 2012:2016){ # year = 2012
  new_data <- read.csv(sprintf("globalprep/fp/v2016/output/wildcaught_weight_v%s.csv", year))
  
  new_data <- new_data %>%
    mutate(year = year) %>%
    select(rgn_id, year, w_fis)
  
  data <- rbind(data, new_data)
  
}

write.csv(data, "globalprep/fp/v2016/output/wildcaught_weight_updated.csv", row.names=FALSE)
