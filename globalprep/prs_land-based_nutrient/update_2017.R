#### 2017 update for new ohi-global framework

library(dplyr)

data <- data.frame()

for (year in 2012:2016){ # year = 2012
  trend <- read.csv(sprintf("globalprep/prs_land-based_nutrient/v2016/output/cw_fertilizers_trend_%s_new.csv", year))
  
  trend <- trend %>%
    mutate(year = year) %>%
    select(rgn_id, year, trend)
  
  data <- rbind(data, trend)
  
}


write.csv(data, "globalprep/prs_land-based_nutrient/v2016/output/cw_fertilizers_trend_updated.csv")
