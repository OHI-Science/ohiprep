#### 2017 update for new ohi-global framework

library(dplyr)

data <- read.csv("globalprep/cw_trend_trash/v2016/output/cw_trash_trend.csv")
head(data)

data <- data %>%
  mutate(year=2016) %>%
  select(rgn_id, year, trend)

write.csv(data, "globalprep/cw_trend_trash/v2016/output/cw_trash_trend_updated.csv",
          row.names=FALSE)
