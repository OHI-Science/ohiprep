#### 2017 update for new ohi-global framework

library(dplyr)

data <- read.csv("globalprep/cw_pressure_trash/v2015/output/trash_eez_2015.csv")
head(data)

data <- data %>%
  mutate(year=2015) %>%
  select(rgn_id, year, pressure_score)

write.csv(data, "globalprep/cw_pressure_trash/v2015/output/trash_eez_2015_updated.csv",
          row.names=FALSE)
