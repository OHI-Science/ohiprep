#### 2017 update for new ohi-global framework

library(dplyr)

data <- read.csv("ohiprep/globalprep/res_mora_ao/v2013/data/r_mora_s4_2013a.csv")
head(data)

data <- data %>%
  mutate(year=2013) %>%
  select(rgn_id, year, value)

write.csv(data, "../ohiprep/globalprep/res_mora_ao/v2013/data/r_mora_s4_2013a_updated.csv")
