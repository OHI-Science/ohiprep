############################################
## gapfilling
############################################
library(dplyr)

data <- read.csv('globalprep/whounicef_sanitation/v2015/int/rgn_jmp_san_2015a_attr.csv')

data <- data %>%
  select(rgn_id = id, year = yr, pressure_score=z_level) %>%
  mutate(pressure_score = ifelse(pressure_score == "v", 0, 0.5))  # only the sanitation score is gapfilled (50% of the score)

data_pressure <- data %>%
  filter(year == 2015) %>%
  select(rgn_id, pressure_score) %>%
  arrange(rgn_id)

write.csv(data_pressure, 'globalprep/whounicef_sanitation/v2015/data/po_pathogens_popdensity25mi_2015a_gf.csv', row.names=FALSE)


data_trend <- data %>%
  filter(year %in% 2011:2015) %>%
  group_by(rgn_id) %>%
  summarize(trend = mean(pressure_score, na.rm=TRUE)) %>%
  arrange(rgn_id)
write.csv(data_trend, 'globalprep/whounicef_sanitation/v2015/data/pathogens_popdensity25mi_trend_2015a_gf.csv', row.names=FALSE)
