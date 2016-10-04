##########################################
## gapfilling for AO need variable
##########################################

library(dplyr)

data <- read.csv('Global/WorldBank-Statistics_v2012/data/rgn_wb_gdppcppp_2014a_ratio-gapfilled_attr.csv') %>%
  filter(yr==2013) %>%
  select(rgn_id = id, gapfill = z_level) %>%
  mutate(gapfill = ifelse(gapfill=="v", 0, 1)) %>%
  arrange(rgn_id)

write.csv(data, 'Global/WorldBank-Statistics_v2012/data/rgn_wb_gdppcppp_rescaled_2014a_gf.csv', row.names=FALSE)
