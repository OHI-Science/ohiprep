################################################
## gapfilling Mora data
################################################

## Data were provided at the 2012 Territory/region scale, but then 
## downscaled in 2013.  So, it is conservative to call these data gapfilled, 
## but I am going to do it.

## The whence_2013a data shows the original regional gapfilling, but not the 
## downscaling gapfilling. So this will add that. 

library(dplyr)

d_regions <- read.csv('../ohi-global/global2015/gapFilling/dissaggregated_gap_fill.csv') %>%
  filter(description == "Territories, disaggregated in 2013, probably gapfilled") %>%
  select(rgn_id = rgn_id_2013, region_id_2012) %>%
  mutate(gap_fill_1 = "disagg2012_gap_fill") %>%
  arrange(region_id_2012)

# checking:
data <- read.csv('globalprep/resilience_mora/v2013/data/r_mora_s4_2013a.csv') %>%
  left_join(d_regions) %>%
  arrange(region_id_2012)

mora <- read.csv('globalprep/resilience_mora/v2013/whence_2013a/mora_whence.csv') 


mora <- mora %>%
  mutate(value = ifelse(whencev01=="SG", 1, 0)) %>%
  mutate(value = ifelse(rgn_id %in% d_regions$rgn_id, 1, value)) %>%
  select(rgn_id, value)
  
## These two files have the same gapfilling
write.csv(mora, "globalprep/resilience_mora/v2013/data/r_mora_2013a_gf.csv", row.names=FALSE)
write.csv(mora, "globalprep/resilience_mora/v2013/data/r_mora_s4_2013a_gf.csv", row.names=FALSE)