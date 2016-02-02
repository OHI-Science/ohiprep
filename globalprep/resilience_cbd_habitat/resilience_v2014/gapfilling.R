#############################################
## Figuring out gapfilling for the 
## CBD resilience data 
## MRF: Feb 1 2016
#############################################

## Notes in paper:
# Data for 147 regions and used geographical means, weighted by country area for the remaining regions
# Then, there was the disaggregation that occurred in the 2013 analysis

# There are some whence files, but those may only deal with the disaggregation gapfilling.

library(dplyr)

## disaggregation regions
d_regions <- read.csv('../ohi-global/global2015/gapFilling/dissaggregated_gap_fill.csv') %>%
  select(rgn_id = rgn_id_2013, region_id_2012) %>%
  mutate(gap_fill_1 = "disagg2012_gap_fill") %>%
  arrange(region_id_2012)


## alien data
# for some reason there are a lot of repeats here
gapfill <- read.csv('globalprep/resilience_cbd_habitat/resilience_v2014/whence_2013a/CBDaliens_whence.csv') %>%
  unique()

table(gapfill$whencev01)

aliens <- read.csv('globalprep/resilience_cbd_habitat/resilience_v2014/data/r_alien_species_Nature2012disaggregated.csv') %>%
  left_join(d_regions) %>%
  arrange(region_id_2012) %>%
  left_join(gapfill)

aliens <- aliens %>%
  mutate(whencev01 = ifelse(rgn_id == 163, "OD", as.character(whencev01))) %>%  # going to assume US is not gapfilled
  filter(rgn_id != 213)  # cut antarctica

aliens_gf <- aliens %>%
  mutate(resilience.score = ifelse(whencev01 == 'SG', 1, 0)) %>%
  select(rgn_id, resilience.score)

write.csv(aliens_gf, 
          'globalprep/resilience_cbd_habitat/resilience_v2014/data/r_alien_species_Nature2012disaggregated_gf.csv',
          row.names=FALSE)

### these appear to have the same gapfilling (based on quick inspection, 
### supplementary materials, and the fact that they use the same source data):
### will just streamline saving the files

# tourism:
write.csv(aliens_gf, 
          'globalprep/resilience_cbd_habitat/resilience_v2014/data/r_tourism_2013a_gf.csv',
          row.names=FALSE)
# water
write.csv(aliens_gf, 
          'globalprep/resilience_cbd_habitat/resilience_v2014/data/r_water_2013a_gf.csv',
          row.names=FALSE)

## mariculture data
write.csv(aliens_gf, 
          'globalprep/resilience_cbd_habitat/resilience_v2014/data/r_mariculture_2013a_gf.csv',
          row.names=FALSE)

## habitat (CBD habitat data)
write.csv(aliens_gf, 
          'globalprep/resilience_cbd_habitat/resilience_v2014/data/r_habitat_2013a_gf.csv',
          row.names=FALSE)

## habitat_combo (combines the CBD habitat and MPAs)
## No gapfilling for MPAs:
## the habitat_combo layer uses MPAs in 3km off coast
## the habtitat_combo_eez uses MPAs in eez
## gapfilling is the same for both of these (50% of values gapfilled if CBD habitat is gapfilled)

hab_combo <- aliens_gf %>%
  mutate(resilience.score = resilience.score/2)

write.csv(hab_combo, 
          'globalprep/resilience_cbd_habitat/resilience_v2014/data/r_habitat_combo_2014a_gf.csv',
          row.names=FALSE)

write.csv(hab_combo, 
          'globalprep/resilience_cbd_habitat/resilience_v2014/data/r_habitat_combo_eez_2014a_gf.csv',
          row.names=FALSE)

### Mora data: this is needed to get gapfilling for fishing resilience layers




### look at others...
hab <- read.csv('globalprep/resilience_cbd_habitat/resilience_v2014/tmp/r_habitat_2013a.csv') %>%
  left_join(d_regions) %>%
  arrange(region_id_2012)


aliens <- aliens %>%
  mutate(whencev01 = ifelse(rgn_id == 163, "OD", as.character(whencev01))) %>%  # going to assume US is not gapfilled
  filter(rgn_id != 213)  # cut antarctica

aliens_gf <- aliens %>%
  mutate(resilience.score = ifelse(whencev01 == 'SG', 1, 0)) %>%
  select(rgn_id, resilience.score)

write.csv(aliens_gf, 
          'globalprep/resilience_cbd_habitat/resilience_v2013/data/r_alien_species_Nature2012disaggregated_gf.csv',
          row.names=FALSE)