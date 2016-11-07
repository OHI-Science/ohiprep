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
  filter(description == "Territories, disaggregated in 2013, probably gapfilled") %>%
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
  mutate(resilience.score = ifelse(whencev01 %in% 'SG' | gap_fill_1 %in% 'disagg2012_gap_fill', 1, 0)) %>%
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
## the habitat_combo_eez uses MPAs in eez
## gapfilling is the same for both of these (50% of values gapfilled if CBD habitat is gapfilled)

hab_combo <- aliens_gf %>%
  mutate(resilience.score = resilience.score/2)

write.csv(hab_combo, 
          'globalprep/resilience_cbd_habitat/resilience_v2014/data/r_habitat_combo_2014a_gf.csv',
          row.names=FALSE)

write.csv(hab_combo, 
          'globalprep/resilience_cbd_habitat/resilience_v2014/data/r_habitat_combo_eez_2014a_gf.csv',
          row.names=FALSE)


#################################################################
### Mora data: this is needed to get gapfilling for fishing resilience layers
## These use Mora or Mora S4 data which have the same gapfilling

mora <- read.csv('globalprep/resilience_mora/v2013/data/r_mora_2013a_gf.csv') %>%
  select(rgn_id, mora = value)
mora4 <- read.csv('globalprep/resilience_mora/v2013/data/r_mora_s4_2013a_gf.csv') %>%
  select(rgn_id, mora4 = value)
habitat <- read.csv('globalprep/resilience_cbd_habitat/resilience_v2014/data/r_habitat_2013a_gf.csv')

fish_v1 <- mora %>%
  left_join(habitat) %>%
  mutate(score = (mora + resilience.score)/3) %>%
  select(rgn_id, resilience.score = score)

write.csv(fish_v1, "globalprep/resilience_cbd_habitat/resilience_v2014/data/r_fishing_v1_2014a_gf.csv",
          row.names = FALSE)

write.csv(fish_v1, "globalprep/resilience_cbd_habitat/resilience_v2014/data/r_fishing_v1_eez_2014a_gf.csv",
          row.names = FALSE)

# Fish V2
fish_v2 <- mora %>%
  left_join(mora4) %>%
  left_join(habitat) %>%
  mutate(score = (mora + mora4 + resilience.score)/4) %>%
  select(rgn_id, resilience.score = score)

write.csv(fish_v2, "globalprep/resilience_cbd_habitat/resilience_v2014/data/r_fishing_v2_eez_2014a_gf.csv",
          row.names = FALSE)

# Fish V3
fish_v3 <- mora4 %>%
  left_join(habitat) %>%
  mutate(score = (mora4 + resilience.score)/3) %>%
  select(rgn_id, resilience.score = score)

write.csv(fish_v3, "globalprep/resilience_cbd_habitat/resilience_v2014/data/r_fishing_v3_2014a_gf.csv",
          row.names = FALSE)
write.csv(fish_v3, "globalprep/resilience_cbd_habitat/resilience_v2014/data/r_fishing_v3_eez_2014a_gf.csv",
          row.names = FALSE)

