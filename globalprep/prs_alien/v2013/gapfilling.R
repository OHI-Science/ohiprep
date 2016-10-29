###############################################
## Pressure: alien invasions gap-filling
###############################################
library(dplyr)
library(tidyr)


## disaggregation regions
d_regions <- read.csv('../ohi-global/global2015/gapFilling/dissaggregated_gap_fill.csv') %>%
  select(rgn_id = rgn_id_2013, region_id_2012) %>%
  mutate(gap_fill_1 = "disagg2012_gap_fill") %>%
  arrange(region_id_2012)


## data: 
## This is probably a conservative estimate of gapfilling because the regions located within the same 
## MEOW as the original 2012 regions data probably shouldn't be considered gapfilling.  The source data
## was at the scale of the MEOW, and this was downscaled to the original OHI 2012 regions.  Then, the 
## data were dissagregated.
alien <- read.csv('globalprep/pressures_alien/v2013/data/p_sp_alien_2013a.csv') %>%
  mutate(pressures.score = ifelse(rgn_id %in% d_regions$rgn_id, "1", "0"))

write.csv(alien, 'globalprep/pressures_alien/v2013/data/p_sp_alien_2013a_gf.csv', row.names=FALSE)
