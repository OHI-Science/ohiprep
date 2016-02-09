#####################################
## Gapfilling artisanal fishing data
#####################################

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


## hb: 
hb <- read.csv('globalprep/pressures_artisanal_fp/v2012/data/p_fp_art_hb_disaggregateNature2012.csv') %>%
  mutate(pressures.score = ifelse(rgn_id %in% d_regions$rgn_id, "1", "0"))

write.csv(hb, 
          'globalprep/pressures_artisanal_fp/v2012/data/p_fp_art_hb_disaggregateNature2012_gf.csv', 
          row.names=FALSE)


## lb (gapfilling was same for 2012 and 2013 data):
## NOTE: more gapfilling for this, according to notes, data were
## supplied for 59 countries and then estimated for remaining.  
## Look in original pressures paper for more details

lb <- read.csv('globalprep/pressures_artisanal_fp/v2013/data/fp_art_lb_2013_NEW.csv') %>%
  mutate(pressures.score = ifelse(rgn_id %in% d_regions$rgn_id, "1", "0"))

write.csv(lb, 
          'globalprep/pressures_artisanal_fp/v2013/data/fp_art_lb_2013_NEW_gf.csv', 
          row.names=FALSE)
