##########################################
## CITES gapfilling
##########################################

d_regions <- read.csv('../ohi-global/global2015/gapFilling/dissaggregated_gap_fill.csv') %>%
  select(rgn_id = rgn_id_2013, rgn_name = rgn_nam_2013, sov_id, description)

data <- read.csv('globalprep/cites_signatories/v2015/data/rgn_cites_eez2015.csv') %>%
  left_join(d_regions) %>%
  arrange(description)

# description in SOM suggests that territories are assigned same value as sovereign country
# this does not appear to be the case

# Doesn't appear to be any gapfilling.

data <- read.csv('globalprep/cites_signatories/v2015/data/rgn_cites_eez2015.csv') %>%
  mutate(resilience_score = 0)
write.csv(data, 'globalprep/cites_signatories/v2015/data/rgn_cites_eez2015_gf.csv', row.names=FALSE)
