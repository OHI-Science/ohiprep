##################################################
## Using the LSP data to get resilience variables
## NOTE: we use the old eez data 
## (not generated for the 2015 assessment)
## This script may be simplified when the same data source is used!!!
####################################################

library(dplyr)

# reference points = 30% of area
ref_pct_cmpa=30 
ref_pct_cp=30


##########################
#### 3nm region
#########################

lsp_3nm <- read.csv('globalprep/WDPA_MPA/v2015/data/lsp_protarea_offshore3nm.csv')
area_3nm <- read.csv('../ohi-global/eez2013/layers/rgn_area_offshore3nm.csv') %>%
  select(rgn_id, area_km2_3nm = area_km2)

status_year = 2014

# fill in time series for all regions and generate cumulative sum
r.yrs <- expand.grid(rgn_id = unique(lsp_3nm$rgn_id),
                     year = unique(lsp_3nm$year)) %>%
  left_join(lsp_3nm, by=c('rgn_id', 'year')) %>%
  arrange(rgn_id, year) %>%
  mutate(area_km2 = ifelse(is.na(area_km2), 0, area_km2)) %>%
  group_by(rgn_id) %>%
  mutate(cmpa_cumsum  = cumsum(area_km2)) %>%
  filter(year == status_year) %>%
  select(rgn_id, cmpa_cumsum) %>%
  ungroup() 

# get percent of total area that is protected 
r.yrs = r.yrs %>%
  full_join(area_3nm, by="rgn_id") %>%
  mutate(pct_cmpa  = pmin(cmpa_cumsum / area_km2_3nm * 100, 100),
         prop_protected    = ( pmin(pct_cmpa / ref_pct_cmpa, 1))) %>%
  filter(!is.na(prop_protected)) %>%
  select(rgn_id, pressure.score = prop_protected)

write.csv(r.yrs, 'globalprep/WDPA_MPA/v2015/data/resilience_lsp_score_offshore3nm.csv')


##########################
#### eez region
#########################

lsp_eez <- read.csv('globalprep/WDPA_MPA/v2014_archive/data/lsp_protarea_eez.csv')
area_eez <- read.csv('../ohi-global/eez2013/layers/rgn_area.csv') %>%
  select(rgn_id, area_km2_eez = area_km2)

status_year = 2013

# fill in time series for all regions and generate cumulative sum
r.yrs <- expand.grid(rgn_id = unique(lsp_eez$rgn_id),
                     year = unique(lsp_eez$year)) %>%
  left_join(lsp_eez, by=c('rgn_id', 'year')) %>%
  arrange(rgn_id, year) %>%
  mutate(area_km2 = ifelse(is.na(area_km2), 0, area_km2)) %>%
  group_by(rgn_id) %>%
  mutate(cmpa_cumsum  = cumsum(area_km2)) %>%
  filter(year == status_year) %>%
  select(rgn_id, cmpa_cumsum) %>%
  ungroup() 

# get percent of total area that is protected 
r.yrs = r.yrs %>%
  full_join(area_eez, by="rgn_id") %>%
  mutate(pct_cmpa  = pmin(cmpa_cumsum / area_km2_eez * 100, 100),
         prop_protected    = ( pmin(pct_cmpa / ref_pct_cmpa, 1))) %>%
  filter(!is.na(prop_protected)) %>%
  select(rgn_id, pressure.score = prop_protected)

write.csv(r.yrs, 'globalprep/WDPA_MPA/v2015/data/resilience_lsp_score_eez.csv')
