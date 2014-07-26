# ohi_regional_stats.r

# by JSL July 23 2013. 
# pull statistics for some regional assessments

## setup ----
source('../ohiprep/src/R/common.R') # set dir_neptune_data

## read in regions to assess ----
r = read.csv('../ohiprep/tmp/ohi_regional/rgns_regional.csv') %>%
  select(rgn_nam = region,
         status) %>%
  filter(!rgn_nam %in% c('U.S. West Coast', 'Baltic Sea', 'Hawaii')) %>%
  left_join(read.csv('../ohi-global/eez2013/layers/rgn_global.csv') %>%
                       select(rgn_id, 
                              rgn_nam = label),
            by='rgn_nam') %>%
  select(rgn_nam, rgn_id, status) %>%
  arrange(status, rgn_nam); head(r)




# deal with US West Coast separately
Coastal Length (km)  Coastal Population	Coastal Revenue	Coastal Jobs	Seafood production