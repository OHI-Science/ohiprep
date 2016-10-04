##########################################
## Gapfilling for sector evenness
##########################################

# looking at scripts and data there was disaggregration.
# It also looks like there was other gapfilling, but I can't track this.
# This relates to LE goal, which doesn't matter much here, so I'll do what I can.
d_regions <- read.csv('../ohi-global/global2015/gapFilling/dissaggregated_gap_fill.csv') %>%
  select(rgn_id = rgn_id_2013, rgn_name = rgn_nam_2013, sov_id, description)


data <- read.csv('/var/data/ohi/model/GL-NCEAS-Resilience_v2013a/data/r_li_sector_evenness_2013a.csv')

data <- data %>%
  left_join(d_regions) %>%
  arrange(resilience.score)

## original data (assume regions that are not included here were gapfilled)
country <- read.csv('/var/data/ohi/model/GL-NCEAS-SectorEvenness/data/global_li_model_sector_evenness.csv')
codes <- read.csv('src/LookupTables/eez_rgn_2013master.csv')
no_gf <- intersect(country$iso3166, codes$eez_iso3)
setdiff(country$iso3166, codes$eez_iso3) # some additionals (ignoring for now)
gf_country <- setdiff(codes$eez_iso3, country$iso3166) # some additionals
gf_rgn <- filter(codes, eez_iso3 %in% gf_country) %>%
  dplyr::select(rgn_id = rgn_id_2013) %>%
  mutate(original_data= "none")


gapfilling_data <- data %>%
  left_join(gf_rgn) %>%
  mutate(resilience.score = ifelse(is.na(description) & is.na(original_data), 0, 1)) %>%
  select(rgn_id, resilience.score)

write.csv(data, 'globalprep/Resilience_sector_evenness/r_li_sector_evenness_2013a_gf.csv', row.names=FALSE)



