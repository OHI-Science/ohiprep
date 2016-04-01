#######################################
## MSI Gov
#######################################

## current data is calculated as the average of these two datasets:

data1 <- read.csv('/var/data/ohi/model/GL-NCEAS-Resilience_MSIGov/data/global_resilience_region_msi_gov_codeofconduct.csv') %>%
  select(id, value1=value)
data2 <- read.csv('/var/data/ohi/model/GL-NCEAS-Resilience_MSIGov/data/global_resilience_region_msi_gov_traceability.csv') %>%
  left_join(data1)

# and these are all the actual data, as indicated by this:
original <- read.csv('/var/data/ohi/model/GL-NCEAS-Resilience_MSIGov/_output_actuals.csv')

data2 <- data2 %>%
  left_join(original)

### These are the old rgn_id's, need to translate to new IDs

rgns <- read.csv('src/LookupTables/eez_rgn_2013master.csv') %>%
  filter(rgn_typ=="eez") %>%
  select(region_id_2012, rgn_id_2013) %>%
  unique()

data2 <- data2 %>%
  select(region_id_2012 = id) %>%
  left_join(rgns)

### get the new data
d_regions <- read.csv('../ohi-global/global2015/gapFilling/dissaggregated_gap_fill.csv') %>%
  select(rgn_id_2013, rgn_name = rgn_nam_2013, sov_id, description)


msi <- read.csv('/var/data/ohi/model/GL-NCEAS-Resilience_v2013a/data/r_msi_gov_2013a.csv') %>%
  select(rgn_id_2013=rgn_id, resilience.score) %>%
  left_join(data2) %>%
  left_join(d_regions) %>%
  mutate(resilience.score = ifelse(is.na(description), 0, 1)) %>%
  select(rgn_id = rgn_id_2013, resilience.score)
  
write.csv(msi, 'globalprep/resilience_msi/r_msi_gov_2013a_gf.csv', row.names=FALSE)

