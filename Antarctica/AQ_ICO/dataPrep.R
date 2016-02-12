### Antarctica ICO calculation
library(dplyr)


### STATUS
## IUCN status data for each CCAMLR region
status <- read.csv('globalprep/SPP_ICO/v2015/data/ico_status_aq.csv')
setdiff(ico$sciname, status$sciname)  # some species on the ico list that aren't in Antarctic regions (this is correct because it includes all global data)
setdiff(status$sciname, ico$sciname)  #lots of species that aren't iconic - to be expected
write.csv(intersect(status$sciname, ico$sciname), "Antarctica/AQ_ICO/supplement/AQ_iconics.csv", row.names=FALSE) #iconic species that are in analysis

# one think I want to check is that Katie's list is mostly showing up...
# references Katie used for list are also in these data
AQ_icons <- read.csv("Global/HS-AQ-Iconics_2014/raw/AQ_IconicSpecies.csv") %>%
  select(Scientific.name)
setdiff(AQ_icons$Scientific.name, status$sciname)  # these seem like they should be showing up in the data, check on this...

grep('Euphausia', status$sciname, value=TRUE) #krill - not in IUCN apparently
sort(unique(grep('Balaenoptera', status$sciname, value=TRUE))) #data deficient, not included
grep('orca', status$sciname, value=TRUE) #data deficient, not included
#updated list to include specific species (Catharacta antarctica and Catharacta maccormicki):
grep('Catharacta', status$sciname, value=TRUE) # Catharacta antarctica and Catharacta lonnbergi = same species
                                            # Catharacta skua = different species (more north distribution)
                                            # Catharacta chilensis = different species (mostly around chile)
                                            # Catharacta maccormicki = pretty wide distribution including Antarctica

### Subset data to include only iconics:
ico_status <- status %>%
  filter(sciname %in% ico$sciname) %>%
  select(sp_id = rgn_id, sciname, category) %>%
  unique()

write.csv(ico_status, 'Antarctica/AQ_ICO/data/ico_status.csv', row.names=FALSE)

### trend
## IUCN status data for each CCAMLR region
trend <- read.csv('globalprep/SPP_ICO/v2015/data/ico_trend_aq.csv')

ico_trend <- trend %>%
  filter(sciname %in% ico$sciname) %>%
  select(sp_id = rgn_id, sciname, popn_trend) %>%
  unique()
write.csv(ico_trend, 'Antarctica/AQ_ICO/data/ico_trend.csv', row.names=FALSE)
