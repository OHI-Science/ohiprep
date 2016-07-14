
###########################
## Iconics data
## April 27 2016
###########################

library(dplyr)

#iconics species list
ico <- read.csv("Antarctica/AQ_ICO/raw/Antartica_iconic species_from_Katie_Apr_26_2016.csv") %>%
  select(sciname) %>%
  mutate(sciname = as.character(sciname)) %>%
  mutate(sciname = ifelse(sciname == "Arctocephalus gazella", "Arctocephalus gazella, Arctocephalus tropicalis", sciname)) %>%
  mutate(sciname = ifelse(sciname == "Balaenoptera bonaerensis", "Balaenoptera acutorostrata", sciname)) %>%
  mutate(sciname = ifelse(sciname == "Lobodon carcinophaga", "Lobodon carcinophaga, Lobodon carcinophagus", sciname)) %>%
  mutate(sciname = ifelse(sciname == "Stercorarius maccormicki", "Stercorarius maccormicki, Catharacta maccormicki", sciname)) %>%
  mutate(sciname = ifelse(sciname == "Thalassarche melanophrys", "Thalassarche melanophris, Thalassarche melanophris melanophris, Thalassarche melanophrys", sciname)) %>%
  mutate(sciname = ifelse(sciname == "Tursiops truncatus", "Tursiops truncatus, Tursiops gephyreus, Tursiops nuuanu", sciname)) %>%
  mutate(sciname = ifelse(sciname == "Stercorarius lonnbergi", "Catharacta antarctica, Catharacta lonnbergi, Stercorarius lonnbergi", sciname)) 
  

# IUCN data for Antarctica

#### NOTE: this seems to have changed recently.....track down older version
iucn <- read.csv('globalprep/spp_ico/v2016/output/rgn_spp_aq.csv') %>%
  select(sciname, pop_cat, pop_trend, rgn_id)

## these are iconic species, but do not have IUCN antarctica data
## Data Deficient species are not included:
# "Berardius arnuxii", "Caperea marginata", "Globicephala melas",
# "Lagenorhynchus obscurus", "Lissodelphis peronii", "Mesoplodon bowdoini",  
# "Mesoplodon grayi", "Mesoplodon hectori", "Mesoplodon layardii", "Orcinus orca",
# "Phocoena dioptrica", "Tasmacetus shepherdi", "Balaenoptera bonaerensis" 

# not in Antarctic according to IUCN: "Cetorhinus maximus", "Isurus oxyrinchus", "Prionace glauca"

# unclear: 
# "Lamna nasus"  


### STATUS
sort(setdiff(ico$sciname, iucn$sciname))  # some species on the ico list that aren't in Antarctic regions
sort(setdiff(iucn$sciname, ico$sciname))  
intersect(iucn$sciname, ico$sciname)
write.csv(intersect(iucn$sciname, ico$sciname), "Antarctica/AQ_ICO/supplement/iconics_in_AQ_assess.csv", row.names=FALSE) #iconic species that are in analysis


### Subset data to include only iconics:
ico_status <- iucn %>%
  filter(sciname %in% ico$sciname) %>%
  select(sp_id = rgn_id, sciname, category=pop_cat) %>%
  unique()

write.csv(ico_status, 'Antarctica/AQ_ICO/data/ico_status.csv', row.names=FALSE)

### trend
ico_trend <- iucn %>%
  filter(sciname %in% ico$sciname) %>%
  select(sp_id = rgn_id, sciname, popn_trend=pop_trend) %>%
  unique()
write.csv(ico_trend, 'Antarctica/AQ_ICO/data/ico_trend.csv', row.names=FALSE)

### table for paper
status <- read.csv("Antarctica/AQ_ICO/data/ico_status.csv") %>%
  select(sciname, category) %>%
  unique()
trend <- read.csv("Antarctica/AQ_ICO/data/ico_trend.csv") %>%
  select(sciname, popn_trend) %>%
  unique()

table <- left_join(status, trend) %>%
  arrange(sciname)

write.csv(table, 'Antarctica/AQ_ICO/data/table2.csv', row.names=FALSE)

head(status)
table(status$sciname)
