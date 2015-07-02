#######################################################
## Preparing the RAM data for B/Bmsy values
#######################################################
library(dplyr)



# STEP 1:  
##### Prepare data to link RAM stocks to SAUP regions and Taxon IDs
# The initial MatchedPairs.csv file was from Lydia Teh (SAUP).  
# I modified this one by adding in the SAUP/Taxon that had multiple stock
# (indicated by "SAUP region has stocks from multiple FAOs").  These b/bmsy scores will be averaged.
# We also noticed that some saup regions had stocks from multiple FAO
# regions.  She put these in a file
# called TaxonEEZ_FAO.csv.  The following merges and checks these two files.

### Create a file for each unique Taxon/saupregion/faoregion
### This will be used to generate the FAO data for the catch
# catch <- read.csv(file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_data_2015/tmp/Catch_Value_11062015_summary.csv')) 
# eez_fao <- catch %>%
#   filter(EEZID != 0) %>%
#   dplyr::select(EEZID, FAOAreaID, TaxonKey) %>%
#   unique()
# write.csv(eez_fao, "globalprep/SAUP_FIS/tmp/eez_fao_taxa_combos.csv", row.names=FALSE)

EEZandFAO <- read.csv('globalprep/SAUP_FIS/tmp/eez_fao_taxa_combos.csv')

RAM2saup <- read.csv('globalprep/SAUP_FIS/raw/MatchedPairs.csv') %>%
  unique()

tmp <- RAM2saup[duplicated(dplyr::select(RAM2saup, Taxonid, EEZID)), ] # these are SAUP regions with multiple stocks of the same species

RAM2saup_join <- RAM2saup %>%
  left_join(EEZandFAO, by=c('EEZID', 'Taxonid'='TaxonKey')) %>%
  mutate(id=paste(EEZID, Taxonid, sep="_"))

## NOTE: some EEZ/Taxonid's are duplicated upon merge due to taxon/EEZ that are found in multiple FAO regions
## The increase in sample size after merge is good!
tmp <- RAM2saup_join$id[duplicated(RAM2saup_join$id)]
tmp2 <- RAM2saup_join[RAM2saup_join$id %in% tmp, ]
tmp2 <- arrange(tmp2, Taxonid, stocklong, EEZID)
#write.csv(tmp2, "globalprep/SAUP_FIS/tmp/checkingDups.csv", row.names=FALSE) # everything looked ok to me.


RAM2saup2 <- read.csv('globalprep/SAUP_FIS/raw/TaxonEEZ_FAO.csv') %>%
  dplyr::select(Taxonid, EEZID, FAOAreaID = FAO, stocklong, notes) 

RAMstocks <- bind_rows(RAM2saup_join, RAM2saup2) %>%
  filter(!is.na(FAOAreaID))                      # some NA values, but I checked catch and these stocks really aren't in these regions


# STEP 2: 
#### Preparing the RAM data (downloaded from here: )
## subsets the data to the most current 4 years and assigns the OHIscenario year relative to this
## different stocks will have different ranges of years that are used for the analysis

scenarioYear <- data.frame(scenarioYear = c(2010, 2009, 2008, 2007), distMax = c(0, 1, 2, 3))

ram <- read.csv('globalprep/SAUP_FIS/raw/RLSADBv3_timeseries_values_views.csv') %>%
  select(stocklong, year, bbmsy=B.Bmsytouse) %>%
  filter(!is.na(bbmsy)) %>%
  filter(year >= 2004) %>%
  group_by(stocklong) %>%
  mutate(maxYear = max(year)) %>%
  mutate(cutYear = maxYear - 3) %>%
  filter(year >= cutYear) %>%
  mutate(distMax = maxYear - year) %>%
  left_join(scenarioYear) %>%
  mutate(sampleYears = length(distMax)) %>%
  filter(sampleYears >=4) %>%
  ungroup()

ram <- ram %>%
  select(stocklong, scenarioYear, bbmsy)

### STEP 3
## Merging these two datasets
setdiff(ram$stocklong, RAMstocks$stocklong) #35 stocks with no SAUP catch data
tmp <- setdiff(RAMstocks$stocklong, ram$stocklong) #32 stocks with no data in RAM (this is due to the data being too old in RAM and being cut)
dim(RAMstocks[RAMstocks$stocklong %in% tmp, ])

RAM_b_bmsy <- RAMstocks %>%
  filter(!(stocklong %in% tmp)) %>% #cut stock that aren't in the RAM database
  left_join(ram) %>%
  group_by(Taxonid, EEZID, FAOAreaID, scenarioYear) %>%
  summarize(bbmsy = mean(bbmsy)) %>%    #averaging the stocks when they co-occur in the same EEZID/FAO N=6668
  ungroup()

### STEP 4
## Convert to OHI region ID...make sure that this looks reasonable....
saup2ohi <- read.csv('src/LookupTables/new_saup_to_ohi_rgn.csv')
setdiff(saup2ohi$saup_id, RAM_b_bmsy$EEZID)  #some of the countries do not have corresponding RAM catch data
saup2ohi[saup2ohi$notes == "split", ]
setdiff(RAM_b_bmsy$EEZID, saup2ohi$saup_id)  # all of the RAM countries are represented in the master conversion table

# These regions are comprised of multiple OHI regions (the scores will be duplicated for these regions)
tmp <- RAM_b_bmsy %>%
  filter(EEZID %in% c(251, 962, 947, 910, 918, 830, 626, 908))
table(tmp$EEZID)
#trying to determine merged sample size:
3*20 + 2*88 + 2*20 + 1*4 + 2*12 + 2*16 + 2*40 + 2*32
2*20 + 1*88 + 1*20 + 1*12 + 1*16 + 1*40 + 1*32  #these are the number of extra duplicates, N=248 + N=6668 (RAM_b_bmsy) = 6916...which is correct!

RAM_b_bmsy_ohi_rgn <- RAM_b_bmsy %>%   
  left_join(saup2ohi, by=c('EEZID' = 'saup_id')) %>%
  mutate(id = paste(Taxonid, ohi_id_2013, FAOAreaID, scenarioYear, sep="_")) %>%
  mutate(id2 = paste(ohi_id_2013, Taxonid, sep="_"))

## check that merge went well (appears to have gone well based on sample size!)
tmp <- RAM_b_bmsy_ohi_rgn %>%
  filter(EEZID %in% c(251, 962, 947, 910, 918, 830, 626, 908))
table(tmp$EEZID)
data.frame(RAM_b_bmsy_ohi_rgn[RAM_b_bmsy_ohi_rgn$EEZID == 251, ])

## check on duplicates due to some OHI regions having multiple SAUP regions
dups <- RAM_b_bmsy_ohi_rgn$id[duplicated(RAM_b_bmsy_ohi_rgn$id)]
tmp <- RAM_b_bmsy_ohi_rgn[RAM_b_bmsy_ohi_rgn$id %in% dups, ]

data.frame(tmp[tmp$ohi_id_2013==16, ])
library(ggplot2)
ggplot(tmp, aes(x=scenarioYear, y=bbmsy, col=as.factor(EEZID), group=as.factor(EEZID))) +
  geom_point() +
  geom_line() +
  facet_wrap(~id2, scales='free')
ggsave('globalprep/SAUP_FIS/tmp/combining_bbmsy_rgns_multiple_saup.png')

# a couple worrisome ones...but not many
data.frame(tmp[tmp$id2 == "163_600143", ]) # difference captured by different FAO regions
data.frame(tmp[tmp$id2 == "163_600223", ]) 
data.frame(tmp[tmp$id2 == "163_600361", ]) 
data.frame(tmp[tmp$id2 == "163_600504", ]) 
data.frame(tmp[tmp$id2 == "62_600226", ]) 
data.frame(tmp[tmp$id2 == "182_600142", ]) 

### question: should I do a weighted average?
### answer: I think so....
