#######################################################
## Preparing the RAM data for B/Bmsy values
#######################################################
source('../ohiprep/src/R/common.R')

# STEP 1:  
##### Prepare data to link RAM stocks to SAUP regions and Taxon IDs
# The initial MatchedPairs.csv file was from Lydia Teh (SAUP).  
# I modified this file by adding in another list from Lydia for saup regions with >1 stocks sharing 
# the same Taxaid and saup regions (indicated by "multiple stocks in same region").  
# These b/bmsy scores will be averaged.
# We also noticed that some saup regions have >1 FAO and >1 stock with the same Taxa ID 
# In these cases, the stock was linked with the saup and FAO region.
# She put these in a file
# called TaxonEEZ_FAO.csv.  The following merges and checks these two files.

### Create a template file for each unique Taxon/saupregion/faoregion
### This will be used to generate the FAO data for the catch
catch <- read.csv(file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_FIS_data/v2015/tmp/Catch_v16072015_summary.csv')) 
eez_fao <- catch %>%
  filter(EEZID != 0) %>%
  dplyr::select(EEZID, FAOAreaID, TaxonKey) %>%
  unique()
write.csv(eez_fao, "globalprep/SAUP_FIS/v2015/tmp/eez_fao_taxa_combos.csv", row.names=FALSE)

## my template data with eez/fao/species combinations
EEZandFAO <- read.csv('globalprep/SAUP_FIS/v2015/tmp/eez_fao_taxa_combos.csv')

## translates RAM data to SAUP regions/species (dataset 1)
RAM2saup <- read.csv('globalprep/SAUP_FIS/v2015/raw/MatchedPairs.csv') %>%
  unique() %>%
  filter(!(Taxonid == 607039 & EEZID == 910)) # removing here and adding to FAO/SAUP data because we do not have this region


# these are SAUP/FAO regions with multiple stocks of the same species (the b/bmsy scores will be averaged):
tmp <- RAM2saup[RAM2saup$notes=='multiple stocks in same region', ]

## joining with template data to get FAO regions
RAM2saup_join <- RAM2saup %>%
  left_join(EEZandFAO, by=c('EEZID', 'Taxonid'='TaxonKey')) %>%
  mutate(id=paste(EEZID, Taxonid, sep="_"))

## Checking the data
## NOTE: some EEZ/Taxonid's are duplicated upon merge due to EEZ's with >1 FAO region
## The increase in sample size after merge is good!
tmp <- RAM2saup_join$id[duplicated(RAM2saup_join$id)]
tmp2 <- RAM2saup_join[RAM2saup_join$id %in% tmp, ]
tmp2 <- arrange(tmp2, Taxonid, stocklong, EEZID)
write.csv(tmp2, "globalprep/SAUP_FIS/v2015/tmp/checkingDups.csv", row.names=FALSE) # everything looked ok to me.

## translates RAM data to SAUP regions/species (dataset 2): these ones had FAO regions included
## (in this case the saup had multiple FAO regions and multiple stocks - so Lydia determined which saup/fao
## the stock belonged to)
RAM2saup2 <- read.csv('globalprep/SAUP_FIS/v2015/raw/TaxonEEZ_FAO.csv') %>%
  dplyr::select(Taxonid, EEZID, FAOAreaID = FAO, stocklong, notes) 

## add in some data to replace EEZID=910
new910 <- data.frame(Taxonid = 607039, 
                     EEZID = 0, 
                     FAOAreaID = c(41, 48), 
                     stocklong = "Antarctic toothfish Ross Sea",
                     notes=NA)  
## Bind the two saup/fao/taxon datasets together:
RAM2saup2 <- rbind(RAM2saup2, new910)

RAMstocks <- bind_rows(RAM2saup_join, RAM2saup2) %>%  #data.frame(RAMstocks[is.na(RAMstocks$FAOAreaID), ])
  filter(!is.na(FAOAreaID))             
# some NA values, but I checked catch and these stocks really aren't in these regions
# Taxonid/EEZID/stocklong combo in MatchedPairs data from Lydia, but not actually any catch in these regions


# STEP 2: 
#### Preparing the RAM b/bmsy data (v3, downloaded from here: http://ramlegacy.org/database/)
## subsets the data to the most current 6 years of data and assigns a relative year to correspond to the catch data 
## different stocks will have different ranges of years that are used for the analysis
## NOTE: data prior to 2003 is not used

catchYear <- data.frame(catchYear_rel = c(2010, 2009, 2008, 2007, 2006, 2005), distMax = c(0, 1, 2, 3, 4, 5))

ram <- read.csv('globalprep/SAUP_FIS/v2015/raw/RLSADBv3_timeseries_values_views.csv') %>%
  select(stocklong, year, bbmsy=B.Bmsytouse) %>%
  filter(!is.na(bbmsy)) %>%
  filter(year >= 2002) %>%
  group_by(stocklong) %>%
  mutate(maxYear = max(year)) %>%
  mutate(cutYear = maxYear - 5) %>%
  filter(year >= cutYear) %>%
  mutate(distMax = maxYear - year) %>%
  left_join(catchYear) %>%
  mutate(sampleYears = length(distMax)) %>%
  filter(sampleYears >= 6) %>%
  ungroup()

sum(table(ram$stocklong)>0)
table(ram$maxYear)

ram[ram$stocklong=="Albacore tuna Indian Ocean", ]

ram <- ram %>%
  select(stocklong, catchYear_rel, bbmsy)

#### STEP3:
# Adding in some data because the RAM data didn't have the most up-to-date data for this important species 
# (based on ICCAT documents - which is the datasource used by RAM for these stocks) 
ram[ram$stocklong == "Skipjack tuna Western Atlantic",] #these were cut because data was too old, so no values should show up
ram[ram$stocklong == "Skipjack tuna Eastern Atlantic",]
skipjacks <- read.csv('globalprep/SAUP_FIS/v2015/tmp/Skipjack_Bmsy.csv') %>%
  mutate(catchYear_rel = catchYear_rel - 2)  # making the most recent year of data correspond to the 2010 catch

ram <- ram %>%
  bind_rows(skipjacks)
ram[ram$stocklong == "Skipjack tuna Western Atlantic",]
ram[ram$stocklong == "Skipjack tuna Eastern Atlantic",]
head(ram)

### STEP 4
## Merging the SAUP/taxon key with the RAM data
setdiff(ram$stocklong, RAMstocks$stocklong) #35 stocks with no SAUP catch data
tmp <- setdiff(RAMstocks$stocklong, ram$stocklong) #30 stocks with no data in RAM (this is due to the data being too old in RAM and being cut)
dim(RAMstocks[RAMstocks$stocklong %in% tmp, ])

RAM_b_bmsy <- RAMstocks %>%
  filter(!(stocklong %in% tmp)) %>% #cut stock that aren't in the RAM database
  left_join(ram) %>%   # expands data by adding a year for each stocklong, 1809*6=10854 years
  group_by(Taxonid, EEZID, FAOAreaID, catchYear_rel) %>%  
  summarize(bbmsy = mean(bbmsy, na.rm=TRUE)) %>%    #averaging the stocks of the same Taxa within an EEZID/FAO N=6668
  ungroup()
data.frame(RAM_b_bmsy[RAM_b_bmsy$Taxonid == 600142 & RAM_b_bmsy$FAOAreaID == 57, ])
data.frame(filter(RAM_b_bmsy, Taxonid == 600107 & FAOAreaID == 71))


### STEP 5
## Convert to OHI region ID...make sure that this looks reasonable....
saup2ohi <- read.csv('src/LookupTables/new_saup_to_ohi_rgn.csv')
setdiff(saup2ohi$saup_id, RAM_b_bmsy$EEZID)  #some of the countries do not have corresponding RAM catch data
saup2ohi[saup2ohi$notes == "split", ]
setdiff(RAM_b_bmsy$EEZID, saup2ohi$saup_id)  # all of the RAM countries are represented in the master conversion table

# These SAUP regions are comprised of multiple OHI regions (the scores will be duplicated for these regions)
# this will expand the sample size when merged.
tmp <- RAM_b_bmsy %>%
  filter(EEZID %in% c(251, 962, 947, 918, 830, 626, 908))
table(tmp$EEZID)
#trying to determine merged sample size:
2*30 + 1*60 + 1*24 + 1*54 + 1*24 + 1*36 + 1*150  #these are the number of extra duplicates, N=408 + N=10,674 (RAM_b_bmsy) = 11,082...which is correct!

RAM_b_bmsy_ohi_rgn <- RAM_b_bmsy %>%   
  left_join(saup2ohi, by=c('EEZID' = 'saup_id')) %>%
  mutate(id = paste(Taxonid, ohi_id_2013, FAOAreaID, catchYear_rel, sep="_")) %>%
  mutate(id2 = paste(ohi_id_2013, Taxonid, sep="_"))
RAM_b_bmsy_ohi_rgn[RAM_b_bmsy_ohi_rgn$Taxonid == 600142 & RAM_b_bmsy_ohi_rgn$FAOAreaID == 57, ]

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
ggplot(tmp, aes(x=catchYear_rel, y=bbmsy, col=as.factor(EEZID), group=as.factor(EEZID))) +
  geom_point() +
  geom_line() +
  facet_wrap(~id2, scales='free')
ggsave('globalprep/SAUP_FIS/v2015/tmp/combining_bbmsy_rgns_multiple_saup.png')

# a couple worrisome ones...impetus for weighting by catch when multiple SAUP regions within an OHI region
data.frame(tmp[tmp$id2 == "163_600143", ]) # difference captured by different FAO regions
data.frame(tmp[tmp$id2 == "163_600223", ]) 
data.frame(tmp[tmp$id2 == "163_600361", ]) 
data.frame(tmp[tmp$id2 == "163_600504", ]) 
data.frame(tmp[tmp$id2 == "62_600226", ]) 
data.frame(tmp[tmp$id2 == "182_600142", ]) 

### STEP 6 ----
## Weight b/bmsy values by mean catch within regions

catch <- read.csv('globalprep/SAUP_FIS/v2015/tmp/mean_catch_saup_fao.csv') %>%
  select(EEZID, FAOAreaID, Taxonid=TaxonKey, Year, Catch=mean_catch) %>%
  filter(EEZID != 0)

data.frame(filter(catch, EEZID==8 & FAOAreaID==37 & Taxonid==100039)) #should all be the same
data.frame(filter(catch, EEZID==50 & FAOAreaID==57 & Taxonid==600142)) 
data.frame(filter(catch, EEZID==36 & FAOAreaID==57 & Taxonid==600142)) 
data.frame(filter(catch, EEZID==50 & FAOAreaID==57 & Taxonid==600107))


RAM_b_bmsy_ohi_rgn_catch <- RAM_b_bmsy_ohi_rgn %>%
  left_join(catch, by=c('FAOAreaID', 'EEZID', 'Taxonid', 'catchYear_rel'='Year'))


data.frame(RAM_b_bmsy_ohi_rgn_catch[RAM_b_bmsy_ohi_rgn_catch$Taxonid == 600504 & 
                           RAM_b_bmsy_ohi_rgn_catch$ohi_id_2013==163 & 
                           RAM_b_bmsy_ohi_rgn_catch$FAOAreaID == 67, ])

data.frame(filter(RAM_b_bmsy_ohi_rgn_catch, EEZID==50 & FAOAreaID==57 & Taxonid==600142))
filter(catch, EEZID==50 & FAOAreaID==57 & Taxonid==600142)

data.frame(filter(RAM_b_bmsy_ohi_rgn_catch, EEZID==50 & FAOAreaID==57 & Taxonid==600107))
filter(catch, EEZID==50 & FAOAreaID==57 & Taxonid==600107)

data.frame(RAM_b_bmsy_ohi_rgn_catch[RAM_b_bmsy_ohi_rgn_catch$Taxonid == 600142 & RAM_b_bmsy_ohi_rgn_catch$FAOAreaID == 57, ])


RAM_b_bmsy_ohi_rgn_catch <- RAM_b_bmsy_ohi_rgn_catch %>%
  filter(!is.na(Catch)) %>%
  group_by(Taxonid, FAOAreaID, ohi_id_2013, catchYear_rel) %>%
  summarize(bbmsy = weighted.mean(bbmsy, Catch, na.rm=TRUE)) %>%
  select(Taxonid, FAO_rgn=FAOAreaID, ohi_rgn=ohi_id_2013, year=catchYear_rel, bbmsy); head(RAM_b_bmsy_ohi_rgn_catch)

## check: 2010 value should be just under 1.490 (yes):
RAM_b_bmsy_ohi_rgn_catch[RAM_b_bmsy_ohi_rgn_catch$Taxonid == 600504 & 
                           RAM_b_bmsy_ohi_rgn_catch$ohi_rgn==163 & 
                           RAM_b_bmsy_ohi_rgn_catch$FAO_rgn == 67, ]

filter(RAM_b_bmsy_ohi_rgn_catch, Taxonid==600142 & FAO_rgn==57 & ohi_rgn==204)
filter(RAM_b_bmsy_ohi_rgn_catch, Taxonid==600107 & FAO_rgn==57 & ohi_rgn==204)


write.csv(RAM_b_bmsy_ohi_rgn_catch, 'globalprep/SAUP_FIS/v2015/tmp/RAM_fao_ohi.csv', row.names=FALSE)
