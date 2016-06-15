#######################################
## Preparing data files for FIS analysis
## MRF: June 16 2015
#######################################
library(tidyr)
source('src/R/common.R')

#####################################################
#############Prepare catch data  ####################
#####################################################
eez <- read.csv(file.path(dir_neptune_data, 
    "git-annex/globalprep/SAUP_FIS_data/v2015/raw/Catch_v16072015.csv"), header = FALSE)
names(eez) <- c('EEZID', 'FAOAreaID', 'Year', 'TaxonKey', 'CatchAmount', "Value") #N=2000082

### straighten out data. There are repeats with same EEZID, FAOAreaID, Year, and TaxonKey.  This is fine and just needs to be summarized.
# should be N=1222380

eez_catch <- eez %>%
  group_by(EEZID, FAOAreaID, Year, TaxonKey) %>%
  summarize(catch = sum(CatchAmount, na.rm=TRUE)) %>%
  ungroup()

write.csv(eez_catch, file.path(dir_neptune_data, 
  'git-annex/globalprep/SAUP_FIS_data/v2015/tmp/Catch_v16072015_summary.csv'), 
  row.names=FALSE)


#####################################################
#############Prepare b/bmsy data  ####################
#####################################################
b_bmsy <- eez %>%
  group_by(FAOAreaID, Year, TaxonKey) %>%
  summarize(catch=sum(CatchAmount, na.rm=TRUE)) %>%
  filter(Year >= 1980) %>%
  ungroup() %>%
  filter(substring(TaxonKey, 1, 1) == 6) %>%
  mutate(stock_id = paste(TaxonKey, FAOAreaID, sep="_")) %>%
  arrange(TaxonKey, FAOAreaID, Year)


### number of years of non-zero data for each stock
stock_count <- b_bmsy %>%
  group_by(stock_id) %>%
  summarize(nonzero = sum(stock_id>0)) %>%
  filter(nonzero >= 10)

### analyze stocks with >= 10 nonzero catch records
b_bmsy <- b_bmsy %>%
  filter(stock_id %in% stock_count$stock_id)

length(unique(b_bmsy$stock_id)) #2,684 stocks

write.csv(b_bmsy,  
      file.path('globalprep/SAUP_FIS/v2015/tmp/b_bmsy_v16072015.csv'), row.names=FALSE)









