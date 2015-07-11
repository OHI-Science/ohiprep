################################################################
## Preparing the b/bmsy values calculated using cmsy technique
################################################################
library(dplyr)
library(tidyr)
library(zoo)

## 1. take the 5 year running average of b/bmsy values to smooth data
constrained <- read.csv('globalprep/SAUP_FIS/tmp/b_bmsy_scores_constrained.csv')
uniform <- read.csv('globalprep/SAUP_FIS/tmp/b_bmsy_scores_uniform.csv')

new_b_bmsy <- function(b_bmsy=constrained){
  b_bmsy <- b_bmsy %>%
    select(stock_id, year, bbmsy_mean, prior) %>%
    arrange(stock_id, year) %>%
    group_by(stock_id) %>%
    mutate(mean_5year = rollmean(bbmsy_mean, 5, align="right", fill=NA))
  write.csv(b_bmsy, sprintf('globalprep/SAUP_FIS/tmp/b_bmsy_scores_%s_mean5yrs.csv', unique(b_bmsy$prior)), row.names=FALSE)
} 

new_b_bmsy(constrained)
new_b_bmsy(uniform)

## 2. CMSY b/bmsy data selection based on stock resilience scores
# B/bmsy data is generated using different models 
# depending on the resilience score of the stock

# resilience scores to select the appropriate b/bmsy 
res <- read.csv("globalprep/SAUP_FIS/tmp/stock_resil_06cutoff_2015.csv")

b_bmsy_uniform <- read.csv("globalprep/SAUP_FIS/tmp/b_bmsy_scores_uniform_mean5yrs.csv")
b_bmsy_uniform <- b_bmsy_uniform %>%
  select(stock_id, year, b_bmsy_uniform=mean_5year) 

b_bmsy_constrained <- read.csv("globalprep/SAUP_FIS/tmp/b_bmsy_scores_constrained_mean5yrs.csv")
b_bmsy_constrained <- b_bmsy_constrained %>%
  select(stock_id, year, b_bmsy_constrained=mean_5year) 


setdiff(res$stock_id, b_bmsy_constrained$stock_id)
setdiff(b_bmsy_constrained$stock_id, res$stock_id)
setdiff(res$stock_id, b_bmsy_uniform$stock_id)
setdiff(b_bmsy_uniform$stock_id, res$stock_id)

bmsy <- b_bmsy_uniform %>%
  left_join(b_bmsy_constrained, by=c("stock_id", "year")) %>%
  left_join(res, by="stock_id")

bmsy <- bmsy %>%
  mutate(b_bmsy = ifelse(unif_prior==1, b_bmsy_uniform, b_bmsy_constrained)) 

bmsy <- separate(bmsy, stock_id, c("TaxonKey", "fao_id")) %>%
  mutate(TaxonKey = as.integer(TaxonKey),
         fao_id = as.integer(fao_id)) %>%
  dplyr::select(TaxonKey, fao_id, year, b_bmsy); head(bmsy)

#### Expand so that each stock_region_id has a value (previously only needed stock_id)
## This is done because the RAM data are reported at this 
## resolution.

catch <- read.csv('globalprep/SAUP_FIS/data/mean_catch.csv')
catch <- separate(catch_raw, fao_ohi_id, c("fao_id", "rgn_id")) %>%
  mutate(fao_id = as.numeric(fao_id)) %>%
  mutate(rgn_id = as.numeric(rgn_id))
catch <- separate(catch, taxon_name_key, c("TaxonName", "TaxonKey"), sep="_") %>%
  mutate(TaxonKey = as.numeric(TaxonKey)) 
  
catch <- select(catch, TaxonKey, fao_id, rgn_id, year); head(catch); summary(catch)

dim(unique(catch))

bmsy_fao_rgn <- catch %>%
  left_join(bmsy) %>%
  filter(!is.na(b_bmsy)) %>%
  filter(year > 2006)
head(bmsy_fao_rgn)
summary(bmsy_fao_rgn)

# NA catch data: non-species catch, species with < 10 years non-zero data
summary(bmsy_fao_rgn[is.na(bmsy_fao_rgn$b_bmsy), ])
bmsy_fao_rgn[is.na(bmsy_fao_rgn$b_bmsy) & bmsy_fao_rgn$TaxonKey>=600000, ]
source('../ohiprep/src/R/common.R') # set dir_neptune_data
data <- read.csv(file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_data_2015/tmp/Catch_Value_11062015_summary.csv')) 
data[data$TaxonKey == 690177 & data$FAOAreaID == 71, ] #rgn_id=7 is saup_id=90 


filter(bmsy_fao_rgn, fao_id==37 & TaxonKey==600030 & year==2006) #should all have the same value (and they do)

### Read in RAM data and replace where possible
ram <- read.csv('globalprep/SAUP_FIS/tmp/RAM_fao_ohi.csv') %>%
  select(TaxonKey=Taxonid, fao_id=FAOAreaID, rgn_id=ohi_id_2013, year=scenarioYear, ram_b_bmsy=bbmsy) %>%
  filter(!is.na(ram_b_bmsy)); head(ram); summary(ram)

bmsy_final <- bmsy_fao_rgn %>%
  left_join(ram)

sum(!is.na(bmsy_final$ram_b_bmsy)) #6994/52178, about 13% of Taxon/fao/rgn records ram b/bmsy data 

bmsy_final$bbmsy <- ifelse(is.na(bmsy_final$ram_b_bmsy), bmsy_final$b_bmsy, bmsy_final$ram_b_bmsy) 
bmsy_final <- bmsy_final %>%
  mutate(fao_ohi_id = paste(fao_id, rgn_id, sep='_')) %>%
  select(fao_ohi_id, TaxonKey, year, b_bmsy=bbmsy); head(bmsy_final); summary(bmsy_final)

write.csv(bmsy_final, 'globalprep/SAUP_FIS/data/fnk_fis_b_bmsy_lyr.csv', row.names=F, na='')

#### Check against catch data
catch <-  read.csv('globalprep/SAUP_FIS/data/mean_catch.csv'); head(catch)
catch <- separate(catch, taxon_name_key, c("TaxonName", "TaxonKey"), sep="_")
catch$stock_ohi_id <- paste(catch$TaxonKey, catch$fao_ohi_id, sep="_")

bmsy_final$stock_ohi_id <- paste(bmsy_final$TaxonKey, bmsy_final$fao_ohi_id, sep="_") 
setdiff(bmsy_final$stock_ohi_id, catch$stock_ohi_id)  #should all be in there...and they are...
