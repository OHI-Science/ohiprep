##############################################
## Preparing mean catch data for the toolbox
## OHI 2016 global
## MRF June 9 2016
###############################################
library(dplyr)
library(tidyr)

source('../ohiprep/src/R/common.R') # set dir_neptune_data

#--------------------------
# get data and check for duplicates 
# (sample size should stay the same after below)
# --------------------------

## SAUP catch data:
data <- read.csv(file.path(dir_M, 'git-annex/globalprep/fis/v2016/int/catch_saup.csv'))

data <- data %>%
  dplyr::select(year, saup_rgn = saup_id, fao_rgn, stock_id, TaxonKey, tons) %>%
  group_by(saup_rgn, fao_rgn, TaxonKey, stock_id, year) %>%
  summarize(catch = sum(tons)) %>%
  ungroup()

## SAUP to OHI region data
region <- read.csv("globalprep/fis/v2016/int/saup_to_ohi_key.csv")
dups <- region[duplicated(region$saup_rgn), ]
region[region$saup_rgn %in% dups$saup_rgn, ] #duplicates, 
###### dups occur because a few SAUP regions have lower resolution than OHI regions.
###### This causes the sample size of the following merge to increase, but this is ok.  

#---------------------------------------------
#  Convert from SAUP regions to OHI regions
# --------------------------------------------

data_ohi_rgns <- data %>%
  left_join(region, by="saup_rgn") 


# some saup regions do not correspond to any ohi regions
# region 156 is China, but this polygon overlaps other polygons that better reflect our regions
# region 910 is South Orkney Island, which we include in our Antarctica analysis
# regions >1000 represent the high seas.  These should all be safe to cut for the eez analysis.

### correcting for saup regions represented by more than one ohi-region
### and then summing catch by ohi region (many ohi regions are composed of >1 saup region)

data_ohi_rgns <- data_ohi_rgns %>%
  filter(!is.na(ohi_rgn)) %>%
  mutate(catch_corr = catch * percent_saup) %>%
  group_by(fao_rgn, TaxonKey, stock_id, year, ohi_rgn) %>%
  summarize(catch = sum(catch_corr)) %>%
  ungroup()

#---------------------------------------------
### Calculate mean catch for ohi regions (using data from 1980 onward)
### These data are used to weight the RAM b/bmys values 
# --------------------------------------------

mean_catch <- data_ohi_rgns %>%
  filter(year >= 1980) %>%
  group_by(ohi_rgn, fao_rgn, TaxonKey, stock_id) %>%
  mutate(mean_catch=mean(catch, na.rm=TRUE))%>%
  filter(mean_catch != 0)  %>%      ## some stocks have no reported catch for time period
  ungroup()

#---------------------------------------------
# Toolbox formatting and save
# --------------------------------------------
mean_catch_toolbox <- mean_catch %>%
  mutate(taxon_key_stock = paste(TaxonKey, stock_id, sep = "-")) %>%
  dplyr::select(rgn_id=ohi_rgn, taxon_key_stock, year, mean_catch) %>%
  filter(year>=2005) %>%  # filter to include only analysis years
  data.frame()


write.csv(mean_catch_toolbox, "globalprep/fis/v2016/data/mean_catch.csv", row.names=FALSE)


#---------------------------------------------
# Get regions weights for FP weighting
# --------------------------------------------

total_catch_FP <- mean_catch %>%
  group_by(ohi_rgn, year) %>%
  summarize(fis_catch = sum(catch)) %>%
  dplyr::select(rgn_id = ohi_rgn, year, fis_catch) %>%
  filter(year >= 2005) # filter to include only the relevant analysis years

write.csv(total_catch_FP, "globalprep/fis/v2016/data/FP_fis_catch.csv", row.names=FALSE)

#############################################################
#############################################################
# Do calculations for high seas
#############################################################

data_hs_rgns <- data_ohi_rgns %>%
  filter(saup_rgn > 1000) %>%
  filter(year >= 1980) %>%
  mutate(ohi_rgn = 0) %>%
  group_by(ohi_rgn, fao_rgn, TaxonKey, stock_id) %>%
  mutate(mean_catch=mean(catch, na.rm=TRUE))%>%
  filter(mean_catch != 0)  %>%      ## some stocks have no reported catch for time period
  ungroup() %>%
  dplyr::select(fao_rgn, ohi_rgn, TaxonKey, stock_id, year, mean_catch) %>%
  data.frame()

write.csv(data_hs_rgns, "globalprep/fis/v2016/data/mean_catch_hs.csv", row.names=FALSE)

