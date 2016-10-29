##############################################
## Preparing mean catch data for ohi-global
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

catch <- catch <- read.csv(file.path(dir_M,'git-annex/globalprep/fis/v2016/int/spatial_catch_saup.csv')) %>%
  rename(common = Common_Name, fao_id = fao_rgn, species=Scientific_Name)
summary(catch)

## filter out non ohi eez regions
catch <- catch %>%
  filter(!is.na(rgn_id)) %>%
  filter(!is.na(fao_id)) %>%
  filter(rgn_id <= 250) %>%
  filter(rgn_id != 213)

catch <- catch %>%
  dplyr::select(year, rgn_id, fao_id, stock_id, TaxonKey, tons) %>%
  group_by(rgn_id, fao_id, TaxonKey, stock_id, year) %>%
  summarize(catch = sum(tons)) %>%
  ungroup()

data.frame(filter(catch, stock_id == "Elasmobranchii-57" & rgn_id==1))
data.frame(filter(catch, stock_id == "Carcharhinidae-57" & rgn_id==1))
#---------------------------------------------
# for years with no reported catch, add zero values
# (after first reported catch)
# --------------------------------------------

## these data have no zero catch values, so this is added here:
catch_zeros <- catch %>%
  spread(year, catch) %>%
  data.frame() %>%
  gather("year", "catch", num_range("X", 1950:2010)) %>%
  mutate(year = as.numeric(gsub("X", "", year))) %>%
  mutate(catch = ifelse(is.na(catch), 0, catch))

## this part eliminates the zero catch values prior to the first reported non-zero catch   
catch_zeros <- catch_zeros %>%
  group_by(fao_id, TaxonKey, stock_id, rgn_id) %>%
  arrange(year) %>%
  mutate(cum_catch = cumsum(catch)) %>%
  filter(cum_catch > 0) %>%
  select(-cum_catch) %>%
  ungroup()


#---------------------------------------------
### Calculate mean catch for ohi regions (using data from 1980 onward)
### These data are used to weight the RAM b/bmys values 
# --------------------------------------------

mean_catch <- catch_zeros %>%
  filter(year >= 1980) %>%
  group_by(rgn_id, fao_id, TaxonKey, stock_id) %>%
  mutate(mean_catch=mean(catch, na.rm=TRUE))%>%
  filter(mean_catch != 0)  %>%      ## some stocks have no reported catch for time period
  ungroup()
filter(mean_catch, stock_id == "Elasmobranchii-57" & rgn_id==1)

data.frame(filter(mean_catch, stock_id == "Carcharhinidae-57" & rgn_id==1))

#---------------------------------------------
# Toolbox formatting and save
# --------------------------------------------
mean_catch_toolbox <- mean_catch %>%
  mutate(stock_id_taxonkey = paste(stock_id, TaxonKey, sep="_")) %>%
  dplyr::select(rgn_id, stock_id_taxonkey, year, mean_catch) %>%
  filter(year>=2001) %>%  # filter to include only analysis years
  data.frame()


write.csv(mean_catch_toolbox, "globalprep/fis/v2016/data/mean_catch.csv", row.names=FALSE)


#---------------------------------------------
# Get regions weights for FP weighting
# --------------------------------------------

total_catch_FP <- mean_catch %>%
  group_by(rgn_id, year) %>%
  summarize(fis_catch = sum(catch)) %>%
  dplyr::select(rgn_id, year, fis_catch) %>%
  filter(year >= 2005) # filter to include only the relevant analysis years

write.csv(total_catch_FP, "globalprep/fis/v2016/data/FP_fis_catch.csv", row.names=FALSE)

#############################################################
#############################################################
# Do calculations for high seas
# NOTE: will do this later on...
#############################################################

# data_hs_rgns <- data %>%
#   filter(saup_rgn > 1000) %>%
#   group_by(fao_rgn, TaxonKey, stock_id) %>%
#   arrange(year) %>%
#   mutate(cum_catch = cumsum(catch)) %>%
#   filter(cum_catch > 0) %>%
#   select(-cum_catch) %>%
#   ungroup()
# 
# 
# data_hs_rgns <- data_hs_rgns %>%
#   filter(year >= 1980) %>%
#   mutate(ohi_rgn = 0) %>%
#   group_by(ohi_rgn, fao_rgn, TaxonKey, stock_id) %>%
#   mutate(mean_catch=mean(catch, na.rm=TRUE))%>%
#   filter(mean_catch != 0)  %>%      ## some stocks have no reported catch for time period
#   ungroup() %>%
#   dplyr::select(fao_rgn, ohi_rgn, TaxonKey, stock_id, year, mean_catch) %>%
#   data.frame()
# 
# write.csv(data_hs_rgns, "globalprep/fis/v2016/data/mean_catch_hs.csv", row.names=FALSE)
