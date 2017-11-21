######################################
## Formatting human development data
## For post score analyses
######################################

# load libraries, set directories
library(ohicore)  #devtools::install_github('ohi-science/ohicore@dev')
library(dplyr)
library(stringr)
library(tidyr)

## comment out when knitting
setwd("globalprep/supplementary_information/v2017")


### Load FAO-specific user-defined functions
source('../../../src/R/common.R') # directory locations


### HDI data
hdi <- read.csv(file.path(dir_M, "git-annex/globalprep/_raw_data/UnitedNations_HumanDevelopmentIndex/d2017/HDI_2015_data.csv"))

hdi <- hdi %>%
  mutate(Country = as.character(Country))


### Function to convert to OHI region ID
hdi_rgn <- name_2_rgn(df_in = hdi, 
                        fld_name='Country')

## duplicates of same region
dups <- hdi_rgn$rgn_id[duplicated(hdi_rgn$rgn_id)]
hdi_rgn[hdi_rgn$rgn_id %in% dups, ]

# population weighted average:
# http://www.worldometers.info/world-population/china-hong-kong-sar-population/
# 
pops <- data.frame(Country = c("Hong Kong, China (SAR)", "China"), 
                   population = c(7346248, 1382323332))

hdi_rgn <- hdi_rgn %>%
  left_join(pops, by="Country") %>%
  mutate(population = ifelse(is.na(population), 1, population)) %>%
  group_by(rgn_id) %>%
  summarize(value = weighted.mean(HDI_2014, population)) %>%
  ungroup()
hdi_rgn[hdi_rgn$rgn_id %in% dups, ]

write.csv(hdi_rgn, "HDI_data.csv", row.names=FALSE)
