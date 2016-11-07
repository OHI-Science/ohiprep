###############################################
## Proportion FIS/(MAR+FIS) catch to calculate
## FP
## MRF: Jul 28 2015
###############################################
library(dplyr)
library(tidyr)

source('src/R/common.R')

mar <- read.csv('globalprep/mariculture_status/data/MAR_FP_data.csv')
mar <- mar %>%
  group_by(id, year) %>%
  summarize(mar_t = sum(value, na.rm=TRUE)) %>%
  select(rgn_id=id, year, mar_t) %>%
  ungroup()

fis <- read.csv("globalprep/SAUP_FIS/v2015/data/FP_fis_data.csv")
fis <- fis %>%
  separate(fao_ohi_id, c("fao_id", "rgn_id"), sep="_") %>%
  mutate(rgn_id = as.numeric(as.character(rgn_id))) %>%
  group_by(rgn_id, year) %>%
  summarize(fis_t = sum(catch, na.rm=TRUE)) %>%
  ungroup()


weight <- merge(mar, fis, by=c('rgn_id', 'year'), all=TRUE)
weight <- weight %>%
  filter(year >= 1980) %>%
  mutate(fp_weight = fis_t/sum(c(fis_t, mar_t, na.rm=TRUE)))

weight[weight$rgn_id == 227, ]
weight[weight$rgn_id == 232, ]

## function to combine different years of data:
extract <- function(scenarioYear, marYear, fisYear){ #scenarioYear=2015; marYear=2013; fisYear=2010
fis_tmp <- fis[fis$year == fisYear, ] %>%
  select(rgn_id, fis_t)

mar_tmp <- mar[mar$year == marYear, ] %>%
  select(rgn_id, mar_t)

tmp <- merge(fis_tmp, mar_tmp, by='rgn_id', all=TRUE)

tmp <- tmp %>%
  mutate(mar_t = ifelse(is.na(mar_t), 0, mar_t)) %>%
  mutate(w_fis = fis_t/(fis_t + mar_t)) %>%
  select(rgn_id, w_fis) 
  
#hist(tmp$w_fis)

write.csv(tmp, sprintf('globalprep/food_provision/data/wildcaught_weight_v%s.csv', scenarioYear), row.names=FALSE)
}
  
extract(scenarioYear=2015, marYear=2013, fisYear=2010)
extract(scenarioYear=2014, marYear=2012, fisYear=2010)
extract(scenarioYear=2013, marYear=2011, fisYear=2010)
extract(scenarioYear=2012, marYear=2010, fisYear=2009)
