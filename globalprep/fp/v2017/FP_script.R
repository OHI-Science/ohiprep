###############################################
## Proportion FIS/(MAR+FIS) catch to calculate
## FP
## MRF: 10/2/2017
###############################################

library(dplyr)
library(tidyr)

source('src/R/common.R')

mar <- read.csv('globalprep/mar/v2017/output/MAR_FP_data.csv')
mar <- mar %>%
  group_by(rgn_id, year) %>%
  summarize(mar_t = sum(value, na.rm=TRUE)) %>%
  select(rgn_id, year, mar_t) %>%
  ungroup()

# this one is turnign to NA in FP
filter(mar, rgn_id ==95) # ok, this makes sense

fis <- read.csv("globalprep/fis/v2017/data/FP_fis_catch.csv") %>%
  select(rgn_id, year, fis_t = fis_catch)

## weirdness with a few regions
## appears that some regions have 0 catch in 2014 catch data,
## despite having catch in previous years. 
## Going to gapfill these cases with previous years data
data.frame(filter(fis, rgn_id %in% c(3, 89, 4, 95, 105)))  ## look to see what is happening

fis <- fis %>%
  mutate(fis_t = ifelse(fis_t==0, NA, fis_t)) %>%  # 8 NA values is correct
  group_by(rgn_id) %>%
  arrange(year) %>%
  fill(fis_t)

#Check
data.frame(filter(fis, rgn_id %in% c(3, 89, 4, 95, 105)))  ## this looks correct

### adjust years so they are equivalent 
adjust <- max(mar$year) - max(fis$year)
mar <- mar %>%
  mutate(year = year - adjust)

tmp <- full_join(fis, mar, by=c('rgn_id', 'year'), all=TRUE)

tmp <- tmp %>%
  mutate(fis_t = ifelse(is.na(fis_t), 0, fis_t)) %>%
  mutate(mar_t = ifelse(is.na(mar_t), 0, mar_t)) %>%
  mutate(w_fis = fis_t/(fis_t + mar_t)) %>%
  mutate(w_fis = ifelse(mar_t==0 & fis_t == 0, NA, w_fis)) %>%
  filter(year >= 2005) %>%
  select(rgn_id, year, w_fis) 
  
hist(tmp$w_fis)

#compare to previous year data (a big jump in fish data, so not super compatible, but should be correlated at least)
old <- read.csv("globalprep/fp/v2016/output/wildcaught_weight_updated.csv") %>%
  rename(w_fis_old = w_fis) %>%
  left_join(tmp, by=c('rgn_id', 'year'))
plot(old$w_fis_old, old$w_fis)
abline(0, 1, col="red")

write.csv(tmp, 'globalprep/fp/v2017/output/wildcaught_weight.csv', row.names=FALSE)
