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

fis <- read.csv("globalprep/fis/v2017/data/FP_fis_catch.csv") %>%
  select(rgn_id, year, fis_t = fis_catch)

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
