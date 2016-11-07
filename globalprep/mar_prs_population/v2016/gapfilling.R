#########################################################
## gapfilling
##########################################################

library(dplyr)

## looked over R files and didn't seem to be any gapfilling

data <- read.csv('globalprep/coastal_population/v2015/data/rgn_popn5yrtrend_inland25mi_2011to2015.csv') %>%
  mutate(trend = 0)

write.csv(data, 'globalprep/coastal_population/v2015/data/rgn_popn5yrtrend_inland25mi_2011to2015_gf.csv', row.names=FALSE)
