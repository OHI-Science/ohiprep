### Check on largish changes from 2016 vs 2015 assessment in eez2015
library(dplyr)

new_t <- read.csv('globalprep/mar/v2016/output/mar_harvest_tonnes.csv')
rgn50_t_new <- filter(new_t, rgn_id==50 & year==2013)


old_t <- read.csv('globalprep/mar/v2015/data/mar_harvest_tonnes_2015a_lyr.csv')
rgn50_t_old <- filter(old_t, rgn_id==50 & year==2013)

#717,"Indian white prawn"


new_t <- read.csv('globalprep/mar/v2016/output/mar_harvest_tonnes.csv')
rgn207_t_new <- filter(new_t, rgn_id==207 & year==2013)


old_t <- read.csv('globalprep/mar/v2015/data/mar_harvest_tonnes_2015a_lyr.csv')
rgn207_t_old <- filter(old_t, rgn_id==207 & year==2013)
