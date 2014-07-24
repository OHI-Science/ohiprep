# check_le_workforcesize_adj.r

# setup
source('src/R/common.R') # set dir_neptune_data; load reshape2, plyr, dplyr
w = read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-LayersDisaggregated_v2013a/data/le_workforcesize_adj.csv'), na.strings=''); head(w); summary(w)

# check for duplicates
stopifnot(anyDuplicated(w[,c('cntry_key', 'year')]) == 0)
w[duplicated(w[,c('cntry_key', 'year')]),]

w_na = w %>%
  filter(is.na(cntry_key)) %>%
  group_by(cntry_key) %>%
  summarize(count = n())

w_notna = w %>%
  filter(!is.na(cntry_key)) %>%
  group_by(cntry_key) %>%
  summarize(count = n())

filter(w, cntry_key == 'ARG')

# So two things here: 
# why can OHI2013 read the same file with no problem?
# what happened to this file--why so many NA's? 
# solution July 24: use total labor force (World Bank) instead: 6 more years of data, and known origin. this is done in ohiprep/Global/WorldBank-Statistics_v2012/data_prep.R