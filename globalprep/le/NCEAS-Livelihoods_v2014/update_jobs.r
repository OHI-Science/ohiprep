# update_jobs.R. 

# combine updated jobs data from FAO and WTTC (total contribution) to 2013a file 'liv_jobs.csv'
# by JStewartLowndes Mar2014.
#

# setup ----

# load libraries
library(dplyr)

# from get paths configuration based on host machine name
source('src/R/ohi_clean_fxns.R') # get functions
source('src/R/common.R') # set dir_neptune_data
# Otherwise, presume that scripts are always working from your default ohiprep folder
dir_d = 'Global/NCEAS-Livelihoods_v2014'

## read in all files ----

# 2013a file
jobs2013 = read.csv(file.path(dir_neptune_data, 
                              'model/GL-NCEAS-LayersDisaggregated_v2013a/data/liv_jobs.csv')); head(jobs2013)
jobs2013$sector = as.character(jobs2013$sector)
jobs2013 = jobs2013[jobs2013$cntry_key != '' ,]; head(jobs2013) # remove odd blanks from original file

# lookup
lkup = read.csv('src/LookupTables/cntry_rgn_2013.csv'); head(lkup)

# new data 
fismar = read.csv('Global/FAO-Fishers_v2012/data/rgn_fao_jobs_fismar_v2012.csv'); head(fismar)
tour = read.csv('Global/WTTC-Tourism_v2013/data/rgn_wttc_empt_2014a.csv') %.% # total contribution
  mutate(sector = 'tour'); head(tour)


## prepare new data ----

# rbind new data together
dnew = tour %.%
  select(rgn_id, year, sector, jobs = count) %.% 
  rbind(fismar %.%
          select(rgn_id, year, sector, jobs)) %.% 
  arrange(rgn_id, sector, year); head(dnew)

# change to cntry_key from rgn_id using lkup
dnew_tobind = dnew %.%
  left_join(lkup, by='rgn_id') %.% 
  select(cntry_key, sector, year, jobs); head(dnew_tobind)


## insert into original jobs data and save ----

# first exclude any cntry_key/sector/year combos that have updated information to prevent duplicates
jobs2013_tojoin = jobs2013 %.%
  anti_join(dnew_tobind, by=c('cntry_key', 'sector', 'year'))

# create jobs2014:: rbind old unduplicated data and new 
jobs2014 = rbind(dnew_tobind, jobs2013_tojoin) %.%
  arrange(cntry_key, sector, year); head(jobs2014)

# # check for duplicates--none
# d.dup = jobs2014[duplicated(jobs2014[,c('cntry_key', 'sector', 'year')]),]

# save 
write.csv(jobs2014, file.path(dir_d, 'data', 'cntry_jobs_2014a.csv'), na = '', row.names=F)


# ---- fin ----




