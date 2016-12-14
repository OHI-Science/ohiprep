# update_rev.R. 

# combine updated revenue data from WTTC (total contribution to gdp) to 2013a file 'eco_rev_2013a.csv'
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
rev2013 = read.csv(file.path(dir_neptune_data, 
                              'model/GL-NCEAS-Livelihoods_v2013a/data/eco_rev_2013a.csv')); head(rev2013)
rev2013$sector = as.character(rev2013$sector)

# lookup
lkup = read.csv('src/LookupTables/cntry_rgn_2013.csv'); head(lkup)

# new data 
tour = read.csv('Global/WTTC-Tourism_v2013/data/rgn_wttc_gdpt_2014a.csv') %.% # total contribution
  mutate(sector = 'tour'); head(tour)


## prepare new data ----

# change to cntry_key from rgn_id using lkup
dnew_tobind = tour %.%
  left_join(lkup, by='rgn_id') %.% 
  select(cntry_key, sector, year, USD); head(dnew_tobind)


## insert into original jobs data and save ----

# remove duplicates from original rev2013 file
chn = filter(rev2013, cntry_key == 'CHN') %.%
  group_by(cntry_key, year, sector) %.% # this and below will group the sectors; duplicates because of the job_status
  summarise(rev = sum(USD, na.rm=T)) %.%
  select(cntry_key, year, sector, USD = rev); head(chn)

rev2013fix = data.frame(rbind(rev2013 %.% 
                                filter(cntry_key != 'CHN'), 
                              chn)) %.%
  arrange(cntry_key); head(rev2013fix)

# then exclude any cntry_key/sector/year combos that have updated information to prevent duplicates
rev2013_tojoin = rev2013fix %.%
  anti_join(dnew_tobind, by=c('cntry_key', 'sector', 'year'))

# create jobs2014:: rbind old unduplicated data and new 
rev2014 = rbind(dnew_tobind, rev2013_tojoin) %.%
  arrange(cntry_key, sector, year); head(rev2014)

# # check for duplicates--just Guadeloupe, Martinique (fine), and Montenegro?
# d.dup = rev2014[duplicated(rev2014[,c('cntry_key', 'sector', 'year')]),]

# save 
write.csv(rev2014, file.path(dir_d, 'data', 'cntry_rev_2014a.csv'), na = '', row.names=F)


# ---- fin ----




