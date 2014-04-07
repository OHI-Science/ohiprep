# data_prep.R

# processing and adding rgn_ids to CITES Signatories
# by JSLowndes March 2014; previously had been clean_CITES.r (by JStewart May2013)


# setup ----

# load libraries
library(dplyr)

# from get paths configuration based on host machine name
source('src/R/common.R') # set dir_neptune_data
# Otherwise, presume that scripts are always working from your default ohiprep folder
dir_d = 'Global/CITES-Signatories_v2014'

# get functions
source('src/R/ohi_clean_fxns.R')


## read in and process files ----

d = read.csv(file.path(dir_d, 'raw', 'CITES_MemberCountries_2014Mar.csv')); head(d)
d.1 = data.frame(unlist(d[,2]))
d.1$citesSignatory = rep.int(1, dim(d)[1])  
names(d.1) = c('Country', 'boolean'); head(d.1)

## run add_rgn_id and save ----
uifilesave = file.path(dir_d, 'raw', 'CITES-Signatories_v2013-cleaned.csv')
add_rgn_id(d.1, uifilesave)

## create final data layer: ----
# join CITES signatories with other rgn_ids 

# setup
dpath = 'src/LookupTables'
rf = read.csv(file.path(dpath, 'rgn_details.csv')); head(rf) # rgns file

cleaned_layer = read.csv(uifilesave); head(cleaned_layer)


# identify which rgn_ids are missing from cleaned_layer (using anti_join); then
# left_join to the UN georegions with boolean values.
cites_0 = rf %.%
  select(rgn_id, rgn_nam) %.%
  filter(rgn_id < 255) %.% # remove open ocean and disputed
  anti_join(cleaned_layer, by='rgn_id') %.%
  mutate(boolean = 0); head(cites_0) 

# combine cites 0's with  1's
cites_all = rbind(cleaned_layer, cites_0) %.%
  select(rgn_id, boolean) %.%
  arrange(rgn_id); head(cites_all)
  
f_save = 'rgn_cites_2014a.csv'
write.csv(cites_all, file.path(dir_d, 'data', f_save), na = '', row.names=FALSE)
  
  
 # --- fin




