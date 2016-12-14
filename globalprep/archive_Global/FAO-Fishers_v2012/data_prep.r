# data_prep.R. 

# Add rgn_ids for FAO Fishers data
# by JStewartLowndes Mar2014.
#

# setup ----

# load libraries
library(mgcv) # for troubleshooting below
library(reshape2)
library(gdata)
library(dplyr)
library(zoo) # install.packages("zoo") # for na.locf: Last Observation Carried Forward

# from get paths configuration based on host machine name
source('src/R/ohi_clean_fxns.R') # get functions
source('src/R/common.R') # set dir_neptune_data
# Otherwise, presume that scripts are always working from your default ohiprep folder
dir_d = 'Global/FAO-Fishers_v2012'


# read in and process files ----
d = read.csv(file.path(dir_d, 'raw', 'Employment_for_Longo_trimCols.csv'), skip=1, check.names=F); head(d)

# some name cleaning
names(d)[1] = 'rgn_nam'
names(d)[2] = 'sector'
names(d)[3] = 'job_status'

# fill down NAs with zoo package na.locf
d$rgn_nam[d$rgn_nam == ''] = NA
d$rgn_nam = na.locf(d$rgn_nam); head(d)
d$sector[d$sector == ''] = NA
d$sector = na.locf(d$sector); head(d)

# melt together
d.m = melt(data=d, id.vars=names(d)[1:3], variable.name='year'); head(d.m)
d.m$year = as.numeric(as.character(d.m$year)); head(d.m) 

# clean up value
d.m$value[d.m$value == ''] = NA; head(d.m) 
d.m$value = gsub(',', '', d.m$value); head(d.m) 
d.m$value = as.numeric(as.character(d.m$value)); head(d.m) 

# remove several sectors; see 2012 Nature SOM
unique(d.m$sector)
d.mf = d.m %.%
  filter(sector != 'Inland Waters Fishing', sector != 'Subsistence', sector != 'Unspecified')

# deal with full time/part time stuff. See SOM Nature 2012 and README.md
unique(d.m$job_status)
d.new = d.mf %.%
  mutate(job_weight = 1); head(d.new)
d.new$job_weight[d.new$job_status == 'Part time'] = 0.6 
d.new$job_weight[d.new$job_status == 'Occasional'] = 0.15 
d.new$job_weight[d.new$job_status == 'Status Unspecified'] = 0.5

# multiply job count by job_weight, and group then sum by sector
d.tot = d.new %.%
  mutate(n_jobs_adj = as.numeric(value) * job_weight) %.%
  select(rgn_nam, year, sector, n_jobs_adj) %.% 
  group_by(rgn_nam, year, sector) %.% # this and below will group the sectors; duplicates because of the job_status
  summarise(jobs = sum(n_jobs_adj, na.rm=T)); head(d.tot)

# rearrange as add_rgn_id expects
d.fin = d.tot %.%
  select(rgn_nam, year, jobs, sector) %.%
  arrange(sector, rgn_nam, year); head(d.fin)
d.fin$rgn_nam = as.character(d.fin$rgn_nam) 

## run add_rgn_id and save ----
uifilesave = file.path(dir_d, 'raw', 'FAO-Fishers_v2012-cleaned.csv')
add_rgn_id(d.fin, uifilesave)


## check for duplicate regions, sum them ----

# explore; identify dups
dclean = read.csv(uifilesave); head(dclean); length(unique(dclean$rgn_id))
d.dup = dclean[duplicated(dclean[,c('rgn_id', 'year', 'sector')]),]; head(d.dup)
dup_ids = unique(d.dup$rgn_id) # 116, 140, 209
filter(dclean, rgn_id == 116, year == 2010)
filter(dclean, rgn_id == 140, year == 2010)
filter(dclean, rgn_id == 209, year == 2010)

# remove duplicates with sum_duplicates.r
d_fix = sum_duplicates(dclean, dup_ids, fld.nam = 'jobs'); head(d_fix)

filter(d_fix, rgn_id == 116, year == 2010)
filter(d_fix, rgn_id == 140, year == 2010)
filter(d_fix, rgn_id == 209, year == 2010)

## no gapfilling--but do save as .csv
d_fix$sector = gsub('Marine Waters Fishing', 'cf', d_fix$sector) 
d_fix$sector = gsub('Aquaculture', 'aq', d_fix$sector); head(d_fix) 

write.csv(d_fix, file.path(dir_d, 'data', 'rgn_fao_jobs_fismar_v2012.csv'), na = '', row.names=F)

# ---- fin ----


