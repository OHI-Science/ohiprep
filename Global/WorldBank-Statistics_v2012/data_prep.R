# data_prep.R. 
# Reformat and add rgn_ids for World Bank statistics data
# Previously had been named clean_BWstats.r (by JStewart May2013). This script was created by JStewartLowndes Mar2014 with improved functions by BBest in Jun2014
#   Data: 
#         GDP = Gross Domestic Product (current $USD)
#         LAB = Labor force, total (# people)
#         UEM = Unemployment, total (% of total labor force)
#         PPP = Purchase power parity
#         PPPpcGDP = GDP adjusted per capita by PPP     
#         POP = Total population count

#   add OHI region_ids with name_to_rgn_id.r  ** differs from data_prep.old
#   georegional gapfilling with gapfill_georegions.R ** differs from data_prep.old
#
# TODO: manually fix any missing GDP with Wikipedia lookups

# setup ----

# from get paths configuration based on host machine name
setwd('~/github/ohiprep')
source('src/R/common.R') # set dir_neptune_data; load reshape2, plyr, dplyr
source('src/R/ohi_clean_fxns.R') # get functions
dir_d = 'Global/WorldBank-Statistics_v2012'

# load libraries
library(gdata)
library(stringr)

#library(ohicore)  # for github/ohicore/R/gapfill_georegions.R # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
library(devtools)
load_all('~/github/ohicore')

# read in and process files ----
if (exists('d_all')) rm(d_all)
count = 0
for (f in list.files(path = file.path(dir_d, 'raw'), pattern=glob2rx('*xls'), full.names=T)) {  # f = "Global/WorldBank-Statistics_v2012/raw/sl.tlf.totl.in_Indicator_en_excel_v2.xls"
  cat(sprintf('processing %s\n', basename(f)))
  
  count = count + 1
  #d = read.xls(file.path(dir_d, 'raw', f), sheet=1, skip=1, check.names=F) # do not add the stupid X in front of the numeric column names
  d = read.xls(f, sheet=1, skip=1, check.names=F);  head(d) # do not add the stupid X in front of the numeric column names
  
  # remove final year column if it is completely NAs
  if(nrow(d)[1] - sum(is.na(d[,ncol(d)])) == 0) {
    d = d[,-ncol(d)]
  }
  
  #   # remove any countries that have no data for the whole dataset:
  #   d.1 = matrix(nrow=0, ncol=0)
  #   for(i in 1:dim(d)[1]){             # d = d[complete.cases(d),] # suboptimal: this removes anytime there are missing values
  #     bb = dim(d)[2] - sum(is.na(d[i,]))
  #     if(bb != 2) { # this means just the countryname and country code name are not NA
  #       d.1 = rbind(d.1,d[i,])
  #     }
  #   }
  #   # BB: WOOPS! This didn't remove Aruba or Andorra for sl.tlf.totl.in_Indicator_en_excel_v2.xls
  
  # melt data
  d = d %>%
    select(country=1, matches('\\d')) %>% # get first column as country and all other year columns that are \\digits in regex speak
    melt(id='country', variable='year') %>%
    arrange(country, year) %>%    
    # remove NAs
    filter(!is.na(value))  

  # add layer column
  d$layer = str_split_fixed(basename(f), fixed('.'), 3)[2]
        
  # rbind
  if (!exists('d_all')){
    d_all = d
  } else {
    d_all = rbind_list(d_all, d)
  }
}

filter(d_all, country=='Albania' & year==1990)

# remove Channel Islands and Isle of Man
d_all = d_all %>%
  filter(!country %in% c('Channel Islands', 'Isle of Man'))
  
# Print out all the unique indicators
print('These are all the variables with year counts included in the cleaned file: ')
print(table(d_all$layer))
#  gdp  tlf  uem 
# 9936 4919 3184 

# add rgn_id: country to rgn_id  # source('../ohiprep/src/R/ohi_clean_fxns.R') ----
source('src/R/ohi_clean_fxns.R') # get functions
r = name_to_rgn(d_all, fld_name='country', flds_unique=c('country','year','layer'), fld_value='value', add_rgn_name=T) 

# remove Antarctica[213] and DISPUTED[255]
r = r %>%
  filter(!rgn_id %in% c(213,255))

# georegional gapfilling with gapfill_georegions.r ----

# read in lookups
georegions = read.csv('../ohi-global/eez2013/layers/rgn_georegions.csv', na.strings='') %.%
  dcast(rgn_id ~ level, value.var='georgn_id')

georegion_labels = read.csv('../ohi-global/eez2013/layers/rgn_georegion_labels.csv') %.%    
  mutate(level_label = sprintf('%s_label', level)) %.%
  dcast(rgn_id ~ level_label, value.var='label') %.%
  left_join(
    read.csv('../ohi-global/eez2013/layers/rgn_labels.csv') %.%
      select(rgn_id, v_label=label),
    by='rgn_id') %.%
  arrange(r0_label, r1_label, r2_label, v_label); head(georegion_labels)

# get global population (layer le_popn)
popn = read.csv(sprintf('%s/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_pop_2013a_updated.csv', dir_neptune_data)) %>%
  group_by(rgn_id) %>%
  summarize(popn = last(count, order_by=year))

# setup list to process gapfilling of layers
layers = list(  
  # Total Labor Force (TLF)
  tlf = list(
    units           = 'count',
    var_rgn_weights = 'popn',  # NULL or string name of data.frame variable having columns: rgn_id, [any var name as weight]
    ratio_weights   = TRUE),   # TRUE to multiply by ratio w_i / w_regional in cases where value is a relative total, eg total labor force
  # Gross Domestic Product (GDP)
  gdp = list(
    units           = 'usd',
    var_rgn_weights = 'popn',
    ratio_weights   = TRUE),
  # Unemployment Rate (UEM)
  uem = list(
    units           = 'percent',
    var_rgn_weights = 'popn',
    ratio_weights   = FALSE))

for (lyr in names(layers)){ # lyr='tlf'

  # setup vars
  units           = layers[[lyr]][['units']]
  ratio_weights   = layers[[lyr]][['ratio_weights']]
  var_rgn_weights = layers[[lyr]][['var_rgn_weights']]
  if (is.null(var_rgn_weights)){
    rgn_weights = NULL
  } else {
    rgn_weights = get(var_rgn_weights)
  }
  csv_dat         = sprintf('%s/data/rgn_wb_%s_2014a_ratio-gapfilled.csv', dir_d, lyr)
  csv_attr        = sprintf('%s/data/rgn_wb_%s_2014a_ratio-gapfilled-attr.csv'   , dir_d, lyr)  
  cat(sprintf('processing %s: %s\n', lyr, csv_dat))
  
  # extract data
  d = r %>%
    filter(layer==lyr) %>%    
    select(rgn_id, year, value)
  
  #load_all('~/github/ohicore')
  d_g = gapfill_georegions(
    data              = d,
    fld_id            = 'rgn_id',
    fld_value         = 'value',
    georegions        = georegions,
    rgn_weights       = rgn_weights,
    ratio_weights     = ratio_weights,
    georegion_labels  = georegion_labels,
    r0_to_NA          = TRUE, 
    attributes_csv    = (sprintf('%s/data/rgn_wb_tlf_2014a_ratio-gapfilled_attr.csv', dir_d, lyr)))
    
  # rename value to units
  d_g = d_g %>%
    select(rgn_id, year, value) %>%
    arrange(rgn_id, year) %>%
    rename(
      setNames(units, 'value'))
  
  # write to csv
  write.csv(d_g, csv_dat, na='', row.names=F)
}

# TODO: check that tlf < popn

# --- fin

# # compare gapfill_georegions.r by BB to add_gapfill.r by JSL
# 
# # unemployment
# gg = read.csv('Global/WorldBank-Statistics_v2012/data/rgn_wb_uem_2014a.csv'); head(gg)
# ag = read.csv('Global/WorldBank-Statistics_v2012/tmp/rgn_wb_uem_2014awith_add_gapfill.csv'); head(ag)
# 
# vs = gg %.%
#   select(rgn_id, 
#          year,
#          value_gg = value) %.%
#   left_join(ag %.%
#               select(rgn_id, 
#                      year, 
#                      value_ag = perc), 
#             by = c('rgn_id', 'year')) %.%
#   mutate(
#     val_dif    = value_gg - value_ag,
#     val_notna  = is.na(value_gg)!=is.na(value_ag)) %.%   
#   filter(abs(val_dif) > 0.01 | val_notna == T) 
#   
# van = vs %.%
#   filter(rgn_id == 6)
# #               arrange(goal, desc(dimension), desc(score_notna), desc(abs(score_dif))) %.%
# #         select(goal, dimension, region_id, region_label, score_old, score, score_dif)
#               


# other trouble shooting-- this actually doesn't work because ohicore requires these packages. So this is not the problem. 
#     # ensure dplyr's summarize overrides plyr's summarize by loading in succession
#     if ('package:reshape2'  %in% search()) detach('package:reshape2')
#     if ('package:plyr'      %in% search()) detach('package:plyr')
#     if ('package:dplyr'     %in% search()) detach('package:dplyr')
#     library(reshape2)
#     library(plyr)
#     library(dplyr)