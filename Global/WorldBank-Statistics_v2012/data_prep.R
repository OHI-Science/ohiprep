# data_prep.R. 
# Reformat and add rgn_ids for World Bank statistics data
# Previously had been named clean_BWstats.r (by JStewart May2013). This script was created by JStewartLowndes Mar2014 with improved functions by BBest in Jun2014
#   Data: 
#       * gdp: Gross Domestic Product (current \$USD)
#         + http://data.worldbank.org/indicator/NY.GDP.MKTP.CD
#       * uem = Unemployment, total (\% of total labor force) 
#         + http://data.worldbank.org/indicator/SL.UEM.TOTL.ZS
#       * tlf = Total Labor force, total (\# people)
#       	+ http://data.worldbank.org/indicator/SL.TLF.TOTL.IN
#       * ppppcgdp = GDP adjusted per capita by PPP                 # GDP per capita based on purchasing power parity (PPP).
#       	+ http://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
  

#   add OHI region_ids with name_to_rgn_id.r          ** differs from data_prep.old
#   georegional gapfilling with gapfill_georegions.R  ** differs from data_prep.old
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
  d = read.xls(f, sheet=1, skip=1, check.names=F);  head(d) # do not add the stupid X in front of the numeric column names
  
  # remove final year column if it is completely NAs
  if(nrow(d)[1] - sum(is.na(d[,ncol(d)])) == 0) {
    d = d[,-ncol(d)]
  }
    
  # melt data
  d = d %>%
    select(country=1, matches('\\d')) %>% # get first column as country and all other year columns that are \\digits in regex speak
    melt(id='country', variable='year') %>%
    arrange(country, year) %>%    
    # remove NAs
    filter(!is.na(value))  

  # add layer column, overwriting if gdp.pcap.pp
  d$layer = str_split_fixed(basename(f), fixed('.'), 3)[2]
  if (basename(f) == 'ny.gdp.pcap.pp.cd_Indicator_en_excel_v2.xls'){
    d$layer = 'gdppcppp' 
  } 
        
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
#  gdp   gdppcppp      tlf      uem 
# 9936       5147     4919     3184 

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
  # Gross Domestic Product Per Capita PPP (GDPPCPPP)
  gdppcppp = list(
    units           = 'intl_dollar', #GDP to international dollars (intl_dollar) using PPP rates. intl_dollar has same purchasing power over GDP as the USD has in the US.
    var_rgn_weights = NULL,
    ratio_weights   = FALSE),
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
  csv_attr        = sprintf('%s/data/rgn_wb_%s_2014a_ratio-gapfilled_attr.csv'   , dir_d, lyr)  
  cat(sprintf('processing %s: %s\n', lyr, basename(csv_dat)))
  
  # extract data
  d = r %>%
    filter(layer==lyr) %>%    
    select(rgn_id, year, value)
  
  #load_all('~/github/ohicore')
  source('../ohicore/R/gapfill_georegions.R')
  d_g = gapfill_georegions(
    data              = d,
    fld_id            = 'rgn_id',
    fld_value         = 'value',
    georegions        = georegions,
    rgn_weights       = rgn_weights,
    ratio_weights     = ratio_weights,
    georegion_labels  = georegion_labels,
    r0_to_NA          = TRUE, 
    attributes_csv    = csv_attr)
    
  # rename value to units
  d_g = d_g %>%
    select(rgn_id, year, value) %>%
    arrange(rgn_id, year) %>%
    rename(
      setNames(units, 'value'))
  stopifnot(anyDuplicated(sp_pressure[,c('rgn_id', 'year')]) == 0)
  # write to csv
  write.csv(d_g, csv_dat, na='', row.names=F)
}

# TODO: check that tlf < popn

## final GDPpcPPP rescaling ----


## rescale GDPpcPPP by the max of each year

ppp = read.csv(file.path(dir_d, 'data', 'rgn_wb_gdppcppp_2014a_ratio-gapfilled.csv')); head(ppp)

p = ppp %>%
  left_join(ppp %>%
              group_by(year) %>%
              summarize(max_intl_dollar = max(intl_dollar, na.rm=T)),
            by='year') %>% 
  mutate(scaled = intl_dollar/max_intl_dollar,
         value = 1-scaled); head(p); summary(p)

## save
scenarios = list('2012a'=2011,
                 '2013a'=2012,
                 '2014a'=2013)

for (scen in names(scenarios)){ # scen = names(scenarios)[1]
  
  yr = scenarios[[scen]]
  cat(sprintf('\nScenario %s using year == %d\n', scen, yr))
  
  p_yr = p %>%
    filter(year <= yr) %>% # remove any years greater than the scenario
    select(rgn_id, year, value)
  
  csv = sprintf('rgn_wb_gdppcppp_rescaled_%s.csv', scen)
  write.csv(p_yr, file.path(dir_d, 'data', csv), row.names=F)
  
}

# --- fin ---
