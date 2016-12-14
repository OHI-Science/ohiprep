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
source('src/R/common.R') # set dir_neptune_data; load reshape2, plyr, dplyr
source('src/R/ohi_clean_fxns.R') # get functions
dir_a = file.path(dir_neptune_data, 'git-annex/Global/WorldBank-Statistics_v2012')
dir_d = 'Global/WorldBank-Statistics_v2012'

# load libraries
library(gdata)
library(stringr)

#library(ohicore)  # for github/ohicore/R/gapfill_georegions.R # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
library(devtools)
load_all('../ohicore')
# library(ohicore)

# read in and process files ----
if (exists('d_all')) rm(d_all)
count = 0
for (f in list.files(path = file.path(dir_a, 'raw'), pattern=glob2rx('*xls'), full.names=T)) {  # f = "/var/data/ohi/git-annex/Global/WorldBank-Statistics_v2012/raw/ny.gdp.pcap.pp.cd_Indicator_en_excel_v2.xls"
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
  
  # convert year from factor to integer
  d$year = as.integer(as.character(d$year))
  
  # rbind
  if (!exists('d_all')){
    d_all = d
  } else {
    d_all = rbind(d_all, d)
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

# remove pop from d_all
pop_csv   = d_all %>% 
  filter(layer == 'pop') %>%
  select(country, year, w_popn=value)
write.csv(pop_csv, file.path(dir_d, 'data', 'country_total_pop.csv'), row.names=F)

d_all = d_all %>% filter(layer != 'pop')


# name_to_rgn with collapse_fxn = sum_na for all but gdppcppp and uem ----
r1 = name_to_rgn(d_all %>%
                  filter(!layer %in% c('gdppcppp', 'uem')), 
                fld_name='country', flds_unique=c('country','year','layer'), fld_value='value', add_rgn_name=T, 
                collapse_fxn = 'sum_na') 

# name_to_rgn with collapse_fxn = sum_weight_by_pop for gdppcppp and uem ----

pop_csv = '~/github/ohiprep/Global/WorldBank-Statistics_v2012/data/country_total_pop.csv'

r2 = name_to_rgn(
  d_all %>%
    filter(layer %in% c('gdppcppp', 'uem')), 
  fld_name='country', flds_unique=c('country','year','layer'), fld_value='value', add_rgn_name=T,
  collapse_fxn='weighted.mean', collapse_csv=pop_csv)

# spot checks ----

# spot check Qatar and China
# r2 %>% group_by(layer, year) %>%  
#   mutate(
#     rank = row_number(desc(value))) %>%
#   filter(year==2013 & rgn_name %in% c('China','Qatar','United States'))
# 
# r2 %>%  
#   filter(layer == 'uem', year>2008 & rgn_name %in% c('China','Qatar','United States'))
# 
# # spot check uem China 
# d_all %>%  
#   filter(layer == 'uem' & 
#            year > 2008 & country %in% c('China', 'Hong Kong SAR, China', 'Macao SAR, China'))
# d_all %>%  
#   filter(layer == 'uem' & 
#            country %in% c('Guam', 'Northern Mariana Islands'))
# 
# d_all %>%  
#   filter(layer == 'uem' & 
#            country %in% c('Taiwan'))


# rbind together from name_to_rgn calls ----
r = rbind(r1, r2)

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
  stopifnot(anyDuplicated(d_g[,c('rgn_id', 'year')]) == 0)
  # write to csv
  write.csv(d_g, csv_dat, na='', row.names=F)
}

# TODO: check that tlf < popn

## final GDPpcPPP rescaling; save for each scenario ----


## rescale GDPpcPPP by the max of each year

ppp = read.csv(file.path(dir_d, 'data', 'rgn_wb_gdppcppp_2014a_ratio-gapfilled.csv')); head(ppp)

p = ppp %>%
  left_join(ppp %>%
              group_by(year) %>%
              summarize(max_intl_dollar = max(intl_dollar, na.rm=T)),
            by='year') %>% 
  mutate(value = intl_dollar/max_intl_dollar); head(p); summary(p)

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


## translate total labor force to cntry_key; save for each scenario ----
# this makes obsolete the troublesome (because of NAs and unknown origin) file on Neptune: model/GL-NCEAS-LayersDisaggregated_v2013a/data/le_workforcesize_adj.csv 

fname = 'rgn_wb_tlf_2014a_ratio-gapfilled.csv'
f = read.csv(file.path(dir_d, 'data', fname)); head(f)
cp = read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-CoastalPopulation_v2013/data', 
                                 'cntry_popsum2013_inland25mi_complete.csv'))

lf1 = f %>%
  select(rgn_id, year, count) %>%
  left_join(cp,
            by = 'rgn_id') 

lf = lf1 %>%
  left_join(lf1 %>%
              group_by(rgn_id, year) %>%
              summarize(rgn_popsum = sum(cntry_popsum2013_inland25mi)), 
            by = c('rgn_id', 'year')) %>%
  mutate(pop_ratio = cntry_popsum2013_inland25mi/rgn_popsum,
         jobs = count * pop_ratio) %>% # debug: lf %>% filter(pop_ratio != 1) see below
  select(cntry_key, year, jobs) %>%
  arrange(cntry_key, year); head(lf); summary(lf)

# show which rgn_ids and cntry_keys are split with pop_ratio
# lf %>% filter(pop_ratio != 1)
# 13  GUM
# 13  MNP
# 116 PRI
# 116 VIR
# 137 ECU
# 137 Galapagos Islands
# 140 MTQ
# 140 GLP
# 163 Hawaii
# 163 USA
# 163 Alaska
# 171 Trindade
# 171 BRA
# 224 Easter Island
# 224 CHL

# lf %>% filter(rgn_id == 163, year == 2012)
# lf %>% filter(rgn_id == 171, year == 2012)


# save for each scenario 

scenarios = list('2012a'= max(lf$year, na.rm=T)-2,
                 '2013a'= max(lf$year, na.rm=T)-1,
                 '2014a'= max(lf$year, na.rm=T))

for (scen in names(scenarios)){ # scen = names(scenarios)[1]
  
  yr = scenarios[[scen]]
  cat(sprintf('\nScenario %s using year == %d\n', scen, yr))
  
  lf_yr = lf %>%
    filter(year <= yr) # remove any years greater than the scenario
  stopifnot(anyDuplicated(lf_yr[,c('cntry_key', 'year')]) == 0)
  
  csv = sprintf('cntry_wb_tlf_%s_ratio-gapfilled.csv', scen) # file_path_sans_ext(fname)
  write.csv(lf_yr, file.path(dir_d, 'data', csv), row.names=F)
  
}


# --- fin ---
