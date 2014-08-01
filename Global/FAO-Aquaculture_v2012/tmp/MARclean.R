##############################
# load libraries, set directories
source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)
library(stringr)
library(tidyr)

dir_d = '../ohiprep/Global/FAO-Aquaculture_v2012'

d <- read.csv(file.path(dir_d,'raw','FAO_raw_aquaculture_quant_1950_2012.csv'),check.names=F, stringsAsFactors=F) ; head(d)

# this is code for cleaning up NP data, which is also from FAO and is processed in a similar way:
# 1) deal with the weird FAO format with the ...'s and 0 0's. Modify this code for MAR prep.
# 2) carry endyear-1 value forward if endyear is NA
# 3) deal with Netherlands Antilles disag (but some regions may be reported separately already--check)
# 4) melt into long format from long format


# melt and clean up
suppressWarnings({ # warning: attributes are not identical across measure variables; they will be dropped
#  test2 = d %.%    
#     rename(c(
#       'Country (Country)'       = 'country',
#       'Species (ASFIS species)' = 'species',
#       'Aquaculture area (FAO major fishing area)'   = 'fao',
#       'Environment (Environment)' = 'environment')) %.%
#     gather(year, yield, '1950':'2012',convert=T)
#  head(test2)
  
  m = d %.%    
    rename(c(
      'Country (Country)'       = 'country',
      'Species (ASFIS species)' = 'species',
      'Aquaculture area (FAO major fishing area)'   = 'fao',
      'Environment (Environment)' = 'environment')) %.%
    melt(id=c('country','species','fao','environment'), variable='year')
m = m %.%
  filter(!country %in% c('Totals', 'Yugoslavia SFR'), environment!='Freshwater') %.%
  mutate(  
    value = str_replace(value, fixed( ' F'),    ''), # FAO denotes with F when they have estimated the value using best available data
    value = str_replace(value, fixed('0 0'), '0.1'), # FAO denotes something as '0 0' when it is > 0 but < 1/2 of a unit. 
    value = str_replace(value, fixed(  '-'),   '0'), # FAO's 0
    value = str_replace(value, fixed('...'),    NA), # FAO's NA
    value = str_replace(value, fixed('.'),      NA),
    value = ifelse(value =='', NA, value),  
    value = as.numeric(as.character(value)),
    year  = as.integer(as.character(year))) %.%       # search in R_inferno.pdf for "shame on you"
  select(country, species, fao, environment, year, value) %.%
  arrange(country, species, fao, environment, is.na(value), year) %.%
  group_by(country, species, fao, environment) %.%
  mutate(
    # gapfill: Carry previous year's value forward if value for max(year) is NA.
    #   This gives wiggle room for regions still in production but not able to report by the FAO deadline.            
#    year_max   = max(year),    # note: currenly year_max is always max year of whole dataset b/c input is wide format
    year_prev  = lag(year,  order_by=year),
    value_prev = lag(value, order_by=year),
    value_ext  =        is.na(value) & year==year_max & year_prev==year-1,
    value      = ifelse(is.na(value) & year==year_max & year_prev==year-1, value_prev, value),
    # extend all other NAs as zeros after year_beg
    year_beg   = first(year),  # since ordered by is.na(value) before year, should pickup first non-NA year      
    value      = ifelse(is.na(value) & year>year_beg, 0, value)) %.%
  # drop NAs before year_beg
  # filter(!is.na(value)) %.%
  ungroup() %.%
  arrange(country, species, fao, environment, year, value)

###########################################################################################################
## check against last year's file for country names (to match ohi id) and species names (to match sust score) changes 

## pad with 0s after earliest year of non 0 data

# For MAR, need to see which regions are reported in addition to Netherlands Antilles (may not be 4/6 like here
# antilles: break up Netherlands Antilles into the 4 of 6 regions not already included ('Aruba','Curaçao')
stopifnot( sum(c('Bonaire','Saba','Sint Maarten','Sint Eustatius') %in% m$country) == 0 )
m_ant = m %.%
  filter(country == 'Netherlands Antilles') %.%
  select(country, commodity, year, year_beg, value) %.%
  mutate(
    value            = value/4,
    'Bonaire'        = value,
    'Saba'           = value,
    'Sint Maarten'   = value,
    'Sint Eustatius' = value) %.%
  select(-value, -country) %.%
  melt(id=c('commodity','year','year_beg'), variable.name='country')
suppressWarnings({ # warning: Unequal factor levels: coercing to character
  m_a = m %.%
    filter(country != 'Netherlands Antilles') %.%
    rbind_list(m_ant)    
})

# cast wide to expand years
m_w = m_a %.%
  select(country, commodity, year, year_beg, value) %.%
  dcast(country + commodity + year_beg ~ year)

# melt long and apply 0's where NA since first available year
m_l = m_w %.%
  melt(id=c('country','commodity','year_beg'), variable='year') %.%
  mutate(year = as.integer(as.character(year))) %.%
  arrange(country, commodity, year) %.%
  filter(!is.na(value))

# rgn_id: country to rgn_id  # source('src/R/ohi_clean_fxns.R')
m_r = name_to_rgn(m_l, fld_name='country', flds_unique=c('country','commodity','year'), fld_value='value', add_rgn_name=T) %.%
  select(rgn_name, rgn_id, commodity, year, value) %.%
  arrange(rgn_name, commodity, year)

# products join
m_p = m_r %.%
  inner_join(com2prod, by='commodity') %.%
  arrange(rgn_name, product, commodity, year) %.%
  select(rgn_name, rgn_id, product, commodity, year, value)

# show max year per product, commodity
cat('\n\nShowing max(year) per product, commodity:\n')
print(m_p %.%
        group_by(product, commodity) %.%
        summarize(
          year_max = max(year)))

# product summarize
m_s = m_p %.%
  group_by(rgn_name, rgn_id, product, year) %.%
  summarize(value  = sum_na(value))

# check for duplicates
stopifnot( sum(duplicated(m_s[,c('rgn_id', 'product', 'year')])) == 0 )

# debug: wide with all commmodities and product subtotal for comparison with input data
m_d = rbind_list(
  m_p,
  mutate(m_s, commodity='Z_TOTAL')) %.%
  arrange(rgn_name, product, commodity, year) %.%
  dcast(rgn_name + rgn_id + product + commodity ~ year)
write.csv(m_d, sprintf('%s/tmp/np_harvest_%s_wide.csv', dir_d, units), row.names=F, na='')

# units: rename value field to units based on filename
m_u = rename(m_s, setNames(units, 'value'))  

# output
f_out = sprintf('%s/data/%s_%s.csv', dir_d, basename(dir_d), units)
write.csv(m_u, f_out, row.names=F, na='')
}

## save as different scenarios ----

# this code was stolen from ohiprep/Global/WorldBank-Statistics_v2012/data_prep.R as an example of how to


# example data
lf = data.frame(rgn_id = 1:20, year = 1994:2013, count = sample(1:100, 20))

# identify scenarios and max years
scenarios = list('2012a'= max(lf$year)-2,
                 '2013a'= max(lf$year)-1,
                 '2014a'= max(lf$year))

# loop through and save
for (scen in names(scenarios)){ # scen = names(scenarios)[1]
  
  yr = scenarios[[scen]]
  cat(sprintf('\nScenario %s using year == %d\n', scen, yr))
  
  lf_yr = lf %>%
    filter(year <= yr) # remove any years greater than the scenario
  stopifnot(anyDuplicated(lf_yr[,c('rgn_id', 'year')]) == 0)
  
  csv = sprintf('rgn_id_fao_mar_%s_.csv', scen) 
  write.csv(lf_yr, file.path(dir_d, 'data', csv), row.names=F)
  
}
