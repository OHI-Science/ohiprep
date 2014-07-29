# data_prep.r


# this is code for cleaning up NP data, which is also from FAO and is processed in a similar way:
# 1) deal with the weird FAO format with the ...'s and 0 0's. Modify this code for MAR prep.
# 2) carry endyear-1 value forward if endyear is NA
# 3) deal with Netherlands Antilles disag (but some regions may be reported separately already--check)
# 4) melt into long format from long format


# melt and clean up
suppressWarnings({ # warning: attributes are not identical across measure variables; they will be dropped
  m = d %.%    
    rename(c(
      'Country (Country)'       = 'country',
      'Commodity (Commodity)'   = 'commodity',
      'Trade flow (Trade flow)' = 'trade')) %.%
    melt(id=c('country','commodity','trade'), variable='year')})
m = m %.%
  filter(!country %in% c('Totals', 'Yugoslavia SFR')) %.%
  mutate(  
    value = str_replace(value, fixed( ' F'),    ''), # FAO denotes with F when they have estimated the value using best available data
    value = str_replace(value, fixed('0 0'), '0.1'), # FAO denotes something as '0 0' when it is > 0 but < 1/2 of a unit. 
    value = str_replace(value, fixed(  '-'),   '0'), # FAO's 0
    value = str_replace(value, fixed('...'),    NA), # FAO's NA
    value = str_replace(value, fixed('.'),      NA),
    value = ifelse(value =='', NA, value),  
    value = as.numeric(as.character(value)),
    year  = as.integer(as.character(year))) %.%       # search in R_inferno.pdf for "shame on you"
  select(country, commodity, year, value) %.%
  arrange(country, commodity, is.na(value), year) %.%
  group_by(country, commodity) %.%
  mutate(
    # gapfill: Carry previous year's value forward if value for max(year) is NA.
    #   This gives wiggle room for regions still in production but not able to report by the FAO deadline.            
    year_max   = max(year),    # note: currenly year_max is always max year of whole dataset b/c input is wide format
    year_prev  = lag(year,  order_by=year),
    value_prev = lag(value, order_by=year),
    value_ext  =        is.na(value) & year==year_max & year_prev==year-1,
    value      = ifelse(is.na(value) & year==year_max & year_prev==year-1, value_prev, value),
    # extend all other NAs as zeros after year_beg
    year_beg   = first(year),  # since ordered by is.na(value) before year, should pickup first non-NA year      
    value      = ifelse(is.na(value) & year>year_beg, 0, value)) %.%
  # drop NAs before year_beg
  filter(!is.na(value)) %.%
  ungroup() %.%
  arrange(country, commodity, year)

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
