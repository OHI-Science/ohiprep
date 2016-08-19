mar_split_antilles <- function(m) {
  ### Deal with special cases of countries, specific to MAR: Netherlands Antilles reported multiple ways, including 'Bonaire/S.Eustatius/Saba' 
  ### - FAO reports 'Antilles' as one region, but OHI considers as four 
  ###   reported regions; break up and distribute values 
  
 # stopifnot( sum(c('Bonaire','Saba','Sint Maarten','Sint Eustatius') %in% m$country) == 0 )
  m_ant <- m %>%
    filter(country == 'Netherlands Antilles') %>%  # Conch was cultivated for restoration purposes in a joint programme across these 3 countries
    mutate(
      value            = value/3,
      'Aruba'        = value,
      'Bonaire'           = value,
      'Curacao'   = value) %>%
    select(-value, -country) %>%
    gather(country, value, -species, -fao, -environment, -year) %>%
    mutate(country = as.character(country)) %>% # otherwise, m_ant$country is factor; avoids warning in bind_rows bel
    arrange(country, species, fao, environment, year, value)
  m <- m %>%
    filter(country != 'Netherlands Antilles') %>%
#    bind_rows(m_ant) # commented out until you update dplyr 
    arrange(country, species, fao, environment, year, value) %>% # replace with bind_rows command once updated dplyr  
    rbind(m_ant) # replace with bind_rows command once updated dplyr

m_ant2 <- m %>%
  filter(country == 'Bonaire/S.Eustatius/Saba') %>%  # Cobia was probably mostly in Curacao, but can't find evidence for it
  mutate(
    value            = value/3,
    'Bonaire'        = value,
    'Saba'           = value,
    'Sint Eustatius'   = value) %>%
  select(-value, -country) %>%
  gather(country, value, -species, -fao, -environment, -year) %>%
  mutate(country = as.character(country))  # otherwise, m_ant$country is factor; avoids warning in bind_rows bel
m <- m %>%
  filter(country != 'Bonaire/S.Eustatius/Saba') %>%
  #    bind_rows(m_ant) # commented out until you update dplyr 
  arrange(country, species, fao, environment, year, value) %>% # replace with bind_rows command once updated dplyr  
  rbind(m_ant2) # replace with bind_rows command once updated dplyr

m_ant3 <- m %>%
  filter(country == 'Channel Islands') %>%
  mutate(
    value            = value/2,
    'Guernsey'        = value,
    'Jersey'           = value) %>%
  select(-value, -country) %>%
  gather(country, value, -species, -fao, -environment, -year) %>%
  mutate(country = as.character(country))  
m <- m %>%
  filter(country != 'Channel Islands') %>%
  #    bind_rows(m_ant) # commented out until you update dplyr 
  arrange(country, species, fao, environment, year, value) %>% # replace with bind_rows command once updated dplyr  
  rbind(m_ant3) # replace with bind_rows command once updated dplyr


  return(m)
}

#### 
# make country to region function - for now hard coded in MARclean_2015

######
# h <- h %>% np_harvest_preclip
# ### clips out years prior to first reporting year, for each commodity per region
# 
# h <- h %>% np_harvest_gapflag  
# ### Adds flag for required gap-filling, based upon NAs in data. 
# ### NOTE: Does not perform any gap-filling.
# ### At this point, h includes: 
# ###   rgn_name   rgn_id   commodity   product   year   tonnes   usd   gapfill
# ### 'gapfill' will be in (zerofill, endfill, tbd, none)
# 
# data_check <- h %>% np_datacheck()
# ### for each commodity within each region, creates summary info:
# ###   num_years:        the length of the data series for this commodity in this region
# ###   usd_unique_nz:    (or 'tns') number of unique non-zero values for usd or tonnes 
# ###   usd_na & tns_na:  number of NA occurrences
# ###   paired_obs:       number of non-zero paired observations
# ###   usd_unique_pairs: (or 'tns') within set of paired observations, count of unique usd and tonnes
# ###   unique_pairs:     lesser of usd_unique_pairs and tns_unique_pairs
# ###   count_no_data:    number of paired NAs - years with no value reported
# 
# h <- h %>% np_zerofill
# ### for post-reporting years with NA for both tonnes and USD, fill zero - 
# ### assumes that non-reporting indicates zero harvest to report.
# ### Also cross-fills zeros where one side is 0, other is NA (not flagged as gapfill)
# 
# h <- h %>% np_lowdata_filter()
# ### Exclude commodities (within a region) that have few non-zero data points.
# ### Optional parameter with default: nonzero_h_yr_min = 4
# ### NOTE: This filter has consequences for the regression, but also has meaning in terms of 
# ###       not inflicting a penalty on regions trying, and then stopping, an experimental harvest.
# 



