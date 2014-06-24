# data_prep.R

# Prepare FAO commodities data for Natural Products goal. 
# By JSLowndes/bbest Jun2014; File was originally clean_FAOcommodities.r:(by JStewart Apr2013)

#   read in quant/value files
#   remove Totals, Yugoslavia rows
#   translate FAO data codes (F, ..., -, 0 0)
#   carry previous year's value forward if value for max(year) is NA
#   merge with commodities and collapse to product
#   add rgn_id using new cbind_rgn function (@bbest updated add_rgn_id())
#   save single files for each commodity 

# setup ----

# debug> options(warn=2); options(error=recover) # options(warn=0); options(error=NULL)

# load libraries (dplyr last)
library(reshape2)
library(stringr)
library(plyr)
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
library(dplyr)   # NOTE: I think dplyr needs to be loaded AFTER plyr to override functions, which prob was reason behind wierdness with group_by() and summarize() giving 1 value.

# get paths
# NOTE: The default path should already be your ohiprep root directory for the rest to work.
#       Otherwise, presume that scripts are always working from your default ohiprep folder
source('src/R/common.R') # set dir_neptune_data
dir_d = 'Global/FAO-Commodities_v2011' 
# NOTE: Set output paths to here, but do not use setwd().
#       This way, any scripts and code in ohiprep will still work, b/c always in root ohiprep dir.

# get functions
source('src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()

# lookup for converting commodities to products
com2prod = read.csv(file.path(dir_d, 'commodities2products.csv'), na.strings='')

# read in and process files ----
for (f in list.files(file.path(dir_d, 'raw'), pattern=glob2rx('*.csv'), full.names=T)){ # f=list.files(file.path(dir_d, 'raw'), pattern=glob2rx('*.csv'), full.names=T)[1]
  
  # data read in
  cat(sprintf('\n\n\n====\nfile: %s\n', basename(f)))
  d = read.csv(f, check.names=F, strip.white=TRUE) # , stringsAsFactors=T
  units = c('tonnes','usd')[str_detect(f, c('quant','value'))] # using American English, lowercase
  
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
    
  # show extended values
  cat('\nExtended values:\n')
  m_x = filter(m, value_ext==T)
  print(m_x)  
        
  # check for commodities in data not found in lookup, per product by keyword
  commodities = sort(as.character(unique(m$commodity)))
  keywords = c(
    'sponges'     = 'sponge', 
    'fish_oil'    = 'oil', 
    'seaweeds'    = 'seaweed', 
    'ornamentals' = 'ornamental', 
    'corals'      = 'coral', 
    'shells'      = 'shell')
  for (i in 1:length(keywords)){ # i=1
    prod = names(keywords)[i]
    keyword = keywords[i]
    d_missing_l = setdiff(
      commodities[str_detect(commodities, ignore.case(keyword))], 
      subset(com2prod, product==prod, commodity, drop=T))
    if (length(d_missing_l)>0){
      cat(sprintf("\nMISSING in the lookup the following commodites in product='%s' having keyword='%s' in data file %s:\n    %s\n", 
                  prod, keyword, basename(f), paste(d_missing_l, collapse='\n    ')))
    }
  }

  # check for commodities in lookup not found in data
  l_missing_d = anti_join(com2prod, m, by='commodity')
  if (length(l_missing_d)>0){
    cat(sprintf('\nMISSING: These commodities in the lookup are not found in the data %s:\n    ', basename(f)))
    print(l_missing_d)
  }
  
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
  m_r = name_to_rgn_id(m_l, fld_name='country', flds_unique=c('country','commodity','year'), fld_value='value', add_rgn_name=T) %.%
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

# Make readable layers by removing rgn_name
read.csv('Global/FAO-Commodities_v2011/data/FAO-Commodities_v2011_tonnes.csv', na.strings='') %>%
  select(rgn_id, product, year, tonnes) %>%
  write.csv('Global/FAO-Commodities_v2011/data/FAO-Commodities_v2011_tonnes_lyr.csv', na='', row.names=F)

read.csv('Global/FAO-Commodities_v2011/data/FAO-Commodities_v2011_usd.csv', na.strings='') %>%
  select(rgn_id, product, year, usd) %>%
  write.csv('Global/FAO-Commodities_v2011/data/FAO-Commodities_v2011_usd_lyr.csv', na='', row.names=F)
