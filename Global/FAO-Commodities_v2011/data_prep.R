# data_prep.R

### NOTES: MRF Sep 2 2014: check on how usd_rel is used and cut from functions.R file as a layer read in.

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

# load libraries
library(stringr)
library(zoo)  
# library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall


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

# # read in and process files ----
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
      year_max   = max(year, na.rm=T),    # note: currenly year_max is always max year of whole dataset b/c input is wide format
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
      year_max = max(year, na.rm=T)))

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

#####
# correlate, swap and smooth to generate product peaks ----

harvest_peak_buffer = 0.35
nonzero_harvest_years_min = 4
recent_harvest_years = 10

scenarios_year_max = c(eez2014=2011, 
                       eez2013=2010, 
                       eez2012=2009)

h_tonnes = read.csv(file.path(dir_d, 'data/FAO-Commodities_v2011_tonnes.csv'))
h_usd    = read.csv(file.path(dir_d, 'data/FAO-Commodities_v2011_usd.csv'))

for (scenario in c('eez2012','eez2013','eez2014')){ # scenario  = 'eez2013'

  year_max = scenarios_year_max[[scenario]]

  
  # DEBUG: pre-filter n_years per val <= 2
  h = 
    merge(
      # require at least 2 years of data, otherwise remove product to rely on others or use georegional avg if none left
      h_tonnes %>%
        filter(year <= year_max) %>%
        group_by(rgn_id, product) %>%
        mutate(
          tonnes_orig = tonnes,
          tonnes_n    = sum(tonnes>0)), 
      h_usd %>%
        filter(year <= year_max) %>%
        group_by(rgn_id, product) %>%
        mutate(
          usd_orig = usd,
          usd_n    = sum(usd>0)),
      by=c('rgn_name','rgn_id','product','year'), all=T) %>%
    select(rgn_name, rgn_id, product, year, tonnes_orig, tonnes, usd_orig, usd) %>%
    arrange(rgn_id, product, year) %>%
    group_by(rgn_id, product) %>%
    mutate(
      n_years = n())
  csv = sprintf('%s/tmp/%s_np_harvest_inputs_pre-nonzero-filter.csv', dir_d, scenario)
  write.csv(h, csv, row.names=F, na='')
  #system(sprintf('open %s', csv))
  
  # merge harvest in tonnes and usd
  h = 
    merge(
      # require at least 'nonzero_harvest_years_min' years of data, otherwise remove product to rely on others or use georegional avg if none left
      h_tonnes %>%
        filter(year <= year_max) %>%
        group_by(rgn_id, product) %>%
        mutate(
          tonnes_orig      = tonnes,
          tonnes_nonzero_n = sum(tonnes>0)) %>%
        filter(tonnes_nonzero_n >= nonzero_harvest_years_min), 
      h_usd %>%
        filter(year <= year_max) %>%
        group_by(rgn_id, product) %>%
        mutate(
          usd_orig      = usd,
          usd_nonzero_n = sum(usd>0)) %>%
        filter(usd_nonzero_n >= nonzero_harvest_years_min), 
      by=c('rgn_name','rgn_id','product','year'), all=T) %>%
    select(rgn_name, rgn_id, product, year, tonnes_orig, tonnes, usd_orig, usd) %>%
    arrange(rgn_id, product, year) %>%
    group_by(rgn_id, product) %>%
  mutate(tonnes_na_sum = is.na(tonnes)) %>% # add this filter to remove rgn-product pairs with all NAs 
  filter(!tonnes_na_sum) %>% 
  select(-tonnes_na_sum) %>%
    mutate(
      n_years = n())
  
  # show where NAs usd vs tonnes
  cat(sprintf('  nrow(h): %d, range(h$year): %s\n', nrow(h), paste(range(h$year), collapse=' to ')))
  if (nrow(filter(h, is.na(usd) | is.na(tonnes))) > 0){
    cat('  Table of harvest NAs:\n')
    h_na = h %>% 
      filter(is.na(usd) | is.na(tonnes)) %>% 
      mutate(var_na = ifelse(is.na(usd), 'usd', 'tonnes'))
    print(table(ungroup(h_na) %>% select(var_na)))

    # handle NA mismatch b/n tonnes and usd with correlative model
    m_tonnes = h  %>%
      mutate(tonnes_nas   = sum(is.na(tonnes))) %>%
      filter(tonnes_nas >= 0 & !is.na(usd) & !is.na(tonnes)) %>%
      do(mdl = lm(tonnes ~ usd, data=.)) %>%
      summarize(
        rgn_id   = rgn_id,
        product  = factor(levels(h$product)[product], levels(h$product)),
        usd_ix0  = coef(mdl)['(Intercept)'],
        usd_coef = coef(mdl)['usd'])
    m_usd = h %>%
      mutate(usd_nas = sum(is.na(usd))) %>%
      filter(usd_nas >= 0 & !is.na(usd) & !is.na(tonnes)) %>%
      do(mdl = lm(usd ~ tonnes, data=.)) %>%
      summarize(
        rgn_id      = rgn_id,
        product     = factor(levels(h$product)[product], levels(h$product)),
        tonnes_ix0  = coef(mdl)['(Intercept)'],
        tonnes_coef = coef(mdl)['tonnes'])  
    h = h %>%
      left_join(m_tonnes, by=c('rgn_id','product')) %>%
      mutate(
        tonnes_mdl  = usd_ix0 + usd_coef * usd,
        tonnes      = ifelse(is.na(tonnes), pmax(0, tonnes_mdl), tonnes)) %>% 
      left_join(m_usd, by=c('rgn_id','product')) %>%
      mutate(
        usd_mdl  = tonnes_ix0 + tonnes_coef * tonnes,
        usd      = ifelse(is.na(usd), pmax(0, usd_mdl), usd))
  }

  # smooth harvest over 4 year mean (prior and inclusive of current year)
  h = h %>%
    left_join(
      h %>%
        filter(n_years >= 4) %>%
        mutate(
          # Partial rolling mean with width 4 uses the first and last 3. Note that na.rm=F since otherwise would populate prior to first year of NP harvest data.
          tonnes_rollmean = rollapply(tonnes, width=4, FUN=mean, align='center', partial=T, na.rm=F),
          usd_rollmean    = rollapply(   usd, width=4, FUN=mean, align='center', partial=T, na.rm=F)) %>%
        select(rgn_id, product, year, starts_with('tonnes_roll'), usd_rollmean),
      by=c('rgn_id', 'product','year')) %>%
    mutate(
      tonnes = ifelse(!is.na(tonnes_rollmean), tonnes_rollmean, tonnes),
      usd    = ifelse(!is.na(   usd_rollmean),    usd_rollmean,    usd)) # tonnes_difDEBUG = tonnes - tonnes_orig # DEBUG

  # get peak
  h = h %>%
    mutate(    
      tonnes_peak = max(tonnes, na.rm=T)  * (1 - harvest_peak_buffer),
      usd_peak    = max(   usd, na.rm=T)  * (1 - harvest_peak_buffer))
  
  # product weights per region: w = product peak / (sum of all product peaks)
  w = h %>%
    filter(year == year_max) %>%
    group_by(rgn_id) %>%
    mutate(
      usd_peak_allproducts    = sum(usd_peak, na.rm=T),
      usd_peak_product_weight = usd_peak / usd_peak_allproducts)    
  
  # join w to h
  h = left_join(
    h, 
    w %>%
      select(rgn_id, product, usd_peak_product_weight), 
    by=c('rgn_id','product'))
  
  # strange ifelse behavior in dplyr when condition has NAs throwing "Error: incompatible types, expecting a numeric vector". see https://github.com/hadley/dplyr/issues/299.
  h = within(h, {
    tonnes_rel      = ifelse(tonnes >= tonnes_peak, 1, tonnes / tonnes_peak)
    tonnes_rel_orig = tonnes_rel                                  # 109 NAs
    usd_rel     = ifelse(usd >= usd_peak, 1, usd / usd_peak)    
    # swap usd_rel for tonnes_rel if still NA even after correlative gapfilling above
    tonnes_rel      = ifelse(is.na(tonnes_rel), usd_rel   , tonnes_rel) # no longer necessary b/c above: swap tonnes for usd if still NA, for extreme cases 
  }) %>% group_by(rgn_id, product, year)
  
  # report whether region gapfilled for any product-year per var at either correlative or swap stages
  h_g = h %>%
    # per region-product-year
    mutate(
      tonnes_gapfilled = ifelse( (is.na(tonnes_orig) | is.na(tonnes_rel_orig)) & !is.na(tonnes_rel), T, F),
      usd_gapfilled    = ifelse( is.na(usd_orig) & !is.na(usd_rel), T, F)) %>%
    # per region
    group_by(rgn_id) %>%
    summarize(
      tonnes_gapfilled = ifelse(sum(tonnes_gapfilled) > 0, T, F),
      usd_gapfilled    = ifelse(sum(usd_gapfilled) > 0, T, F))
  write.csv(h  , sprintf('%s/tmp/%s_np_harvest_smoothed_data.csv', dir_d, scenario), row.names=F, na='')
  write.csv(h_g, sprintf('%s/tmp/%s_np_harvest_smoothed_summary.csv', dir_d, scenario), row.names=F, na='')

  # write NP weights layer also used to calculate pressures and resilience
  write.csv(
    select(w, rgn_id, product, weight=usd_peak_product_weight),
    sprintf('%s/data/np_harvest_%s_product-peak_%s-year-max-%d_buffer-%g.csv', dir_d, 'usd', scenario, year_max, harvest_peak_buffer), row.names=F, na='')
  
  # write NP status layers
  for (lyr in c('tonnes','tonnes_rel','usd','usd_rel')){
    write.csv(
      h[,c('rgn_id', 'product', 'year', lyr)],
      sprintf('%s/data/np_harvest_%s_%s-year-max-%d_buffer-%g.csv', dir_d, str_replace(lyr, '_','-'), scenario, year_max, harvest_peak_buffer), row.names=F, na='')
  }
}