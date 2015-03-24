#####################################################################

np_commodity_lookup <- function(data, com2prod) {
### check for commodities in data frame m not found in lookup, per 
### product by keyword.  Prints results, nothing returned.
###   note: mammal oils excluded per supplemental info, Nature 2012
###   Powder and waste of shells?
#####################################################################
  
  commodities <- sort(as.character(unique(data$commodity)))
  keywords <- c(
    'sponges'     = 'sponge', 
    'fish_oil'    = 'oil', 
    'seaweeds'    = 'seaweed', 
    'ornamentals' = 'ornamental', 
    'corals'      = 'coral', 
    'shells'      = 'shell')
  for (i in 1:length(keywords)) { 
    # i=1
    prod <- names(keywords)[i]
    keyword <- keywords[i]
    d_missing_l <- setdiff(
      commodities[str_detect(commodities, ignore.case(keyword))], 
      subset(com2prod, product==prod, commodity, drop=T))
    if (length(d_missing_l) > 0) {
      cat(sprintf("\nMISSING in the lookup the following commodites in product='%s' having keyword='%s' in data file %s:\n    %s\n", 
                  prod, keyword, basename(f), paste(d_missing_l, collapse='\n    ')))
    }
  }
  
  ### check for commodities in lookup not found in data
  l_missing_d <- anti_join(com2prod, data, by='commodity')
  if (length(l_missing_d) > 0) {
    cat(sprintf('\nMISSING: These commodities in the lookup are not found in the data %s:\n    ', basename(f)))
    print(l_missing_d)
  }
  return(NULL)
}

#####################################################################

np_split_antilles <- function(m) {
### Deal with special cases of countries, specific to NP.  
### - FAO reports 'Antilles' as one region, but OHI considers as four 
###   reported regions; break up and distribute values 
#####################################################################
  
  stopifnot( sum(c('Bonaire','Saba','Sint Maarten','Sint Eustatius') %in% m$country) == 0 )
  m_ant <- m %>%
    filter(country == 'Netherlands Antilles') %>%
    mutate(
      value            = value/4,
      'Bonaire'        = value,
      'Saba'           = value,
      'Sint Maarten'   = value,
      'Sint Eustatius' = value) %>%
    select(-value, -country) %>%
    gather(country, value, -commodity, -product, -year) %>%
    mutate(country = as.character(country))  # otherwise, m_ant$country is factor; avoids warning in bind_rows bel
  m <- m %>%
    filter(country != 'Netherlands Antilles') %>%
    bind_rows(m_ant)
  return(m)
}



#####################################################################

np_harvest_cat <- function(h_tonnes, h_usd) {
### Merge harvest in tonnes to harvest in USD.  
### * Identify years with neither tonnes nor USD data (NAs for both),
###     use this to determine first reporting year.
### * Set all no-data year values to zero, and eliminate all years
###     prior to first reporting year.
#####################################################################
  h <- merge(
    h_tonnes %>%
      group_by(rgn_id, product),
    h_usd %>%
      group_by(rgn_id, product),
    by=c('rgn_name', 'rgn_id', 'commodity', 'product', 'year'), all=T) 
  
  h <- h%>%
    arrange(rgn_id, product, commodity, year)

  return(h)
}

#####################################################################

np_harvest_preclip <- function(h) {
### * Identify years with neither tonnes nor USD data (NAs for both),
###     use this to determine first reporting year.
### * Eliminate all years prior to first reporting year.
### * Returns cleaned data with same columns
#####################################################################
  h <- h %>%
    group_by(rgn_id, commodity) %>% 
    mutate(no_data = is.na(tonnes) & is.na(usd)) %>%
    arrange(rgn_id, commodity, no_data, year) %>%
    mutate(
      year_last = max(year, na.rm=T),    
      ### note: currently year_latest is always most recent year of whole dataset
      year_beg  = as.integer(ifelse(no_data[1], (year_last + 1), year[1]))) %>%
    ### Since ordered by (is.na(tonnes) & is.na(usd)) before year, should pickup first non-NA year.
    ###   If no non-NA years, no_data[1] == TRUE, assign year_beg to be beyond the time series. 
    ### Note: The "as.integer" is there to get around an "incompatible types" error.
    
    filter(year>=year_beg) %>%
    ### eliminates years prior to first reporting
    
    select(-year_beg, -year_last, -no_data) %>%
    ### cleans up all columns created in this function
    
    arrange(rgn_id, product, commodity, year)

  return(h)
}


#####################################################################

np_harvest_gapflag <- function(h) {
### * Determines type of gapfilling necessary to deal with NAs
### * Adds new column 'gapfill'
### * NOTE: Does not actually perform any gap-filling!
#####################################################################
  h <- h %>%
    mutate(
      no_data = is.na(tonnes) & is.na(usd),
      year_last = max(year, na.rm=T),    
      gapfill   = 
        ifelse(no_data & year != year_last, 'zero_fill', 
        ifelse(no_data & year == year_last, 'endfill',
        ifelse(is.na(tonnes), 'reg_tonnes', 
        ifelse(is.na(usd), 'reg_usd', 
        'none'))))) %>%
      ### ??? Barf. that is ugly.  Is there a way to use switch() here?
      ### ??? Also: preference for gap-filling flags?  This indicates the type of gap-filling that will be required, but 
      ###     does not indicate that the gap-filling has actually occurred yet.
  
    select(-no_data, -year_last) %>%
    ### clean up temp columns
  
    arrange(rgn_id, product, commodity, year)

  return(h)
}


#####################################################################

np_zerofill <- function(h) {
### Zero-fills observations with NAs for both tonnes and USD.
#####################################################################
  h <- h %>%
    mutate(
      tonnes    = ifelse(gapfill=='zero_fill', 0, tonnes),
      usd       = ifelse(gapfill=='zero_fill', 0, usd)) %>%
      ### for years where neither tonnes nor usd data are available, fill with 0.
    arrange(rgn_id, product, commodity, year)
  
  return(h)
}

#####################################################################

np_lowdata_filter <- function(h, nonzero_h_yr_min = 4) {
### Excludes commodities with few non-zero observations with a region.
#####################################################################
  h <- h %>%
    mutate(
      nonzero_n = sum(tonnes > 0 | usd > 0)) %>%
    filter(nonzero_n >= nonzero_h_yr_min) %>%
    ### Require at least 'nonzero_harvest_years_min' years of data; filter out all
    ###   commodities by region with fewer than this.  This prevents penalizing countries that
    ###   start experimental production but then stop, for example.
    select(-nonzero_n) %>%
    ### clean up temp columns
    arrange(rgn_id, product, commodity, year)
  
  return(h)
}


#####################################################################

np_regr_coef <- function(h) {
### Outputs regression coefficients for tonnes and usd
### Also creates a flag for an NA usd or tonnes model
#####################################################################
  
  m_tonnes <- h  %>%
    ### Create linear regression model for tonnes as a function of USD.
    mutate(tonnes_nas = sum(is.na(tonnes))) %>%  
    filter(tonnes_nas >= 0 & !is.na(usd) & !is.na(tonnes)) %>%
    ### 'tonnes_nas' filter excludes commodities with no NAs, so need for 'tonnes' gap-fill.
    do(mdl = lm(tonnes ~ usd, data=.)) %>%
    summarize(
      rgn_id     = rgn_id,
      commodity  = levels(h$commodity)[commodity], 
      ### "commodity" is treated as a factor here.  This line was factor(levels(h$product)[product], levels(h$product)),
      usd_ix0    = coef(mdl)['(Intercept)'],
      usd_coef   = coef(mdl)['usd'])
  
  m_usd <- h %>%
    ### Create a linear regression model for USD as a function of tonnes.
    mutate(usd_nas = sum(is.na(usd))) %>%
    filter(usd_nas >= 0 & !is.na(usd) & !is.na(tonnes)) %>%
    do(mdl = lm(usd ~ tonnes, data=.)) %>%
    summarize(
      rgn_id      = rgn_id,
      commodity   = levels(h$commodity)[commodity], 
      tonnes_ix0  = coef(mdl)['(Intercept)'],
      tonnes_coef = coef(mdl)['tonnes']) 
  
  regr <- left_join(m_tonnes, m_usd, by=c('rgn_id','commodity'))
  regr <- regr %>%
    mutate(
      na_mdl_usd  = is.na(usd_coef),
      na_mdl_tns  = is.na(tonnes_coef))
  
  return(regr)
}


#####################################################################

np_regr_fill <- function(h, coefficients) {
### Applies regression models to gapfill NAs for both tonnes and usd
#####################################################################
  h <- h %>%
    ### Using regression models, gap-fill NAs in tonnes and USD
    left_join(coefficients, by=c('rgn_id','commodity')) %>%
    mutate(
      tonnes_orig = tonnes, 
      tonnes_mdl  = usd_ix0 + usd_coef * usd,
      tonnes      = ifelse(is.na(tonnes), pmax(0, tonnes_mdl), tonnes)) %>% 
    mutate(
      usd_orig    = usd,
      usd_mdl     = tonnes_ix0 + tonnes_coef * tonnes,
      usd         = ifelse(is.na(usd), pmax(0, usd_mdl), usd)) %>%
    select(-usd_ix0, -tonnes_ix0, -usd_coef, -tonnes_coef, -usd_mdl, -tonnes_mdl) %>%
    arrange(rgn_id, product, commodity, year)

  return(h)
}



#####################################################################

np_end_fill <- function(h) {
### Endfill final data year for observations with neither tonnes nor USD data.
#####################################################################
  h <- h %>%
    group_by(rgn_id, commodity) %>%
    mutate(
      year_last   = max(year, na.rm=TRUE),
      year_prev   = lag(year, order_by=year),
      tonnes_prev = lag(tonnes, order_by=year),
      usd_prev    = lag(usd, order_by=year),
      tonnes      = ifelse((gapfill=='endfill' & year==year_last & year_prev==year-1), tonnes_prev, tonnes),
      usd         = ifelse((gapfill=='endfill' & year==year_last & year_prev==year-1), usd_prev, usd)) %>%
  
    select(-tonnes_prev, -usd_prev, -year_prev, -year_last) %>%
      ### clean up regression model gap-fill variables and end-fill variables.
    arrange(rgn_id, product, commodity, year)
  
  return(h)
}

np_datacheck <- function(h) {
  data_check <- h %>%
    group_by(rgn_name, rgn_id, commodity) %>%
    mutate(
      no_data = is.na(tonnes) & is.na(usd),
      paired = (!is.na(tonnes) & tonnes>0 & !is.na(usd) & usd>0)) %>%
    summarize(
      num_years = length(tonnes),
      usd_unique_nz = length(unique(usd[usd>0 & !is.na(usd)])),
      tns_unique_nz = length(unique(tonnes[tonnes>0 & !is.na(tonnes)])),
      usd_na = sum(is.na(usd)),
      tns_na = sum(is.na(tonnes)),
      paired_obs = sum(paired),
      usd_unique_pairs = length(unique(usd[paired])),
      tns_unique_pairs = length(unique(tonnes[paired])),
      unique_pairs = min(usd_unique_pairs, tns_unique_pairs),
      count_no_data = sum(no_data)
    )
  return(data_check)
}
  