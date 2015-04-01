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
    group_by(rgn_id, commodity) %>%
    mutate(
      no_data = is.na(tonnes) & is.na(usd),
      year_last = max(year, na.rm=T),    
      gapfill   = 
        ifelse(no_data & year != year_last, 'zerofill', 
        ifelse(no_data & year == year_last, 'endfill',
        ifelse(is.na(tonnes) | is.na(usd), 'tbd',
        'none')))) %>%
      ### ??? Barf. that is ugly.  Is there a way to use switch() here?
      ### ??? Also: preference for gap-filling flags?  This indicates the type of gap-filling that will be required, but 
      ###     does not indicate that the gap-filling has actually occurred yet.
      ### 'tbd' is a placeholder, to be corrected depending on which pass of regression
      ###     gapfill is required to fill them.  If any remain as 'tbd' after pass 2, then they never got gap-filled.
  
    select(-no_data, -year_last) %>%
    ### clean up temp columns
  
    arrange(rgn_id, product, commodity, year)

  return(h)
}


#####################################################################

np_zerofill <- function(h) {
### Zero-fills observations with NAs for both tonnes and USD.  Also
### cross-fills zeros for observations where one side is zero, other NA.
#####################################################################
  h <- h %>%
    group_by(rgn_id, commodity) %>%
    mutate(
      tonnes    = ifelse(gapfill=='zerofill', 0, tonnes),
      usd       = ifelse(gapfill=='zerofill', 0, usd)) %>%
      ### for years where neither tonnes nor usd data are available, fill with 0.
    mutate(
      tonnes    = ifelse(is.na(tonnes) & usd==0, 0, tonnes),
      usd       = ifelse(is.na(usd) & tonnes==0, 0, usd)) %>%
      ### for years where one side is zero and the other is NA, fill NA with zero. 
    arrange(rgn_id, product, commodity, year)
  
  return(h)
}

#####################################################################

np_lowdata_filter <- function(h, nonzero_h_yr_min = 4) {
### Excludes commodities with few non-zero observations with a region.
#####################################################################
  h <- h %>%
    group_by(rgn_id, commodity) %>%
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

np_regr1_coef <- function(h) {
### Outputs regression coefficients for (tonnes ~ usd) and (usd ~ tonnes)
### at the commodity level, by country.
#####################################################################
  
  h <- h %>%
    group_by(rgn_id, commodity)
  ### Groups by rgn_id and commodity to regress (tonnes ~ usd) and (usd ~ tonnes) within each region/country
  
  m_tonnes <- h  %>%
    ### Create linear regression model for tonnes as a function of USD.
    mutate(tonnes_nas = sum(is.na(tonnes))) %>%  
    filter(tonnes_nas > 0 & !is.na(usd) & !is.na(tonnes)) %>%
    ### 'tonnes_nas' filter excludes commodities with no NAs, so no need for 'tonnes' gap-fill.
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
    filter(usd_nas > 0 & !is.na(usd) & !is.na(tonnes)) %>%
    do(mdl = lm(usd ~ tonnes, data=.)) %>%
    summarize(
      rgn_id      = rgn_id,
      commodity   = levels(h$commodity)[commodity], 
      tonnes_ix0  = coef(mdl)['(Intercept)'],
      tonnes_coef = coef(mdl)['tonnes']) 
  
  regr <- full_join(m_tonnes, m_usd, by=c('rgn_id','commodity'))
  
  return(regr)
}


#####################################################################

np_regr1_fill <- function(h, years_back=10, min_paired_obs=4) {
### Applies regression model built into np_regr1_coef to gapfill NAs for both tonnes and usd.
### MODEL: (tonnes ~ usd) and (usd ~ tonnes) at commodity level by country.
### Filters by most recent number of years and by a minimum number
### of paired observations.
#####################################################################
  
  lower_bound_year <- max(h$year) - years_back

  h_clipped <- h %>%
    group_by(rgn_id, commodity) %>%
    filter(year >= lower_bound_year) %>%
    mutate(
      n_pairs = sum((!is.na(tonnes) & tonnes>0 & !is.na(usd) & usd>0))) %>%
    filter(n_pairs >= min_paired_obs)

  coefficients <- np_regr1_coef(h_clipped)
  
  h_mdl <- h %>%
    ### Using regression models, gap-fill NAs in tonnes and USD
    left_join(coefficients, by=c('rgn_id','commodity')) %>%
    mutate(
#      tonnes_orig = tonnes, 
      tonnes_mdl  = usd_ix0 + usd_coef * usd,
      gapfill     = ifelse(is.na(tonnes) & year >= lower_bound_year & !is.na(usd_coef), 'r1_tonnes', gapfill),
      ### conditions: tonnes is NA (needs to be filled); year is recent; and coefficient is not NA.
      tonnes      = ifelse(is.na(tonnes) & year >= lower_bound_year, pmax(0, tonnes_mdl), tonnes)) %>% 
    mutate(
#      usd_orig    = usd,
      usd_mdl     = tonnes_ix0 + tonnes_coef * tonnes,
      gapfill     = ifelse(is.na(usd) & year >= lower_bound_year & !is.na(tonnes_coef), 'r1_usd', gapfill),
      ### conditions: usd is NA (needs to be filled); year is recent; and coefficient is not NA.
      usd         = ifelse(is.na(usd) & year >= lower_bound_year, pmax(0, usd_mdl), usd)) %>%
    select(-usd_ix0, -tonnes_ix0, -usd_coef, -tonnes_coef, -usd_mdl, -tonnes_mdl) %>%
      ### removes internal function-specific variables
    arrange(rgn_id, product, commodity, year)
  
  return(h_mdl)
}

#####################################################################

np_regr2_coef <- function(h) {
### Outputs regression coefficients for (tonnes ~ usd + year) and (usd ~ tonnes + year)
### at the commodity level, by global prices
#####################################################################

  h <- h %>%
    group_by(commodity)
  ### Groups by commodity only, to regress (tonnes ~ usd) and (usd ~ tonnes) globally.

  m_tonnes <- h  %>%
    ### Create linear regression model for tonnes as a function of USD.
    mutate(tonnes_nas = sum(is.na(tonnes))) %>%  
    filter(tonnes_nas > 0 & !is.na(usd) & !is.na(tonnes)) %>%
    ### 'tonnes_nas' filter excludes commodities with no NAs, so no need for 'tonnes' gap-fill.
    do(mdl = lm(tonnes ~ usd + year, data=.)) %>%
    summarize(
      commodity  = commodity, 
      ### "commodity" is treated as a string here.  Why does this work here, but not in regr1_coef?
      usd_ix0    = coef(mdl)['(Intercept)'],
      usd_coef   = coef(mdl)['usd'],
      yr_tns_coef  = coef(mdl)['year'])
  
  m_usd <- h %>%
    ### Create a linear regression model for USD as a function of tonnes.
    mutate(usd_nas = sum(is.na(usd))) %>%
    filter(usd_nas >= 0 & !is.na(usd) & !is.na(tonnes)) %>%
    do(mdl = lm(usd ~ tonnes + year, data=.)) %>%
    summarize(
      commodity   = commodity, 
      tonnes_ix0  = coef(mdl)['(Intercept)'],
      tonnes_coef = coef(mdl)['tonnes'], 
      yr_usd_coef = coef(mdl)['year'])

  regr <- full_join(m_tonnes, m_usd, by='commodity')
  
  return(regr)
}


#####################################################################

np_regr2_fill <- function(h, years_back=10, min_paired_obs=4) {
### Applies regression model built into np_regr1_coef to gapfill NAs for both tonnes and usd.
### MODEL: (tonnes ~ usd + year) and (usd ~ tonnes + year) at commodity level, globally.
### Filters by most recent number of years and by a minimum number
### of paired observations.
#####################################################################

  lower_bound_year <- max(h$year) - years_back

# for each given commodity, in each given year, determine the average global price reported in USD per tonne.
#   * group_by (commodity) to include obs across all regions.
#   * identify and include all paired observations across the board.
#   * create regression model for global commodity prices
#   * append this model, and calc as before.

  h_clipped <- h %>%
    group_by(commodity) %>%
    filter(year >= lower_bound_year) %>%
    mutate(
      n_pairs = sum((!is.na(tonnes) & tonnes>0 & !is.na(usd) & usd>0))) %>%
    filter(n_pairs >= min_paired_obs)
  
  coefficients <- np_regr2_coef(h_clipped)
    ### 
    ### Note that the coefficient function does the global grouping, so no need to 
    ### group_by at this level.
  
  h_mdl <- h %>%
    ### Using regression models, gap-fill NAs in tonnes and USD
    left_join(coefficients, by='commodity') %>%
    mutate(
#      tonnes_orig = tonnes, 
      tonnes_mdl  = usd_ix0 + usd_coef * usd + yr_tns_coef * year,
      gapfill     = ifelse(is.na(tonnes) & year >= lower_bound_year & !is.na(usd_coef), 'r2_tonnes', gapfill),
      ### conditions: tonnes is NA (needs to be filled); year is recent; and coefficient is not NA.
      tonnes      = ifelse(is.na(tonnes) & year >= lower_bound_year, pmax(0, tonnes_mdl), tonnes)) %>% 
    mutate(
#      usd_orig    = usd,
      usd_mdl     = tonnes_ix0 + tonnes_coef * tonnes + yr_usd_coef * year,
      gapfill     = ifelse(is.na(usd) & year >= lower_bound_year & !is.na(tonnes_coef), 'r2_usd', gapfill),
      ### conditions: usd is NA (needs to be filled); year is recent; and coefficient is not NA.
      usd         = ifelse(is.na(usd) & year >= lower_bound_year, pmax(0, usd_mdl), usd)) %>%
    select(-usd_ix0, -tonnes_ix0, -usd_coef, -tonnes_coef, -usd_mdl, -tonnes_mdl, -yr_tns_coef, -yr_usd_coef) %>%
      ### removes internal function-specific variables
    arrange(rgn_id, product, commodity, year)
  
  return(h_mdl)
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

#####################################################################

np_datacheck <- function(h) {
### creates a summary dataframe for debugging and output verification
#####################################################################
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
  

#####################################################################

np_harvest_smooth <- function(h, rollwidth = 4) {
### Smooths data using a rolling mean.  Using align='right' means the
### most recent value will be the mean from that year and previous.
### Returns original data frame with tonnes & usd filled with rolling
### mean; adds tonnes_orig and usd_orig in case original values needed.
#####################################################################
  h <- h %>%
    group_by(rgn_id, product) %>%
    left_join(
      h %>%
        filter(n_years >= rollwidth) %>%
        mutate(
          tonnes_orig = tonnes, ### prevent overwriting of reported and gapfilled values
          usd_orig = usd,       ### prevent overwriting of reported and gapfilled values
          tonnes_rollmean = rollapply(tonnes, width=rollwidth, FUN=mean, align='right', partial=T, na.rm=F),
          usd_rollmean    = rollapply(   usd, width=rollwidth, FUN=mean, align='right', partial=T, na.rm=F)),
      by=c('rgn_id', 'product','year')) %>%
    mutate(
      tonnes = ifelse(!is.na(tonnes_rollmean), tonnes_rollmean, tonnes),
      usd    = ifelse(!is.na(   usd_rollmean),    usd_rollmean,    usd)) %>%
    select(rgn_id, product, year, tonnes, usd, tonnes_orig, usd_orig)
  return(h)
}

#####################################################################

np_harvest_peak <- function(h, harvest_peak_buffer = 0.35, recent_harvest_years = 10) {
### Determines the peak harvest values for the input data frame, in
###   tonnes and usd.  Tonnes peak includes entire time series; usd peak
###   includes only the most recent harvest years.
### From peak USD values, determines proportional weighting of product
###   within the region.
#####################################################################
  h <- h %>%
    group_by(rgn_id, product) %>%
    mutate(tonnes_peak = max(tonnes, na.rm=T)  * (1 - harvest_peak_buffer))
  
  h_recent <- h %>%
    filter(year >= year_max - recent_harvest_years) %>%
    mutate(usd_peak = max(   usd, na.rm=T)  * (1 - harvest_peak_buffer)) %>%
    select(rgn_id, product, year, usd_peak) %>%
    summarize(usd_peak = max(usd_peak))
      ### ??? Without the max() it returns error 'expecting single value' ---- but usd_peak should be a single value
  h <- h %>% left_join(h_recent, by=c('rgn_id','product'))

  w <- h %>%
    filter(year == year_max) %>%
    group_by(rgn_id) %>%
    mutate(
      usd_peak_allproducts    = sum(usd_peak, na.rm=T),
      usd_peak_product_weight = usd_peak / usd_peak_allproducts)    
  
  ### join product weighting proportions to h
  h <- h %>% 
    left_join(
      w %>%
        select(rgn_id, product,  usd_peak_product_weight), 
      by=c('rgn_id','product'))

  return(h)
}


np_harvest_status <- function(h) {
  h <- h %>% 
    ### ??? Calculates relative tonnes and usd based on smoothed data proportional to peak.  Does not penalize overharvesting - 
    ###     Should there be an upper value, say peak*(1 - 0.25)?
    ungroup() %>%
      ### ungroup() necessary because of conflict between group_by(), mutate(), and ifelse().
    mutate(tonnes_rel = ifelse(tonnes >= tonnes_peak, 1, tonnes / tonnes_peak),
           usd_rel    = ifelse(usd    >= usd_peak,    1,   usd / usd_peak)) %>%
    group_by(rgn_id, product)
  return (h)
}