### np_fxn.R
###
### Functions to automate processing of FAO commodities data for Natural Products goal.
###   Main script is at ohiprep/globalprep/FAO_commodities/data_prep.R
###
### Provenance:
###  Apr2015: created by Casey O'Hara (oharac)

np_commodity_lookup <- function(data, com2prod) {
### check for commodities in data frame m not found in lookup, per 
### product by keyword.  Prints results, nothing returned.
###   note: mammal oils excluded per supplemental info, Nature 2012
###   Powder and waste of shells?

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



np_split_antilles <- function(m) {
### Deal with special cases of countries, specific to NP.  
### - FAO reports 'Antilles' as one region, but OHI considers as four 
###   reported regions; break up and distribute values 

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
  m1 <- m %>%
    filter(country != 'Netherlands Antilles') %>%
    bind_rows(m_ant)
  return(m1)
}





# np_harvest_cat <- function(h_tonnes, h_usd) {
# ### Merge harvest in tonnes to harvest in USD.  
# ### * forces 'commodity' variable to character, to avoid issues with 
# ###   mutate() and join() and such.
# 
# 
#   h1 <- merge(
#     h_tonnes %>%
#       group_by(rgn_id, product),
#     h_usd %>%
#       group_by(rgn_id, product),
#     by=c('rgn_name', 'rgn_id', 'commodity', 'product', 'year'), all=T) 
#   
#   h1 <- h1 %>%
#     mutate(commodity = as.character(commodity)) %>%
#     arrange(rgn_id, product, commodity, year)
# 
#   return(h1)
# }



np_harvest_preclip <- function(h) {
### * Identify years with neither tonnes nor USD data (NAs for both),
###     use this to determine first reporting year.
### * Eliminate all years prior to first reporting year.
### * Returns cleaned data with same columns

  h1 <- h %>%
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
    ungroup() %>%    
    arrange(rgn_id, product, commodity, year)

  return(h1)
}




np_harvest_gapflag <- function(h) {
### * Determines type of gapfilling necessary to deal with NAs
### * Adds new column 'gapfill'
### * NOTE: Does not actually perform any gap-filling!

  h1 <- h %>%
    group_by(rgn_id, commodity) %>%
    mutate(
      no_data = is.na(tonnes) & is.na(usd),
      year_last = max(year, na.rm=T),    
      gapfill   = 
        ifelse(no_data & year != year_last, 'zerofill', 
        ifelse(no_data & year == year_last, 'endfill',
        ifelse(is.na(tonnes) | is.na(usd), 'tbd',
        'none')))) %>%
    ungroup() %>%
      ### ??? Barf. that is ugly.  Is there a way to use switch() here?
      ### ??? Also: preference for gap-filling flags?  This indicates the type of gap-filling that will be required, but 
      ###     does not indicate that the gap-filling has actually occurred yet.
      ### 'tbd' is a placeholder, to be corrected depending on which pass of regression
      ###     gapfill is required to fill them.  If any remain as 'tbd' after pass 2, then they never got gap-filled.
  
    select(-no_data, -year_last) %>%
    ### clean up temp columns
  
    arrange(rgn_id, product, commodity, year)

  return(h1)
}




np_zerofill <- function(h) {
### Zero-fills observations with NAs for both tonnes and USD.  Also
### cross-fills zeros for observations where one side is zero, other NA.

  h1 <- h %>%
    group_by(rgn_id, commodity) %>%
    mutate(
      tonnes    = ifelse(gapfill=='zerofill', 0, tonnes),
      usd       = ifelse(gapfill=='zerofill', 0, usd)) %>%
      ### for years where neither tonnes nor usd data are available, fill with 0.
    mutate(
      tonnes    = ifelse(is.na(tonnes) & usd==0, 0, tonnes),
      usd       = ifelse(is.na(usd) & tonnes==0, 0, usd)) %>%
      ### for years where one side is zero and the other is NA, fill NA with zero. 
    ungroup() %>%
    arrange(rgn_id, product, commodity, year)
  
  return(h1)
}



np_lowdata_filter <- function(h, nonzero_h_yr_min = 4) {
### Excludes commodities with few non-zero observations with a region.

  h1 <- h %>%
    group_by(rgn_id, commodity) %>%
    mutate(
      nonzero_n = sum(tonnes > 0 | usd > 0, na.rm=TRUE)) %>%
    filter(nonzero_n >= nonzero_h_yr_min) %>%
    ### Require at least 'nonzero_harvest_years_min' years of data; filter out all
    ###   commodities by region with fewer than this.  This prevents penalizing countries that
    ###   start experimental production but then stop, for example.
    select(-nonzero_n) %>%
    ### clean up temp columns
    arrange(rgn_id, product, commodity, year) %>%
    ungroup()
  
  return(h1)
}




np_regr_coef <- function(h, scope = 'rgn_id', vars = 'tdy') {
### Outputs regression coefficients for tonnes and usd in dataframe h, based upon:
### * scope = 'rgn_id' 'georgn_id' 'global': this determines the parameters 
###     passed to group_by() and the full_join() to set the scope of regression.
### * vars = 'td' for  (tonnes ~ dollars) (and vice versa), and 
###          'tdy' for (tonnes ~ dollars + years)
  
  h <- switch(scope,
              rgn_id    = group_by(h, rgn_id,    commodity),
              georgn_id = group_by(h, georgn_id, commodity),
              global    = group_by(h,            commodity))

  model <- switch(vars,
              tdy       = c('tonnes ~ usd + year', 'usd ~ tonnes + year'),
              td        = c('tonnes ~ usd',        'usd ~ tonnes'))
  
  m_tonnes <- h  %>%
    mutate(tonnes_nas = sum(is.na(tonnes))) %>%  
    filter(tonnes_nas > 0 & !is.na(usd) & !is.na(tonnes)) %>%
    do(mdl = lm(as.formula(model[1]), data=.)) %>%
    summarize(
      scope_id    = switch(scope, rgn_id = rgn_id, georgn_id = georgn_id, global = NA),
      commodity   = as.character(commodity), 
      usd_ix0     = coef(mdl)['(Intercept)'],
      usd_coef    = coef(mdl)['usd'],
      yr_tns_coef = ifelse(vars=='tdy', coef(mdl)['year'], 0)) %>%
    ungroup()
  
  m_usd <- h %>%
    mutate(usd_nas = sum(is.na(usd))) %>%
    filter(usd_nas > 0 & !is.na(usd) & !is.na(tonnes)) %>%
    do(mdl = lm(model[2], data=.)) %>%
    summarize(
      scope_id    = switch(scope, rgn_id = rgn_id, georgn_id = georgn_id, global = NA),
      commodity   = as.character(commodity), 
      tonnes_ix0  = coef(mdl)['(Intercept)'],
      tonnes_coef = coef(mdl)['tonnes'],
      yr_usd_coef = ifelse(vars=='tdy', coef(mdl)['year'], 0)) %>%
    ungroup()

  if(dim(m_tonnes)[1]==0) {      
    # m_tonnes = no data; can't full_join, mutate manually
    m <- m_usd    %>% mutate(usd_ix0 = NA, usd_coef = NA, yr_tns_coef = NA)
  } else if(dim(m_usd)[1]==0) {  
    # m_usd = no data; can't full_join, mutate manually
    m <- m_tonnes %>% mutate(tonnes_ix0 = NA, tonnes_coef = NA, yr_usd_coef = NA)
  } else {
    # OK to perform full_join
    m <- full_join(m_tonnes, m_usd, by=c('scope_id','commodity'))
  }
    
  m <- switch(scope, 
              rgn_id    = mutate(m, rgn_id = scope_id),
              georgn_id = mutate(m, georgn_id = scope_id),
              global    = m)
  m <- m %>% select(-scope_id)
  return(m)
}



np_regr_fill <- function(h, years_back=50, min_paired_obs=4, scope = 'rgn_id', vars = 'tdy') {
### Gap-fills NAs for tonnes and usd in dataframe h.  Regression model 
###   and regression scope are denoted by passed parameters.
### * years_back=50:     This determines how far back in the time series to include within the regression.
### * min_paired_obs=4:  This determines how many paired observations are required to attempt a regression.
### * scope = 'rgn_id' 'georgn_id' 'global': this determines the parameters to be
###     passed to group_by() and join functions to set the scope of regression.
### * vars = 'td'  for (tonnes ~ dollars) model (and vice versa), and 
###          'tdy' for (tonnes ~ dollars + years) model.
  
  lower_bound_year <- max(h$year) - years_back
  
  h <- switch(scope,
              rgn_id    = group_by(h, rgn_id,    commodity),
              georgn_id = group_by(h, georgn_id, commodity),
              global    = group_by(h,            commodity))
  
  h_clipped <- h %>%
    filter(year >= lower_bound_year) %>%
    mutate(
      n_pairs = sum((!is.na(tonnes) & tonnes>0 & !is.na(usd) & usd>0))) %>%
    filter(n_pairs >= min_paired_obs)
  
  coefficients <- np_regr_coef(h_clipped, scope, vars)
  
  by_flag <- switch(scope,
                    rgn_id    = c('rgn_id','commodity'),
                    georgn_id = c('georgn_id','commodity'),
                    global    =   'commodity')
  gap_flag <- switch(scope,
                    rgn_id    = c('r1_t_rgn','r1_u_rgn'),
                    georgn_id = c('r2_t_gr','r2_u_gr'),
                    global    = c('r3_t_gl','r3_u_gl'),
                    c('rgn_id','commodity'))
  
  h_mdl <- h %>%
    ### Using regression models, gap-fill NAs in tonnes and USD
    left_join(coefficients, by=by_flag) %>%
    mutate(
      #      tonnes_orig = tonnes, 
      tonnes_mdl  = usd_ix0 + usd_coef * usd + yr_tns_coef * year,
      ### Note that if vars == 'td', then yr_tns_coef == 0, so no effect on outcome.
      gapfill     = ifelse(is.na(tonnes) & year >= lower_bound_year & !is.na(usd_coef), gap_flag[1], gapfill),
      ### conditions: tonnes is NA (needs to be filled); year is recent; and coefficient is not NA.
      tonnes      = ifelse(is.na(tonnes) & year >= lower_bound_year, pmax(0, tonnes_mdl), tonnes)) %>% 
    mutate(
      #      usd_orig    = usd,
      usd_mdl     = tonnes_ix0 + tonnes_coef * tonnes + yr_usd_coef * year,
      gapfill     = ifelse(is.na(usd) & year >= lower_bound_year & !is.na(tonnes_coef), gap_flag[2], gapfill),
      ### conditions: usd is NA (needs to be filled); year is recent; and coefficient is not NA.
      usd         = ifelse(is.na(usd) & year >= lower_bound_year, pmax(0, usd_mdl), usd)) %>%
    select(-usd_ix0, -tonnes_ix0, -usd_coef, -tonnes_coef, -usd_mdl, -tonnes_mdl, -yr_tns_coef, -yr_usd_coef) %>%
    ### removes internal function-specific variables
    arrange(rgn_id, product, commodity, year)
  
  return(h_mdl)
}



np_end_fill <- function(h) {
### Endfill final data year for observations with neither tonnes nor USD data.

  h1 <- h %>%
    group_by(rgn_id, commodity) %>%
    mutate(
      year_last   = max(year, na.rm=TRUE),
      year_prev   = lag(year, order_by=year),
      tonnes_prev = lag(tonnes, order_by=year),
      usd_prev    = lag(usd, order_by=year),
      tonnes      = ifelse((gapfill=='endfill' & year==year_last & year_prev==year-1), tonnes_prev, tonnes),
      usd         = ifelse((gapfill=='endfill' & year==year_last & year_prev==year-1), usd_prev, usd)) %>%
   ungroup() %>%
    select(-tonnes_prev, -usd_prev, -year_prev, -year_last) %>%
      ### clean up regression model gap-fill variables and end-fill variables.
    arrange(rgn_id, product, commodity, year)
  
  return(h1)
}


np_datacheck <- function(h) {
### returns a summary dataframe for debugging and output verification

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
    ) %>%
    ungroup()
  
  return(data_check)
}
  


# np_harvest_smooth <- function(j, rollwidth = 4) {
# ### Smooths data using a rolling mean.  Using align='right' means the
# ### most recent value will be the mean from that year and previous.
# ### Returns original data frame with tonnes & usd filled with rolling
# ### mean; adds tonnes_orig and usd_orig in case original values needed.
# 
#   j1 <- j %>%
#     group_by(rgn_id, product) %>%
#     mutate(n_years = length(year)) %>%
#     filter(n_years >= rollwidth) %>%
#     left_join(
#       j %>%
#         mutate(
#           tonnes_rollmean = rollapply(tonnes, width=rollwidth, FUN=mean, align='right', partial=T, na.rm=F),
#           usd_rollmean    = rollapply(   usd, width=rollwidth, FUN=mean, align='right', partial=T, na.rm=F)) %>%
#         rename(
#           tonnes_orig = tonnes, ### prevent overwriting of reported and gapfilled values
#           usd_orig    = usd),    ### prevent overwriting of reported and gapfilled values
#       by = c('rgn_id', 'rgn_name', 'product', 'year')) %>%
#     mutate(
#       tonnes = ifelse(!is.na(tonnes_rollmean), tonnes_rollmean, tonnes),
#       usd    = ifelse(!is.na(   usd_rollmean),    usd_rollmean,    usd)) %>%
#     select(rgn_id, rgn_name, product, year, tonnes, usd, tonnes_orig, usd_orig)
#   return(j1)
# }




  # np_harvest_peak <- function(j, buffer = 0.35, recent_years = 10) {
  #   ### Determines the peak harvest values for the input data frame, in
  #   ###   tonnes and usd.  Tonnes peak includes entire time series; usd peak
  #   ###   includes only the most recent harvest years.
  #   ### From peak USD values, determines proportional weighting of product
  #   ###   within the region.
  #   
  #   j1 <- j %>%
  #     group_by(rgn_id, product) %>%
  #     mutate(tonnes_peak = max(tonnes, na.rm=T)  * (1 - buffer))
  #   
  #   j_recent <- j1 %>%
  #     filter(year >= year_max - recent_years) %>%
  #     mutate(usd_peak = max(   usd, na.rm=T)  * (1 - buffer)) %>%
  #     select(rgn_id, product, year, usd_peak) %>%
  #     summarize(usd_peak = max(usd_peak))
  #   ### ??? Without the max() it returns error 'expecting single value' ---- but usd_peak should be a single value
  #   j1 <- j1 %>% left_join(j_recent, by=c('rgn_id','product'))
  #   
  #   w <- j1 %>%
  #     filter(year == year_max) %>%
  #     group_by(rgn_id) %>%
  #     mutate(
  #       usd_peak_allproducts    = sum(usd_peak, na.rm=T),
  #       prod_weight = usd_peak / usd_peak_allproducts)    
  #   
  #   ### join product weighting proportions to j
  #   j1 <- j1 %>% 
  #     left_join(
  #       w %>%
  #         select(rgn_id, product,  prod_weight), 
  #       by = c('rgn_id','product'))
  #   
  #   return(j1)
  # }
  



# np_harvest_status <- function(j) {
# ### Calculates relative tonnes and usd based on smoothed data proportional 
# ###   to adjusted peak value.  Does not penalize overharvesting - 
# ### ???  Should there be an upper value, say peak*(1 - 0.25)?
# 
#   j1 <- j %>% 
#     ungroup() %>%
#       ### ungroup() necessary because of incompatible type error, 
#       ###   caused by conflict among group_by(), mutate(), and ifelse() in the dplyr package.
#     mutate(tonnes_rel = ifelse(tonnes >= tonnes_peak, 1, tonnes / tonnes_peak),
#            usd_rel    = ifelse(usd    >= usd_peak,    1,   usd / usd_peak)) %>%
#     group_by(rgn_id, product)
#   return (j1)
# }


add_georegion_id <- function(k) {
### Code from Melanie to attach a georegional id tag to dataframe k.
  
  key <- read.csv("../../../../ohi-global/eez2014/layers/cntry_rgn.csv")
  dups <- key$rgn_id[duplicated(key$rgn_id)]
  key[key$rgn_id %in% dups, ]
  
  key  <- key %>%
    filter(!(cntry_key %in% c('Galapagos Islands', 'Alaska',
                              'Hawaii', 'Trindade', 'Easter Island',
                              'PRI', 'GLP', 'MNP')))  
  #PRI (Puerto Rico) and VIR (Virgin Islands) in the same r2 zone (just selected one), 
  #GLP (Guadalupe) and MTQ (Marinique) in the same r2 zone (just selected one),  
  #MNP (Northern Mariana Islands) and GUM (Guam)
  
  
  
  georegion <- read.csv("raw/cntry_georegions.csv")
  #   unique(georegion$georgn_id[georegion$level=="r0"])  # 1 level
  #   unique(georegion$georgn_id[georegion$level=="r1"])  # 7 levels
  #   unique(georegion$georgn_id[georegion$level=="r2"])  # 22 levels
  
  georegion <- georegion %>%
    filter(level == "r2")
  
  k1 <- k %>%
    left_join(key, by = 'rgn_id') %>%
    left_join(georegion, by = 'cntry_key') %>%
    select(-cntry_key, -level)
      ### cleaning out variables
  return(k1)
}