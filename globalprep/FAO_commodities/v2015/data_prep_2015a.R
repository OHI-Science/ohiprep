# data_prep.R

### NOTES: MRF Sep 2 2014: check on how usd_rel is used and cut from functions.R file as a layer read in.

# Prepare FAO commodities data for Natural Products goal. 

# Updated Mar2015 by oharac. Github ohi-science/issues/issue #370
#   Minor recoding to get away from using reshape and plyr packages.
#   Fixed NA fill error for commodities with no non-NA years.
#   Tracked end-filling (carrying forward prev year's data to most recent year, if necessary) and
#     gap-filling.
#     (1) relative tonnes: tonnes relative to max tonnes for region with 35% buffer. The maximum 
#         corresponds to the year with the highest $ value - but it would probably be better to 
#         just base this off tonnes. When we redo these data lets evaluate this approach.
#     (2) weighting: This weights the contribution of each product according to USD at max year 
#         for a region. It makes sense to use $, because comparing extraction weight of sponges 
#         vs. ornamentals doesn't make sense.
#     (3) Exposure: For fish oil this value is the FIS score (which is a bit different than what 
#         is described in the paper because FIS score can have penalties for underfishing). The other 
#         values are determined by: log (harvest/habitat area + 1) / log[max(harvest/habitat area) +1].
#
#     The habitat area used for seaweeds: rocky reef
#     The habitat area used for coral: coral
#     The habitat area used for shells, ornamentals, sponges: coral plus rocky reef
#
#     Calculation
#       For each product:
#         sustainability = 1- average(exposure, risk)
#         Prod_score = sustainability*relative tonnes
#       Then take a weighted average of the Prod_score using the "weighting" file.
# 
# Notes on modifying the function
#     Several data layers are called that are not used: np_harvest_tonnes, np_harvest_usd, np_harvest_usd_relative
# There are these notes that I'm not sure what they mean in function code: 
#     TODO: add smoothing a la PLoS 2013 manuscript
#     TODO: move goal function code up to np_harvest_usd-peak-product-weight_year-max-%d.csv into 
#           ohiprep so layer ready already for calculating pressures & resilience
#
# Previously updated by JSLowndes/bbest Jun2014; File was originally clean_FAOcommodities.r:(by JStewart Apr2013)
#     read in quant/value files
#     remove Totals, Yugoslavia rows
#     translate FAO data codes (F, ..., -, 0 0)
#     carry previous year's value forward if value for max(year) is NA
#     merge with commodities and collapse to product
#     add rgn_id using new cbind_rgn function (@bbest updated add_rgn_id())
#     save single files for each commodity 

#####################################################################
### setup ---- libraries, pathnames, etc
#####################################################################

### load libraries. Note dplyr, tidyr, stringr are loaded later in common.R
library(zoo)  
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

### set dir_neptune_data and load common libraries (tidyr, dplyr, stringr) 
source('src/R/common.R') 
### NOTE: The default path should already be your ohiprep root directory for the rest to work.
###       Otherwise, presume that scripts are always working from your default ohiprep folder

dir_d <- 'globalprep/FAO_Commodities/v2015'
### NOTE: Set output paths to here, but do not use setwd().
###       This way, any scripts and code in ohiprep will still work, b/c always in root ohiprep dir.



#####################################################################
### read in and process files -- loops across value and quant data
#####################################################################

for (f in list.files(file.path(dir_d, 'raw'), pattern=glob2rx('*.csv'), full.names=T)) { 
  # f=list.files(file.path(dir_d, 'raw'), pattern=glob2rx('*.csv'), full.names=T)[1]
  
  ### data read in
  cat(sprintf('\n\n\n====\nfile: %s\n', basename(f)))
  d <- read.csv(f, check.names=F, strip.white=TRUE) # , stringsAsFactors=T
  units <- c('tonnes','usd')[str_detect(f, c('quant','value'))] # using American English, lowercase
  

  #####################################################################
  ### gather into long format and clean up FAO-specific data foibles
  #####################################################################

  suppressWarnings({
    ### warning: attributes are not identical across measure variables; they will be dropped
    m <- d %>% 
      rename(country   = `Country (Country)`,
             commodity = `Commodity (Commodity)`,
             trade     = `Trade flow (Trade flow)`) %>%
      gather(year, value, -country, -commodity, -trade)
  })
  m <- m %>%
    filter(!country %in% c('Totals', 'Yugoslavia SFR')) %>%
    mutate(  
      value = str_replace(value, fixed( ' F'),    ''),  # FAO denotes with F when they have estimated the value using best available data
      value = str_replace(value, fixed('0 0'), '0.1'),  # FAO denotes something as '0 0' when it is > 0 but < 1/2 of a unit. 
      value = str_replace(value, fixed(  '-'),   '0'),  # FAO's 0
      value = str_replace(value, fixed('...'),    NA),  # FAO's NA
      value = str_replace(value, fixed('.'),      NA),
      value = ifelse(value =='', NA, value),  
      value = as.numeric(as.character(value)),
      year  = as.integer(as.character(year))) %>%       # search in R_inferno.pdf for "shame on you"
    select(-trade) %>%                                  # eliminate 'trade' column
    arrange(country, commodity, is.na(value), year)

  
  #####################################################################
  ### check for commodities in data not found in lookup, per product by keyword
  ###   note: mammal oils excluded per supplemental info, Nature 2012
  ###   Powder and waste of shells?
  #####################################################################

  ### lookup for converting commodities to products
  com2prod <- read.csv(file.path(dir_d, 'commodities2products.csv'), na.strings='')
  
  commodities <- sort(as.character(unique(m$commodity)))
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
  l_missing_d <- anti_join(com2prod, m, by='commodity')
  if (length(l_missing_d) > 0) {
    cat(sprintf('\nMISSING: These commodities in the lookup are not found in the data %s:\n    ', basename(f)))
    print(l_missing_d)
  }
  
  
  #####################################################################
  ### products join: attach product categories from com2prod, and
  ### filter out all entries that do not match a product category
  #####################################################################
  m <- m %>%
    inner_join(com2prod, by='commodity')
  
  
  #####################################################################
  ### Special cases:
  ### Antilles: break up Netherlands Antilles into the 4 of 6 regions not already included ('Aruba','Curaçao')
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

    
  #####################################################################
  ### Add region ID, summarize by region/product/year, save output
  #####################################################################

  m <- m %>% 
    name_to_rgn(fld_name='country', flds_unique=c('country', 'commodity', 'product', 'year'), 
                fld_value='value', add_rgn_name=T)
    ### ???: name_to_rgn has some old code?  (%.% -> %>%, regroup -> group_by) Update to dplyr/tidyr functions
  
  ### units: rename value field to units based on filename
  names(m)[names(m) == 'value'] <- units  
  
  ### output to .csv
  f_out <- sprintf('%s/data/%s_%s.csv', dir_d, basename(dir_d), units)
  write.csv(m, f_out, row.names=F, na='')
}



  
#####################################################################
### Merge harvest in tonnes to harvest in USD.  
### * Identify years with neither tonnes nor USD data (NAs for both),
###     use this to determine first reporting year.
### * Set all no-data year values to zero, and eliminate all years
###     prior to first reporting year.
#####################################################################
  
nonzero_harvest_years_min <- 4
h_tonnes <- read.csv(file.path(dir_d, 'data/v2015_tonnes.csv'))
h_usd    <- read.csv(file.path(dir_d, 'data/v2015_usd.csv'))

h <- merge(
    h_tonnes %>%
# ???      filter(year <= year_max) %>%
  ### ??? Merge and gapfill done for entire dataset.  Filtering by "year_max" will occur
  ###     at the smoothing stage.  Is that reasonable?
      group_by(rgn_id, product),
    h_usd %>%
# ???      filter(year <= year_max) %>%
      group_by(rgn_id, product),
    by=c('rgn_name', 'rgn_id', 'commodity', 'product', 'year'), all=T)

h <- h %>%
  group_by(rgn_id, commodity) %>% 
  mutate(no_data = is.na(tonnes) & is.na(usd)) %>%
  arrange(rgn_id, commodity, no_data, year) %>%
  mutate(
    year_last = max(year, na.rm=T),    
      ### note: currently year_latest is always most recent year of whole dataset
    year_beg  = as.integer(ifelse(no_data[1], (year_last + 1), year[1])),  
      ### Since ordered by (is.na(tonnes) & is.na(usd)) before year, should pickup first non-NA year.
      ###   If no non-NA years, no_data[1] == TRUE, assign year_beg to be beyond the time series. 
      ### Note: The "as.integer" is there to get around an "incompatible types" error.
    tonnes    = ifelse(no_data, 0, tonnes),
    usd       = ifelse(no_data, 0, usd),
      ### for years where neither tonnes nor usd data are available, fill with 0.
    gapfill   = ifelse(is.na(tonnes), 'reg_tonnes', ifelse(
                       is.na(usd), 'reg_usd', ifelse(
                       no_data & year != year_last, 'zero_fill', ifelse(
                       no_data & year == year_last, 'endfill', 
                       'none')))),
      ### ??? Barf. that is ugly.  Is there a way to use switch() here?
      ### ??? Also: preference for gap-filling flags?  This indicates the type of
      ###     gap-filling that will be required, but does not indicate that the gap-
      ###     filling has actually occurred yet.
    nonzero_n = sum(tonnes > 0 | usd > 0))

h <- h %>%
  filter(year>=year_beg) %>%
      ### eliminates years prior to first reporting
  filter(nonzero_n >= nonzero_harvest_years_min) %>%
      ### Require at least 'nonzero_harvest_years_min' years of data; filter out all
      ###   groups with fewer than this, in both "tonnes" and "usd" sets.
      ### ??? Should this just be for regression, or do we actually exclude all data
      ###   from these low-harvest or recent-harvest countries?
  select(-no_data, -year_beg, -nonzero_n) %>%
      ### clean up temp columns
  arrange(rgn_id, product, commodity, year)




#####################################################################
### Regression gap-filling
#####################################################################
### ??? Consider case, like New Caledonia misc corals and shells, where 
###     there are non-zero observations, but no COMPLETE non-zero observations.
###     In this case, returns slope = NA and intercept = 0, and regression
###     gap-fill results in an NA.

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

h <- h %>%
### Using regression models, gap-fill NAs in tonnes and USD
  left_join(m_tonnes, by=c('rgn_id','commodity')) %>%
  mutate(
    tonnes_mdl  = usd_ix0 + usd_coef * usd,
    tonnes      = ifelse(is.na(tonnes), pmax(0, tonnes_mdl), tonnes)) %>% 
  left_join(m_usd, by=c('rgn_id','commodity')) %>%
  mutate(
    usd_mdl  = tonnes_ix0 + tonnes_coef * tonnes,
    usd      = ifelse(is.na(usd), pmax(0, usd_mdl), usd))

h <- h %>%
### Endfill final data year for observations with neither tonnes nor USD data.
  group_by(rgn_id, commodity) %>%
  mutate(
    year_prev   = lag(year, order_by=year),
    tonnes_prev = lag(tonnes, order_by=year),
    usd_prev    = lag(usd, order_by=year),
    tonnes      = ifelse((gapfill=='endfill' & year==year_last & year_prev==year-1), tonnes_prev, tonnes),
    usd         = ifelse((gapfill=='endfill' & year==year_last & year_prev==year-1), usd_prev, usd))

h_comm <- h %>%
### clean up regression model gap-fill variables and end-fill variables.
### Also stores harvest per commodity per year per region
  select(-usd_ix0, -tonnes_ix0, -usd_coef, -tonnes_coef, -usd_mdl, -tonnes_mdl) %>%
  select(-tonnes_prev, -usd_prev, -year_prev, -year_last)



### Summarize each product per country per year, e.g. all corals in Albania in 2011
h_prod <- h_comm %>%
  group_by(rgn_name, rgn_id, product, year) %>%
  summarize(tonnes = sum(tonnes, na.rm=TRUE), 
            usd = sum(usd, na.rm=TRUE),
            gapfill_sum = sum(gapfill %in% c('reg_tonnes', 'reg_usd', 'endfill', 'zero_fill')))



#####################################################################
### Error-checking and table exports
#####################################################################

### Output gapfilling report to .csv.
h_gap <- h_comm %>%
  select(rgn_id, commodity, product, year, gapfill)
write.csv(h_gap, sprintf('%s/data/np_gapfill_report.csv', dir_d), row.names=F, na='')

### error check for duplicates (same product, same year, same region)
stopifnot(sum(duplicated(h_prod[,c('rgn_id', 'product', 'year')])) == 0)

### Check: wide with all commmodities and product subtotal for comparison with input data
h_x_tonnes <- h_comm %>% 
  bind_rows(mutate(h_prod, commodity='Z_TOTAL')) %>%
  select(-gapfill, -gapfill_sum, -usd) %>%
  arrange(rgn_name, product, commodity, year) %>%
  spread(year, tonnes)
h_x_usd <- h_comm %>% 
  bind_rows(mutate(h_prod, commodity='Z_TOTAL')) %>%
  select(-gapfill, -gapfill_sum, -tonnes) %>%
  arrange(rgn_name, product, commodity, year) %>%
  spread(year, usd)
write.csv(h_x_tonnes, sprintf('%s/tmp/np_harvest_tonnes_wide.csv', dir_d, units), row.names=F, na='')
write.csv(h_x_usd, sprintf('%s/tmp/np_harvest_usd_wide.csv', dir_d, units), row.names=F, na='')


#####################################################################
### Smoothing: determine rolling averages for tonnes and USD in
### order to determine peak values.  This is based upon total
### harvests by product group, not individual commodity.  Perform 
### this for all given scenarios, using a for loop.
#####################################################################

recent_harvest_years      <- 10
harvest_peak_buffer       <- 0.35

### ???: update this to something like year_max, year_max - 1, year_max - 2, etc
scenarios_year_max <- c(eez2014=2011, 
                        eez2013=2010, 
                        eez2012=2009)

for (scenario in c('eez2012','eez2013','eez2014')) { 
  # scenario  = 'eez2014'
  year_max <- scenarios_year_max[[scenario]]
  
  h <- h_prod %>%
    filter(year<=year_max) %>%
    group_by(rgn_id, product) %>%
    arrange(rgn_id, product, year)

  # smooth harvest over 4 year mean (prior and inclusive of current year)
  h <- h %>%
    left_join(
      h %>%
#        filter(n_years >= 4) %>%
  ### ??? I believe we already filtered for four non-zero years for each country above.  That filter permanently removed all commodities with fewer than four non-zero observations.
  ###     Since products (at this level) sum commodities, this filter should return all the values.  If the earlier filter is changed (e.g. to be temporary), then this might need to change.
        mutate(
          # Partial rolling mean with width 4 uses the first and last 3. Note that na.rm=F since otherwise would populate prior to first year of NP harvest data.
          tonnes_rollmean = rollapply(tonnes, width=4, FUN=mean, align='center', partial=T, na.rm=F),
          usd_rollmean    = rollapply(   usd, width=4, FUN=mean, align='center', partial=T, na.rm=F)) %>%
        select(rgn_id, product, year, starts_with('tonnes_roll'), usd_rollmean),
      by=c('rgn_id', 'product','year')) %>%
    mutate(
  ### ??? This will replace all tonnes and usd values with the rolling mean for pretty much all instances.  Is that what we want?
  ###     Note: in previous script, maintained a column of "tonnes_orig" or some such.  How many versions of tonnes should we be tracking?
      tonnes = ifelse(!is.na(tonnes_rollmean), tonnes_rollmean, tonnes),
      usd    = ifelse(!is.na(   usd_rollmean),    usd_rollmean,    usd)) # tonnes_difDEBUG = tonnes - tonnes_orig # DEBUG
  
  ### get peak harvest, in tonnes and usd, based upon smoothed values
  h <- h %>%
    mutate(    
      tonnes_peak = max(tonnes, na.rm=T)  * (1 - harvest_peak_buffer),
      usd_peak    = max(   usd, na.rm=T)  * (1 - harvest_peak_buffer))
  
  ### product weighting per region: based upon dollar value, not tonnes.  
  ### w[product] = usd_peak[product] / usd_peak[total for region]
  w <- h %>%
    filter(year == year_max) %>%
    group_by(rgn_id) %>%
    mutate(
      usd_peak_allproducts    = sum(usd_peak, na.rm=T),
      usd_peak_product_weight = usd_peak / usd_peak_allproducts)    
  
  ### join w to h
  h <- left_join(
    h, 
    w %>%
      select(rgn_id, product, usd_peak_product_weight), 
    by=c('rgn_id','product'))
  
  h <- h %>% 
    mutate(tonnes_rel      = ifelse(tonnes >= tonnes_peak, 1, tonnes / tonnes_peak),
           usd_rel         = ifelse(usd >= usd_peak, 1, usd / usd_peak)) %>% 
    group_by(rgn_id, product, year)
  
  write.csv(h  , sprintf('%s/tmp/%s_np_harvest_smoothed_data.csv', dir_d, scenario), row.names=F, na='')
  
  # write NP weights layer also used to calculate pressures and resilience
  write.csv(
    select(w, rgn_id, product, weight=usd_peak_product_weight),
    sprintf('%s/data/np_harvest_%s_product-peak_%s-year-max-%d_buffer-%g.csv', dir_d, 'usd', scenario, year_max, harvest_peak_buffer), row.names=F, na='')
  
  # write NP status layers
  for (lyr in c('tonnes','tonnes_rel','usd','usd_rel')) {
    write.csv(
      h[,c('rgn_id', 'product', 'year', lyr)],
      sprintf('%s/data/np_harvest_%s_%s-year-max-%d_buffer-%g.csv', dir_d, str_replace(lyr, '_','-'), scenario, year_max, harvest_peak_buffer), row.names=F, na='')
  }
}