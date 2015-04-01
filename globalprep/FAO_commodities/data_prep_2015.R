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


dir_base <- 'globalprep/FAO_Commodities'
dir_d <- sprintf('%s/v2015', dir_base)
### NOTE: Set output paths to here, but do not use setwd().
###       This way, any scripts and code in ohiprep will still work, b/c always in root ohiprep dir.

### Load NP-specific user-defined functions
source(sprintf('%s/R/np_fxn.R', dir_base))



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
  ### Products join: attach product categories from com2prod, and
  ###   filter out all entries that do not match a product category.
  ### Note: commodity_lookup is user-defined function to compare 
  ###   commodities in data vs commodities in lookup table
  #####################################################################
  ### load lookup for converting commodities to products
  com2prod <- read.csv(file.path(dir_d, 'commodities2products.csv'), na.strings='')
  
  #   np_commodity_lookup(m, com2prod) # in './R/np_fxn.R'

  m <- m %>%
    inner_join(com2prod, by='commodity')
  
  
  #####################################################################
  ### Special case: user-defined function deals with 
  ###   breaking up Antilles into separate reported rgns
  #####################################################################
  
  m <- np_split_antilles(m) # in './R/np_fxn.R'
  
    
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
### Gap-filling
#####################################################################
### See issue #397 for details and debate and pretty graphs.
#####################################################################
  
h_tonnes <- read.csv(file.path(dir_d, 'data/v2015_tonnes.csv'))
h_usd    <- read.csv(file.path(dir_d, 'data/v2015_usd.csv'))

h <- np_harvest_cat(h_tonnes, h_usd)
### concatenates h_tonnes and h_usd data
### h includes rgn_name, rgn_id, commodity, product, year, tonnes, usd

h <- h %>% np_harvest_preclip
### clips out years prior to first reporting year, for each commodity per region
    
h <- h %>% np_harvest_gapflag  
### Adds flag for required gap-filling, based upon NAs in data. 
### NOTE: Does not perform any gap-filling.
### At this point, h includes: 
###   rgn_name   rgn_id   commodity   product   year   tonnes   usd   gapfill
### 'gapfill' will be in (zerofill, endfill, tbd, none)

data_check <- h %>% np_datacheck()
### for each commodity within each region, creates summary info:
###   num_years:        the length of the data series for this commodity in this region
###   usd_unique_nz:    (or 'tns') number of unique non-zero values for usd or tonnes 
###   usd_na & tns_na:  number of NA occurrences
###   paired_obs:       number of non-zero paired observations
###   usd_unique_pairs: (or 'tns') within set of paired observations, count of unique usd and tonnes
###   unique_pairs:     lesser of usd_unique_pairs and tns_unique_pairs
###   count_no_data:    number of paired NAs - years with no value reported

h <- h %>% np_zerofill
### for post-reporting years with NA for both tonnes and USD, fill zero - 
### assumes that non-reporting indicates zero harvest to report.
### Also cross-fills zeros where one side is 0, other is NA (not flagged as gapfill)

h <- h %>% np_lowdata_filter()
### Exclude commodities (within a region) that have few non-zero data points.
### Optional parameter with default: nonzero_h_yr_min = 4
### NOTE: This filter has consequences for the regression, but also has meaning in terms of 
###       not inflicting a penalty on regions trying, and then stopping, an experimental harvest.

h <- h %>% np_regr1_fill()
### Estimate missing usd and tonnes based on regression model coefficients: usd ~ tonnes, and tonnes ~ usd.
### Changes 'gapfill' to 'r1_tonnes' or 'r1_usd'.
### Optional parameters with defaults: years_back = 10, min_paired_obs = 4
### columns returned: 
### rgn_name  rgn_id  commodity  product  year  tonnes  usd  gapfill  #tonnes_orig  #usd_orig

h <- h %>% np_regr2_fill(years_back=50)
### Estimate missing usd and tonnes based on regression model coefficients: usd ~ tonnes + year, and tonnes ~ usd + year.
### Changes 'gapfill' to 'r2_tonnes' or 'r2_usd'.
### Optional parameters with defaults: years_back=10, min_paired_obs=4
### columns returned: 
### rgn_name  rgn_id  commodity  product  year  tonnes  usd  gapfill  #tonnes_orig  #usd_orig


h <- h %>% np_end_fill()
### For final year of data, if both usd and tonnes originally reported as NA, pull forward
### values for usd and tonnes from the previous year.  This should happen after regression fill.

h_comm <- h
### Store commodity-level data, before moving on to the product-level smoothing.

#####################################################################
### Output gapfilling report to .csv.
#####################################################################
h_gap <- h_comm %>%
  select(rgn_id, commodity, product, year, gapfill)
write.csv(h_gap, sprintf('%s/data/np_gapfill_report.csv', dir_d), row.names=F, na='')


### Summarize each product per country per year, e.g. all corals in Albania in 2011
h_prod <- h_comm %>%
  group_by(rgn_name, rgn_id, product, year) %>%
  summarize(tonnes = sum(tonnes, na.rm=TRUE), 
            usd = sum(usd, na.rm=TRUE))
            



#####################################################################
### Error-checking and table exports
#####################################################################


### error check for duplicates (same product, same year, same region)
stopifnot(sum(duplicated(h_prod[,c('rgn_id', 'product', 'year')])) == 0)

### Check: wide with all commmodities and product subtotal for comparison with input data
h_x_tonnes <- h_comm %>% 
  bind_rows(mutate(h_prod, commodity='Z_TOTAL')) %>%
  select(rgn_name, rgn_id, commodity, product, year, tonnes) %>%
  arrange(rgn_name, product, commodity, year) %>%
  spread(year, tonnes)
h_x_usd <- h_comm %>% 
  bind_rows(mutate(h_prod, commodity='Z_TOTAL')) %>%
  select(rgn_name, rgn_id, commodity, product, year, usd) %>%
  arrange(rgn_name, product, commodity, year) %>%
  spread(year, usd)
write.csv(h_x_tonnes, sprintf('%s/tmp/np_harvest_tonnes_wide.csv', dir_d, units), row.names=F, na='NA')
write.csv(h_x_usd, sprintf('%s/tmp/np_harvest_usd_wide.csv', dir_d, units), row.names=F, na='NA')


#####################################################################
### Smoothing: determine rolling averages for tonnes and USD in
### order to determine peak values.  This is based upon total
### harvests by product group, not individual commodity.  Perform 
### this for all given scenarios, using a for loop.
#####################################################################

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
    mutate(n_years = length(year)) %>%
    arrange(rgn_id, product, year)

  h <- h %>% np_harvest_smooth()
  ### smooth harvest over 4 year mean (prior and inclusive of current year).
  ### * Tonnes & usd values are smoothed values
  ### * tonnes_orig & usd_orig contain pre-smoothing values
  ### Optional parameter with default: rollwidth = 4
      
  h <- h %>% np_harvest_peak()
  ### get peak harvest, in tonnes and usd, based upon smoothed values.  Also creates 
  ### weighting values by recent USD: w[product] = usd_peak[product] / usd_peak[total for region]
  ### Optional parameters with default: harvest_peak_buffer = 0.35, recent_harvest_years = 10

  h1 <- h %>% np_harvest_status()
  ### Determine relative status score based on harvest (tonnes & usd) relative to peaks.


  
  # ??? How many of these variables do we really need to keep?
  
  
######################got to this point 3/31/15
  

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


