# data_prep.R

### NOTES: MRF Sep 2 2014: check on how usd_rel is used and cut from functions.R file as a layer read in.

# Prepare FAO commodities data for Natural Products goal. 
# To process new data, places to change code:
#   * 'dir_d' (in Setup) to reflect new location for raw, tmp, and data
#   * 'scenarios_year_max' (in Smoothing and Scoring) to include most recent data year.
#   * 'scenario' list in 'for (scenario in...) {}' (in Smoothing and Scoring) to capture
#     updated list of scenarios.
#
# Updated Apr2015 by oharac. Github ohi-science/issues/issue #370
#   Minor recoding to get away from using reshape and plyr packages.
#
#   Fixed NA fill error for commodities with no non-NA years.
#
#   Carved script up into functions for flexibility in trying different approaches.
#     The functions are in ./R/np_fxn.R
#
#   Reworked gapfilling to process at commodity level rather than product level.  See 
#     'Gap-filling' section below for more details.
#
#   Script outputs gapfill report, np_gapfill_report.csv, by region/commodity/year.
#
#


##############################################################################.
### Setup -----
### load libraries, source function files, set pathnames, etc


### load libraries. Note dplyr, tidyr, stringr are loaded later in common.R
library(zoo)  
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

### set dir_neptune_data and load common libraries (tidyr, dplyr, stringr) 
source('src/R/common.R') 
### NOTE: The default path should already be your ohiprep root directory for the rest to work.
###       Otherwise, presume that scripts are always working from your default ohiprep folder


dir_np <- 'globalprep/FAO_Commodities'
data_year <- 'v2014_test'
dir_d <- sprintf('%s/%s', dir_np, data_year)
### NOTE: Set output paths to here, but do not use setwd().
###       This way, any scripts and code in ohiprep will still work, b/c always in root ohiprep dir.

### Load NP-specific user-defined functions and functions specific to FAO data (cleanup)
source(sprintf('%s/R/np_fxn.R', dir_np))
source(sprintf('%s/R/fao_fxn.R', dir_np))



##############################################################################.
### Read and process FAO data -----
### Process FAO data files -- loops across value and quant data


for (f in list.files(file.path(dir_d, 'raw'), pattern=glob2rx('*.csv'), full.names=T)) { 
  # f=list.files(file.path(dir_d, 'raw'), pattern=glob2rx('*.csv'), full.names=T)[1]
  
  ### data read in
  cat(sprintf('\n\n\n====\nfile: %s\n', basename(f)))
  d <- read.csv(f, check.names=F, strip.white=TRUE) # , stringsAsFactors=T
  units <- c('tonnes','usd')[str_detect(f, c('quant','value'))] # using American English, lowercase
  

  ### gather into long format and clean up FAO-specific data foibles

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
    fao_clean_data() %>%  
      # swaps out FAO-specific codes. NOTE: optional parameter 'lowdata_value' can be
      # passed to control how a '0 0' code is interpreted.
    select(-trade)   %>%                                  
      # eliminate 'trade' column
    arrange(country, commodity, is.na(value), year)

  
  ### Products join: attach product categories from com2prod, and
  ###   filter out all entries that do not match a product category.
  ### Note: commodity_lookup is user-defined function to compare 
  ###   commodities in data vs commodities in lookup table

  ### load lookup for converting commodities to products

  com2prod <- read.csv(file.path(dir_np, 'commodities2products.csv'), na.strings='')
  
  #   np_commodity_lookup(m, com2prod) # in './R/np_fxn.R'

  m <- m %>%
    inner_join(com2prod, by='commodity')
  
  
  ### Special case: user-defined function deals with 
  ###   breaking up Antilles into separate reported rgns
  
  m <- np_split_antilles(m) # in './R/np_fxn.R'
  
    
  ### Add region ID, summarize by region/product/year, save output

  m <- m %>% 
    name_to_rgn(fld_name='country', flds_unique=c('country', 'commodity', 'product', 'year'), 
                fld_value='value', add_rgn_name=T)
    ### ???: name_to_rgn has some old code?  (%.% -> %>%, regroup -> group_by) Update to dplyr/tidyr functions
  
  
  ### units: rename value field to units based on filename
  names(m)[names(m) == 'value'] <- units  
  
  ### output to .csv
  f_out <- sprintf('%s/data/%s_%s.csv', dir_d, data_year, units)
  write.csv(m, f_out, row.names=F, na='')
}



  
##############################################################################.
### Gap-filling -----
### See issue #397 for details and debate and pretty graphs.
###   * Zero-fill: for observations with NAs for both values (tonnes & usd), fill both as zero 
###     Also cross-fills zeros where one value is zero, other is NA
###   * Regression fill, first pass: Where enough non-zero paired observations exist at the country level, 
###     use country-level data to create regression models (tonnes ~ usd and vice versa) for 
###     gapfilling.  About 25% success. 
###   * Regression fill, second pass: Where pass 1 failed, and enough non-zero paired observations exist at
###     georegional level, use georegional-level data to create regression models (tonnes ~ usd + year, and
###     vice versa) for gapfilling.  About 90% success. 
###   * Regression fill third pass: Where passes 1 and 2 failed, use global-scale data to create 
###     regression models (tonnes ~ usd + year, and vice versa) for gapfilling.  100% success.
###   * End-fill:  For years where NAs still exist in final year, carry forward data from prior year
###     (after other gapfilling techniques).

  
h_tonnes <- read.csv(file.path(dir_d, sprintf('data/%s_tonnes.csv', data_year)))
h_usd    <- read.csv(file.path(dir_d, sprintf('data/%s_usd.csv',    data_year)))

h <- np_harvest_cat(h_tonnes, h_usd)
### concatenates h_tonnes and h_usd data
### h includes rgn_name, rgn_id, commodity, product, year, tonnes, usd.


h <- h %>% np_harvest_preclip
### clips out years prior to first reporting year, for each commodity per region
    
h <- h %>% np_harvest_gapflag  
### Adds flag for required gap-filling, based upon NAs in data. 
### NOTE: Does not perform any gap-filling.
### At this point, h includes: 
###   rgn_name   rgn_id   commodity   product   year   tonnes   usd   gapfill
### 'gapfill' will be in (zerofill, endfill, tbd, none)

data_check <- h %>% np_datacheck()
### for each commodity within each region, creates (doesn't save...) summary info:
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

h <- h %>% add_georegion_id()
### Melanie's script to add a georegional ID tag based on country keys and IDs.

h1 <- h  %>% np_regr_fill(years_back=10, vars='td', scope='rgn_id')
h2 <- h1 %>% np_regr_fill(vars='tdy', scope='georgn_id')
h3 <- h2 %>% np_regr_fill(vars='tdy', scope='global')
### np_regr_fill() is a generalized regression gapfill function.  Parameters (with defaults):
### * years_back=50 (int):     This determines how far back in the time series to include within the regression.
### * min_paired_obs=4 (int):  This determines how many paired observations are required to attempt a regression.
### * scope = 'rgn_id' (str):  ('rgn_id', 'georgn_id', 'global') Determines grouping scale for regression.
### * vars = 'tdy' (str):      ('td', 'tdy') Determines model: (tonnes ~ usd) or (tonnes ~ usd + year) [and vice versa]


h3 <- h3 %>% np_end_fill()
### For final year of data, if both usd and tonnes originally reported as NA, pull forward
### values for usd and tonnes from the previous year.  This should happen after regression fill.

h_comm <- h3
### Store commodity-level data, before moving on to the product-level smoothing.


### Output gapfilling report to .csv.

h_gap <- h_comm %>%
  select(rgn_id, commodity, product, year, gapfill)
write.csv(h_gap, sprintf('%s/data/np_gapfill_report.csv', dir_d), row.names=F, na='')


### Summarize each product per country per year, e.g. all corals in Albania in 2011
h_prod <- h_comm %>%
  group_by(rgn_name, rgn_id, product, year) %>%
  summarize(tonnes = sum(tonnes, na.rm=TRUE), 
            usd = sum(usd, na.rm=TRUE))
            



### Error-checking and table exports

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


##############################################################################.
### Smoothing and scoring ------
### Determine rolling averages for tonnes and USD in order to determine peak 
### values.  This is based upon total harvests by product group, not 
### individual commodity.  
### ??? NOTE: Since we are using a right-aligned rolling average, it is
###     likely that we can just run rolling average once over whole data
###     set, rather than re-running for each series of years?  Not gonna
###     save a lot of time, but more efficient...
### Score harvest (tonnes and usd) relative to peaks. Output values as .csvs.
### Perform this for all given scenarios, using a for loop.


### ???: update this to something like year_max, year_max - 1, year_max - 2, etc
scenarios_year_max <- c(eez2014=2011, 
                        eez2013=2010, 
                        eez2012=2009)

for (scenario in c('eez2012','eez2013','eez2014')) { 
  # scenario  = 'eez2012'
  year_max <- scenarios_year_max[[scenario]]
  
  j <- h_prod %>%
    filter(year<=year_max) %>%
    group_by(rgn_id, product) %>%
    mutate(n_years = length(year)) %>%
    arrange(rgn_id, product, year)

  j <- j %>% np_harvest_smooth()
  ### smooth harvest over 4 year mean (prior and inclusive of current year).
  ### * Tonnes & usd values are smoothed values
  ### * tonnes_orig & usd_orig contain post-gap-filling, pre-smoothing values
  ### Optional parameter with default: rollwidth = 4
      
  harvest_peak_buffer = 0.35
  j <- j %>% np_harvest_peak(buffer = harvest_peak_buffer)
  ### get peak harvest, in tonnes and usd, based upon smoothed values.  Also creates 
  ### weighting values by recent USD: w[product] = usd_peak[product] / usd_peak[total for region]
  ### Optional parameters with default: buffer = 0.35, recent_harvest_years = 10

  j <- j %>% np_harvest_status()
  ### Determine relative status score based on harvest (tonnes & usd) relative to peaks.


  
##############################################################################.
### Write .csv files -----
### Output the results to .csvs for use in toolbox.
### ??? How many of these variables do we really need to keep, how many to be reported to the .csv files?

  write.csv(j  , sprintf('%s/tmp/%s_np_harvest_smoothed_data.csv', dir_d, scenario), row.names=F, na='')
  
  # write NP weights layer also used to calculate pressures and resilience
  write.csv(
    j %>% filter(year == year_max) %>% select(rgn_id, product, weight=usd_peak_product_weight),
    sprintf('%s/data/np_harvest_%s_product-peak_%s-year-max-%d_buffer-%g.csv', dir_d, 'usd', scenario, year_max, harvest_peak_buffer), row.names=F, na='')
  
  # write NP status layers
  for (lyr in c('tonnes','tonnes_rel','usd','usd_rel')) {
    write.csv(
      j[,c('rgn_id', 'product', 'year', lyr)],
      sprintf('%s/data/np_harvest_%s_%s-year-max-%d_buffer-%g.csv', dir_d, str_replace(lyr, '_','-'), scenario, year_max, harvest_peak_buffer), row.names=F, na='')
  }
}


