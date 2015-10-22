### data_prep_NP.R
### Prepare FAO commodities data for Natural Products goal. 

### Provenance:
# Apr2015 Updated by oharac. Github ohi-science/issues/issue #370
#   * Minor recoding to get away from using reshape and plyr packages.
#   * Fixed NA fill error for commodities with no non-NA years.
#   * Carved script up into functions for flexibility in trying different approaches.
#     The functions are in ./R/np_fxn.R
#   * Reworked gapfilling to process at commodity level rather than product level.  See 
#     'Gap-filling' section below for more details.
#   * Script outputs gapfill report, np_gapfill_report.csv, by region/commodity/year.
#
# Sep2014 NOTES: MRF: check on how usd_rel is used and cut from functions.R file as a layer read in.
#
# Jun2014 By JSLowndes/bbest ; File was originally clean_FAOcommodities.r:(by JStewart Apr2013)


# To process new data, places to change code:
#   * 'data_year' (in Setup) to reflect new locations for raw, intermediate, and data
#   * 'scenario' to reflect appropriate scenario for output file naming



##############################################################################.
### Setup -----
### load libraries, source function files, set pathnames, etc


### load libraries. Note dplyr, tidyr, stringr are loaded later in common.R
library(zoo)  
library(ohicore) # devtools::install_github('ohi-science/ohicore@dev') # may require uninstall and reinstall

setwd('~/github/ohiprep')

### set dir_neptune_data and load common libraries (tidyr, dplyr, stringr) 
source('src/R/common.R') 
### NOTE: The default path should already be your ohiprep root directory for the rest to work.

### access functions specific to FAO data cleanup
source('src/R/fao_fxn.R')

dir_np    <- 'globalprep/FAO_Commodities'
data_year <- 'v2014_test'
scenario  <- 'eez2014_test'

dir_git   <- file.path('~/github/ohiprep', dir_np)
dir_anx   <- file.path(dir_neptune_data, 'git-annex', dir_np)

### Load NP-specific user-defined functions
source(file.path(dir_np, 'R/np_fxn.R'))


##############################################################################.
### Read and process FAO data -----
### Process FAO data files -- loops across value and quant data

dir_fao_data <- file.path(dir_anx, data_year, 'raw')

for (f in list.files(dir_fao_data, pattern=glob2rx('*.csv'), full.names=T)) { 
  # f = list.files(dir_fao_data, pattern=glob2rx('*.csv'), full.names=T)[1]
  
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
      # swaps out FAO-specific codes. NOTE: optional parameter 'sub_0_0' can be
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
  
  ### examine discrepancies between loaded data and commodity-to-product lookup table
  #   np_commodity_lookup(m, com2prod)

  ### inner_join will attach product names to matching commodities according to
  ### lookup table 'com2prod', and eliminate all commodities that do not appear in the lookup table.
  m <- m %>%
    inner_join(com2prod, by='commodity')
  
  
  ### Special case: user-defined function deals with 
  ###   breaking up Antilles into separate reported rgns
  m <- np_split_antilles(m)
  
    
  ### Add region ID, summarize by region/product/year, save output
  m <- m %>% 
    name_to_rgn(fld_name='country', flds_unique=c('country', 'commodity', 'product', 'year'), 
                fld_value='value', add_rgn_name = T)
    ### ???: name_to_rgn has some old code?  (%.% -> %>%, regroup -> group_by) Update to dplyr/tidyr functions
  
  
  ### units: rename value field to units based on filename
  names(m)[names(m) == 'value'] <- units  
  
  ### output to .csv
  harvest_out <- sprintf('%s/%s/intermediate/%s.csv', dir_git, data_year, units)
  write.csv(m, harvest_out, row.names = FALSE, na = '')
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

  
h_tonnes <- read.csv(file.path(dir_git, data_year, 'intermediate/tonnes.csv'))
h_usd    <- read.csv(file.path(dir_git, data_year, 'intermediate/usd.csv'))

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
### for each commodity within each region, creates (but doesn't save...) summary info:
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

h <- h %>% np_regr_fill(years_back = 10, vars = 'td', scope = 'rgn_id')
h <- h %>% np_regr_fill(vars = 'tdy', scope = 'georgn_id')
h <- h %>% np_regr_fill(vars = 'tdy', scope = 'global')
### np_regr_fill() is a generalized regression gapfill function.  Parameters (with defaults):
### * years_back=50 (int):     This determines how far back in the time series to include within the regression.
### * min_paired_obs=4 (int):  This determines how many paired observations are required to attempt a regression.
### * scope = 'rgn_id' (str):  ('rgn_id', 'georgn_id', 'global') Determines grouping scale for regression.
### * vars = 'tdy' (str):      ('td', 'tdy') Determines model: (tonnes ~ usd) or (tonnes ~ usd + year) [and vice versa]


h <- h %>% np_end_fill()
### For final year of data, if both usd and tonnes originally reported as NA, pull forward
### values for usd and tonnes from the previous year.  This should happen after regression fill.

h_comm <- h
### Store commodity-level data, before moving on to the product-level smoothing.


### Output gapfilling report to .csv.

h_gap <- h_comm %>%
  select(rgn_id, commodity, product, year, gapfill)
file_loc <- file.path(dir_git, data_year, 'data/np_gapfill_report.csv')
write.csv(h_gap, file_loc, row.names = F, na = '')


### Summarize each product per country per year, e.g. all corals in Albania in 2011
h_prod <- h_comm %>%
  group_by(rgn_name, rgn_id, product, year) %>%
  summarize(tonnes = sum(tonnes, na.rm = TRUE), 
            usd = sum(usd, na.rm = TRUE))
            



### Error-checking and table exports

stopifnot(sum(duplicated(h_prod[ , c('rgn_id', 'product', 'year')])) == 0)

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

write.csv(h_x_tonnes, file.path(dir_git, data_year, 'intermediate/np_harvest_tonnes_wide.csv'), row.names = F, na = 'NA')
write.csv(h_x_usd,    file.path(dir_git, data_year, 'intermediate/np_harvest_usd_wide.csv'),    row.names = F, na = 'NA')


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

year_max <- max(h_prod$year)

j <- h_prod %>% np_harvest_smooth()
### smooth harvest over 4 year mean (prior and inclusive of current year).
### * Tonnes & usd values are smoothed values
### * new variables: tonnes_orig & usd_orig contain post-gap-filling, pre-smoothing values
### Optional parameter with default: rollwidth = 4
#rgn_name  rgn_id	product	year	tonnes	usd
#rgn_id  rgn_name	product	year	tonnes	usd	tonnes_orig	usd_orig
    
j <- j %>% np_harvest_peak()
### get peak harvest, in tonnes and usd, based upon smoothed values.  Also creates 
### weighting values by recent USD: w[product] = usd_peak[product] / usd_peak[total for region]
### New variables:   tonnes_peak	usd_peak	prod_weight
### Optional parameters with default: buffer = 0.35, recent_harvest_years = 10

j <- j %>% np_harvest_status()
### Determine relative status score based on harvest (tonnes & usd) relative to peaks.
### New variables: tonnes_rel  usd_rel

  
##############################################################################.
### Write .csv files -----
### Output the results to .csvs for use in toolbox.

### Write entire data frame to .csv:
write.csv(j, file.path(dir_git, data_year, 'intermediate/np_harvest_smoothed_data.csv'), row.names = F, na = '')



### Write single NP output dataframe, incl:
###   rgn_id, rgn_name, product, year, tonnes, tonnes_rel, prod_weight for all years
# write.csv(
#   j %>% select(rgn_id, rgn_name, product, year, tonnes, tonnes_rel, prod_weight),
#   sprintf('%s/data/np_harvest-%s-year_max_%d.csv', dir_git, scenario, year_max), row.names = F, na = '')


### Write individual data layers:
### Write NP weights layer also used to calculate pressures and resilience:
write.csv(
  j %>% 
    filter(year == year_max) %>% 
    select(rgn_id, product, weight = prod_weight),
  sprintf('%s/%s/data/np_harvest_usd_product-peak-year-max-%d.csv', dir_git, data_year, year_max), row.names = F, na = '')

for (lyr in c('tonnes','tonnes_rel')) {
  write.csv(
    j[ , c('rgn_id', 'product', 'year', lyr)],
    sprintf('%s/%s/data/np_harvest_%s-year_max_%d.csv', dir_git, data_year, str_replace(lyr, '_', '-'), year_max), row.names = F, na = '')
}


