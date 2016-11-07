# process_WTTC.R
# Do not run stand-alone - source from main data_prep.R for TourismRecreation.
#
# Add rgn_ids for World Travel and Tourism Council (WTTC)
# Previously had been named clean_WTTC.r (by JStewart May2013). This script created by JStewartLowndes Mar2014.
#
#   read in .xls files from the WTTC raw folder downloaded from www.wttc.org/research/economic-data-search-tool
#   data_prep.R. will reformat so that data aren't on every other line like the .xls table
#   files must be accessed to the same year (eg end year 2013, assuming all after that is projected)
#   adds identifier and units columns, and then runs add_rgn_id.r. 
#   no georegional gapfilling -- but save as separate files  
#

##############################################################################=
### setup ----
##############################################################################=
### Libraries and such are set up within data_prep.R

dir_wttc <- file.path(dir_M, 'git-annex/globalprep/_raw_data/WTTC/d2015')
  # backs out of TourismRecreation and then to WTTC_tourism directory


##############################################################################=
### read in and process WTTC files ----
##############################################################################=
### Here are the files I'm working with for 2015.
###   * rgn_wttc_empd_2013.csv : Direct Contribution To Employment: The number of direct jobs within travel and tourism
###   * rgn_wttc_empt_2013.csv : Total Contribution To Employment: The number of jobs generated directly in the Travel and Tourism sector plus the indirect and induced contributions
###   * rgn_wttc_gdpt_2013.csv : Total Contribution to GDP: GDP generated directly by the Travel and Tourism sector plus its indirecdt and induced impacts
### Format is similar to: 
#   Direct contribution to employment      1988      1989     1990     1991      1992      1993      1994      1995      1996 ...
#                             Algeria        NA        NA       NA       NA        NA        NA        NA        NA        NA ...
#                   Thousands of jobs 131.27000 97.014900 96.75310 96.04490 102.00300 112.08700 104.26400 110.29600 120.64600 ...
#                    Percentage share   2.32336  2.260370  2.26751  2.15160   2.24575   2.22479   2.02518   2.02912   2.14551 ...
#                              Angola        NA        NA       NA       NA        NA        NA        NA        NA        NA ...
#                   Thousands of jobs  10.97360 16.327900 20.97170 21.39380  19.71360  30.54360  39.61110  44.13890  42.50190 ... 
#                    Percentage share   0.65895  0.943581  1.15234  1.13635   1.00022   1.46309   1.81137   1.93826   1.79914 ...

wttc_files <- list.files(path = file.path(dir_wttc, 'raw'), full.names = TRUE, pattern = glob2rx('*csv'))
#   ~/github/ohiprep/globalprep/WTTC_tourism/v2015/raw/rgn_wttc_empd_2014.csv
#   ~/github/ohiprep/globalprep/WTTC_tourism/v2015/raw/rgn_wttc_empt_2014.csv
#   ~/github/ohiprep/globalprep/WTTC_tourism/v2015/raw/rgn_wttc_gdpt_2014.csv

empd_file <- wttc_files[str_detect(wttc_files, 'empd')]
empt_file <- wttc_files[str_detect(wttc_files, 'empt')]
# gdpt_file <- wttc_files[str_detect(wttc_files, 'gdpt')] # not used in TR goal

### Function to break down employment data into count and percentage, and join into 
###   rgn_name | year | jobs_ct | jobs_pct
process_emp_data <- function(emp_file) {
  d <- read.csv(emp_file, check.names = FALSE, stringsAsFactors = FALSE)
  names(d)[1] <- 'temp'
  head(d)
  
  data_rows <- unlist(d[2:3, 1])
  
  rgn_name <- d %>% 
    filter(!temp %in% data_rows) %>% 
    select(country = temp)
  rgn_name[nrow(rgn_name), 1]
  # note: last one is 'Source: WTTC'
  rgn_name <- rgn_name[1:(nrow(rgn_name) - 1), ]
  
  data_count <- cbind(rgn_name,
                      d %>% filter(temp == data_rows[1]) %>%
                        select(-temp))
  data_count <- data_count %>%
    gather(year, jobs_ct, -rgn_name) %>%
    mutate(jobs_ct = round(jobs_ct * 1000),
           year = as.integer(as.character(year)))
  data_perct <- cbind(rgn_name,
                      d %>% filter(temp == data_rows[2]) %>%
                        select(-temp))
  data_perct <- data_perct %>%
    gather(year, jobs_pct, -rgn_name) %>%
    mutate(year = as.integer(as.character(year)))
  
  data_full <- full_join(data_count, data_perct, by = c('rgn_name', 'year'))
  return(data_full)
}
  
empd <- process_emp_data(empd_file)
empt <- process_emp_data(empt_file)

empd <- empd %>%
  filter(!(rgn_name=='Macau' & year < 1990))
    ### NOTE: Macau's reported tourism job percentage is 3.34595e+06 and 4.00790e+06... 
    ### ... just gonna filter those out
  
### Prep direct and total employment data with name_to_rgn function
empd_rgn <- name_to_rgn(empd, 
                        fld_name='rgn_name', flds_unique=c('rgn_name','year', 'jobs_pct'), 
                        fld_value='jobs_ct', add_rgn_name = TRUE, 
                        collapse_fxn = 'sum_na',
                        dir_lookup = "src/LookupTables")
# ??? collapse_fxn = 'sum_na' is not summing them - leaves separate
dupes <- unique(empd_rgn$rgn_name[duplicated(empd_rgn[ , 1:2])])
cat(sprintf('Duplicated regions: \n%s\n', paste(dupes, collapse = ', ')))
# ??? This affects China, as well as 
#     "Guadeloupe and Martinique" and
#     "Puerto Rico and Virgin Islands of the United States"
# ??? Adding this section to get around it for now:
if(length(dupes) > 0) {
  empd_rgn <- empd_rgn %>%
    group_by(rgn_id, year, rgn_name) %>%
    rename(jobs_ct_orig = jobs_ct, jobs_pct_orig = jobs_pct) %>%  
      ### temporary rename, so summary can end up with the original column names
    summarize(jobs_ct = sum(jobs_ct_orig), jobs_pct = weighted.mean(jobs_pct_orig, jobs_ct_orig, na.rm = TRUE))
} else cat('No duplicates in direct tourism employment to fix.\n')

# ??? NOTE: the same problem will occur here, but not using this data anyway...
empt_rgn <- name_to_rgn(empt, 
                        fld_name='rgn_name', flds_unique=c('rgn_name','year', 'jobs_pct'), 
                        fld_value='jobs_ct', add_rgn_name = TRUE, 
                        collapse_fxn = 'sum_na')
dupes <- unique(empd_rgn$rgn_name[duplicated(empd_rgn[ , 1:2])])
cat(sprintf('Duplicated regions: \n%s\n', paste(dupes, collapse = ', ')))
if(length(dupes) > 0) {
  empt_rgn <- empt_rgn %>%
    group_by(rgn_id, year, rgn_name) %>%
    summarize(jobs_ct = sum(jobs_ct))
} else cat('No duplicates in total tourism employment to fix.\n')

### write csvs to github in TourismRecreation/v2015/intermediate location.
write_csv(empd_rgn, file.path(dir_int, 'wttc_empd_rgn.csv'))
write_csv(empt_rgn, file.path(dir_int, 'wttc_empt_rgn.csv'))

### spot check:
# us_test <- empt_rgn %>% filter(rgn_id == 163) %>% arrange(year)
# us_test1 <- empt %>% filter(rgn_name == 'United States') %>% arrange(year)

##############################################################################=
### Further examine employment data for TR layers ----
##############################################################################=
### Which data set is used for tr_jobs_tourism?  Direct employment in tourism
###   appears to be better fit than total employment in tourism.  Refer to checks below.

### comparisons.  See more analysis in data_compare.R
# old_tr_jobs_tour <- read.csv('~/github/ohi-global/eez2013/layers/tr_jobs_tourism.csv')
# 
# new_jobs_tot <- empt_rgn %>%
#   select(-rgn_name, -jobs_pct, count_tot = jobs_ct) %>%
#   arrange(rgn_id, year)
# new_jobs_dir <- empd_rgn %>%
#   select(-rgn_name, -jobs_pct, count_dir = jobs_ct) %>%
#   arrange(rgn_id, year)
# jobs_compare <- old_tr_jobs_tour %>%
#   full_join(new_jobs_tot, by = c('rgn_id', 'year')) %>%
#   full_join(new_jobs_dir, by = c('rgn_id', 'year')) %>%
#   mutate(tot_ratio = round(count/count_tot * 100, 2),
#          dir_ratio = round(count/count_dir * 100, 2))
# ### Comparing the two data sets, pretty clear that direct employment fits better
# ### to older data.  Let's examine outliers outside of specified difference:
# x <- 10
# jobs_poor_match <- jobs_compare %>%
#   filter(dir_ratio <= 100-x | dir_ratio >= 100+x)
# unique(jobs_poor_match$rgn_id)
# 
# 
