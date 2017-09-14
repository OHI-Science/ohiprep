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
### read in and process WTTC files ----
##############################################################################=
### Here are the files I'm working with for 2016.
###   * rgn_wttc_empd_2013.csv : Direct Contribution To Employment: The number of direct jobs within travel and tourism
### Format is similar to: 
#   Direct contribution to employment      1988      1989     1990     1991      1992      1993      1994      1995      1996 ...
#                             Algeria        NA        NA       NA       NA        NA        NA        NA        NA        NA ...
#                   Thousands of jobs 131.27000 97.014900 96.75310 96.04490 102.00300 112.08700 104.26400 110.29600 120.64600 ...
#                    Percentage share   2.32336  2.260370  2.26751  2.15160   2.24575   2.22479   2.02518   2.02912   2.14551 ...
#                              Angola        NA        NA       NA       NA        NA        NA        NA        NA        NA ...
#                   Thousands of jobs  10.97360 16.327900 20.97170 21.39380  19.71360  30.54360  39.61110  44.13890  42.50190 ... 
#                    Percentage share   0.65895  0.943581  1.15234  1.13635   1.00022   1.46309   1.81137   1.93826   1.79914 ...

wttc_files <- list.files(path = file.path(dir_wttc), full.names = TRUE, pattern = glob2rx('*csv'))
#   ~/github/ohiprep/globalprep/WTTC_tourism/v2015/raw/rgn_wttc_empd_2014.csv
#   ~/github/ohiprep/globalprep/WTTC_tourism/v2015/raw/rgn_wttc_empt_2014.csv
#   ~/github/ohiprep/globalprep/WTTC_tourism/v2015/raw/rgn_wttc_gdpt_2014.csv

emp_file <- wttc_files[str_detect(wttc_files, 'empd')]

### Following breaks down employment data into count and percentage, and join into 
###   rgn_name | year | jobs_ct | jobs_pct

  d <- read.csv(emp_file, check.names = FALSE, stringsAsFactors = FALSE)
  names(d)[1] <- 'temp'
  head(d)

  d <- filter(d, temp != "% growth")  ## appears a new variable was added to WTTC analysis
  
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
  
  empd <- full_join(data_count, data_perct, by = c('rgn_name', 'year'))


## some region complications
# Sudan and South Sudan : actually two of our regions, but will consider as Sudan here
# Former Netherlands Antilles : considered 6 regions by OHI: all will get the same score (gapfilling)

empd <- empd %>%
  mutate(rgn_name = as.character(rgn_name)) %>%
  mutate(rgn_name = ifelse(rgn_name == "Sudan and South Sudan", "Sudan", rgn_name))

names <- data.frame(rgn_name = unique(empd$rgn_name), new_rgn_name = unique(empd$rgn_name)) %>%
  filter(rgn_name != "Former Netherlands Antilles") %>%
  rbind(data.frame(rgn_name = "Former Netherlands Antilles", new_rgn_name = c("Bonaire", "Curacao", "Sint Eustatius", "Saba", "Sint Maarten")))
   # Aruba is also considered the "Former Netherlands Antilles, but it is reported separately in the data

empd <- empd %>%
  left_join(names, by='rgn_name') %>%
  select(rgn_name=new_rgn_name, year, jobs_ct, jobs_pct)

### Prep direct and total employment data with name_to_rgn function
empd_rgn <- name_2_rgn(df_in = empd, 
                        fld_name='rgn_name', 
                     flds_unique=c('rgn_name','year'))

  empd_rgn <- empd_rgn %>%
    group_by(rgn_id, year, rgn_name) %>%
    rename(jobs_ct_orig = jobs_ct, jobs_pct_orig = jobs_pct) %>%  
      ### temporary rename, so summary can end up with the original column names
    summarize(jobs_ct = sum(jobs_ct_orig, na.rm=TRUE), 
              jobs_pct = weighted.mean(jobs_pct_orig, jobs_ct_orig, na.rm = TRUE)) %>%
    data.frame()

  
### write csvs to github in intermediate location.
write_csv(empd_rgn, 'intermediate/wttc_empd_rgn.csv')

