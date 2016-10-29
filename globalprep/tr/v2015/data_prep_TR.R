# data_prep.R for Tourism & Recreation - master data_prep.R file
# Jul2015: Casey O'Hara - combining multiple scripts and data sets into one
#   master script.  
#   Supplemental scripts are located in TourismRecreation/R.

#   Outputs:
#   * tr_unemployment.csv
#     * rgn_id, year, percent
#     * Percent unemployment (0-100%)
#   * tr_sustainability.csv
#     * rgn_id, score (no year value - only current year)
#     * TTCI score, not normalized (1-7)
#   * tr_jobs_tourism.csv
#     * rgn_id, year, jobs_ct (individuals)
#     * Number of jobs, direct employment in tourism
#   * tr_jobs_pct_tourism.csv
#     * rgn_id, year, jobs_pct
#     * Percent of direct tourism jobs
#   * tr_jobs_total.csv
#     * rgn_id, year, count (individuals)
#     * Total jobs

##############################################################################=
### setup -----
##############################################################################=
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

setwd('~/github/ohiprep')
source('src/R/common.R')
library(readr)

goal     <- 'globalprep/tr'
scenario <- 'v2015'
dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal) 
dir_git  <- file.path(goal, scenario)
dir_data <- file.path(dir_git, 'data')
dir_int  <- file.path(dir_git, 'intermediate')
# dir_git points to TourismRecreation; dir_data and dir_int point to v201X/data and v201x/intermediate
#   within the TourismRecreation directory.

source(file.path(dir_git, 'R/tr_fxns.R'))



##############################################################################=
### Process data and layers ----
##############################################################################=

tr_data_files <- c(unem      = file.path(dir_int, 'wb_rgn_uem.csv'),
                   jobs_tot  = file.path(dir_int, 'wb_rgn_tlf.csv'),
                   sust      = file.path(dir_int, 'wef_ttci_2015.csv'),
                   jobs_tour = file.path(dir_int, 'wttc_empd_rgn.csv'))

tr_prep_data(tr_data_files, reload = TRUE)
### Process each data set and saves tidied data in v201X/intermediate directory.

tr_layers <- c(unem     = file.path(dir_int, 'tr_pregap_unemployment.csv'),
               jobs_tot = file.path(dir_int, 'tr_pregap_jobs_total.csv'),
               sust     = file.path(dir_int, 'tr_pregap_sustainability.csv'),
               jobs_tour     = file.path(dir_int, 'tr_pregap_jobs_tourism.csv'),
               jobs_pct_tour = file.path(dir_int, 'tr_pregap_jobs_pct_tourism.csv'))

tr_prep_layers(tr_layers, tr_data_files, reload = TRUE)
### Separate out just the model variables and save these to v201X/data directory,
### ready for use in the toolbox.


##############################################################################=
### Assembling the data from layers -----
##############################################################################=
year_max    <- 2013

tr_data_raw <- tr_assemble_layers(tr_layers)


### Attach georegions and per-capita GDP info for various gapfilling
georegions       <- read.csv('../ohi-global/eez2013/layers/rgn_georegions.csv', na.strings='')
georegion_labels <- read.csv('../ohi-global/eez2013/layers/rgn_georegion_labels.csv')

tr_data_raw <- tr_data_raw %>%
  left_join(georegion_labels %>%
              spread(level, label) %>%
              select(-r0),
            by = 'rgn_id') %>%
  filter(rgn_id != 255) # ditch disputed regions...

gdppcppp <- read.csv(file.path(dir_int, 'wb_rgn_gdppcppp.csv')) %>%
  select(rgn_id, year, pcgdp = intl_dollar)
tr_data_raw <- tr_data_raw %>%
  left_join(gdppcppp, by = c('rgn_id', 'year'))


### Add gapfill flag variable 

tr_data_raw <- tr_data_raw %>% gapfill_flags()

write_csv(tr_data_raw, file.path(dir_int, 'tr_data_raw.csv'))


  
##############################################################################=
### Gapfilling ----
##############################################################################=

### Gapfill S using r1 and/or r2 regional data and PPP-adjusted per-capita GDP
tr_data_raw <- read.csv(file.path(dir_int, 'tr_data_raw.csv'), stringsAsFactors = FALSE)

tr_data <- tr_data_raw %>% gdp_gapfill()
### gap fill any missing GDP values, so the TTCI Score gapfill can use gdp as a proxy

tr_data <- s_gapfill_r2_r1(tr_data)

# Apply only the 2013 S_score to all years - so it's consistent, as we only have
# actual scores from the current year.  NOTE: doesn't change gapfill flag for past years...
tr_data <- tr_data %>%
  select(-S_score) %>%
  left_join(tr_data %>%
              filter(year == year_max) %>%
              select(rgn_id, S_score),
            by = 'rgn_id')

### Gapfill Ep using regional averages

tr_data <- tr_data %>%
  group_by(r2, year) %>%
  mutate(E_mdl2 = mean(Ep, na.rm = TRUE),
         gaps   = ifelse(is.na(Ep) & !is.na(E_mdl2), str_replace(gaps, 'E', 'r'), gaps),
         gaps   = ifelse(is.na(Ep) & !is.na(E_mdl2), str_replace(gaps, 'U', '*'), gaps),
         gaps   = ifelse(is.na(Ep) & !is.na(E_mdl2), str_replace(gaps, 'L', '*'), gaps),
         Ep     = ifelse(is.na(Ep), E_mdl2, Ep)) %>%
  select(-E_mdl2) %>%
  ungroup()

# summary(lm(E_mdl2 ~ Ep, data = tr_data)) # R^2 = .3124 before replacement
# 
# library(ggplot2)
# ggplot(data1 %>% filter(year == year_max), 
#        aes(x = Ep, y = E_mdl2, color = r1)) +
#   geom_point() + 
#   geom_abline(slope = 1, intercept = 0, color = 'red') +
#   labs(x = '% tourism employment, reported',
#        y = '% tourism employment, rgn avg',
#        title = 'Comparison of Tourism/Total jobs')

### write full gapfilled data set to the intermediate directory
write_csv(tr_data, file.path(dir_int, 'tr_data_processed.csv'))

### write layers, post-gapfill, to data directory-----
write_csv(tr_data %>% select(rgn_id, year, U),       file.path(dir_data, 'tr_unemployment.csv'))
write_csv(tr_data %>% select(rgn_id, year, Ed),      file.path(dir_data, 'tr_jobs_tourism.csv'))
write_csv(tr_data %>% select(rgn_id, year, Ep),      file.path(dir_data, 'tr_jobs_pct_tourism.csv'))
write_csv(tr_data %>% select(rgn_id, year, L),       file.path(dir_data, 'tr_jobs_total.csv'))
write_csv(tr_data %>% filter(year == year_max) %>% select(rgn_id, S_score), file.path(dir_data, 'tr_sustainability.csv'))
  ### NOTE: only writing most recent year of sustainability index - use same value across all years.

write_csv(tr_data %>% select(rgn_id, year, gaps),    file.path(dir_data, 'tr_gapfill.csv'))

##############################################################################=
### Run model (transfer to functions.R) -----
##############################################################################=
### Load data layers, reassemble, and process them through the model calculations.

# rgn_names        <- read_csv('~/github/ohi-global/eez2013/layers/rgn_global.csv', stringsAsFactors = FALSE) %>%
#   rename(rgn_name = label)
# 
# tr_data1 <- read_csv(file.path(dir_data, 'tr_jobs_pct_tourism.csv')) %>%
#   full_join(read_csv(file.path(dir_data, 'tr_jobs_tourism.csv')), by = c('rgn_id', 'year')) %>%
#   full_join(read_csv(file.path(dir_data, 'tr_unemployment.csv')), by = c('rgn_id', 'year')) %>%
#   full_join(read_csv(file.path(dir_data, 'tr_jobs_total.csv')), by = c('rgn_id', 'year')) %>%
#   full_join(read_csv(file.path(dir_data, 'tr_sustainability.csv')), by = c('rgn_id')) %>%
#     ### NOTE: just using most recent sustainability year value across all years, since TTCI data
#     ###   is only available for most recent year
#   full_join(read_csv(file.path(dir_data, 'tr_gapfill.csv')), by = c('rgn_id', 'year')) %>%
#   full_join(rgn_names, by = 'rgn_id') %>%
#   filter(year <= year_max)
# 
# tr_model <- tr_calc_model(tr_data1)  %>%
#   filter(year <= year_max & year > year_max - 5) 
# # five data years, four intervals
# 
# 
# 
# ### Write the un-normalized model calculations.
# write_csv(tr_model, file.path(dir_int, 'tr_model.csv'))
# 
# # regions with Travel Warnings at http://travel.state.gov/content/passports/english/alertswarnings.html
# rgn_travel_warnings <- read.csv(file.path(dir_data, 'tr_travelwarnings_2015.csv'), stringsAsFactors = F) %>%
#   select(rgn_name) %>%
#   left_join(rgn_names, by = 'rgn_name') %>%
#   filter(!is.na(rgn_id))
# # TODO: check if regions with travel warnings are gapfilled (manually checked for 2013)
# tr_model <- tr_model %>%
#   filter(!rgn_id %in% rgn_travel_warnings$rgn_id) %>%
#   bind_rows(tr_model %>%
#               filter(rgn_id %in% rgn_travel_warnings$rgn_id) %>%
#               mutate(
#                 Xtr = 0.1 * Xtr))
# 
# ### Calculate status based on quantile reference
# pct_ref <- 90 # the threshold for quantile where status score = 1.0
# 
# tr_scores <- tr_model %>%
#   select(rgn_id, S_score, year, rgn_name, Xtr, gaps) %>%
#     left_join(tr_model %>%
#               group_by(year) %>%
#               summarize(Xtr_q = quantile(Xtr, probs = pct_ref/100, na.rm = TRUE)),
#             by = 'year') %>%
#   mutate(
#     Xtr_rq  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q)) # rescale to qth percentile, cap at 1
# 
# write_csv(tr_scores, file.path(dir_int, 'tr_scores.csv'))
# 
