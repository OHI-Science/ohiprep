# data_prep.R for CITES signatories

# processing and adding rgn_ids to CITES Signatories
# Jul2015 by CO'Hara; updating for new data
# Mar2014 by JSLowndes; previously had been clean_CITES.r (by JStewart May2013)

# Input:
# * ~/github/ohiprep/globalprep/cites_signatories/raw/cites_member_countries_<year-month>.csv
#   - .csv of CITES signatories from https://cites.org/eng/disc/parties/chronolo.php
# Output:
# * ~/github/ohiprep/globalprep/cites_signatories/v201x/data/rgn_cites_sigs.csv

##############################################################################=
### setup -----
##############################################################################=
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

setwd('~/github/ohiprep')
source('src/R/common.R')

goal     <- 'globalprep/cites_signatories'
scenario <- 'v2015'
dir_git  <- file.path('~/github/ohiprep', goal)
dir_raw  <- file.path(dir_git, 'raw')
dir_data <- file.path(dir_git, scenario, 'data')

# get functions
source('src/R/ohi_clean_fxns.R')


## read in and process files ----

cites_raw <- read.csv(file.path(dir_raw, 'cites_member_countries_2015-07.csv'), stringsAsFactors = FALSE) %>%
  select(rgn_name    = Official.name,
         rgn_iso2    = ISO,
         georgn      = Region,
         type        = Type,
         date_signed = Date.of.joining,
         date_force  = Entry.into.force) %>%
  separate(date_signed, c('d_s', 'm_s', 'y_s')) %>%
  mutate(y_s = as.integer(y_s),
         y_s = ifelse(y_s < 50, y_s + 2000,
                 ifelse(y_s < 100, y_s + 1900,
                   y_s))) # 

cites <- cites_raw %>%
  select(rgn_name, year_signed = y_s) %>%
  name_to_rgn(fld_name     = 'rgn_name', 
              fld_value    = 'year_signed',
              add_rgn_name = TRUE)

rgn_names <- read.csv('~/github/ohi-global/eez2013/layers/rgn_global.csv') %>%
  rename(rgn_name = label)

cites <- cites %>% 
  full_join(rgn_names, by = c('rgn_id', 'rgn_name')) %>%
  mutate(year_signed = ifelse(is.na(year_signed), 0, year_signed)) %>%
  arrange(rgn_id)


f_save <- 'rgn_cites_sigs.csv'
write.csv(cites, file.path(dir_data, f_save), na = '', row.names=FALSE)
  
  
 # --- fin




