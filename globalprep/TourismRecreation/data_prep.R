# data_prep.R for Tourism & Recreation - master data_prep.R file

#   Outputs:
#   * tr_unemployment.csv
#     * rgn_id, year, percent
#   * tr_sustainability.csv
#     * rgn_id, score (no year value - most recent year?)
#   * tr_jobs_tourism.csv
#     * rgn_id, year, count (individuals)
#   * tr_jobs_total.csv
#     * rgn_id, year, count (individuals)

##############################################################################=
### setup ----
##############################################################################=
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

setwd('~/github/ohiprep')
source('src/R/common.R')
library(readr)

goal     <- 'globalprep/TourismRecreation'
scenario <- 'v2015'
dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal) 
dir_git  <- file.path('~/github/ohiprep', goal)
dir_data <- file.path(dir_git, scenario, 'data')
dir_int  <- file.path(dir_git, scenario, 'intermediate')
# dir_git points to TourismRecreation; dir_data and dir_int point to v201X/data and v201x/intermediate
#   within the TourismRecreation directory.

source(file.path(dir_git, 'process_WTTC.R'))
source(file.path(dir_git, 'process_WEF.R'))
source(file.path(dir_git, 'process_WorldBank.R'))

#   * tr_unemployment.csv from World Bank data
tr_unem <- read.csv(file.path(dir_int, 'wb_rgn_uem.csv'), stringsAsFactors = FALSE) %>%
  select(rgn_id, year, percent)
write_csv(tr_unem, file.path(dir_data, 'tr_unemployment.csv'))

#   * tr_sustainability.csv from WEF TTCI
tr_sust <- read.csv(file.path(dir_int, 'wef_ttci_2015.csv'), stringsAsFactors = FALSE) %>%
  select(rgn_id, score)
write_csv(tr_sust, file.path(dir_data, 'tr_sustainability.csv'))

#   * tr_jobs_tourism.csv from WTTC direct tourism employment
tr_jobs_tour <- read.csv(file.path(dir_int, 'wttc_empd_rgn.csv'), stringsAsFactors = FALSE) %>%
  select(rgn_id, year, count = jobs_ct)
write_csv(tr_jobs_tour, file.path(dir_data, 'tr_jobs_tourism.csv'))

#   * tr_jobs_total.csv from World Bank total labor force
#     * rgn_id, year, count (individuals)
tr_jobs_tot <- read.csv(file.path(dir_int, 'wb_rgn_tlf.csv'), stringsAsFactors = FALSE) %>%
  select(rgn_id, year, count)
write_csv(tr_jobs_tot, file.path(dir_data, 'tr_jobs_total.csv'))
