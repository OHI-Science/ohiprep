# data_prep.R for Tourism & Recreation - master data_prep.R file

#   Outputs:
#   * tr_unemployment.csv
#     * rgn_id, year, percent
#     * Percent unemployment (0-100%)
#   * tr_sustainability.csv
#     * rgn_id, score (no year value - only current year)
#     * TTCI score, not normalized (1-7)
#   * tr_jobs_tourism.csv
#     * rgn_id, year, count (individuals)
#     * Number of jobs, direct employment in tourism
#   * tr_jobs_total.csv
#     * rgn_id, year, count (individuals)
#     * Total jobs

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


##############################################################################=
### Process data sets individually ----
##############################################################################=
### Process each data set and saves tidied data in v201X/intermediate directory.

source(file.path(dir_git, 'process_WTTC.R'))
source(file.path(dir_git, 'process_WEF.R'))
source(file.path(dir_git, 'process_WorldBank.R'))


##############################################################################=
### Prep data into individual layers for toolbox ----
##############################################################################=
### Separate out just the model variables and save these to v201X/data directory,
### ready for use in the toolbox.

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


##############################################################################=
### Testing the model ----
##############################################################################=

tr_unem      <- read.csv(file.path(dir_data, 'tr_unemployment.csv'))
tr_sust      <- read.csv(file.path(dir_data, 'tr_sustainability.csv'))
tr_jobs_tour <- read.csv(file.path(dir_data, 'tr_jobs_tourism.csv'))
tr_jobs_tot  <- read.csv(file.path(dir_data, 'tr_jobs_total.csv'))
rgn_names    <- read_csv('~/github/ohi-global/eez2013/layers/rgn_global.csv') %>%
  rename(rgn_label = label)
year_max     <- 2013

tr_data <- tr_jobs_tour %>%
  rename(Ed = count) %>%
  full_join(tr_jobs_tot %>%
              rename(L = count),
            by = c('rgn_id', 'year')) %>%
  full_join(tr_unem %>%
              rename(U = percent) %>%
              mutate(U = U/100),
            by = c('rgn_id', 'year')) %>%
  full_join(tr_sust %>%
              rename(S_score = score),
            by = 'rgn_id') %>%
  full_join(rgn_names, by = 'rgn_id') %>%
  filter(year <= year_max)

tr_model <- tr_data %>%
  mutate(
    E     = Ed / (L - (L * U)),
    S     = (S_score - 1) / 5,
    Xtr   = E * S ) 

### Identify the gaps in data
tr_model <- tr_model %>%
  mutate(ed_gap = ifelse(is.na(Ed), 'E', '_'),
         u_gap  = ifelse(is.na(U),  'U', '_'),
         s_gap  = ifelse(is.na(S),  'S', '_'),
         l_gap  = ifelse(is.na(L),  'L', '_'),
         gaps   = paste(ed_gap, u_gap, s_gap, l_gap, sep = '')) %>%
  select(-ed_gap, -u_gap, -s_gap, -l_gap)

### Explore gaps by georegion
georegions       <- read.csv('../ohi-global/eez2013/layers/rgn_georegions.csv', na.strings='')
georegion_labels <- read.csv('../ohi-global/eez2013/layers/rgn_georegion_labels.csv')

tr_model <- tr_model %>%
  left_join(georegion_labels %>%
              spread(level, label) %>%
              select(-r0),
            by = 'rgn_id')

tr_rgns2013 <- tr_model %>%
  filter(year == year_max)
x1 <- tr_model %>%
  filter(year == year_max) %>%
  filter(gaps != '____')

x_l <- nrow(x1 %>% filter(str_detect(gaps, 'L')))
x_u <- nrow(x1 %>% filter(str_detect(gaps, 'U')))
x_s <- nrow(x1 %>% filter(str_detect(gaps, 'S')))
x_e <- nrow(x1 %>% filter(str_detect(gaps, 'E')))
x_count <- 4 - x1$gaps %>% str_count('_')
sum(x_count == 1); sum(x_count == 2); sum(x_count == 3); sum(x_count == 4)

head(x1)
cat(sprintf('# gaps, total jobs: %s; tourism jobs: %s; unemployment: %s; sustainability score: %s\n', x_l, x_e, x_u, x_s))

### Sustainability gaps:
s_gap_rgns <- tr_rgns2013 %>% 
  filter(str_detect(gaps, 'S')) %>% 
  select(r1, r2) %>% 
  unique()

s_gap <- tr_rgns2013 %>% 
  filter(r1 %in% s_gap_rgns$r1) %>%
  group_by(r1) %>%
  mutate(r1_S_NA_ratio = sum(is.na(S))/n(),
         r1_points = sum(!is.na(S))) %>%
  group_by(r2) %>%
  mutate(r2_S_NA_ratio = sum(is.na(S))/n(),
         r2_points = sum(!is.na(S))) %>%
  select(rgn_id, rgn_label,  gaps, r1,	r2,	r1_S_NA_ratio, r1_points, r2_S_NA_ratio, r2_points) %>%
  filter(str_detect(gaps, 'S')) %>%
  arrange(r2, r1, rgn_id)

sust2 <- tr_rgns2013 %>%
  filter(r2 %in% s_gap_rgns$r2 )
par(mar = c(8, 3, 4, 1))
boxplot(sust2$S ~ sust2$r2, las = 2)
  title('Sustainability score by georegion r2')
sust1 <- tr_rgns2013 %>%
  filter(r1 %in% s_gap_rgns$r1 )
boxplot(sust1$S ~ sust1$r1, las = 2)
  title('Sustainability score by georegion r1')


### Explore L as a proportion of total pop
### could also examine pop by age groups
