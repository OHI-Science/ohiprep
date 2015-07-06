# data_prep.R for Tourism & Recreation - master data_prep.R file

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

reload = FALSE

tr_data_files <- c(unem      = file.path(dir_int, 'wb_rgn_uem.csv'),
                   jobs_tot  = file.path(dir_int, 'wb_rgn_tlf.csv'),
                   sust      = file.path(dir_int, 'wef_ttci_2015.csv'),
                   jobs_tour = file.path(dir_int, 'wttc_empd_rgn.csv'))

if(any(!file.exists(tr_data_files)) | reload == TRUE) {
  cat(sprintf('Raw data will be processed into: \n  %s\n', dir_int))
  cat(sprintf('    %s\n', basename(tr_data_files)))
  
  cat('Processing data from World Bank...\n')
  source(file.path(dir_git, 'process_WorldBank.R'))
  cat('Processing data from WTTC...\n')
  source(file.path(dir_git, 'process_WTTC.R'))
  cat('Processing data from World Economic Forum...\n')
  source(file.path(dir_git, 'process_WEF.R'))

}
cat(sprintf('Raw data has been processed; files exist in: \n  %s\n', dir_int))
cat(sprintf('    %s\n', list.files(dir_int)))


##############################################################################=
### Prep data into individual layers for toolbox ----
##############################################################################=
### Separate out just the model variables and save these to v201X/data directory,
### ready for use in the toolbox.

tr_layers <- c(unem     = file.path(dir_data, 'tr_unemployment.csv'),
               jobs_tot = file.path(dir_data, 'tr_jobs_total.csv'),
               sust     = file.path(dir_data, 'tr_sustainability.csv'),
               jobs_tour     = file.path(dir_data, 'tr_jobs_tourism.csv'),
               jobs_pct_tour = file.path(dir_data, 'tr_jobs_pct_tourism.csv'))

if(any(!file.exists(tr_layers)) | reload == TRUE) {
  #   * tr_unemployment.csv from World Bank data
  tr_unem <- read.csv(tr_data_files[['unem']], stringsAsFactors = FALSE) %>%
    select(rgn_id, year, percent)
  write_csv(tr_unem, tr_layers[['unem']])
  
  #   * tr_jobs_total.csv from World Bank total labor force
  #     * rgn_id, year, count (individuals)
  tr_jobs_tot <- read.csv(tr_data_files[['jobs_tot']], stringsAsFactors = FALSE) %>%
    select(rgn_id, year, count)
  write_csv(tr_jobs_tot, tr_layers[['jobs_tot']])
  
  #   * tr_sustainability.csv from WEF TTCI
  tr_sust <- read.csv(tr_data_files[['sust']], stringsAsFactors = FALSE) %>%
    select(rgn_id, score)
  write_csv(tr_sust, tr_layers[['sust']])
  
  #   * tr_jobs_tourism.csv from WTTC direct tourism employment and 
  #   * tr_jobs_pct_tourism.csv from WTTC direct tourism employment percentage
  tr_jobs_tour <- read.csv(tr_data_files[['jobs_tour']], stringsAsFactors = FALSE) 
  write_csv(tr_jobs_tour %>%
              select(rgn_id, year, jobs_ct), 
            tr_layers[['jobs_tour']])
  write_csv(tr_jobs_tour %>%
              select(rgn_id, year, jobs_pct),
            tr_layers[['jobs_pct_tour']])
}
cat(sprintf('Data layers have been processed; files exist here: \n  %s\n', dir_data))
cat(sprintf('    %s\n', list.files(dir_data)))


##############################################################################=
### Assembling the data from layers -----
##############################################################################=

tr_unem          <- read.csv(tr_layers[['unem']],          stringsAsFactors = FALSE)
tr_sust          <- read.csv(tr_layers[['sust']],          stringsAsFactors = FALSE)
tr_jobs_tour     <- read.csv(tr_layers[['jobs_tour']],     stringsAsFactors = FALSE)
tr_jobs_pct_tour <- read.csv(tr_layers[['jobs_pct_tour']], stringsAsFactors = FALSE)
tr_jobs_tot      <- read.csv(tr_layers[['jobs_tot']],      stringsAsFactors = FALSE)

rgn_names        <- read_csv('~/github/ohi-global/eez2013/layers/rgn_global.csv') %>%
  rename(rgn_name = label)

year_max     <- 2013

tr_data_raw <- tr_jobs_tour %>%
  rename(Ed = jobs_ct) %>%
  full_join(tr_jobs_pct_tour,
            by = c('rgn_id', 'year')) %>%
    rename(Ep = jobs_pct) %>%
    mutate(Ep = Ep/100,
           Ep = ifelse(Ep > 1, NA, Ep)) %>%
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


##############################################################################=
### Attach georegions and per-capita GDP info for various gapfilling -----
##############################################################################=

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


##############################################################################=
### Add gapfill flag variable -----
##############################################################################=
gapfill_flags <- function(data) {
  ### Identify the gaps in data.  '_' indicates no gap; a letter indicates a gap
  ### that will force an NA result.  If E term used the Ep data, then U and L are no barrier;
  ### mark them with a '*'. If S_score is present, then GDP gaps don't matter; mark with '*'.
  data <- data %>%
    mutate(ed_gap  = ifelse(is.na(Ed), 'E', '_'),
           u_gap   = ifelse(is.na(U),  ifelse(!is.na(Ep), '*', 'U'), '_'),
           s_gap   = ifelse(is.na(S_score), 'S', '_'),
           l_gap   = ifelse(is.na(L),  ifelse(!is.na(Ep), '*', 'L'), '_'),
           gdp_gap = ifelse(is.na(pcgdp), ifelse(is.na(S_score), 'G', '*'), '_'),
           gaps    = paste(ed_gap, u_gap, s_gap, l_gap, gdp_gap, sep = '')) %>%
    select(-ed_gap, -u_gap, -s_gap, -l_gap, -gdp_gap)
  return(data)
}

tr_data_raw <- tr_data_raw %>% gapfill_flags()


write_csv(tr_data_raw, file.path(dir_int, 'tr_data_raw.csv'))

##############################################################################=

  
##############################################################################=
### Gapfill S using r1 and/or r2 regional data and PPP-adjusted per-capita GDP ----
##############################################################################=
tr_data_raw <- read.csv(file.path(dir_int, 'tr_data_raw.csv'), stringsAsFactors = FALSE)


gdp_gapfill <- function(data) {
  ### Gapfill GDP figures using CIA data to sub for missing WB data for 
  ### current year, so that the TTCI regression can include values.
  no_gdp <- data %>% filter(is.na(pcgdp) & year == year_max) %>% select(rgn_name) %>% arrange(rgn_name)
  # missing gdp data for the following countries (2013): 
  # Anguilla | Argentina | Aruba | Barbados | Bermuda | British Virgin Islands
  # Cayman Islands | Cuba | East Timor | French Polynesia | Guadeloupe and Martinique | Kuwait
  # Myanmar | New Caledonia | North Korea | Northern Mariana Islands and Guam | Oman | R_union
  # Somalia | Syria | Taiwan | United Arab Emirates
  
  gdp_cia <- read.csv(file.path(dir_git, 'raw/cia_gdp_pc_ppp.csv'), stringsAsFactors = FALSE, header = FALSE)
  
  gdp_cia <- gdp_cia %>%
    select(rgn_name = V2, pcgdp_cia = V4) %>%
    mutate(pcgdp_cia = as.numeric(str_replace(pcgdp_cia, ',', '')))
  
  gdp_cia1 <- name_to_rgn(gdp_cia, 
                          fld_name='rgn_name', flds_unique = c('rgn_name'), 
                          fld_value='pcgdp_cia', add_rgn_name = TRUE, 
                          collapse_fxn = 'mean') %>%
    mutate(year = year_max)  # technically, 2014 estimates, but call it 2013
  
  data1 <- data %>%
    left_join(gdp_cia1 %>% select(-rgn_id),
              by = c('rgn_name', 'year')) 
  #   plot(pcgdp_cia ~ pcgdp, data = data1)
  #   abline(0,  1, col = 'red')
  data1 <- data1 %>%
    mutate(gaps  = ifelse(is.na(pcgdp) & !is.na(pcgdp_cia), str_replace(gaps, 'G', 'c'), gaps),
           pcgdp = ifelse(is.na(pcgdp) & !is.na(pcgdp_cia), pcgdp_cia, pcgdp)) %>%
    select(-pcgdp_cia)
  
  # hand-fill Guadeloupe/Martinique and Reunion Island.  
  gdp_reun <- 23501 # from http://www.insee.fr/fr/insee_regions/reunion/themes/dossiers/ter/ter2008_resultats_economiques.pdf
  # in 2007, not PPP
  gdp_mart <- 24118 # from: http://web.archive.org/web/20080216021351/http://prod-afd.afd.zeni.fr/jahia/webdav/site/cerom/users/admin_cerom/public/Pdf/CR2006_ma.pdf
  # in 2006, real exchange rate (PPP?)
  gdp_guad <- 21780 # from: http://www.insee.fr/fr/regions/guadeloupe/default.asp?page=publications/publications.htm
  # in 2006 dollars, not PPP.
  data1 <- data1 %>%
    mutate(pcgdp = ifelse(rgn_id == 32  & year == year_max, gdp_reun, pcgdp),
           pcgdp = ifelse(rgn_id == 140 & year == year_max, (gdp_guad+gdp_mart)/2, pcgdp),
           gaps  = ifelse(rgn_id %in% c(32, 140) & year == year_max, str_replace(gaps, 'G', 'h'), gaps))
  return(data1)
}

s_regr_r1 <- function(data, y_max = year_max) {
### create a regression model of S as a function of PPP-adjusted per-capita GDP, and
### with a dummy variable correlating to r1 level georegions
  s1 <- data %>% filter(year == y_max)
  s1_coef <- unlist(lm(S_score ~ pcgdp + r1, data = s1)['coefficients'])
  
  s1_mdl  <- stack(s1_coef)
  colnames(s1_mdl) <- c('r1_coef', 'r1')
  
  s1_mdl  <- s1_mdl %>%
    mutate(r1_int      = s1_coef[1],
           r1_gdp_coef = s1_coef[2],
           r1 = str_replace(r1, 'coefficients.r1', '')) # strip the prefix
  data <- data %>%
    left_join(s1_mdl, by = 'r1')
  
  # process r1 level model:
  dropped_rgn <- levels(as.factor(data$r1))[1] # auto figure out which region was first in the list
  data <- data %>%
    mutate(r1_coef = ifelse(r1 == dropped_rgn, 0, r1_coef),
           r1_mdl  = r1_int + r1_gdp_coef * pcgdp + r1_coef) %>%
  select(-r1_coef, -r1_gdp_coef, -r1_int)
  
  return(data)
}
  
s_regr_r2 <- function(data, y_max = year_max) {
### create a regression model of S as a function of PPP-adjusted per-capita GDP, and
### with a dummy variable correlating to r2 level georegions
  s2 <- data %>% filter(year == y_max)
  s2_coef <- unlist(lm(S_score ~ pcgdp + r2, data = s2)['coefficients'])
  
  s2_mdl  <- stack(s2_coef)
  colnames(s2_mdl) <- c('r2_coef', 'r2')
  
  s2_mdl  <- s2_mdl %>%
    mutate(r2_int      = s2_coef[1],
           r2_gdp_coef = s2_coef[2],
           r2 = str_replace(r2, 'coefficients.r2', '')) # strip the prefix
  data <- data %>%
    left_join(s2_mdl, by = 'r2')
    
  # process r2 level model:
  dropped_rgn <- levels(as.factor(data$r2))[1] # auto figure out which region was first in the list
  data <- data %>%
    mutate(r2_coef = ifelse(r2 == dropped_rgn, 0, r2_coef),
           r2_mdl  = r2_int + r2_gdp_coef * pcgdp + r2_coef) %>%
  select(-r2_coef, -r2_gdp_coef, -r2_int)
  
  return(data)
}

s_gapfill_r2_r1 <- function(data, y_max = year_max) {
  data <- data %>% 
    s_regr_r2(y_max) %>%
    s_regr_r1(y_max)
  
  data <- data %>%
    mutate(s_mdl = ifelse(!is.na(r2_mdl), r2_mdl, r1_mdl),
           gaps  = ifelse(is.na(S_score) & (!is.na(s_mdl)), str_replace(gaps, 'S', 'g'), gaps),
           S_score = ifelse(is.na(S_score), s_mdl, S_score)) %>%
    select(-r1_mdl, -r2_mdl, -s_mdl)
  
  return(data)
}

tr_data <- tr_data_raw %>% gdp_gapfill()

tr_data <- s_gapfill_r2_r1(tr_data)

# Apply only the 2013 S_score to all years - so it's consistent, as we only have
# actual scores from the current year.
tr_data1 <- tr_data %>%
  select(-S_score) %>%
  left_join(tr_data %>%
              filter(year == year_max) %>%
              select(rgn_id, S_score),
            by = 'rgn_id')

##############################################################################=
### Gapfill Ep using regional averages ----

tr_data <- tr_data %>%
  group_by(r2, year) %>%
  mutate(E_mdl2 = mean(Ep, na.rm = TRUE),
         gaps   = ifelse(is.na(Ep) & !is.na(E_mdl2), str_replace(gaps, 'E', 'r'), gaps),
         Ep     = ifelse(is.na(Ep), E_mdl2, Ep)) %>%
  select(-E_mdl2)

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

write_csv(tr_data, file.path(dir_int, 'tr_data_processed.csv'))


##############################################################################=
### Model modified to prefer the Ep term (percent of tourism jobs from WTTC) to determine E;
### if Ep term is not available, calculate old way: E = Ed / (L - (L * U))
### Model modified to normalize S_score by the maximum value, after subtracting one.
tr_data <- read.csv(file.path(dir_int, 'tr_data_processed.csv'), stringsAsFactors = FALSE)

tr_calc_model <- function(data_chunk) {
  data_chunk <- data_chunk %>%
    mutate(
      E       = ifelse(is.na(Ep), Ed / (L - (L * U)), Ep),
      S       = (S_score - 1) / (7 - 1), # scale score from 1 to 7.
      Xtr     = E * S )
  return(data_chunk)
}

tr_model <- tr_calc_model(tr_data)  %>%
  filter(year <= year_max & year >= year_max - 4) 
# five data years, four intervals? or six and five?



# Calculate status based on quantile reference
pct_ref <- 95 # the threshold for quantile where status score = 1.0
Xtr_max <- max(tr_model$Xtr, na.rm = TRUE)

tr_scores <- tr_model %>%
  select(rgn_id, S_score, year, rgn_name, Xtr, gaps, r1) %>%
    left_join(tr_model %>%
              group_by(year) %>%
              summarize(Xtr_q = quantile(Xtr, probs = pct_ref/100, na.rm = TRUE)),
            by = 'year') %>%
  mutate(
    Xtr_rq  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q), # rescale to qth percentile, cap at 1
    Xtr_rmax = Xtr / Xtr_max )                         # rescale to max value   

# Comparing to 2013, and examining distributions to find cutoff for score of 100
library(ggplot2)

s_2013 <- read.csv('~/github/ohi-global/eez2013/layers/tr_sustainability.csv')
s_compare <- tr_scores %>% filter(year == year_max) %>%
  select(rgn_id, Xtr, S_score, gaps) %>%
  full_join(s_2013, by = 'rgn_id') %>%
  mutate(gaps = ifelse(str_detect(gaps, 'g'), TRUE, FALSE))

ggplot(s_compare, aes(x = score, y = S_score, color = gaps)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1/100, color = 'red')

tr_2013 <- read.csv('~/github/ohi-global/eez2013/scores.csv') %>%
  filter(goal == 'TR' & dimension %in% c('status', 'trend', 'score')) %>%
  spread(dimension, score)



tr_check <- full_join(tr_scores, tr_2013, by = c('rgn_id' = 'region_id')) %>%
  filter(year == year_max) %>%
  mutate(gaps = ifelse(str_detect(gaps, 'g'), TRUE, FALSE))

ggplot(tr_check, aes(x = status, y = Xtr_rq, color = gaps)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1/100, color = 'red')

x_100 <- tr_check %>% filter(Xtr_rq == 1)

x_coef <- lm(Xtr_rq ~ status, data = tr_check)[['coefficients']]

ggplot(tr_check, aes(x = status, y = Xtr_rq, color = r1)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1/100, color = 'red') + 
  geom_abline(intercept = x_coef[1], slope = x_coef[2], color = 'blue') + 
  labs(x = 'TR status 2013', y = 'TR status new',
       title = sprintf('TR comparison; ref pt at %d%% of raw score', pct_ref))
  


s_2013 <- read.csv('~/github/ohi-global/eez2013/layers/tr_sustainability.csv')
l_2013 <- read.csv('~/github/ohi-global/eez2013/layers/tr_jobs_total.csv')
u_2013 <- read.csv('~/github/ohi-global/eez2013/layers/tr_unemployment.csv')
e_2013 <- read.csv('~/github/ohi-global/eez2013/layers/tr_jobs_tourism.csv')

tr_model_2013 <- l_2013 %>% 
  rename(L = count) %>%
  left_join(s_2013 %>%
              rename(S_score = score),
            by = 'rgn_id') %>%
  left_join(u_2013 %>% 
              rename(U = percent), 
            by = c('rgn_id', 'year')) %>%
  left_join(e_2013 %>%
              rename(Ed = count),
            by = c('rgn_id', 'year')) %>%
  mutate(
    E       = Ed / (L - (L * U/100)),
    S       = (S_score - 1) / (7 - 1), # scale score from 1 to 7.
    Xtr     = E * S ) %>%
  filter(year == 2012)
  
tr_temp <- tr_model %>% 
  filter(year == year_max) %>%
  select(rgn_id, new_Xtr = Xtr) %>%
  left_join(tr_model_2013 %>% 
              select(rgn_id, old_Xtr = Xtr), 
            by = c('rgn_id'))

ggplot(tr_temp, aes(x = old_Xtr, y = new_Xtr)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = 'red') + 
  labs(x = 'TR model old', y = 'TR model new',
       title = 'TR comparison')

# awesome distribution with quantiles plot
dens <- density(tr_check$Xtr)
df <- data.frame(x=dens$x, y=dens$y)
probs <- c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)
quantiles <- quantile(tr_check$Xtr, prob=probs)
df$quant <- factor(findInterval(df$x,quantiles))
ggplot(df, aes(x,y)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
  scale_x_continuous(breaks=quantiles) + 
  scale_fill_brewer(guide="none") +
  labs(x = 'Employment * Sustainability, raw',
       y = 'Frequency',
       title = 'Quantiles of TR status pre-normalized')