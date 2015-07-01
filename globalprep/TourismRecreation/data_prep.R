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
  select(rgn_id, year, count = jobs_ct, jobs_pct)
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

##############################################################################=
### Assembling the data from layers
### Note: if we use the Ep term, may need to save it as a separate layer.

tr_data <- tr_jobs_tour %>%
  rename(Ed = count, Ep = jobs_pct) %>%
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
### Attach georegions to explore data variances by region...
georegions       <- read.csv('../ohi-global/eez2013/layers/rgn_georegions.csv', na.strings='')
georegion_labels <- read.csv('../ohi-global/eez2013/layers/rgn_georegion_labels.csv')

tr_model <- tr_data %>%
  left_join(georegion_labels %>%
              spread(level, label) %>%
              select(-r0),
            by = 'rgn_id') %>%
  filter(rgn_id != 255) # ditch disputed regions...

gdppcppp <- read.csv(file.path(dir_int, 'wb_rgn_gdppcppp.csv')) %>%
  select(rgn_id, year, pcgdp = intl_dollar)
tr_model <- tr_model %>%
  left_join(gdppcppp, by = c('rgn_id', 'year'))

no_gdp <- tr_model %>% filter(is.na(pcgdp) & year == year_max) %>% select(rgn_label) %>% arrange(rgn_label)
# missing gdp data for the following countries (2013): 
# Anguilla | Argentina | Aruba | Barbados | Bermuda | British Virgin Islands
# Cayman Islands | Cuba | East Timor | French Polynesia | Guadeloupe and Martinique | Kuwait
# Myanmar | New Caledonia | North Korea | Northern Mariana Islands and Guam | Oman | R_union
# Somalia | Syria | Taiwan | United Arab Emirates
gdp_imf <- read.csv(file.path(dir_git, 'raw/imf_gdp_pc_ppp.csv'), stringsAsFactors = FALSE)

gdp_imf <- gdp_imf %>%
  select(starts_with('X'), rgn_label = Country) %>%
  gather(year, pcgdp_imf, -rgn_label) %>%
  mutate(year = as.integer(as.character(str_replace(year, 'X', ''))),
         pcgdp_imf = as.numeric(str_replace(pcgdp_imf, ',', '')))

gdp_imf1 <- name_to_rgn(gdp_imf, 
                        fld_name='rgn_label', flds_unique = c('rgn_label', 'year'), 
                        fld_value='pcgdp_imf', add_rgn_name = TRUE, 
                        collapse_fxn = 'mean') %>%
  rename(rgn_label = rgn_name)

# gdp_compare <- tr_model %>% filter(year == year_max) %>% select(rgn_label, pcgdp) %>%
#   full_join(gdp_imf1 %>% filter(year == year_max), by = 'rgn_label') %>% arrange(rgn_label)
# plot(pcgdp ~ pcgdp_imf, data = gdp_compare)
# summary(lm(pcgdp ~ pcgdp_imf, data = gdp_compare))
tr_model <- tr_model %>%
  left_join(gdp_imf1 %>% 
              filter(rgn_label %in% no_gdp$rgn_label) %>%
              select(-rgn_id),
            by = c('rgn_label', 'year'))
  
##############################################################################=
### Plots of E vs Ed -----

# library(ggplot2)
test_new_formulas <- tr_model %>%
  mutate(
    E_calc = Ed / (L - (L * U)),
    S_orig = (S_score - 1) / 5) %>%
  select(rgn_id, rgn_label, year, E, Ep, E_calc, S, S_orig, r1, r2)

# plot all for E calculated vs direct
ggplot(test_new_formulas, 
       aes(x = E_calc, y = Ep, color = r1)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  labs(x = 'E = (Ed / (L - (L * U)) original model',
       y = 'E = Ep, direct percentage of tourism employment from WTTC data',
       title = 'Comparison of Tourism/Total total jobs')

# plot all for E calculated vs direct, zoomed in to lower end of scale
ggplot(test_new_formulas %>% filter(E_calc < .25), 
       aes(x = E_calc, y = Ep, color = r1)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  labs(x = 'E = (Ed / (L - (L * U)) original model',
       y = 'E = Ep, direct percentage of tourism employment from WTTC data',
       title = 'Comparison of Tourism/Total jobs: zoom in on E < .25')

# E calculated vs direct, regions
ggplot(test_new_formulas %>% filter(E_calc < .25) %>%
         filter(r1 %in% c('Africa', 'Asia', 'Latin America and the Caribbean')) %>%
         filter(year >= 2009), 
       aes(x = E_calc, y = Ep, color = r2)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  labs(x = 'E = (Ed / (L - (L * U)) original model',
       y = 'E = Ep, direct percentage of tourism employment from WTTC data',
       title = 'Comparison of Tourism/Total jobs: zoom in on E < .25')
ggplot(test_new_formulas %>% filter(E_calc < .25) %>%
         filter(r1 %in% c('Europe', 'Americas', 'Oceania')) %>%
         filter(year >= 2009), 
       aes(x = E_calc, y = Ep, color = r1)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  labs(x = 'E = (Ed / (L - (L * U)) original model',
       y = 'E = Ep, direct percentage of tourism employment from WTTC data',
       title = 'Comparison of Tourism/Total jobs: zoom in on E < .25')
# -----

##############################################################################=
### Examine S vs PPP PC GDP ----

s_corr <- tr_model %>%
  select(rgn_id, rgn_label, year, S, E, pcgdp, r1, r2) %>%
  filter(year == 2013)

# plot all for E calculated vs direct
ggplot(s_corr, 
       aes(x = pcgdp, y = S, color = r1)) +
  geom_point() + 
  labs(x = 'PPP-adjusted per capita GDP',
       y = 'S (normalized TTCI)',
       title = 'TTCI vs pc GDP')

summary(lm(s_corr$S ~ s_corr$pcgdp))
summary(lm(s_corr$S ~ s_corr$r1))
summary(lm(s_corr$S ~ s_corr$r2))
summary(lm(s_corr$S ~ s_corr$pcgdp + s_corr$r1))
summary(lm(s_corr$S ~ s_corr$pcgdp + s_corr$r2))
#-----
  
##############################################################################=
### Gapfill S using r1 and/or r2 regional data and PPP-adjusted per-capita GDP ----

S_regr_r1 <- function(data, y_max = year_max) {
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
           r1_mdl  = r1_int + r1_gdp_coef * pcgdp + r1_coef,
           r1_mdl1 = ifelse(is.na(pcgdp), (r1_int + r1_gdp_coef * r2_mean_gdp + r1_coef), r1_mdl)) %>%
  select(-r1_coef, -r1_gdp_coef, -r1_int)
  
  return(data)
}
  
S_regr_r2 <- function(data, y_max = year_max) {
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
           r2_mdl  = r2_int + r2_gdp_coef * pcgdp + r2_coef,
           r2_mdl1 = ifelse(is.na(pcgdp), (r2_int + r2_gdp_coef * r2_mean_gdp + r2_coef), r2_mdl)) %>%
  select(-r2_coef, -r2_gdp_coef, -r2_int)
  
  return(data)
}

S_gapfill_r2_r1 <- function(data, y_max = year_max) {
  # if per capita GDP not available for a region, fill with R2 level georegional average
  data <- data %>%
    group_by(r2, year) %>%
    mutate(r2_mean_gdp = mean(pcgdp, na.rm = TRUE))
  data <- data %>% 
    S_regr_r2(y_max) %>%
    S_regr_r1(y_max)
  
  data <- data %>%
    mutate(s_mdl = ifelse(!is.na(r2_mdl), r2_mdl, r1_mdl)) #%>%
    #select(-r1_mdl, -r2_mdl)
  
  return(data)
}

tr_model1 <- S_gapfill_r2_r1(tr_model)

# plot all for S vs model
ggplot(tr_model1 %>% filter(year == year_max), 
       aes(x = S, y = s_mdl, color = r1)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  labs(x = 'S from TTCI',
       y = 'Predicted from model: S ~ gdp + r2 then S ~ gdp + r1',
       title = 'Georegional regression model vs scaled TTCI scores')



##############################################################################=
### Model modified to prefer the Ep term (percent of tourism jobs from WTTC) to determine E;
### if Ep term is not available, calculate old way: E = Ed / (L - (L * U))
### Model modified to normalize S_score by the maximum value, after subtracting one.

TR_process_model <- function(data_chunk) {
  data_chunk <- data_chunk %>%
    mutate(
      E     = ifelse(is.na(Ep), Ed / (L - (L * U)), Ep),
      S     = (S_score - 1) / (7 - 1), # 7 because that's the max, - 1 because that's the min
      #  maybe divide by 6 (overall maximum range), or divide by (max(S_score - 1)) * 1.1 as a 10% buffer, etc.
      Xtr   = E * S )
  return(data_chunk)
}


##############################################################################=
### Identify the gaps in data.  '_' indicates no gap; a letter indicates a gap
### that will force an NA result.  If E term used the Ep data, then U and L are no barrier;
### mark them with a '*'. 
gapfill_flags <- function(tr_model) {
  tr_model <- tr_model %>%
    mutate(ed_gap = ifelse(is.na(Ed), 'E', '_'),
           u_gap  = ifelse(is.na(U),  ifelse(!is.na(Ep), '*', 'U'), '_'),
           s_gap  = ifelse(is.na(S),  ifelse(!is.na(s_mdl), '+', 'S'), '_'),
           l_gap  = ifelse(is.na(L),  ifelse(!is.na(Ep), '*', 'L'), '_'),
           gaps   = paste(ed_gap, u_gap, s_gap, l_gap, sep = '')) %>%
    select(-ed_gap, -u_gap, -s_gap, -l_gap)
  
}

##############################################################################=
### Explore gaps by georegion -----
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
