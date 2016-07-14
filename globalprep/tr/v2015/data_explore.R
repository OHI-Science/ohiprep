# data_explore.R for Tourism & Recreation

setwd('~/github/ohiprep')
source('src/R/common.R')


library(ggplot2)

goal      <- 'globalprep/TourismRecreation'
scenario  <- 'v2015'
dir_git  <- file.path('~/github/ohiprep', goal)
dir_data <- file.path(dir_git, scenario, 'data')
dir_int  <- file.path(dir_git, scenario, 'intermediate')
dir_eez2013 <- file.path('~/github/ohi-global/eez2013/layers')

#############################################################################=
scatterPlot <- function(data_orig, data_new, title_text, x_text = title_text, y_text = title_text,
                        fig_save = file.path(dir_git, scenario, paste0(title_text, '_scatterPlot.png'))) {
  
  require(git2r)
  require(ggplot2)
  require(RColorBrewer)
  
  names <- read.csv("~/github/ohi-global/eez2013/layers/rgn_labels.csv") %>%
    filter(type=="eez") %>%
    select(rgn_id, label)
  
  
  data_combo <- data_orig %>%
    dplyr::rename(scores_old = score) %>%
    left_join(data_new %>%
                dplyr::rename(scores_new = score), 
              by=c('rgn_id')) %>%
    mutate(change = scores_new - scores_old) %>%
    mutate(mean = mean(change, na.rm=TRUE),
           sd =  sd(change, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(z_score = (change-mean)/sd) %>%
    mutate(z_greater_1 = ifelse(abs(z_score) > 1, "yes", "no")) %>%
    left_join(names, by='rgn_id') %>%
    filter(rgn_id != 0) %>%
    mutate(plotLabel = ifelse(z_greater_1=="yes", as.character(label), NA)) 
  
  ggplot(data_combo, aes(x = scores_old, y = scores_new)) +
    geom_point(shape = 19) +
    theme_bw() + 
    labs(title=sprintf('Score differences for %s', title_text), 
         x = paste0('Orig: ', x_text), 
         y = paste0('New: ',  y_text) ) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    geom_text(aes(label = plotLabel), vjust = 1.5, size = 3)
  
#   ggsave(fig_save, width = 10, height = 8)
}

#############################################################################=
### TR Comparison Graphs -----
#############################################################################=

# tr_unemployment ----
year_compare <- 2005

tr_unem_orig <- read.csv(file.path(dir_eez2013, 'tr_unemployment.csv'), stringsAsFactors = FALSE) %>%
  filter(year == year_compare) %>%
  rename(score = percent)
tr_unem_new  <- read.csv(file.path(dir_data,    'tr_unemployment.csv'), stringsAsFactors = FALSE) %>%
  filter(year == year_compare) %>%
  rename(score = percent)

scatterPlot(data_orig = tr_unem_orig,
            data_new  = tr_unem_new,
            title_text = 'WB unemployment 2005')

# tr_sustainability ----
# comparison of 2015 scores to previous year's scores (2013-2014? I think)
tr_sust_orig <- read.csv(file.path(dir_eez2013, 'tr_sustainability.csv'), stringsAsFactors = FALSE) 
tr_sust_new  <- read.csv(file.path(dir_data,    'tr_sustainability.csv'), stringsAsFactors = FALSE)

#  this taken from .xls from 2013 report:
  tr_sust_2013 <- read.csv(file.path(dir_git, 'TTCI_2013.csv'))
  names(tr_sust_2013) <- tolower(names(tr_sust_2013))
  tr_sust_2013 <- name_to_rgn(tr_sust_2013, fld_name = 'economy',
                              fld_value = 'score', collapse_fxn = 'mean',
                              add_rgn_name = T)
  tr_sust_2013 <- tr_sust_2013 %>%
    mutate(score = score/7)

scatterPlot(data_orig = tr_sust_orig,
            data_new  = tr_sust_new,
            title_text = 'TTCI sust OHI2013 vs 2015')
scatterPlot(data_orig = tr_sust_2013,
            data_new  = tr_sust_new,
            title_text = 'TTCI sust WEF2013 vs 2015')

# tr_jobs_tourism ----
year_compare <- 2013

tr_jobs_tour_orig <- read.csv(file.path(dir_eez2013, 'tr_jobs_tourism.csv'), stringsAsFactors = FALSE) %>%
  filter(year == year_compare) %>%
  rename(score = count) %>%
  mutate(score = log(score))
tr_jobs_tour_new  <- read.csv(file.path(dir_data,    'tr_jobs_tourism.csv'), stringsAsFactors = FALSE) %>%
  filter(year == year_compare) %>%
  rename(score = count) %>%
  mutate(score = log(score))

scatterPlot(data_orig = tr_jobs_tour_orig,
            data_new  = tr_jobs_tour_new,
            title_text = 'WTTC log jobs-direct tourism 2013')

# tr_jobs_total ----
year_compare <- 2012

tr_jobs_tot_orig <- read.csv(file.path(dir_eez2013, 'tr_jobs_total.csv'), stringsAsFactors = FALSE) %>%
  filter(year == year_compare) %>%
  rename(score = count) %>%
  mutate(score = log(score))
tr_jobs_tot_new  <- read.csv(file.path(dir_data,    'tr_jobs_total.csv'), stringsAsFactors = FALSE) %>%
  filter(year == year_compare) %>%
  rename(score = count) %>%
  mutate(score = log(score))

scatterPlot(data_orig = tr_jobs_tot_orig,
            data_new  = tr_jobs_tot_new,
            title_text = 'WB log jobs-total labor force 2012')

##############################################################################=
### Plots of E vs Ed -----

# library(ggplot2)
test_new_formulas <- tr_model %>%
  mutate(
    E_calc = Ed / (L - (L * U)),
    S_orig = (S_score - 1) / 5) %>%
  select(rgn_id, rgn_name, year, E, Ep, E_calc, S, S_orig, r1, r2)

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
  select(rgn_id, rgn_name, year, S, E, pcgdp, r1, r2) %>%
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

tr_model <- read.csv(file.path(dir_int, 'tr_model.csv'), stringsAsFactors = F)
tr_scores <- read.csv(file.path(dir_int, 'tr_scores.csv'), stringsAsFactors = F)

##############################################################################=
### test new functions.R and new data against old...

st_old <- read.csv(file.path(dir_git, 'old_model/scores_2015.csv')) %>%
  filter(goal == 'TR' & dimension %in% c('status'))
st_new <- read.csv(file.path(dir_git, 'new_model/scores_2015.csv')) %>%
  filter(goal == 'TR' & dimension %in% c('status'))
scatterPlot(data_orig = st_old %>% 
              select(rgn_id = region_id, score),
            data_new  = st_new %>%
              select(rgn_id = region_id, score),
            title_text = 'TR status script check 2015',
            x_text = 'TR status, old functions.R', y_text = 'TR status, functions.R update')

tr_old <- read.csv(file.path(dir_git, 'old_model/scores_2015.csv')) %>%
  filter(goal == 'TR' & dimension %in% c('trend'))
tr_new <- read.csv(file.path(dir_git, 'new_model/scores_2015.csv')) %>%
  filter(goal == 'TR' & dimension %in% c('trend'))
scatterPlot(data_orig = tr_old %>% 
              select(rgn_id = region_id, score),
            data_new  = tr_new %>%
              select(rgn_id = region_id, score),
            title_text = 'TR trend script check 2015',
            x_text = 'TR trend, old functions.R', y_text = 'TR trend, functions.R update')

tr_model1 <- tr_model %>%
  select(rgn_id, rgn_name, year, Xtr) %>%
  left_join(tr_model %>%
              group_by(year) %>%
              summarize(Xtr_q = quantile(Xtr, probs = pct_ref/100, na.rm = TRUE)),
            by = 'year') %>%
  mutate(
    Xtr_rq  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q)) # rescale to qth percentile, cap at 1


# calculate trend
tr_trend <- tr_model1 %>%
  filter(!is.na(Xtr_rq)) %>%
  arrange(year, rgn_id) %>%
  group_by(rgn_id) %>%
  do(mod = lm(Xtr_rq ~ year, data = .)) %>%
  do(data.frame(
    rgn_id    = .$rgn_id,
    dimension = 'trend',
    coef      = coef(.$mod)[['year']],
    score     =  max(min(coef(.$mod)[['year']] * 5, 1), -1)))
tr_model <- tr_model %>% 
  left_join(tr_model1 %>% select(rgn_id, year, Xtr_q, Xtr_rq), by = c('rgn_id', 'year')) %>%
  left_join(tr_trend  %>% select(rgn_id, coef, trend = score), by = 'rgn_id')

tr_model_2013 <- tr_model_2013 %>%
  mutate(Xtr = round(Xtr, 3),
         E   = round(E, 3),
         Xtr_q = round(Xtr_q, 3),
         Xtr_rq = round(Xtr_rq, 3),
         coef = round(coef, 3),
         trend = round(trend, 3))

tr_model_2013 <- tr_model_2013 %>%
  left_join(tr_model_2015 %>% select(rgn_id, rgn_name) %>% unique(),
            by = 'rgn_id')

scatterPlot(data_orig = tr_model_2013 %>% 
              select(rgn_id, score = trend),
            data_new  = tr_model2015 %>%
              select(rgn_id, score = trend),
            title_text = 'TR trend script check - new script',
            x_text = 'TR trend, old layers', y_text = 'TR trend, new layers')

# check info on countries with odd trends:
prob_trends <- c('Cambodia', 'Cayman Islands', 'Belize', 'Bahrain', 'Bermuda', 'Costa Rica', 'Egypt', 'Netherlands', 'Taiwan')
trendcheck <- filter(tr_scores, rgn_name %in% prob_trends)

##############################################################################=
### recreate un-normalized model using 2013 data layers -----

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
    Xtr     = E * S) %>%
#  filter(year == 2012) %>%
  left_join(rgn_names, by = 'rgn_id')

scatterPlot(data_orig = tr_model_2013 %>% 
              select(rgn_id, score = Xtr) %>%
              filter(score < 1),
            data_new  = tr_model %>%
              filter(year == 2013) %>%
              select(rgn_id, score = Xtr),
            title_text = 'TR raw model scores, 2013 vs new',
            x_text = 'TR model, 2013 data', y_text = 'TR model, 2015 data')

# -----

# Quantile cutoff - where should it be?
# Comparing to 2013, and examining distributions to find cutoff for score of 100

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