### Compare status and trend 

setwd('~/github/ohiprep')
source('src/R/common.R')
library(ggplot2)

dir_global <- ('~/github/ohi-global')
comp_scenario  <- 'eez2013'

goal     <- 'globalprep/SpeciesDiversity'
scenario <- 'v2015'
dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal) 
               
spp_scores_current <- read.csv(file.path(dir_anx, scenario, 'summary/rgn_summary.csv')) %>%
  select(rgn_id, cur_status = status, cur_trend = rgn_mean_trend) %>%
  mutate(cur_status = cur_status/100)
spp_status_compare <- read.csv(file.path(dir_global, comp_scenario, 'layers/spp_status.csv')) %>%
  rename(comp_status = score)
spp_trend_compare <- read.csv(file.path(dir_global, comp_scenario, 'layers/spp_trend.csv')) %>%
  rename(comp_trend = score)
spp_scores_compare <- full_join(spp_status_compare, spp_trend_compare, by = 'rgn_id')

spp_scores <- full_join(spp_scores_current, spp_scores_compare, by = 'rgn_id')

ggplot(spp_scores, aes(x = cur_status, y = comp_status)) +
  geom_point() + 
  ylim(0, 1) + xlim(0, 1) +
  geom_abline(intercept = 0, slope = 1, color = 'red') + 
  labs(x = sprintf('%s status', scenario), 
       y = sprintf('%s status', comp_scenario), 
       title = 'Status comparison for new SSP algorithms vs prior year')

ggplot(spp_scores, aes(x = cur_trend, y = comp_trend)) +
  geom_point() + 
  ylim(-.5, .5) + xlim(-.5, .5) +
  geom_abline(intercept = 0, slope = 1, color = 'red') + 
  labs(x = sprintf('%s trend', scenario), 
       y = sprintf('%s trend', comp_scenario), 
       title = 'Trend comparison for new SSP algorithms vs prior year')
