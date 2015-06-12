### Compare status and trend 

setwd('~/github/ohiprep')
source('src/R/common.R')
library(ggplot2)

dir_global <- ('~/github/ohi-global')
comp_scenario  <- 'eez2014'

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
  ylim(0.5, 1) + xlim(0.5, 1) +
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


scatterPlot <- function(csv_orig, csv_new, title_text,
                        fig_save = file.path(dir_git, scenario, paste0(title_text, '_scatterPlot.png'))) {
  
  require(git2r)
  require(ggplot2)
  require(RColorBrewer)
  
  names <- read.csv("~/github/ohi-global/eez2013/layers/rgn_labels.csv") %>%
    filter(type=="eez") %>%
    select(rgn_id, label)
  
  
  data_combo <- read.csv(csv_orig) %>%
    dplyr::rename(scores_old = score) %>%
    left_join(read.csv(csv_new) %>%
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
         x = paste0('Orig: ', basename(csv_orig)), 
         y = paste0('New: ', basename(csv_new)) ) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    geom_text(aes(label = plotLabel), vjust = 1.5, size = 3)
  
  ggsave(fig_save, width = 10, height = 8)
}

scatterPlot(csv_orig = '~/github/ohi-global/eez2013/layers/spp_status.csv',
            csv_new  = '~/github/ohiprep/globalprep/SpeciesDiversity/v2015/data/spp_status.csv',
            title_text = 'spp_status')

scatterPlot(csv_orig = '~/github/ohi-global/eez2013/layers/spp_trend.csv',
            csv_new  = '~/github/ohiprep/globalprep/SpeciesDiversity/v2015/data/spp_trend.csv',
            title_text = 'spp_trend')