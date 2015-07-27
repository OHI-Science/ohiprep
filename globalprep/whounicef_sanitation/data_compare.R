# data_explore.R for Tourism & Recreation

setwd('~/github/ohiprep')
source('src/R/common.R')


library(ggplot2)

goal     <- 'globalprep/whounicef_sanitation'
scenario  <- 'v2015'
dir_git  <- file.path('~/github/ohiprep', goal)
dir_data <- file.path(dir_git, scenario, 'data')
dir_data_old <- file.path(dir_git, 'v2012', 'data')

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
### some checks and plots:
trendnew2013 <- read.csv(file.path(dir_data, 'pathogens_popdensity25mi_trend_2013a.csv'))
trendold2013 <- read.csv(file.path(dir_data_old, 'pathogens_popdensity25km_trend_2013a.csv'))

presnew2013 <- read.csv(file.path(dir_data, 'po_pathogens_popdensity25mi_2013a.csv'))
presold2013 <- read.csv(file.path(dir_data_old, 'data/po_pathogens_popdensity25km_2013a.csv'))

rgn_names <- read.csv("~/github/ohi-global/eez2013/layers/rgn_labels.csv") %>%
  filter(type=="eez") %>%
  select(rgn_id, label)

scatterPlot(data_orig = trendold2013 %>% rename(score = trend),
            data_new  = trendnew2013 %>% rename(score = trend),
            title_text = 'Pathogens trend 2013')

scatterPlot(data_orig = presold2013 %>% rename(score = pressure_score),
            data_new  = presnew2013 %>% rename(score = pressure_score),
            title_text = 'Pathogens pressure 2013')

presdiff <- full_join(presnew2013 %>% rename(presnew = pressure_score), 
                      presold2013 %>% rename(presold = pressure_score),
                      by = 'rgn_id') %>%
  left_join(rgn_names, by = 'rgn_id') %>%
  mutate(diffpct = (presnew - presold)/presold) %>%
  filter(diffpct > 0.10)