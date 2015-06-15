### Compare status and trend 

setwd('~/github/ohiprep')
source('src/R/common.R')
library(ggplot2)

dir_global <- ('~/github/ohi-global')
comp_scenario  <- 'eez2013'

goal     <- 'globalprep/SpeciesDiversity'
scenario <- 'v2015'
dir_git  <- file.path('~/github/ohiprep', goal) 
 
#############################################################################=
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

#############################################################################=
### SPP Comparison Graphs -----
#############################################################################=

scatterPlot(csv_orig = file.path(dir_global, comp_scenario, 'layers/spp_status.csv'),
            csv_new  = file.path(dir_git, scenario, 'data/spp_status.csv'),
            title_text = 'spp_status')

scatterPlot(csv_orig = file.path(dir_global, comp_scenario, 'layers/spp_trend.csv'),
            csv_new  = file.path(dir_git, scenario, 'data/spp_trend.csv'),
            title_text = 'spp_trend')


#############################################################################=
### ICO Comparison Graphs -----
#############################################################################=

# ico_status and trend not in correct format in ohi-global/eez2013.
cat_conv    <- data.frame(category    = c("LC", "NT", "VU", "EN", "CR", "EX"), 
                          cat_score   = c(   0,  0.2,  0.4,  0.6,  0.8,   1))
trend_conv  <- data.frame(popn_trend  = c("Decreasing", "Stable", "Increasing"), 
                          trend_score = c(   -0.5,         0,          0.5   ))

ico_status_raw <- read.csv(file.path(dir_global, comp_scenario, 'layers/ico_spp_extinction_status.csv'), stringsAsFactors = FALSE) 
ico_status_compare <- ico_status_raw %>%
  left_join(cat_conv, by = 'category') %>%
  group_by(rgn_id) %>%
  summarize(mean_cat = mean(cat_score, na.rm = TRUE)) %>%
  mutate(score = ((1 - mean_cat) - 0.25) / 0.75)
ico_trend_raw <- read.csv(file.path(dir_global, comp_scenario, 'layers/ico_spp_popn_trend.csv'), stringsAsFactors = FALSE)
ico_trend_compare <- ico_trend_raw %>%
  left_join(trend_conv, by = 'popn_trend') %>%
  group_by(rgn_id) %>%
  summarize(score = mean(trend_score, na.rm = TRUE))

write_csv(ico_status_compare, file.path(dir_git, sprintf('tmp/ico_status_%s.csv', comp_scenario)))
write_csv(ico_trend_compare,  file.path(dir_git, sprintf('tmp/ico_trend_%s.csv',  comp_scenario)))



scatterPlot(csv_orig = file.path(dir_git, sprintf('tmp/ico_status_%s.csv', comp_scenario)),
            csv_new  = file.path(dir_git, scenario, 'data/ico_status.csv'),
            title_text = 'ico_status')

scatterPlot(csv_orig = file.path(dir_git, sprintf('tmp/ico_trend_%s.csv',  comp_scenario)),
            csv_new  = file.path(dir_git, scenario, 'data/ico_trend.csv'),
            title_text = 'ico_trend')

