# data_explore.R for Tourism & Recreation

library(ggplot2)

goal      <- 'globalprep/TourismRecreation'
scenario  <- 'v2015'
dir_git   <- file.path('~/github/ohiprep/', goal)
dir_data  <- file.path('~/github/ohiprep', goal, scenario, 'data')
dir_eez2013 <- file.path('~/github/ohi-global/eez2013/layers')

#############################################################################=
scatterPlot <- function(data_orig, data_new, title_text,
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
         x = paste0('Orig: ', title_text), 
         y = paste0('New: ',  title_text) ) +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    geom_text(aes(label = plotLabel), vjust = 1.5, size = 3)
  
  ggsave(fig_save, width = 10, height = 8)
}

#############################################################################=
### TR Comparison Graphs -----
#############################################################################=

# tr_unemployment ----
year_compare <- 2012

tr_unem_orig <- read.csv(file.path(dir_eez2013, 'tr_unemployment.csv'), stringsAsFactors = FALSE) %>%
  filter(year == year_compare) %>%
  rename(score = percent)
tr_unem_new  <- read.csv(file.path(dir_data,    'tr_unemployment.csv'), stringsAsFactors = FALSE) %>%
  filter(year == year_compare) %>%
  rename(score = percent)

scatterPlot(data_orig = tr_unem_orig,
            data_new  = tr_unem_new,
            title_text = 'WB unemployment 2012')

# tr_sustainability ----
# comparison of 2015 scores to previous year's scores (2013-2014? I think)
tr_sust_orig <- read.csv(file.path(dir_eez2013, 'tr_sustainability.csv'), stringsAsFactors = FALSE) %>% 
  mutate(score = score/7) # ???
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

