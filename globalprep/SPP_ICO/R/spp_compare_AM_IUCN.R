### Compare status and trend 

setwd('~/github/ohiprep')
library(ggplot2)
source('src/R/common.R')

goal     <- 'globalprep/SPP_ICO'
scenario <- 'vAM_IUCN'
dir_data  <- file.path('~/github/ohiprep', goal, scenario, 'data') 


#############################################################################=
scatterPlot <- function(csv_orig, csv_new, title_text, zero_one_new = FALSE, zero_one_old = FALSE,
                        fig_save = file.path(dir_data, '../graphs', paste0(title_text, '_scatterPlot.png'))) {
  
  require(git2r)
  require(ggplot2)
  require(RColorBrewer)
  
  names <- read.csv("~/github/ohi-global/eez2013/layers/rgn_labels.csv") %>%
    filter(type=="eez") %>%
    select(rgn_id, label)
  
  data_orig <- read.csv(csv_orig)
  names(data_orig) <- c('rgn_id', 'score')
  data_new <- read.csv(csv_new)
  names(data_new) <- c('rgn_id', 'score')
  if(zero_one_old) data_orig$score = data_orig$score*100
  if(zero_one_new) data_new$score = data_new$score*100
  
  data_combo <- data_orig %>%
    dplyr::rename(scores_old = score) %>%
    left_join(data_new %>%
                dplyr::rename(scores_new = score), 
              by=c('rgn_id')) %>%
    dplyr::mutate(change = scores_new - scores_old) %>%
    dplyr::mutate(mean = mean(change, na.rm=TRUE),
           sd =  sd(change, na.rm=TRUE)) %>%
    ungroup() %>%
    dplyr::mutate(z_score = (change-mean)/sd) %>%
    dplyr::mutate(z_greater_1 = ifelse(abs(z_score) > 1, "yes", "no")) %>%
    left_join(names, by='rgn_id') %>%
    filter(rgn_id != 0) %>%
    dplyr::mutate(
#      label     = ifelse(is.na(label), as.character(rgn_id), label),
      plotLabel = ifelse(z_greater_1=="yes", as.character(label), NA)
    ) 
    
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

pref_flag <- c('IUCN', 'AM')
prob_flag <- c('0.4', '0.05')

i <- c(1, 2, 2, 2)

status_base <- file.path(dir_data, sprintf('spp_status_global_%spref_prob%s.csv', pref_flag[i[1]], prob_flag[i[3]]))
status_test <- file.path(dir_data, sprintf('spp_status_global_%spref_prob%s.csv', pref_flag[i[2]], prob_flag[i[4]]))

trend_base <- file.path(dir_data, sprintf('spp_trend_global_%spref_prob%s.csv', pref_flag[i[1]], prob_flag[i[3]]))
trend_test <- file.path(dir_data, sprintf('spp_trend_global_%spref_prob%s.csv', pref_flag[i[2]], prob_flag[i[4]]))

scatterPlot(status_base, status_test, sprintf('GL_SPP_st_pref%svs%s_prob%svs%s', pref_flag[i[1]], pref_flag[i[2]], prob_flag[i[3]], prob_flag[i[4]]))
scatterPlot(trend_base,  trend_test,  sprintf('GL_SPP_tr_pref%svs%s_prob%svs%s',  pref_flag[i[1]], pref_flag[i[2]], prob_flag[i[3]], prob_flag[i[4]]))

#############################################################################=
### ICO Comparison Graphs -----
#############################################################################=

# # ico_status and trend not in correct format in ohi-global/eez2013.
# cat_conv    <- data.frame(category    = c("LC", "NT", "VU", "EN", "CR", "EX"), 
#                           cat_score   = c(   0,  0.2,  0.4,  0.6,  0.8,   1))
# trend_conv  <- data.frame(popn_trend  = c("Decreasing", "Stable", "Increasing"), 
#                           trend_score = c(   -0.5,         0,          0.5   ))
# 
# ico_status_raw <- read.csv(file.path(dir_global, comp_scenario, 'layers/ico_spp_extinction_status.csv'), stringsAsFactors = FALSE) 
# ico_status_compare <- ico_status_raw %>%
#   left_join(cat_conv, by = 'category') %>%
#   group_by(rgn_id) %>%
#   summarize(mean_cat = mean(cat_score, na.rm = TRUE)) %>%
#   mutate(score = ((1 - mean_cat) - 0.25) / 0.75)
# ico_trend_raw <- read.csv(file.path(dir_global, comp_scenario, 'layers/ico_spp_popn_trend.csv'), stringsAsFactors = FALSE)
# ico_trend_compare <- ico_trend_raw %>%
#   left_join(trend_conv, by = 'popn_trend') %>%
#   group_by(rgn_id) %>%
#   summarize(score = mean(trend_score, na.rm = TRUE))
# 
# write_csv(ico_status_compare, file.path(dir_git, sprintf('tmp/ico_status_%s.csv', comp_scenario)))
# write_csv(ico_trend_compare,  file.path(dir_git, sprintf('tmp/ico_trend_%s.csv',  comp_scenario)))



scatterPlot(csv_orig = file.path(dir_git, sprintf('tmp/ico_status_%s.csv', comp_scenario)),
            csv_new  = file.path(dir_git, scenario, 'data/ico_status.csv'),
            title_text = 'ico_status')

scatterPlot(csv_orig = file.path(dir_git, sprintf('tmp/ico_trend_%s.csv',  comp_scenario)),
            csv_new  = file.path(dir_git, scenario, 'data/ico_trend.csv'),
            title_text = 'ico_trend')


status_x   <- left_join(ico_status_compare, 
                       read_csv(file.path(dir_git, scenario, 'data/ico_status.csv')), 
                       by = 'rgn_id')
status_mdl <- lm(status_x$score.x ~ status_x$score.y)
summary(status_mdl)

# parents omitted; sum of parents/subpops
# (Intercept)       0.51120    0.05873   8.704 1.38e-15 ***
#   status_x$score.y -0.10883    0.11154  -0.976     0.33    
# Residual standard error: 0.1367 on 194 degrees of freedom
# Multiple R-squared:  0.004883,  Adjusted R-squared:  -0.0002463 
# F-statistic: 0.952 on 1 and 194 DF,  p-value: 0.3304

# parents omitted; mean of parents/subpops
# (Intercept)       0.51194    0.05914   8.656 1.87e-15 ***
#   status_x$score.y -0.11009    0.11218  -0.981    0.328    
# Residual standard error: 0.1367 on 194 degrees of freedom
# Multiple R-squared:  0.004939,  Adjusted R-squared:  -0.0001898 
# F-statistic: 0.963 on 1 and 194 DF,  p-value: 0.3277

# parents kept; mean of parents and all subpops
# (Intercept)       0.49162    0.06393   7.690 7.22e-13 ***
#   status_x$score.y -0.07033    0.12034  -0.584     0.56    
# Residual standard error: 0.1369 on 194 degrees of freedom
# Multiple R-squared:  0.001757,  Adjusted R-squared:  -0.003388 
# F-statistic: 0.3415 on 1 and 194 DF,  p-value: 0.5596

# parents kept; sum of parents and all subpops
# (Intercept)       0.47643    0.06475   7.357 5.16e-12 ***
#   status_x$score.y -0.04200    0.12368  -0.340    0.735    
# Residual standard error: 0.137 on 194 degrees of freedom
# Multiple R-squared:  0.000594,  Adjusted R-squared:  -0.004558 
# F-statistic: 0.1153 on 1 and 194 DF,  p-value: 0.7346




trend_x   <- left_join(ico_trend_compare, 
                     read_csv(file.path(dir_git, scenario, 'data/ico_trend.csv')), 
                     by = 'rgn_id')
trend_mdl <- lm(trend_x$score.x ~ trend_x$score.y)
summary(trend_mdl)
# parents removed; sum of subpops
# (Intercept)     -0.11557    0.01815  -6.366 1.54e-09 ***
#   trend_x$score.y  0.69799    0.06705  10.410  < 2e-16 ***
# Residual standard error: 0.1475 on 182 degrees of freedom
# Multiple R-squared:  0.3732,  Adjusted R-squared:  0.3697 
# F-statistic: 108.4 on 1 and 182 DF,  p-value: < 2.2e-16

# parents removed; mean of subpops
# (Intercept)     -0.10951    0.01828  -5.989  1.1e-08 ***
#   trend_x$score.y  0.71088    0.06671  10.656  < 2e-16 ***
# Residual standard error: 0.1462 on 182 degrees of freedom
# Multiple R-squared:  0.3842,  Adjusted R-squared:  0.3808 
# F-statistic: 113.5 on 1 and 182 DF,  p-value: < 2.2e-16

# parents kept; mean of subpops/parents
# (Intercept)     -0.09361    0.01925  -4.863 2.49e-06 ***
#   trend_x$score.y  0.71873    0.06633  10.835  < 2e-16 ***
# Residual standard error: 0.1453 on 182 degrees of freedom
# Multiple R-squared:  0.3921,  Adjusted R-squared:  0.3888 
# F-statistic: 117.4 on 1 and 182 DF,  p-value: < 2.2e-16

# parents kept; sum of parents and subpops
# (Intercept)     -0.11003    0.01927   -5.71 4.53e-08 ***
#   trend_x$score.y  0.66614    0.06702    9.94  < 2e-16 ***
# Residual standard error: 0.15 on 182 degrees of freedom
# Multiple R-squared:  0.3519,  Adjusted R-squared:  0.3483 
# F-statistic:  98.8 on 1 and 182 DF,  p-value: < 2.2e-16
