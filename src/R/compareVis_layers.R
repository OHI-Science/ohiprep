#### functions to visualize scores comparing two input scores.csv files. 


comparePlot <- function(csv_orig = 'scores_orig.csv',
                        csv_new = 'scores_1.csv', 
                        title_text = 'ao_need',
                        fig_save = 'scores_1_changePlot.png'){
  require(tools)
  require(dplyr)
  require(git2r)
  require(ggplot2)
  require(RColorBrewer)
  
  data_combo <- read.csv(csv_orig) %>%
    select(goal, dimension, rgn_id, scores_old = score) %>%
    left_join(read.csv(csv_new) %>%
                dplyr::rename(scores_new = score), 
              by=c('goal', 'dimension', 'rgn_id')) %>%
    mutate(change = scores_new - scores_old)  
  
  ggplot(data_combo, aes(x=goal, y=change, color=dimension)) +
    geom_point(shape=19, size=1) +
    theme_bw() + 
    labs(title=(paste0('Score differences after modifying ', title_text)), y="Change in score", x="") +
    scale_x_discrete(limits = c("Index", "AO", "SPP", "BD", "HAB", "CP", "CS", "CW", "FIS", "FP", 
                                "MAR", "ECO", "LE", "LIV", "NP", "LSP", "SP", "ICO", "TR")) +
    scale_colour_brewer(palette="Dark2") +
    geom_jitter(position = position_jitter(width=0.2, height=0), shape=19, size=1)
  
  ggsave(fig_save, width=8, height=5)
  
}


scatterPlot <- function(csv_orig = '~/github/ohiprep/globalprep/FAO_targetedharvest/data/rgn_fao_targeted_2012a.csv',
                        csv_new  = '~/github/ohiprep/globalprep/FAO_targetedharvest/data/rgn_fao_targeted_2015a.csv', 
                        title_text = 'targeted_harvest',
                        fig_save = paste0(title_text,'_scatterPlot.png')){
  
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
  
  ggplot(data_combo, aes(x=scores_old, y=scores_new)) +
    geom_point(shape=19) +
    theme_bw() + 
    labs(title=sprintf('Score differences for %s', title_text), y=basename(csv_orig), x=basename(csv_new)) +
    geom_abline(slope=1, intercept=0, color="red") +
    geom_text(aes(label=plotLabel), vjust=1.5, size=3)
  
  ggsave(fig_save, width=10, height=8)
}
