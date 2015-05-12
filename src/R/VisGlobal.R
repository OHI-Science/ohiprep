#### functions to visualize global scores within a scenario and commit.

changePlot <- function(repo="~/ohi-global", scenario="eez2013", commit="previous", fileSave){
  #   scenario <- "eez2013"  ## options: 'eez2012', 'eez2013', 'eez2014', 'eez2015'
  #   commit <- "final_2014"   ## 'final_2014', 'previous', a commit code (ie., 'e30e7a4')
  #   fileSave <- 'LSP_trend_update'
  ## Useful code: repository(repo)
  ## Useful code: commits(repo)
  
  # devtools::install_github('ropensci/git2r') # to get latest version
  require(git2r)
  require(ggplot2)
  require(RColorBrewer)
  
  if(commit=="previous"){
    commit2 = substring(commits(repo)[[1]]@sha, 1, 7)
  } else{
     if (commit == "final_2014"){
    commit2 = '4da6b4a'
  } else {commit2 = commit}
  }
  path = paste0(scenario, '/scores.csv')
  
  data_old <- read_git_csv(repo, commit2, path) %>%
    select(goal, dimension, region_id, old_score=score)
  data_new <- read.csv(file.path(repo, path)) %>%
    left_join(data_old, by=c('goal', 'dimension', 'region_id')) %>%
    mutate(change = score-old_score) 
  
  ggplot(data_new, aes(x=goal, y=change, color=dimension)) +
    geom_point(shape=19, size=1) +
    theme_bw() + 
    labs(title=paste(scenario, commit, sep=" "), y="Change in score", x="") +
    scale_x_discrete(limits = c("Index", "AO", "SPP", "BD", "HAB", "CP", "CS", "CW", "FIS", "FP", 
                              "MAR", "ECO", "LE", "LIV", "NP", "LSP", "SP", "ICO", "TR")) +
    scale_colour_brewer(palette="Dark2") +
    geom_jitter(position = position_jitter(width=0.2, height=0), shape=19, size=1)
  
  ggsave(file.path(repo, 'figures/DataCheck', paste0(fileSave, "_changePlot_", Sys.Date(), '.png')), width=8, height=5)
  write.csv(data_new, file.path(repo, 'figures/DataCheck', paste0(fileSave, "_diff_data_", Sys.Date(), '.csv')), row.names=FALSE)
}


scatterPlot <- function(repo="~/ohi-global", scenario="eez2013", commit="previous", goal, dim="score", fileSave){
  #   scenario <- "eez2013"  ## options: 'eez2012', 'eez2013', 'eez2014', 'eez2015'
  #   commit <- "final_2014"   ## 'final_2014', 'previous', a commit code (ie., 'e30e7a4')
  #   fileSave <- 'LSP_trend_data'
  #   goal <- 'LSP'
  ## Useful code: repository(repo)
  ## Useful code: commits(repo)
  
  # devtools::install_github('ropensci/git2r') # to get latest version
  require(git2r)
  require(ggplot2)
  require(RColorBrewer)
  
  if(commit=="previous"){
    commit2 = substring(commits(repo)[[1]]@sha, 1, 7)
  } else{
    if (commit == "final_2014"){
      commit2 = '4da6b4a'
    } else {commit2 = commit}
  }
  path = paste0(scenario, '/scores.csv')
  
  names <- read.csv("eez2013/layers/rgn_labels.csv") %>%
    filter(type=="eez") %>%
    select(region_id=rgn_id, label)
  
  data_old <- read_git_csv(repo, commit2, path) %>%
    select(goal, dimension, region_id, old_score=score)
  
  criteria <- ~dimension == dim
  
  data_new <- read.csv(file.path(repo, path)) %>%
    left_join(data_old, by=c('goal', 'dimension', 'region_id')) %>%
    mutate(change = score-old_score) %>%
    filter_(criteria) %>%
    group_by(goal) %>% 
    mutate(mean = mean(change, na.rm=TRUE),
           sd =  sd(change, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(z_score = (change-mean)/sd) %>%
    mutate(z_greater_1 = ifelse(abs(z_score) > 1, "yes", "no")) %>%
    left_join(names) %>%
    filter(region_id != 0) %>%
    mutate(plotLabel = ifelse(z_greater_1=="yes", as.character(label), NA))
  
  data_new <- data_new[data_new$goal==goal,]  

  ggplot(data_new, aes(x=old_score, y=score)) +
    geom_point(shape=19) +
    theme_bw() + 
    labs(title=paste(scenario, goal, dim, commit, sep=": "), y="New scores", x="Scores from 2014 analysis") +
    geom_abline(slope=1, intercept=0, color="red") +
    geom_text(aes(label=plotLabel), vjust=1.5, size=3)
    
    ggsave(file.path(repo, 'figures/DataCheck', paste0(fileSave, "_scatterPlot_", Sys.Date(), '.png')), width=10, height=8)
}


goalHistogram <- function(repo="~/ohi-global", scenario="eez2013", goal, dim="score", fileSave){
  #   scenario <- "eez2013"  ## options: 'eez2012', 'eez2013', 'eez2014', 'eez2015'
  #   fileSave <- 'LSP_trend_data'
  #   goal <- 'LSP'
  ## Useful code: repository(repo)
  ## Useful code: commits(repo)
  
  # devtools::install_github('ropensci/git2r') # to get latest version
  require(git2r)
  require(ggplot2)
  require(RColorBrewer)
  path = paste0(scenario, '/scores.csv')
  
  criteria  <- ~dimension == dim
  
  data_new <- read.csv(file.path(repo, path)) %>%
    filter_(dim)
  
  data_new <- data_new[data_new$goal == goal, ]
           
  
  ggplot(data_new, aes(x=score)) +
    geom_histogram(color='black', fill="gray") +
    theme_bw() + 
    labs(title=paste(scenario, goal, "2015 analysis", sep=": "), y="Regions", x="Scores") 
  
  ggsave(file.path(repo, 'figures/DataCheck', paste0(fileSave, "_histPlot_", Sys.Date(), '.png', width=8, height=5)))
}