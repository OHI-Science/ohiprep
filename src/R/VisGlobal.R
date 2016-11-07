#### functions to visualize global scores within a scenario and commit.

library(git2r)
library(ggplot2)



scatterPlot <- function(repo="~/ohi-global", scenario="eez2013", commit="previous", goal, dim="score", fileSave){
  #   scenario <- "eez2013"  ## options: 'eez2012', 'eez2013', 'eez2014', 'eez2015'
  #   commit <- "final_2014"   ## 'final_2014', 'previous', a commit code (ie., 'e30e7a4')
  #   fileSave <- 'LSP_trend_data'
  #   goal <- 'LSP'
  ## Useful code: repository(repo)
  ## Useful code: commits(repo)
  
  repo2 <- sprintf("../%s", repo)
  
  if (commit == "previous") {
    commit2 = substring(git2r::commits(git2r::repository(repo2))[[1]]@sha, 
                        1, 7)
  } else {
    if (commit == "final_2014") {
      commit2 = "4da6b4a"
    } else {
      commit2 = commit
    }
  }
  
  tmp <- git2r::remote_url(git2r::repository(repo2))
  org <- stringr::str_split(tmp, "/")[[1]][4]
  path = paste0(scenario, "/scores.csv")
  data_old <- read_git_csv(paste(org, repo, sep = "/"), commit2, 
                           path) %>% dplyr::select(goal, dimension, region_id, old_score = score)
  
  
  names <- read.csv("eez2013/layers/rgn_labels.csv") %>%
    filter(type=="eez") %>%
    select(region_id=rgn_id, label)
  
  
  criteria <- ~dimension == dim
  
  data_new <- read.csv(file.path(path)) %>%
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

  p <- ggplot(data_new, aes(x=old_score, y=score)) +
    geom_point(aes(text = paste0("rgn = ", label)), shape=19) +
    theme_bw() + 
    #labs(title=paste(scenario, goal, dim, commit, sep=": "), y="New scores", x="Scores from previous analysis") +
    geom_abline(slope=1, intercept=0, color="red") 
    #+
    #geom_text(aes(label=plotLabel), vjust=1.5, size=3)
    #geom_text(aes(label=label), vjust=1.5, size=3)
    
  plotly_fig <- plotly::ggplotly(p)
  htmlwidgets::saveWidget(plotly::as.widget(plotly_fig), "tmp_file.html", 
                          selfcontained = TRUE)
  my.file.rename <- function(from, to) {
    todir <- dirname(to)
    if (!isTRUE(file.info(todir)$isdir)) 
      dir.create(todir, recursive = TRUE)
    file.rename(from = from, to = to)
  }
  my.file.rename(from = "tmp_file.html", to = file.path("changePlot_figures", 
                                                        paste0(fileSave, "_changePlot_", Sys.Date(), ".html")))
    ggsave(file.path('changePlot_figures', paste0(fileSave, "_scatterPlot_", Sys.Date(), '.png')), width=10, height=8)
}


goalHistogram <- function(scenario="eez2013", goal, dim="score", fileSave){
  #   scenario <- "eez2013"  ## options: 'eez2012', 'eez2013', 'eez2014', 'eez2015'
  #   fileSave <- 'NP_function'
  #   goal <- 'NP'  
  path = paste0(scenario, '/scores.csv')
  
  criteria  <- ~dimension == dim
  
  data_new <- read.csv(path) %>%
    filter_(criteria)
  
  data_new <- data_new[data_new$goal == goal, ]
           
  
  ggplot(data_new, aes(x=score)) +
    geom_histogram(color='black', fill="gray") +
    theme_bw() + 
    labs(title=paste(scenario, goal, "2015 analysis", sep=": "), y="Regions", x="Scores") 
  
  ggsave(file.path('changePlot_figures', paste0(fileSave, "_histPlot_", Sys.Date(), '.png')), width=8, height=5)
}