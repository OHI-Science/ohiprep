#### functions to visualize global scores within a scenario and commit.

library(git2r)
library(devtools)
library(ggplot2)
#devtools::install_github('hadley/ggplot2')


score_check = function(scenario_year, commit="previous", 
                       file_name, save_csv=FALSE, save_png=FALSE, NA_compare=TRUE){

  cat("Wait for it....this takes a few seconds \n\n")
    
  path_components <- unlist(strsplit(getwd(), "/"))
  scenario_name <- path_components[length(path_components)]
  repo_name <- path_components[length(path_components) -1]
  repo_path <- paste(path_components[1:(length(path_components)-1)], collapse = '/')
  scenario_path <- paste(path_components[1:(length(path_components))], collapse = '/')
  
  # get commit SHA
  if(commit=="previous"){
    commit2 = substring(git2r::commits(git2r::repository(repo_path))[[1]]@sha, 1, 7)
  } else{
    if (commit == "final_2014"){
      commit2 = '4da6b4a'
    } else {commit2 = commit}
  }
  
  
  # Get repository name
    tmp <- git2r::remote_url(git2r::repository(repo_path))
  org <- stringr::str_split(tmp, "/")[[1]][4]
  

  # get data from previous commit
  data_old <- read.csv(file.path("https://raw.githubusercontent.com", org, repo_name, commit2, scenario_name, "scores.csv")) %>%
      dplyr::rename(old_score=score) 
  
  # create dummy year variable if there is no year variable in the data
  if(sum(names(data_old)=="year") < 1){
    data_new <- read.csv("scores.csv") %>%
      dplyr::left_join(data_old, by=c('goal', 'dimension', 'region_id')) %>%
      dplyr::mutate(year == substring(date(), 21, 24)) %>%  # uses current year as year
      dplyr::mutate(change = score-old_score)
      scenario_year <- substring(date(), 21, 24)
      } else{
  data_new <- read.csv("scores.csv") %>%
    dplyr::left_join(data_old, by=c('year', 'goal', 'dimension', 'region_id')) %>%
    dplyr::mutate(change = score-old_score)
  
      }
  
  ## get region names, if available (this needs to be called "rgns_list" and located in the "spatial" folder)
  if(list.files("spatial", pattern="rgns_list.csv") == "rgns_list.csv"){
    
    rgns <- read.csv("spatial/rgns_list.csv", stringsAsFactors = FALSE) %>%
      dplyr::select(region_id = rgn_id, rgn_name)
    
    data_new <- data_new %>%
      dplyr::left_join(rgns, by="region_id") %>%
      dplyr::mutate(rgn_name = ifelse(region_id == 0, "Region", rgn_name))
  } else{
    data_new$rgn_name = ""
  }
  
  suppressWarnings(
  p <- ggplot2::ggplot(filter(data_new, year==scenario_year), aes(x=goal, y=change, color=dimension)) +
    #geom_point(shape=19, size=1) +
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    labs(title=paste("Score compared to commit:", commit, sep=" "), y="Change in score", x="") +
    scale_x_discrete(limits = c("Index", "AO", "SPP", "BD", "HAB", "CP", "CS", "CW", "FIS", "FP", 
                                "MAR", "ECO", "LE", "LIV", "NP", "LSP", "SP", "ICO", "TR")) +
    scale_colour_brewer(palette="Dark2") +
    geom_jitter(aes(text=paste0("rgn = ", region_id, "\n", rgn_name)), position = position_jitter(width=0.2, height=0), shape=19, size=1)
  )
  
  plotly_fig <- plotly::ggplotly(p, width = 800, height = 450)
  htmlwidgets::saveWidget(plotly::as_widget(plotly_fig), "tmp_file.html", selfcontained=TRUE)

  # Function to save files in particular place  
  my.file.rename <- function(from, to) {
    todir <- dirname(to)
    if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
    file.rename(from = from,  to = to)
  }

  my.file.rename(from = "tmp_file.html",
                 to = file.path('../score_check', paste0(file_name, "_score_check_", Sys.Date(), '.html')))

  cat("An interactive plot in the 'score_check' folder has been created \n")
  
  if(save_png){
    ggplot2::ggsave(file.path('../score_check', paste0(file_name, "_check_plot_", Sys.Date(), '.png')), width=8, height=5)
    cat("A png plot has been saved in the 'score_check' folder \n")
  }
  
  if(save_csv){
    write.csv(data_new, file.path('../score_check', paste0(file_name, "_diff_data_", Sys.Date(), '.csv')), row.names=FALSE)
    cat("A csv file comparing the scores has been saved in the 'score_check' folder \n")
  }
  
  if(NA_compare){
    data_NA <- data_new %>%
      filter(year == scenario_year) %>%
      mutate(NA_same = ifelse(is.na(score) & is.na(old_score), 1, 0)) %>%
      mutate(NA_new = ifelse(is.na(score), 1, 0)) %>%
      mutate(NA_old = ifelse(is.na(old_score), 1, 0)) %>%
      mutate(diff_new = NA_new - NA_same) %>%
      mutate(diff_old = NA_old - NA_same) %>%
      summarize(new = sum(diff_new),
                old = sum(diff_old))
    
    cat("\n NA check results: \n")
    
    if(sum(data_NA) == 0){
      cat(sprintf("Excellent! The number of NA values in %s has not changed! \n", scenario_year))
    } else{
      cat(sprintf("The new version of data has an additional %s missing values compared to the previous version \n
                  The previous version of data has an additional %s missing values compared to the new version \n
                  Examine the .csv file in the 'score_check' folder to determine where these discrepancies occur", 
                  data_NA$new, data_NA$old))
      }
    }
    
  }
  


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
    
  plotly_fig <- ggplotly(p)
  htmlwidgets::saveWidget(plotly::as_widget(plotly_fig), "tmp_file.html", 
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