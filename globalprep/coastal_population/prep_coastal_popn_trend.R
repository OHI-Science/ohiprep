### prep_coastal_popn_trend.R
### Jul2015 - Casey O'Hara

##############################################################################=
### setup -----
##############################################################################=

library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

setwd('~/github/ohiprep')
source('src/R/common.R')

goal     <- 'globalprep/coastal_population'
scenario <- 'v2015'
dir_git  <- file.path('~/github/ohiprep', goal)
dir_int  <- file.path(dir_git, scenario, 'int')
dir_data <- file.path(dir_git, scenario, 'data')
dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal)


##############################################################################=
### calculate population trend for scenario years -----
##############################################################################=

pop_data <- read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-CoastalPopulation_v2013/data/rgn_popsum_area_density_2005to2015_inland25mi.csv'), stringsAsFactors = FALSE)

scenario_years <- c('eez2012' = 2012, 'eez2013' = 2013, 'eez2014' = 2014, 'eez2015' = 2015)

### The rgn_popsum_area_density_2005to2015_inland25mi.csv file already has intermediate years
### filled in with a linear interpolation.

for (i in 1:length(scenario_years)) {  #i = 2
  year_max <- scenario_years[i]
  scenario <- names(scenario_years)[i]
  
  ### filter to years of interest (past five data years, not five intervals)
  pop_years <- pop_data %>%
    filter(year_max - 5 < year & year <= year_max) %>%
    arrange (rgn_id)
  
  ### calc linear model and pull annual change in population; convert to % change
  pop_trend <- pop_years %>%
    group_by(rgn_id) %>% 
    do(annual_change = lm(popsum ~ year, data = .)$coefficients[['year']]) %>%
    left_join(pop_years %>% 
                filter(year == min(year)) %>%
                select(rgn_id, popsum), by = 'rgn_id') %>%
    mutate(annual_change_pct = annual_change/popsum) %>%
    select(-pop_sum)
  
  pop_years <- pop_years %>%
    left_join(pop_trend, by = 'rgn_id') 
  
  ### Summarize total trend by sum(annual_change_pct) -- since there are five
  ### data years, basically the same as multiplying by 5...
  pop_trend_sum <- pop_years %>%
    group_by(rgn_id) %>%
    summarize(trend = sum(annual_change_pct))
  
  ### write .csv
  write.csv(pop_trend_sum, file.path(dir_data, sprintf('cw_coastalpopn_trend_%s.csv', scenario)))
}