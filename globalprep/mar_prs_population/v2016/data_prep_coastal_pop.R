### data_prep_coastal_pop.R
############################################################################################
### Sept 25 2016 - MRF
###
### I am reformatting these data for use in the mariculture and habitat destruction
### intertidal pressure
###
### Previously, our focus was on calculating trend as a proxy for the trash trend of
### the clean water goal.
### We now directly estimate the trend for trash.  So, those data are no longer relevant
### and have been deleted.
###
### However, we should be using these data for the habitat destruction intertidal pressure,
### so this script formats the data for those specific layers
###
### NOTE: This is just a modest reformatting of the data that was generated in v2015
###       (see that location for the scripts that extracted these data from the raster scale and 
###        interpolated across years, etc.)
###
###########################################################################################

##############################################################################=
### setup -----
##############################################################################=

library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

source('src/R/common.R')

goal     <- 'globalprep/mar_prs_population'
scenario <- 'v2016'

##############################################################################=
### rescale population from zero to one for pressures purposes -----
##############################################################################=
### Rescaling population 
# for each year, find max density; take log+1 of it; mutate that into a new column
# for each region for each year, take log+1 of density, divide by (max * 110%); mutate that into rescaled pop

pop_rescaled <- read.csv(file.path(goal, scenario, 'int/rgn_pop_dens_adjusted_2005-2015.csv'), stringsAsFactors = FALSE) %>%
  select(rgn_id, rgn_name, year, pop, adjusted, area_km2, pop_per_km2) %>%
  mutate(log_dens = log(pop_per_km2 + 1)) %>%
#  mutate(scalar = quantile(log_dens, c(0.99))) %>%
  mutate(scalar = max(log_dens)) %>%
  mutate(dens_rescaled = log_dens/scalar) %>%
  mutate(dens_rescaled = ifelse(dens_rescaled>1, 1, dens_rescaled))

for(data_year in 2011:2015){
  
  scenario_year <- data_year + 1
  
  tmp <- pop_rescaled[pop_rescaled$year == data_year, ]
  
  tmp <- tmp %>%
    select(rgn_id, pressure_score = dens_rescaled)
  
  write.csv(tmp, file.path(goal, scenario, sprintf("output/prs_pop_density_%s.csv", scenario_year)))
}