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

library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

source('src/R/common.R')

goal     <- 'globalprep/mar_prs_population'
scenario <- 'v2016'


##############################################################################=
### Add data for region 18
##############################################################################=
pop <- read.csv(file.path(goal, scenario, 'int/rgn_pop_dens_adjusted_2005-2015.csv'), stringsAsFactors = FALSE)

# library(sp)
# library(rgdal)
# library(raster)

## for some reason Fiji is the not in the spatial data
# area <- readOGR(dsn = file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/data/"), layer="rgn_inland25mi_gcs")
# sp_inland25mi_gcs
# rgn_inland25mi_gcs

# As a rough estimate, I am going to count the entire of area of Fiji as within 25 miles of the ocean.
# This appears to match the last numbers we generated, and seems reasonable.
# However, this estimate should be improved in the future

# data from here: http://countryeconomy.com/demography/population/fiji
pop_18 <- data.frame(rgn_id = 18,
                     rgn_name = "Fiji",
                     year = 2010:2015,
                     pop_gpw = NA,
                     w_pop_adj = NA,
                     pop = c(861000, 868000, 875000, 881000, 885000, 892145),
                     adjusted = FALSE, 
                     area_km2 = 18270)
pop_18 <- pop_18 %>%
  mutate(pop_per_km2 = pop/area_km2)

pop <- rbind(pop, pop_18)

write.csv(pop, file.path(goal, scenario, "int/rgn_pop_dens_adjusted_2005-2015_rgn18added.csv"), row.names=FALSE)

##############################################################################=
### Pressure: hd_intertidal (coastal population density, proxy for habitat destruction)
### rescale population from zero to one for pressures purposes -----
##############################################################################=
### Rescaling population 
# for each year, find max density; take log+1 of it; mutate that into a new column
# for each region for each year, take log+1 of density, divide by (max * 110%); mutate that into rescaled pop

pop_rescaled <- read.csv(file.path(goal, scenario, 'int/rgn_pop_dens_adjusted_2005-2015_rgn18added.csv'), stringsAsFactors = FALSE) %>%
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
  
  write.csv(tmp, file.path(goal, scenario, sprintf("output/prs_pop_density_%s.csv", scenario_year)), row.names=FALSE)
}


##############################################################################=
### Mariculture population data
##############################################################################=

pop_mar <- read.csv(file.path(goal, scenario, 'int/rgn_pop_dens_adjusted_2005-2015_rgn18added.csv'), stringsAsFactors = FALSE) %>%
  select(rgn_id, year, popsum=pop) 


write.csv(pop_mar, file.path(goal, scenario, "output/mar_pop_25mi.csv"), row.names=FALSE)

