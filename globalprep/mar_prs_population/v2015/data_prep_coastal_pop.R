### data_prep_coastal_pop.R
##############################################################################=
### Jul2015 - Casey O'Hara
###
### Interpolate intermediate years for coastal population (25 miles inland).
### Determine rescaled population density and population trend
###
### From OHI 2013 supplement:
###   Description: ...
### ... Years in between those provided were temporally interpolated. For instance, d2013 =
###   0.4 * d2010 + 0.6 * d2015. For each year, rasters were projected to 1 km Mollweide, converted to
###   units of total population per cell at the new resolution and summed per region within the 25 mi
###   inland area For the following 19 small island regions lacking sufficient resolution from the input
###   rasters, coastal populations were set to the total population (see layer 5.71. Total population):
###   Amsterdam Island and Saint Paul Island, Bassas da India, Bouvet Island, British Indian Ocean
###   Territory, Clipperton Island, Crozet Islands, Glorioso Islands, Heard and McDonald Islands, Ile
###   Europa, Ile Tromelin, Jan Mayen, Johnston Atoll, Juan de Nova Island, Kerguelen Islands,
###   Macquarie Island, Palmyra Atoll, Prince Edward Islands, South Georgia and the South Sandwich
###   Islands, Wake Island.
### For 2015: 
### * Not reprojected to 1 km Mollweide.  Left as 2.5 arc-minute raster, extracted against
###   25 mi inland region polygons (GCS).
### * Linear interpolation by same method.
### 
### Input rgn pop files:     
###   rgn pops 2005, 2010, 2015: 
###     github:ohiprep/globalprep/coastal_population/v2015/int
### Output files: 
###   rgn pop, density, area 2005-2015:  
###     github:ohiprep/globalprep/coastal_population/v2015/data/rgn_rescaled_popsum_area_density_2005to2015_inland25mi.csv
###   pop trends:   
###     github:ohiprep/globalprep/coastal_population/v2015/data/rgn_popn5yrtrend_insland25mi_2011to2015.csv (and other scenarios)


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
### interpolate pop data and calculate density -----
##############################################################################=

### NOTE: Raster data must first be processed using the coastal_popn_extract.R script.
# source(file.path(dir_git, 'coastal_popn_extract.R'))

build_pop_data <- function() {
### first fill pops between 2005-2010, then 2010-2015, straight linear interpolation.
### then read in .csv of areas (extracted from 25 mi inland shape file), and calc density.
  pop_2005 <- read.csv(file.path(dir_int, 'pop_2005_25_sum_25mi.csv'), stringsAsFactors = FALSE) %>%
    rename(pop_2005 = pop_total)
  pop_2010 <- read.csv(file.path(dir_int, 'pop_2010_25_sum_25mi.csv'), stringsAsFactors = FALSE) %>%
    rename(pop_2010 = pop_total)
  pop_2015 <- read.csv(file.path(dir_int, 'pop_2015_25_sum_25mi.csv'), stringsAsFactors = FALSE) %>%
    rename(pop_2015 = pop_total)
  
  pop_all <- pop_2005 %>% 
    left_join(pop_2010, by = c('rgn_id', 'rgn_name')) %>%
    left_join(pop_2015, by = c('rgn_id', 'rgn_name'))
  
  for (y in 2006:2009) {
    y_name <- paste('pop', y, sep = '_')
    i <- y - 2005
    pop_all[[y_name]] <- ((5 - i) / 5) * pop_all$pop_2005 + (i / 5) * pop_all$pop_2010
  }
  for (y in 2011:2014) {
    y_name <- paste('pop', y, sep = '_')
    i <- y - 2010
    pop_all[[y_name]] <- ((5 - i) / 5) * pop_all$pop_2010 + (i / 5) * pop_all$pop_2015
  }
  pop_all <- pop_all %>%
    gather(year, pop, -rgn_id, -rgn_name) %>%
    mutate(year = as.integer(str_replace(year, 'pop_', ''))) %>%
    arrange(rgn_id, year)
}

adjust_pop_by_world_bank <- function(pop_all) {
  ### examine populations based on World Bank population data -----
  ### Attach World Bank population data for comparison purposes.
  ### * for small regions, population by raster method should include total population
  ##############################################################################=
  
  pop_tot_wb <- read.csv(file.path(dir_git, '../TourismRecreation/v2015/intermediate/wb_country_total_pop.csv'), stringsAsFactors = FALSE) %>%
    name_to_rgn(fld_name = 'country', fld_value = 'w_popn', flds_unique = c('country', 'year'), add_rgn_name = TRUE)
  pop_all_wb <- pop_all %>%
    left_join(pop_tot_wb %>% select(-rgn_name), by = c('rgn_id', 'year')) 
  
  ### fill no-data years by linear model
  pmdl <- left_join(
    pop_all_wb %>%
      group_by(rgn_id) %>%
      filter(!is.na(w_popn)) %>%
      do(mdl_p0 = lm(w_popn ~ year, data = .)$coefficients[[1]]),
    pop_all_wb %>%
      group_by(rgn_id) %>%
      filter(!is.na(w_popn)) %>%
      do(mdl_yr = lm(w_popn ~ year, data = .)$coefficients[[2]]),
    by = 'rgn_id')
  
  ### these variables cause problems with NULL values when left as class == list.
  pmdl$mdl_p0 <- unlist(pmdl$mdl_p0)
  pmdl$mdl_yr <- unlist(pmdl$mdl_yr)
  
  pop_all_wb <- pop_all_wb %>%
    left_join(pmdl, by = 'rgn_id') %>%
    mutate(w_popn_mdl = mdl_p0 + mdl_yr * year,
           w_pop_adj  = ifelse(!is.na(w_popn), w_popn, w_popn_mdl),
           pop_adj    = ifelse(!is.na(w_pop_adj) & pop > w_pop_adj, w_pop_adj, pop)) %>%
    select(rgn_id, rgn_name, year, pop_gpw = pop, w_pop_adj, pop_adj)
  
  ### fill the designated small island nations with total pop from World Bank
  fill_rgn_id <- c(4, 12, 30, 33, 34, 35, 36, 38, 89, 90, 91, 92, 93, 94, 105, 107, 144, 150, 159)
  pop_all_wb <- pop_all_wb %>%
    mutate(pop_adj  = ifelse(rgn_id %in% fill_rgn_id & !is.na(w_pop_adj), w_pop_adj, pop_adj),
           adjusted = !(pop_adj == pop_gpw)) %>%
    rename(pop = pop_adj)
  
  return(pop_all_wb)
}

calc_pop_density <- function(pop_all) {
  rgn_area <- read.csv(file.path(dir_git, 'rgn_area_inland25mi.csv'), stringsAsFactors = FALSE)
  
  pop_all <- pop_all %>%
    left_join(rgn_area %>% 
                select(rgn_id, area_km2),
              by = 'rgn_id') %>%
    mutate(pop_per_km2 = pop/area_km2)
}


pop_all <- build_pop_data()

pop_all <- pop_all %>% adjust_pop_by_world_bank()

pop_all <- pop_all %>% calc_pop_density()

### Examine differences, where calculated pop is greater than WB reported pop
x <- pop_all %>% filter(pop_gpw > w_pop_adj) %>% mutate(ratio = pop_gpw/w_pop_adj)
hist(x$ratio)
# x %>% filter(year %in% c(2005, 2013) & ratio > 1.2) %>% select(rgn_name, pop, w_popn)
#     These regions have surprisingly large 'overages' in the calculation WRT WB data
#     (year == 2013)                                rgn_name       pop  w_popn
#     1                                      Solomon Islands  677427.6  561231
#     2                                           Micronesia  163772.6  103549
#     3                    Northern Mariana Islands and Guam  318130.4  218979
#     4                                       Comoro Islands 1014880.8  734917
#     5  Puerto Rico and Virgin Islands of the United States 4469241.6 3719827
#     6                                       American Samoa   98533.2   55165
#     7                                               Monaco   61025.8   37831
#     8                                         Sint Maarten   58981.0   39689
#     9                                Northern Saint-Martin   65729.8   31264
#     10                                               Aruba  149520.8  102911

write.csv(pop_all, file.path(dir_int, 'rgn_pop_dens_adjusted_2005-2015.csv'), row.names = FALSE)
      
##############################################################################=
### rescale population from zero to one for pressures purposes -----
##############################################################################=
### Rescaling population 
# for each year, find max density; take log+1 of it; mutate that into a new column
# for each region for each year, take log+1 of density, divide by (max * 110%); mutate that into rescaled pop

pop_rescaled <- read.csv(file.path(dir_int, 'rgn_pop_dens_adjusted_2005-2015.csv'), stringsAsFactors = FALSE) %>%
  select(rgn_id, rgn_name, year, pop, adjusted, area_km2, pop_per_km2) %>%
  mutate(dens_max     = max(pop_per_km2, na.rm = TRUE),
         log_dens_max = log(dens_max + 1)) %>%
  group_by(rgn_id, year) %>%
  mutate(log_dens      = log(pop_per_km2 + 1),
         dens_rescaled = log_dens/log_dens_max)

pdens_max <- pop_rescaled %>% 
  filter(pop_per_km2 == dens_max)
#     rgn_id  rgn_name year     pop adjusted area_km2 pop_per_km2 dens_max log_dens_max log_dens dens_rescaled
#        208 Singapore 2015 4949892    FALSE  600.601    8241.565 8241.565     9.017067 9.017067             1

pop_rescaled <- pop_rescaled %>%
  select(rgn_id, rgn_name, year, pop, area_km2, pop_per_km2, dens_rescaled)

write.csv(pop_rescaled, file.path(dir_data, 'rgn_rescaled_popsum_area_density_2005to2015_inland25mi.csv'), row.names = FALSE)

##############################################################################=
### calculate population trend for scenario years -----
##############################################################################=

pop_data <- read.csv(file.path(dir_data, 'rgn_rescaled_popsum_area_density_2005to2015_inland25mi.csv'), stringsAsFactors = FALSE) %>%
  rename(popsum = pop)
# pop_data <- read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-CoastalPopulation_v2013/data/rgn_popsum_area_density_2005to2015_inland25mi.csv'), stringsAsFactors = FALSE)

scenario_years <- c('eez2012' = 2012, 'eez2013' = 2013, 'eez2014' = 2014, 'eez2015' = 2015)

for (i in 1:length(scenario_years)) {  #i = 2
  year_max <- scenario_years[i]
  year_min <- year_max - 4 # (past five data points, not five intervals)
  scenario <- names(scenario_years)[i]
  
  ### filter to years of interest 
  pop_years <- pop_data %>%
    filter(year_min <= year & year <= year_max) %>%
    arrange (rgn_id)
  
  ### calc linear model and pull annual change in population; convert to % change
  pop_trend <- pop_years %>%
    group_by(rgn_id) %>% 
    do(annual_change = lm(popsum ~ year, data = .)$coefficients[['year']]) %>%
    left_join(pop_years %>% 
                filter(year == min(year)) %>%
                select(rgn_id, popsum), by = 'rgn_id') %>%
    mutate(annual_change_pct = annual_change/popsum) %>%
    select(-popsum)
  
  ### Summarize total trend by sum(annual_change_pct) -- since there are five
  ### data years, basically the same as multiplying by 5...
  pop_trend_sum <- pop_trend %>%
    mutate(trend = 5 * annual_change_pct) %>%
    select(rgn_id, trend)
  
  ### write .csv
  pop_file <- file.path(dir_data, sprintf('rgn_popn5yrtrend_inland25mi_%dto%d.csv', year_min, year_max))
  cat(sprintf('Writing trend data for %s scenario (%d to %d) to: \n  %s\n', scenario, year_min, year_max, pop_file))
  write.csv(pop_trend_sum, pop_file, row.names = FALSE)
}


##############################################################################=
### compare results to those using original data from 2013 -----
##############################################################################=
# 
# x <- read.csv(file.path(dir_data, 'rgn_popn5yrtrend_inland25mi_2009to2013.csv'))
# # y <- read.csv(file.path(dir_data, 'orig_data/rgn_popn5yrtrend_inland25mi_2009to2013.csv'))
# y <- read.csv('~/github/ohi-global/eez2013/layers/cw_coastalpopn_trend.csv')
# z <- left_join(x %>% rename(trend_new = trend), y %>% rename(trend_orig = trend), by = 'rgn_id')
# plot(trend_new ~ trend_orig, data = z)
# abline(0, 1, col = 'red')
# 
# xx <- read.csv(file.path(dir_data, 'rgn_rescaled_popsum_area_density_2005to2015_inland25mi.csv')) %>% 
#   filter(year == 2013)
# yy <- read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-CoastalPopulation_v2013/data/rgn_popsum_area_density_2005to2015_inland25mi.csv')) %>% 
#   filter(year == 2013)
# zz <- left_join(xx %>% rename(pop_new = pop), yy %>% rename(pop_orig = popsum), by = 'rgn_id')
# plot(pop_new ~ pop_orig, data = zz)
# abline(0, 1, col = 'red')
# 
# plot(log(pop_new+1) ~ log(pop_orig+1), data = zz)
# abline(0, 1, col = 'red')
# 
