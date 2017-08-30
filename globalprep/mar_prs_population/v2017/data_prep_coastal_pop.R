### data_prep_coastal_pop.R

# NEW: updated dataset from source.  UN standardized and higher quality resolution data.

library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
library(raster)
library(sp)
library(stringr)

source('src/R/common.R')

goal     <- 'globalprep/mar_prs_population'
scenario <- 'v2017'

### Convert density data to population (units: people per km2)

den_raster <- list.files(file.path(dir_M, 'git-annex/impact_acceleration/stressors/direct_human/int'), 
                         pattern = "mol.tif", full = TRUE)

den_raster <- den_raster[!grepl("log", den_raster)]
den_raster <- den_raster[!grepl("tif.", den_raster)]
den_raster # check to make sure this looks correct

for(rast in den_raster){ # rast = den_raster[16]
  nm <- basename(rast)
  yr <- str_sub(nm,-12,-9)
  cat(nm)
  tmp <- raster(rast)
  calc(tmp, fun = function(x){x * (934.4789 * 934.4789 * 0.000001)},
       filename = file.path(dir_M, 
                  sprintf("git-annex/globalprep/mar_prs_population/v2017/int/human_count_%s_mol.tif", yr)),
       overwrite=TRUE)
}

## Check raster against latest UN population data:
pop_rast <- raster(file.path(dir_M, 
          "git-annex/globalprep/mar_prs_population/v2017/int/human_count_2015_mol.tif"))

zones <- raster(file.path(dir_M, 
            "git-annex/globalprep/spatial/v2017/regions_land_ocean.tif"))

pop_counts <- zonal(pop_rast, zones, fun = "sum", na.rm=TRUE)

pop_UN <- read.csv("globalprep/mar_prs_population/v2017/output/UN_population.csv") %>%
  dplyr::filter(year==2015) %>%
  dplyr::select(rgn_id, pop_UN=population)

compare_pop <- data.frame(pop_counts) %>%
  dplyr::select(rgn_id = zone, pop_rast = sum) %>%
  dplyr::left_join(pop_UN, by="rgn_id")

plot(log(compare_pop$pop_rast), log(compare_pop$pop_UN))
abline(0,1, col="red")

## looks reasonably good!
#################################################

## create a raster that includes the entire eez and 25 miles inland
library(sf)
library(fasterize)

# eez raster
eez <- raster(file.path(dir_M, "git-annex/globalprep/spatial/v2017/regions_eez_with_fao_ant.tif"))

# create the 25 mile inland raster
inland <- sf::read_sf(dsn = file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/data"),
                                 layer = "sp_inland25mi_gcs")

inland <- st_transform(inland, sp::proj4string(eez))

inland_raster <- fasterize::fasterize(inland, eez, field = 'rgn_id')

tmp <- raster::freq(inland_raster) 

tmp2 <- data.frame(tmp) %>%
  dplyr::arrange(value)

raster::merge(eez, inland_raster, filename=file.path(dir_M, 
                      "git-annex/globalprep/mar_prs_population/v2017/int/eez_25mi_inland.tif"))


####################################
### extract population data from all population layers

zones <- raster(file.path(dir_M, "git-annex/globalprep/mar_prs_population/v2017/int/eez_25mi_inland.tif"))

pop_rasts <- list.files(file.path(dir_M, "git-annex/globalprep/mar_prs_population/v2017/int"), 
           pattern = "mol.", full=TRUE)

pop_stack <- raster::stack(pop_rasts)

coastal_pop <- raster::zonal(pop_stack, zones, fun="sum", progress="text")

write.csv(coastal_pop, "globalprep/mar_prs_population/v2017/int", row.names=FALSE)

coastal_pop2 <- data.frame(coastal_pop) %>%
  tidyr::gather("year", "coastal_pop_25mi", -1) %>%
  dplyr::select(rgn_id=zone, year=year, popsum=coastal_pop_25mi) %>%
  dplyr::mutate(year = as.numeric(as.character(substring(year, 13, 16)))) %>%
  dplyr::filter(rgn_id <= 250)

write.csv(coastal_pop2, "globalprep/mar_prs_population/v2017/output/mar_pop_25mi.csv", row.names = FALSE)


## quick compare with previous data

old <- read.csv("globalprep/mar_prs_population/v2016/output/mar_pop_25mi.csv") %>%
  dplyr::select(rgn_id, year, popsum_old=popsum)

tmp <- coastal_pop2 %>%
  dplyr::left_join(old, by=c("rgn_id", "year"))

plot(log(tmp$popsum), log(tmp$popsum_old))
abline(0,1, col="red")

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

