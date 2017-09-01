### data_prep_coastal_pop.R

# NEW: updated dataset from source.  UN standardized and higher quality resolution data.

library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
library(raster)
library(sp)
library(stringr)
library(dplyr)

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
eez_raster <- raster(file.path(dir_M, "git-annex/globalprep/spatial/v2017/regions_eez_with_fao_ant.tif"))

# need to add in Fiji...which appears to get cut
# include all of Fiji land which all falls within the 50 mile boundary
fiji <- sf::read_sf(dsn = file.path(dir_M, "git-annex/globalprep/spatial/v2017"),
                              layer ="regions_2017_update") %>%
  filter(rgn_type == "land" & rgn_id==18) %>%
  select(rgn_id, geometry)

# create the 25 mile inland raster and add Fiji
inland <- sf::read_sf(dsn = file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/data"),
                                 layer = "sp_inland25mi_gcs") %>%
  select(rgn_id, geometry)

inland <- st_transform(inland, st_crs(fiji))

inland <- rbind(inland, fiji)

# # save shapefile for future reference
# st_write(inland, dsn = file.path(dir_M, "git-annex/globalprep/spatial/v2017",
#                                      layer = "EEZ_inland_50mi"), 
#          driver="ESRI Shapefile")


inland_raster <- fasterize::fasterize(inland, eez_raster, field = 'rgn_id')

tmp <- raster::freq(inland_raster) 

tmp2 <- data.frame(tmp) %>%
  dplyr::arrange(value)

raster::merge(eez_raster, inland_raster, filename=file.path(dir_M, 
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

