##############################################################################=
### Pressure: hd_intertidal (coastal population density, proxy for habitat destruction)
### rescale population from zero to one for pressures purposes -----
##############################################################################=
library(dplyr)
source('src/R/common.R')

library(sp)
library(raster)
library(sf)
library(units)

# density is calculated by dividing population/area_km2
# data are logged due to massive skew
# density data are rescaled using log(max(density) + 1)


## last year, Fiji was dropped due to the shapefile not having these data.
## corrected in these spatial data, so I will regenerate these data.
inland <- sf::read_sf(dsn = file.path(dir_M, "git-annex/globalprep/spatial/v2017/EEZ_inland_50mi"),
                      layer = "EEZ_inland_50mi")

inland$area_km2 <- st_area(inland)

area <- data.frame(inland) %>%
  select(rgn_id, area_km2) %>%
  mutate(area_km2 = round(area_km2/1000000)) %>%
  mutate(area_km2 = as.numeric(as.character(gsub(" m^2", "", area_km2)))) %>%
  filter(rgn_id <=250) %>%
  filter(rgn_id != 213) %>%
  group_by(rgn_id) %>%
  dplyr::summarize(area_km2 = sum(area_km2))

## compare with previous year (should look very similar)
old_area <- read.csv("globalprep/mar_prs_population/v2015/rgn_area_inland25mi.csv") %>%
  select(rgn_name, rgn_id, old_area_km2=area_km2) %>%
  mutate(old_area_km2 = round(old_area_km2)) %>%
  left_join(area, by="rgn_id")

plot(old_area$old_area_km2, old_area$area_km2)
abline(0,1, col="red")

summary(old_area)

## save area data (basically same as last year, but includes Fiji, rgn_id=18)
write.csv(area, "globalprep/mar_prs_population/v2017/int/area_km2_25mi.csv", row.names=FALSE)

############################################
## Calculate population pressure

area <- read.csv("globalprep/mar_prs_population/v2017/int/area_km2_25mi.csv")

pop_rescaled <- read.csv("globalprep/mar_prs_population/v2017/output/mar_pop_25mi.csv") %>%
  left_join(area, by="rgn_id") %>%
  mutate(density = popsum/area_km2) %>%
  mutate(ln_density = log(density + 1)) %>%
  mutate(scalar = max(ln_density, na.rm=TRUE)) %>%
  mutate(dens_rescaled = ln_density/scalar) %>%
  mutate(dens_rescaled = ifelse(dens_rescaled>1, 1, dens_rescaled))

filter(pop_rescaled, is.na(area_km2)) # missing value is Antarctica...which is good!

pressure_data <- pop_rescaled %>%
  select(rgn_id, year, pressure_score = dens_rescaled)

write.csv(pressure_data, "globalprep/mar_prs_population/v2017/output/prs_pop_density.csv", 
          row.names = FALSE)