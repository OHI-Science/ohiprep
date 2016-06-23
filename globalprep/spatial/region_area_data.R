################################################
## updating region data for toolbox
## there were some slight changes in the areas
## and I just want to update them for when we summarize the data
## (currently, when I was calculating the global values these rounding errors were making
## it so I would have to reweight the values)
## I believe the main spatial file is the only one that actually matters
###############################################################################

source('../ohiprep/src/R/common.R')

library(rgdal)
library(dplyr)
library(tidyr)

old <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-OceanRegions_v2013a/data/rgn_fao_gcs_area.csv")) %>%
  select(rgn_id, area_km2_old=area_km2)

new_poly <- readOGR(
  dsn=file.path(dir_neptune_data, "git-annex/globalprep/spatial/v2015/data"),
  "regions_mol")

new <- new_poly@data
new_area <- new %>%
  filter(rgn_typ %in% c('eez', 'fao')) %>%
  group_by(rgn_id) %>%
  summarize(area_km2 = sum(are_km2)) %>%
  left_join(old, by='rgn_id')

## check of data suggests the results are reasonable  
plot(area_km2~area_km2_old, data=new_area)
abline(0,1, col="red")
new_area[new_area$area_km2 > 3e+07, ]  # new area of Antarctica is much larger (which is correct)

new_area <- new_area %>%
  select(rgn_id, area_km2)

write.csv(new_area, "globalprep/spatial/v2015/data/rgn_fao_gcs_area.csv",
          row.names=FALSE)