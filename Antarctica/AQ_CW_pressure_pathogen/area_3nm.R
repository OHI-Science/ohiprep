##################################
## get offshore area 3nm offshore
## (used to standardize pathogen pollution)
##################################

library(sp)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(colorspace)
library(rgeos)

# Load in data ----
map <- readOGR(dsn="/home/shares/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_offshore3nm_mol")
map <- map[map@data$sp_type %in% c("eez-ccamlr"), ]
gArea(map, byid=TRUE)/1000000
map@data$area_km2   
# I get slightly different areas, but in the realm of rounding error.  I'll go with the shapefile data.
write.csv(select(map@data, sp_id, area_km2), 
          "Antarctica/AQ_CW_pressure_pathogen/intermediate/area_3nm_offshore.csv", 
          row.names=FALSE)
