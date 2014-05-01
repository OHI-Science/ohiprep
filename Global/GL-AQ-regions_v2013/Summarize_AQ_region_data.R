#################################################
## If I need to extract data from shape files
#################################################

library(sp)
library(rgdal)
library(RColorBrewer)
library(colorspace)

rm(list = ls())

map <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_gcs")
map <- map[map@data$sp_type %in% c("eez-ccamlr", "land-ccamlr"), ]
plot(map)

