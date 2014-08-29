#################################################
## If I need to extract data from shape files
#################################################

library(sp)
library(rgdal)
library(RColorBrewer)
library(colorspace)

rm(list = ls())

## checking on ccamlr region areas (make sure they match what is in the spreadsheet):
map <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_gcs")
map <- map[map@data$sp_type %in% c("eez-ccamlr"), ]
plot(map)

data <- read.csv("/var/data/ohi/git-annex/Global/NCEAS-Antarctica-Other_v2014/rgn_labels_ccamlr.csv")

join(map@data, data, by=c("sp_type", "sp_id", "sp_name", "sp_key", "rgn_id", "rgn_name", "rgn_key"))

## yep....looks good....