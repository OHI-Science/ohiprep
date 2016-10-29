################################################
## Describes preparation of spatial files
################################################

require(sp)
library(raster)
library(rgdal)
library(maptools)
library(gridExtra)
# library(fields)
# library(maps)
# library(gpclib)
# library(animation)
#library(plyr)
#library(reshape2)

source("src/R/common.R")

## arcGIS processes:
## For North pole: transformed this file: N:\model\GL-NCEAS-OceanRegions_v2013a\data\rgn_fao_gcs
## to the coordinate systems used in the shp files located in the "Testing old data" 
## and saved in folder: n_type_rgns_pts to get N:\git-annex\globalprep\NSIDC_SeaIce\v2015\raw\New_n_rgn_fao

## The South pole (to get CCAMLR regions) was based on these data: /var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data/sp_gcs
## and transformed to coordinate reference system used in this file: s_type_rgns_pts and saved in this folder:
## 

## The south pole was updated to reflect the CCAMLR regions, but the North pole should be the same, hence the
## two different starting spatial datasets.

### Preparing the south pole data from the Antarctica project
s_pole <- readOGR(file.path(dir_neptune_data, "model/GL-AQ-SeaIce_v2014/raw"), layer='sp_s')
s_pole <- s_pole[s_pole$sp_type %in% c("eez", "eez-ccamlr", "fao"), ]
# feel for naming protocols
head(s_pole)
s_pole@data[s_pole$sp_type == "eez-ccamlr",]
s_pole@data[s_pole$sp_type == "fao",]
s_pole@data[duplicated(s_pole$rgn_id),]

## in general, use rgn_id, but use the sp_id for ccamlr regions
s_pole@data$rgn_id <- ifelse(s_pole@data$sp_type == "eez-ccamlr", s_pole@data$sp_id, s_pole@data$rgn_id)

## need to merge matching polygons so the same id is only repeated once
s_pole_union <- unionSpatialPolygons(s_pole, s_pole$rgn_id)
s_pole_data <- s_pole@data %>%
  select(sp_type, rgn_id, rgn_name, rgn_key, area_km2) %>%
  group_by(sp_type, rgn_id, rgn_name, rgn_key) %>%
  summarize(area_km2=sum(area_km2))
row.names(s_pole_data) <- as.character(s_pole_data$rgn_id)
s_pole_data <- data.frame(s_pole_data)

s_pole_union <- SpatialPolygonsDataFrame(s_pole_union, s_pole_data)
s_pole_union@data <- s_pole_union@data %>%
  select(rgn_typ=sp_type, rgn_id, rgn_nam=rgn_name, rgn_key) # realized the area might be deceptive because I think some of the northern regions are not included.

#writeOGR(s_pole_union, file.path(dir_neptune_data, "git-annex/globalprep/NSIDC_SeaIce/v2015/raw"), layer="New_s_rgn_fao", driver="ESRI Shapefile")

new_s_pole <- readOGR(file.path(dir_neptune_data, "git-annex/globalprep/NSIDC_SeaIce/v2015/raw"), layer="New_s_rgn_fao")

## The next step is actually embedded in the "ObtainingData.R" script.  If the spatial points file is not there, it creates one.
## This would make more sense in this file, but it requires loading a lot of the NSDIC data first to use as a template. 
## I think the best bet is to do this in a step-wise fashion: 1) walk through the script and check the output spatial file, 2)
## then run the sea ice collection.

