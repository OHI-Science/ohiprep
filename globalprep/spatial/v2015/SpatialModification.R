########################################################
## Making a few modifications of the spatial files that Ben B
## created so they will be more useful for our analyses
## June 7 2015, MRF
########################################################

library(sp)
library(rgdal)
library(dplyr)

### mollweide CRS
## reading in the data
regions_mol <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_mol")

old <- regions_mol@data$rgn_id
regions_mol@data$rgn_id_ccamlr <- ifelse(regions_mol@data$sp_type %in% c("eez-ccamlr", "land-ccamlr"), 
                                  regions_mol@data$sp_id, 
                                  regions_mol@data$rgn_id)

new <- regions_mol@data$rgn_id
old[old != new]
dups <- regions_mol@data$rgn_id[duplicated(regions_mol@data$rgn_id)] 
regions_mol@data[regions_mol@data$rgn_id %in% dups, ] 

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

