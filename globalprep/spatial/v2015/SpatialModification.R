########################################################
## Making a few modifications of the spatial files that Ben B
## created so they will be more useful for our analyses
## June 7 2015, MRF
########################################################

library(maptools)
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

new <- regions_mol@data$rgn_id_ccamlr
old[old != new]

regions_mol@data$rgn_id_ccamlr_rgn_type <- paste(regions_mol@data$rgn_id_ccamlr, regions_mol@data$rgn_type, sep="_")
head(regions_mol@data)

## some data checks:
## NOTE: All these seem correct
dups <- regions_mol@data$rgn_id_ccamlr_rgn_type[duplicated(regions_mol@data$rgn_id_ccamlr_rgn_type)] 
duplicatedData <- regions_mol@data[regions_mol@data$rgn_id_ccamlr_rgn_type %in% dups, ] 
write.csv(duplicatedData, "globalprep/spatial/DataCheckofDuplicatedRegions.csv", row.names=FALSE)
regions_mol@data[regions_mol@data$rgn_id == 213, ] 

### I think we now want to combine regions with the same rgn_id_ccamlr-rgn_type ID so they are functionally the same polygon
# dont think I want to merge the disputed regions
regions_mol@data$union <- ifelse(regions_mol@data$rgn_id_ccamlr == 255, regions_mol@data$sp_id, regions_mol@data$rgn_id_ccamlr)  
writeOGR(regions_mol, dsn="/var/data/ohi/git-annex/globalprep/spatial", "spatial_file_pre_union", driver="ESRI Shapefile")


# tried this approach but there was an error - resorting to Arc
# data <- regions_mol@data  #saving data to merge with unionized polygons
# rgn_mol_union <- unionSpatialPolygons(regions_mol, regions_mol@data$rgn_id_ccamlr_rgn_type)




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

