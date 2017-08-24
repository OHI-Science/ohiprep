########################################################
## Making a few modifications of the spatial files that Ben B
## created so they will be more useful for our analyses
## June 7 2015, MRF
## NOTE: 6/21/2016: file locations have changed to match our current data organization method
########################################################

library(maptools)
library(sp)
library(rgdal)
library(dplyr)
library(gpclib)
gpclibPermit()

### mollweide CRS
## reading in the data
regions_mol <- readOGR(dsn="/home/shares/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_mol")

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
regions_mol@data[regions_mol@data$rgn_id_ccamlr_rgn_type %in% "171_eez", ] 
write.csv(duplicatedData, "globalprep/spatial/DataCheckofDuplicatedRegions.csv", row.names=FALSE)
regions_mol@data[regions_mol@data$rgn_id == 213, ] 

### I think we now want to combine regions with the same rgn_id_ccamlr-rgn_type ID so they are functionally the same polygon
# dont think I want to merge the disputed regions
regions_mol@data$union <- ifelse(regions_mol@data$rgn_id_ccamlr == 255, regions_mol@data$sp_id, regions_mol@data$rgn_id_ccamlr)  
regions_mol@data$union <- paste(regions_mol@data$union, regions_mol@data$rgn_type, sep="_")
data <- regions_mol@data  #saving data to merge with unionized polygons
#writeOGR(regions_mol, dsn="/var/data/ohi/git-annex/globalprep/spatial/v2015/tmp", "spatial_file_pre_union", driver="ESRI Shapefile", overwrite_layer=TRUE)

# # tried this approach but there was an error - resorting to Arc
 rgn_mol_union <- unionSpatialPolygons(regions_mol, regions_mol@data$rgn_id_ccamlr_rgn_type)

### Now merging all these data with the output file from Arc
#rgn_mol_union <- readOGR(dsn="/var/data/ohi/git-annex/globalprep/spatial/v2015/tmp", layer="spatial_file_post_union")
setdiff(rgn_mol_union@data$union, data$union)
setdiff(data$union, merge_shape$union)

data_tmp <- data %>%
  select(sp_type, rgn_type, rgn_id, rgn_name, rgn_key, rgn_id_ccamlr, union, area_km2) %>%
  group_by(sp_type, rgn_type, rgn_id, rgn_name, rgn_key, rgn_id_ccamlr, union) %>%
  summarize(area_km2_v2 = sum(area_km2))  %>% # checked the areas and they looked good (highly correlated but slightly different), will just go with arcgis areas
  select(sp_type, rgn_type, rgn_id, rgn_name, rgn_key, rgn_id_ccamlr, union)
#rgn_mol_union@data$area3 <- gArea(rgn_mol_union, byid=TRUE) # this nearly perfectly matched the output from arcgis
  
rgn_mol_union@data <- left_join(rgn_mol_union@data, data_tmp, by="union")
rgn_mol_union@data <- dplyr::select(rgn_mol_union@data, sp_type, rgn_type, rgn_id, rgn_name, rgn_key, rgn_id_ccamlr, area_km2=Shape_Area)
rgn_mol_union@data$area_km2 = rgn_mol_union@data$area_km2/1000000

plot(data_tmp$Shape_Area, data_tmp$area_km2_v2)
data_tmp$area3 <- gArea(merge_shape, byid=TRUE)

#writeOGR(rgn_mol_union, dsn="/var/data/ohi/git-annex/globalprep/spatial/v2015/data", "regions_mol", driver="ESRI Shapefile")

#### checking on data, and changing variable names to more sensible names:

check <- readOGR(dsn="/home/shares/ohi/git-annex/globalprep/spatial/d2014/data", layer="regions_mol")

check@data <- select(check@data, c(rgn_typ, ant_typ=sp_type, rgn_id, ant_id=rgn_d_c, rgn_nam, rgn_key, are_km2))
head(check@data)
table(check@data$ant_typ)
table(check@data$rgn_typ)
check@data[check@data$rgn_id != check@data$ant_id, ]
check@data[check@data$rgn_id == 16, ]

#writeOGR(check, dsn="/var/data/ohi/git-annex/globalprep/spatial/v2015/data", "regions_mol", driver="ESRI Shapefile", overwrite_layer=TRUE)

check <- readOGR(dsn="/home/shares/ohi/git-annex/globalprep/spatial/d2014/data", layer="regions_gcs")
check@data <- select(check@data, c(rgn_typ, ant_typ=sp_type, rgn_id, ant_id=rgn_d_c, rgn_nam, rgn_key, are_km2))
head(check@data)
table(check@data$ant_typ)
table(check@data$rgn_typ)
check@data[check@data$rgn_id != check@data$ant_id, ]
check@data[check@data$rgn_id == 16, ]
#writeOGR(check, dsn="/var/data/ohi/git-annex/globalprep/spatial/v2015/data", "regions_gcs", driver="ESRI Shapefile", overwrite_layer=TRUE)
