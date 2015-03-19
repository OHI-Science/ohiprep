### zonal extraction and summary of acidification data
### MRF: Feb 25 2015

library(raster)
library(rgdal)
library(dplyr)

# read and explore region file.  
# The sp_type includes information on the CCAMLR, EEZ, FAO regions.  
# But, some of the eez regions have multiple sp_regions (i.e., Alaska vs. mainland US).
regions_mol <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_mol")

#explore this shapefile:
regions_mol@data[regions_mol@data$sp_type=="eez-ccamlr",] #note that the rgn_id is 213 for all CCAMLR (Antarctica) regions
table(regions_mol@data$sp_type)   #notice there are ccamlr-land and ccamlr-eez regions for this variable
table(regions_mol@data$rgn_type)  #notice there are no ccamlr-land or ccamlr-eez regions for this variable
plot(regions_mol[regions_mol@data$sp_id=="223",])  ## I think this is a mistake that there are two regions with the same sp_id 
## although, I will keep because below I am converting different sp_id's to the same value when
## they have the same region_id.  This way all the data will be extracted for a country (i.e., reporting region)
## and will not need to be combined later on.

# some regions are showing up with no data for some reason....
plot(regions_mol[regions_mol@data$sp_id=="1",], col="red")  
plot(regions_mol, add=TRUE)
regions_mol@data[regions_mol@data$sp_id=="1",] 
plot(rast_example, add=TRUE)
plot(regions_mol[regions_mol@data$sp_id=="1",], add=TRUE)  
plot(regions_mol[regions_mol@data$sp_id=="86",])  
regions_mol@data[regions_mol@data$sp_id=="86",] 
plot(rast_example, add=TRUE)


## explore these:
regions_mol_bad <- regions_mol[regions_mol@data$sp_id %in% c(1, 86, 88, 105, 107, 159), ]
regions_mol_bad <- regions_mol_bad[regions_mol_bad@data$sp_type %in% c("eez-ccamlr", "fao", "eez"), ]
rast_example <- raster('/var/data/ohi/git-annex/globalprep/Pressures_acid/v2015/working/annual_oa_1km/oa_1km_2005.tif')
rasterize(regions_mol_bad, rast_example, 
          field="sp_id", 
          filename="/var/data/ohi/git-annex/globalprep/Pressures_acid/v2015/working/annual_oa_1km/sp_mol_raster_bad_regions.tif", overwrite=TRUE, 
          progress="text")
bad_regions_raster <- raster("/var/data/ohi/git-annex/globalprep/Pressures_acid/v2015/working/annual_oa_1km/sp_mol_raster_bad_regions.tif")
plot(bad_regions_raster)

regions_mol@data[regions_mol@data$sp_id %in% c(1, 86, 88, 105, 107, 159), ])
plot(regions_mol[regions_mol@data$sp_id %in% c(1, 86, 88, 105, 107, 159), ])  
plot(rast_example, add=TRUE)
plot(regions_mol[regions_mol@data$sp_id %in% c(1, 86, 88, 105, 107, 159), ], add=TRUE, col="red")  

plot(regions_mol[regions_mol@data$sp_type=="fao", ], col="blue")

#select only the eez-ccamlr, fao, and eez regions:
regions_mol <- regions_mol[regions_mol@data$sp_type %in% c("eez-ccamlr", "fao", "eez"), ]

# Giving sp_id's that that differ, but are in the same country, matching sp_id values.
# [NOTE: maybe this is something that should be done at the level of the original shapefiles?]
## find out which ones are duplicated:
dup_regs <- regions_mol@data$rgn_id[duplicated(regions_mol@data$rgn_id)]
regions_mol@data[regions_mol@data$rgn_id %in% dup_regs, ]

# make the changes:
regions_mol@data$sp_id[regions_mol@data$sp_name %in% "Trindade"] <- 171 
regions_mol@data$sp_id[regions_mol@data$sp_name %in% "Galapagos Islands"] <- 137 
regions_mol@data$sp_id[regions_mol@data$sp_name %in% "Hawaii"] <- 163
regions_mol@data$sp_id[regions_mol@data$sp_name %in% "Alaska"] <- 163
regions_mol@data$sp_id[regions_mol@data$sp_name %in% "Easter Island"] <- 224
regions_mol@data$sp_id[regions_mol@data$sp_name %in% "Guadeloupe"] <- 251
regions_mol@data$sp_id[regions_mol@data$sp_name %in% "Virgin Islands of the United States"] <- 255

regions_mol_bad <- regions_mol[regions_mol@data$sp_id %in% c(1, 86, 88, 105, 107, 159), ]

# get the data I will need to associate with the sp_id
region_lu <- regions_mol@data %>%
  select(sp_id, sp_type, rgn_id, rgn_name) %>%
  unique()

## read in one of the raster files to check on data (make sure the overlap between the shape file and the raster looks about right):
rast_example <- raster('/var/data/ohi/git-annex/globalprep/Pressures_acid/v2015/working/annual_oa_1km/oa_1km_2005.tif')
rast_example
plot(rast_example)
plot(regions_mol, add=TRUE)
#It looks like the raster and spatial file align ok.

#convert region file to raster (only need to do this once...)
rasterize(regions_mol, rast_example, 
          field="sp_id", 
          filename="/var/data/ohi/git-annex/globalprep/Pressures_acid/v2015/working/annual_oa_1km/sp_mol_raster.tif", overwrite=TRUE, 
          progress="text")

 
# read in region raster
regions <- raster("/var/data/ohi/git-annex/globalprep/Pressures_acid/v2015/working/annual_oa_1km/sp_mol_raster.tif")
plot(regions)
freq(regions, progress="text", value=1)
freq(regions, progress="text", value=0)

# read in acid data (should be 10 layers, with values 0 to 1)
rasts <- paste0('/var/data/ohi/git-annex/globalprep/Pressures_acid/v2015/working/annual_oa_1km/oa_1km_', c(2005:2014), '.tif')

sst_stack <- stack()
for(i in 1:length(rasts)){
  tmp <- raster(rasts[i])
  sst_stack <- stack(sst_stack, tmp)
}

# extract data for each region:
regions_stats <- zonal(sst_stack,  regions, fun="mean", na.rm=TRUE, progress="text")
regions_stats2 <- data.frame(regions_stats)
setdiff(regions_stats2$zone, region_lu$sp_id)
setdiff(region_lu$sp_id, regions_stats2$zone)



data <- merge(regions_mol@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 
