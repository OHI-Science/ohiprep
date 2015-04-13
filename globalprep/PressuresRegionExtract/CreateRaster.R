### zonal extraction and summary of acidification data
### MRF: Feb 25 2015

source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(dplyr)

save_loc <- file.path(dir_neptune_data, "git-annex/Global/NCEAS-Regions_v2014/data/sp_mol_raster_1km")

## raster template that is used for the 1km grid
raster_template <- raster(file.path(dir_neptune_data, 
                        'git-annex/globalprep/Pressures_acid/v2015/working/annual_oa_rescaled_1km/annual_oa_rescaled_1km_.tif'))

# read and explore region file.  
# The sp_type includes information on the CCAMLR, EEZ, FAO regions.  
# But, some of the eez regions have multiple sp_regions (i.e., Alaska vs. mainland US).
regions_mol <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_mol")

## Make sure the overlap between the shape file and the raster looks about right:
plot(raster_template)
plot(regions_mol, add=TRUE)
#It looks like the raster and spatial file align ok.

#explore the data:
regions_mol@data[regions_mol@data$sp_type=="eez-ccamlr",] #note that the rgn_id is 213 for all CCAMLR (Antarctica) regions
table(regions_mol@data$sp_type)   #notice there are ccamlr-land and ccamlr-eez regions for this variable
table(regions_mol@data$rgn_type)  #notice there are no ccamlr-land or ccamlr-eez regions for this variable

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


# Some island regions are not being included in the final raster for some reason....
# I have explored every single option that I can think of that would explain this to no avail.
# These islands do show up when they are analyzed separately from the larger shape file
# Now, I am going to have to resort to this super unsatisfying and hacky way of accomplishing this.

# Here are the trouble makers:
plot(regions_mol[regions_mol@data$sp_id %in% c(1, 86, 88, 105, 107, 159), ])  

# Create a raster with just these areas that will be merged with the (nearly) full raster:
regions_mol_bad <- regions_mol[regions_mol@data$sp_id %in% c(1, 86, 88, 105, 107, 159), ]
rasterize(regions_mol_bad, raster_template, 
          field    = "sp_id", 
          filename = file.path(save_loc, "sp_mol_raster_bad_regions.tif"), overwrite=TRUE, 
          progress = "text")
bad_regions_raster <- raster(file.path(save_loc, "sp_mol_raster_bad_regions.tif"))
plot(bad_regions_raster)


#convert region file to raster (only need to do this once...)
rasterize(regions_mol, raster_template, 
          field    = "sp_id", 
          filename = file.path(save_loc, "sp_mol_raster_most_regions.tif"), overwrite=TRUE, 
          progress = "text")

# read in region raster
most_regions <- raster(file.path(save_loc, "sp_mol_raster_most_regions.tif"))
plot(most_regions)
freq(most_regions, progress="text", value=1) # missing
freq(most_regions, progress="text", value=0) # not missing
freq(most_regions, progress="text", value=86)# missing

# merge the two rasters
raster::cover(bad_regions_raster, most_regions, filename = file.path(save_loc, "sp_mol_raster_1km.tif"))

regions <- raster(file.path(save_loc, "sp_mol_raster_1km.tif"))
freq(regions, progress="text", value=1) # missing
freq(regions, progress="text", value=0) # not missing
freq(regions, progress="text", value=86)# missing


# get the data to associate with the sp_id
region_lu <- regions_mol@data %>%
  dplyr::select(sp_id, sp_type, rgn_id, rgn_name) %>%
  unique()


# read in acid data (should be 10 layers, with values 0 to 1)
# currently don't have a stack
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
