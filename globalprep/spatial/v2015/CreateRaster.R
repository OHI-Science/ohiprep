### Create general raster that should be able to use on all 1 km2 pressures
### MRF: Feb 25 2015

source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)
library(dplyr)

save_loc <- file.path(dir_M, "git-annex/globalprep/spatial/d2014/data/rgn_mol_raster_1km")

## raster template that is used for the 1km grid
raster_template <- raster(file.path(dir_M, 
                                    'git-annex/globalprep/prs_oa/v2015/working/annual_oa_rescaled_1km/annual_oa_rescaled_1km_2005.tif'))

# read and explore region file.  
regions_mol <- readOGR(dsn= file.path(dir_M, "git-annex/globalprep/spatial/d2014/data"), layer="regions_mol")

## Make sure the overlap between the shape file and the raster looks about right:
plot(raster_template)
plot(regions_mol, add=TRUE)
#It looks like the raster and spatial file align ok.

#explore the data:
head(regions_mol@data)
table(regions_mol@data$rgn_typ)
table(regions_mol@data$ant_typ)

#select only the eez-ccamlr, fao, and eez regions:
regions_mol <- regions_mol[regions_mol@data$ant_typ %in% c("eez-ccamlr", "fao", "eez"), ]
summary(regions_mol@data)

#convert region file to raster (only need to do this once...)
rasterize(regions_mol, raster_template, 
          field    = "ant_id", 
          filename = file.path(save_loc, "sp_mol_raster_most_regions_2.tif"), overwrite=TRUE, 
          progress = "text")

rast <- raster(file.path(save_loc, "sp_mol_raster_most_regions_2.tif"))
tmp <- freq(rast, progress="text")
tmp2 <- data.frame(tmp)
region_list <- regions_mol@data
setdiff(tmp2$value, region_list$ant_id)
setdiff(region_list$ant_id, tmp2$value)

# Some regions are not being included in the final raster for some reason....
# I have explored every single option that I can think of that would explain this to no avail.
# Now, I am going to have to resort to this super unsatisfying and hacky way of accomplishing this.

# Here are the trouble makers:
plot(regions_mol[regions_mol@data$ant_id %in% c(1, 105, 107, 159), ])  

# Create a raster with just these areas that will be merged with the (nearly) full raster:
regions_mol_bad <- regions_mol[regions_mol@data$ant_id %in% c(1, 105, 107, 159), ]
rasterize(regions_mol_bad, raster_template, 
          field    = "ant_id", 
          filename = file.path(save_loc, "sp_mol_raster_bad_regions_2.tif"), overwrite=TRUE, 
          progress = "text")
bad_regions_raster <- raster(file.path(save_loc, "sp_mol_raster_bad_regions_2.tif"))
plot(bad_regions_raster, add=TRUE)


# read in region raster
most_regions <- raster(file.path(save_loc, "sp_mol_raster_most_regions_2.tif"))
plot(most_regions)
freq(most_regions, progress="text", value=1) # missing
freq(most_regions, progress="text", value=86)# not missing

# merge the two rasters
raster::cover(bad_regions_raster, most_regions, filename = file.path(save_loc, "sp_mol_raster_1km.tif"))

regions <- raster(file.path(save_loc, "sp_mol_raster_1km_2.tif"))
freq(regions, progress="text", value=1) # not missing
freq(regions, progress="text", value=86)# not missing
tmp <- freq(regions, progress="text")
tmp2 <- data.frame(tmp)
region_list <- regions_mol@data
setdiff(tmp2$value, region_list$ant_id)
setdiff(region_list$ant_id, tmp2$value)


# save data related to regions
region_lu <- regions_mol@data %>%
  dplyr::select(rgn_typ, ant_typ, rgn_id, ant_id, rgn_nam) %>%
  unique()

write.csv(region_lu, file.path(save_loc, "regionData.csv"), row.names=FALSE)
