library(sf)
library(fasterize) # devtools::install_github("ecohealthalliance/fasterize")
library(raster)

source("src/R/common.R")

### STEP 1: create a polygon file with better labeling
# sp_id includes antarctica regions that are not 213, but 
# is otherwise confusing.
# Plus, there are some regions are represented by multiple polygons 
# (US, Hawaii, Alaska, etc.)...need to merge these.

regions <- sf::read_sf(dsn = "/home/shares/ohi/git-annex/Global/NCEAS-Regions_v2014/data",
                       layer = "sp_mol")

## fixing labels on some disputed regions
regions <- regions %>%
  mutate(rgn_name = ifelse(rgn_id == 255, "DISPUTED", rgn_name)) %>%
  mutate(rgn_key = ifelse(rgn_id == 255, "XD", rgn_key)) %>%
  mutate(sp_type = ifelse(rgn_id == 255 & sp_type == "eez", "eez-disputed", sp_type)) %>%
  mutate(sp_type = ifelse(rgn_id == 255 & sp_type == "land", "land-disputed", sp_type))

old <- regions$rgn_id

regions$rgn_ant_id <- ifelse(regions$sp_type %in% c("eez-ccamlr", "land-ccamlr"), 
                             regions$sp_id, 
                             regions$rgn_id)



new <- regions$rgn_ant_id

old[old != new]  # good: indicates only the antarctica regions are different, which is what we want


## create an ID to combine region ids/types into single polygons (most are, but there are a few exceptions,
# such as Hawaii and AK, in US)
regions$unq_id <- paste(regions$rgn_ant_id, regions$rgn_type, sep="_")
head(regions)

# get data for joining later
data <- data.frame(regions) %>%
  select(sp_type, sp_id, sp_name, sp_key, rgn_type, rgn_id, rgn_name, rgn_key, area_km2, rgn_ant_id, unq_id) 

data <- data %>%
  select(sp_type, rgn_type, rgn_id, rgn_name, rgn_key, rgn_ant_id, unq_id, area_km2) %>%
  group_by(sp_type, rgn_type, rgn_id, rgn_name, rgn_key, rgn_ant_id, unq_id) %>%
  summarize(area_km2_v2 = sum(area_km2))  %>% # checked the areas and they looked good (highly correlated but slightly different), will just go with arcgis areas
  select(sp_type, rgn_type, rgn_id, rgn_name, rgn_key, rgn_ant_id, unq_id, area_km2_v2)


## some data checks:
## NOTE: All these seem correct
dups <- regions$unq_id[duplicated(regions$unq_id)] #41 duplicates (out of 567)
duplicatedData <- regions[regions$unq_id %in% dups, ] 
duplicatedData_csv <- duplicatedData %>%
  st_set_geometry(NULL)
regions[regions$unq_id %in% "171_eez", ] 
write.csv(duplicatedData_csv, "globalprep/spatial/v2017/DataCheckofDuplicatedRegions.csv", row.names=FALSE)
regions[regions$rgn_id == 213, ] 

# ## save file with new ID values
# st_write(regions, dsn=file.path(dir_M, "git-annex/globalprep/spatial/v2017/int"),
#          layer = "regions_merge_id",
#          driver="ESRI Shapefile")

# test <- st_read(file.path(dir_M, "git-annex/globalprep/spatial/v2017/int"), "regions_merge_id")

## correct some topology errors (intersections, etc.)
# when I run this, it seems to work, but I can't save the file!
# st_is_valid(regions)
# sf_extSoftVersion()["lwgeom"]
# regions_tidy <- st_make_valid(regions)
# st_is_valid(regions_tidy)
valid = st_is_valid(regions)

# Another method of fixing (seems to work):
#regions_tidy <- st_buffer(regions[!(valid), ], 0.0)
regions_tidy <- st_buffer(regions, 0.0)
valid = st_is_valid(regions)

# ## save file with corrected topology
st_write(regions_tidy, dsn=file.path(dir_M, "git-annex/globalprep/spatial/v2017/int"),
         layer = "regions_tidy",
         driver="ESRI Shapefile")

regions_tidy <- st_read(dsn=file.path(dir_M, "git-annex/globalprep/spatial/v2017/int"),
         layer = "regions_tidy")

# 
## try this to combine regions
ids <- regions_tidy$unq_id 
t0 <- aggregate(regions_tidy, list(ids = ids), head, n = 1) 
t0_min <- t0 %>%
  select(unq_id, geometry)

setdiff(t0_min$unq_id, data$unq_id)
setdiff(data$unq_id, t0_min$unq_id)
data[duplicated(data$unq_id), ]

t0_min <- left_join(t0_min, data, by="unq_id")
t0_min <- dplyr::select(t0_min, sp_type, rgn_type, rgn_id, rgn_name, rgn_key, rgn_ant_id, area_km2=area_km2_v2)

rgns_final <- t0_min %>% 
  mutate(area = st_area(.) %>% as.numeric()) %>%
  mutate(area = round(area/1000000)) %>% 
  select(type_w_ant = sp_type, rgn_type, rgn_id, rgn_name, rgn_key, rgn_ant_id, area_km2=area, geometry)

st_write(rgns_final, dsn=file.path(dir_M, "git-annex/globalprep/spatial/v2017"),
         layer = "regions_2017_update",
         driver="ESRI Shapefile")


#### Save as raster

# get most recent shapefile and select eez regions only:
regions <- st_read(dsn=file.path(dir_M, "git-annex/globalprep/spatial/v2017"),
                        layer = "regions_2017_update")
table(regions$type_w_ant)
table(regions$rgn_type)
filter(regions, rgn_type == "eez-inland")

regions <- regions[regions$rgn_type %in% c("eez", "fao"), ]

data_rgns <- data.frame(regions) %>%
  filter(rgn_type %in% c("eez", "fao")) %>%
  select(rgn_ant_id, area_km2)

### use the old mask as a template for projection, origin, extents, and resolution
old_mask <- raster(file.path(dir_M, 'git-annex/globalprep/spatial/d2014/data/rgn_mol_raster_1km/sp_mol_raster_1km.tif'))

regions_raster <- fasterize::fasterize(regions, old_mask, field = 'rgn_ant_id')

writeRaster(regions_raster,
            file.path(dir_M, 'git-annex/globalprep/spatial/v2017/regions_with_fao_ant.tif'))

test_raster <- raster(file.path(dir_M, 'git-annex/globalprep/spatial/v2017/regions_with_fao_ant.tif'))
plot(test_raster)
tmp <- freq(test_raster)

tmp_df <- data.frame(tmp)
setdiff(as.numeric(as.character(tmp_df$value)), as.numeric(as.character(data_rgns$rgn_ant_id)))
setdiff(as.numeric(as.character(tmp_df$value)), as.numeric(as.character(data_rgns$rgn_ant_id)))
setdiff(data_rgns$rgn_ant_id, tmp_df$value)

tmp2 <- data.frame(tmp) %>%
  select(rgn_ant_id = value, count) %>%
  dplyr::mutate(area = 934.4789*934.4789*0.000001 * count) %>%
  left_join(data_rgns, by="rgn_ant_id")

plot(log(tmp2$area), log(tmp2$area_km2))
abline(0,1, col="red")

### now make an ocean raster:
raster_ocean <- raster(file.path(dir_M, 'git-annex/globalprep/spatial/v2017/regions_with_fao_ant.tif'))
plot(raster_ocean)
reclassify(raster_ocean, c(0,300000,1),
                filename = file.path(dir_M, 'git-annex/globalprep/spatial/v2017/ocean.tif'))

