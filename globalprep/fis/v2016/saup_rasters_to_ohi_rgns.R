###############################################
## Associates the SAUP raster cells with the 
## corresponding ohi regions
## MRF: June 6 2016
###############################################
install.packages("devtools")
devtools::install_github("SeaAroundUs/rseaaroundus")

library(reshape2)
library(dplyr)
library(seaaroundus)
library(rgdal)
library(raster)

source('src/R/common.R')

ohi_regions <- readOGR(dsn = file.path(dir_M, "git-annex/globalprep/spatial/d2014/data"), layer="regions_gcs")  
inland      <- ohi_regions[ohi_regions@data$rgn_typ == "eez-inland",]
plot(inland)

saup_cells  <- getcells("POLYGON ((-180 90,-180 -90, 180 -90, 180 90, -180 90))")
saup_rast   <- raster(ncol=720, nrow=360)
saup_rast[] <- saup_cells

## some overlap between these fao region polygons:
plot(ohi_regions[ohi_regions@data$rgn_id %in% c(260,262),])
plot(saup_rast, add=TRUE)
plot(ohi_regions[ohi_regions@data$rgn_id %in% c(260),], add=TRUE)
plot(ohi_regions[ohi_regions@data$rgn_id %in% c(262),], add=TRUE)

plot(saup_rast)
plot(ohi_regions[ohi_regions@data$rgn_id==232,], add=TRUE)

ohi_to_saup_raster        <- raster::extract(saup_rast, ohi_regions, weights = TRUE, normalizeWeights = FALSE, progress = 'text') 
names(ohi_to_saup_raster) <- paste(ohi_regions@data$ant_typ, ohi_regions@data$ant_id, sep="_") 
region_prop_df            <- plyr::ldply(ohi_to_saup_raster, rbind)

region_prop_df <- region_prop_df %>%
  separate(.id, c('rgn_typ', 'rgn_id'), sep = '_') %>%
  rename(saup_cell_id = value, 
         proportionArea = weight)

## save all the cell associations
write.csv(region_prop_df, file.path(dir_M, 
                "git-annex/globalprep/fis/v2015/raw/saup_rasters_to_ohi_rgns.csv"), row.names=FALSE)


## save only the water cells (this will probably be the file we actually need)
region_prop_df <- region_prop_df %>%
  filter(rgn_typ %in% c("eez", 'eez-ccamlr', 'eez-disputed', 'eez-inland', 'fao'))

write.csv(region_prop_df, file.path(dir_M, 
                  "git-annex/globalprep/fis/v2015/raw/saup_rasters_to_ohi_rgns_noLand.csv"), row.names=FALSE)

########################################
#### GET ID for FAO regions as well!
########################################

fao_regions <- readOGR(dsn = file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/orig/FAO_AREAS"), layer="FAO_AREAS")  
fao_regions <- fao_regions[fao_regions$F_LEVEL == "MAJOR", ]
plot(fao_regions)

saup_cells <- getcells("POLYGON ((-180 90,-180 -90, 180 -90, 180 90, -180 90))")
saup_rast <- raster(ncol=720, nrow=360)
saup_rast[] <- saup_cells

plot(saup_rast)
plot(fao_regions, add=TRUE)

## not going to worry about weights in this case, error should be marginal due to large size of FAO regions
## This will also make combining with the ohi-region data far less confusing
fao_to_saup_raster <- raster::extract(saup_rast, fao_regions, weights = TRUE, normalizeWeights = FALSE, progress = 'text') 
names(fao_to_saup_raster) <- paste(fao_regions@data$OCEAN, fao_regions@data$F_CODE, sep="_") 
region_df <- plyr::ldply(fao_to_saup_raster, rbind)

region_df <- region_df %>%
  separate(.id, c('ocean', 'fao_id'), sep = '_') %>%
  rename(saup_cell_id = value)

## not going to worry about weights in this case, error should be marginal due to large size of FAO regions
## This will also make combining with the ohi-region data far less confusing
region_df <- region_df %>%
  group_by(saup_cell_id) %>%
  mutate(is.max = ifelse(weight == max(weight), "max", NA)) %>%
  ungroup() %>%
  filter(!is.na(is.max)) %>%
  dplyr::select(saup_cell_id, ocean, fao_id) %>%
  data.frame()

# these are duplicates, must be 50% coverage between two regions, just select one fao id randomly:
dups <- fao_rgns$saup_cell_id[duplicated(fao_rgns$saup_cell_id)]
random_delete <- fao_rgns[fao_rgns$saup_cell_id %in% dups, ] %>%
  group_by(saup_cell_id) %>%
  sample_n(1)
## here is what was randomly selected for deletion:
region_df <- filter(region_df, !(saup_cell_id == 211186 & fao_id == 41))
region_df <- filter(region_df, !(saup_cell_id == 211906 & fao_id == 41))
region_df <- filter(region_df, !(saup_cell_id == 212626 & fao_id == 41))
region_df <- filter(region_df, !(saup_cell_id == 213346 & fao_id == 41))
region_df <- filter(region_df, !(saup_cell_id == 214066 & fao_id == 87))
region_df <- filter(region_df, !(saup_cell_id == 214786 & fao_id == 87))
region_df <- filter(region_df, !(saup_cell_id == 215506 & fao_id == 41))



## save all the cell associations
write.csv(region_df, file.path(dir_M, 
                                    "git-annex/globalprep/fis/v2015/raw/saup_rasters_to_fao_rgns.csv"), row.names=FALSE)

#########################################################
### combine with other data so it is all in one dataset:
ohi_rgns <- read.csv(file.path(dir_M, 
                                    "git-annex/globalprep/fis/v2015/raw/saup_rasters_to_ohi_rgns_noLand.csv"))
fao_rgns <- read.csv(file.path(dir_M, 
                               "git-annex/globalprep/fis/v2015/raw/saup_rasters_to_fao_rgns.csv"))

final <- ohi_rgns %>%
  left_join(fao_rgns, by="saup_cell_id")
write.csv(final, file.path(dir_M, 
                                    "git-annex/globalprep/fis/v2015/raw/saup_rasters_to_ohi_fao_rgns_noLand.csv"), row.names=FALSE)

