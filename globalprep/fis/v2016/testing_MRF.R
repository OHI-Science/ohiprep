source('src/R/common.R')

install.packages("devtools")
devtools::install_github("SeaAroundUs/rseaaroundus")

library(reshape2)
library(dplyr)
library(seaaroundus)
library(rgdal)
library(raster)

source('src/R/common.R')

ohi_regions <- readOGR(dsn = file.path(dir_M, "git-annex/globalprep/spatial/v2015/data"), layer="regions_gcs")  
inland <- ohi_regions[ohi_regions@data$rgn_typ == "eez-inland",]
plot(inland)

saup_cells <- getcells("POLYGON ((-180 90,-180 -90, 180 -90, 180 90, -180 90))")
saup_rast <- raster(ncol=720, nrow=360)
saup_rast[] <- saup_cells

plot(saup_rast)
plot(ohi_regions, add=TRUE)

ohi_to_saup_raster <- raster::extract(saup_rast, ohi_regions, weights = TRUE, normalizeWeights = FALSE, progress = 'text') 
names(ohi_to_saup_raster) <- paste(ohi_regions@data$ant_typ, ohi_regions@data$ant_id, sep="_") 
region_prop_df     <- plyr::ldply(ohi_to_saup_raster, rbind)

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


data <- getcelldata(year=2010, 89568)

