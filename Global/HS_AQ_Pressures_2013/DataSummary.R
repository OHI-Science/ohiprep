#######################################################################
### NCEAS/OHI
### This is for the pressures summary for 2013 HS/AQ regions
### Melanie Frazier March 17 2014
#######################################################################

## Downloading packages
library(rgdal)
library(raster)
library(sp)
library(maptools)
library(rgeos)
library(plotKML)
library(plyr)

setwd('/var/data/ohi/model/GL-HS-AQ-PressuresSummary_v2013')

# SST ----
# take a look at SST raster that I need to extract data from:
SST <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/sst_05_10-82_86i_mol.tif")

# take a look at region file, make a few changes:
regions <- readOGR(dsn=".", layer="sp_mol")
regions@data[regions@data$sp_type=="ccamlr",]
table(regions@data$sp_type)
table(regions@data$rgn_type)
regions <- regions[regions@data$sp_type %in% c("ccamlr", "fao", "eez"), ]
regions@data$sp_id <- as.numeric(1:dim(regions@data)[1])  
  
plot(SST)
plot(regions, add=TRUE)
################################################
## Preparing zone data
## (should only need to be done once)
################################################
#convert to raster
rasterize(regions, SST, 
          field="sp_id", 
          filename="sp_mol_raster", overwrite=TRUE, 
          progress="text")


regions_raster <- raster("sp_mol_raster")

regions_stats <- zonal(SST,  regions_raster, progress="text")

data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 
data <- subset(data, select=c(-Shape_Leng, -Shape_Area))

write.csv(data,"SST_SummaryData.csv", 
          row.names=FALSE)  


################################################
## 2013 data
################################################

#-----------------------------------------------
# Create stack of all pressure layers
#-----------------------------------------------

#setwd("\\\\neptune\\halpern2008\\mnt\\storage\\marine_threats\\impact_layers_2013_redo\\global_impact_model_2013\\averaged_by_num_ecosystems\\by_threat")
setwd("H:\\mnt\\storage\\marine_threats\\impact_layers_2013_redo\\global_impact_model_2013\\averaged_by_num_ecosystems\\by_threat")
artisanal_fishing_combo <- raster("artisanal_fishing_combo.tif")
demersal_destructive_fishing_combo <- raster("demersal_destructive_fishing_combo.tif")
demersal_nondest_high_bycatch_combo <- raster("demersal_nondest_high_bycatch_combo.tif")
demersal_nondest_low_bycatch_combo <- raster("demersal_nondest_low_bycatch_combo.tif")
inorganic_combo <- raster("inorganic_combo.tif")
invasives_combo <- raster("invasives_combo.tif")

night_lights_combo <- raster("night_lights_combo.tif")
night_lights_combo <- extend(night_lights_combo, FAO_CCAMLR_raster@extent)
extent(night_lights_combo) = extent(FAO_CCAMLR_raster) 

ocean_acidification_combo <- raster("ocean_acidification_combo.tif")

ocean_pollution_combo <- raster("ocean_pollution_combo.tif")
ocean_pollution_combo <- extend(ocean_pollution_combo, FAO_CCAMLR_raster@extent)

oil_rigs_combo <- raster("oil_rigs_combo.tif")
oil_rigs_combo <- extend(oil_rigs_combo, FAO_CCAMLR_raster@extent)
extent(oil_rigs_combo) = extent(FAO_CCAMLR_raster) 

pelagic_high_bycatch_combo <- raster("pelagic_high_bycatch_combo.tif")
pelagic_low_bycatch_combo <-  raster("pelagic_low_bycatch_combo.tif")

plumes_fert_combo <- raster("plumes_fert_combo.tif")
plumes_fert_combo <- extend(plumes_fert_combo, FAO_CCAMLR_raster@extent)

plumes_pest_combo <- raster("plumes_pest_combo.tif")
plumes_pest_combo <- extend(plumes_pest_combo, FAO_CCAMLR_raster@extent)

population_combo <- raster("population_combo.tif")
population_combo <- extend(population_combo, FAO_CCAMLR_raster@extent)
shipping_combo <- raster("shipping_combo.tif")
slr_combo <- raster("slr_combo.tif")
sst_combo <- raster("sst_combo.tif")
uv_combo <- raster("uv_combo.tif")

#global_cumulative_impact_2013_all_layers <- raster("\\\\neptune\\halpern2008\\mnt\\storage\\marine_threats\\impact_layers_2013_redo\\global_impact_model_2013\\averaged_by_num_ecosystems\\all_layers\\cumulative\\global_cumulative_impact_2013_all_layers.tif")

stack2013 <- stack(artisanal_fishing_combo,
                   demersal_destructive_fishing_combo,
                   demersal_nondest_high_bycatch_combo,
                   demersal_nondest_low_bycatch_combo,
                   inorganic_combo,
                   invasives_combo,
                   night_lights_combo,
                   ocean_acidification_combo,
                   ocean_pollution_combo,
                   oil_rigs_combo,
                   pelagic_high_bycatch_combo,
                   pelagic_low_bycatch_combo,
                   plumes_fert_combo,
                   plumes_pest_combo,
                   population_combo,
                   shipping_combo,
                   slr_combo,
                   sst_combo,
                   uv_combo)
#global_cumulative_impact_2013_all_layers)

#-----------------------------------------------
# Zonal Statistics: 2013 data
#-----------------------------------------------

FAO_CCAMLR_stats <- zonal(stack2013,  FAO_CCAMLR_raster, progress="text")

data <- merge(FAO_CCAMLR@data, FAO_CCAMLR_stats, all.y=TRUE, by.x="rgn_id", by.y="zone") 
data <- subset(data, select=c(-Shape_Leng, -Shape_Area))

write.csv(data,"N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\SummaryData.csv", 
          row.names=FALSE)  


####
wd = '/var/data/ohi/model/GL-NCEAS-SpeciesDiversity_v2013a'
setwd(file.path(wd, 'raw/AquaMaps_OHI_082013'))
spp.hdr       = read.csv('hdr_speciesoccursum.csv'    , stringsAsFactors=F, header=F)[,1]
loginfo('read in aquamaps data (tbl_*.csv)\n  cells')
cells = read.csv('tbl_hcaf.csv', header=F, na.strings='\\N')

setwd("/var/data/ohi/model/GL-NCEAS-SpeciesDiversity_v2013a/tmp")
test <- readOGR(dsn="/var/data/ohi/model/GL-NCEAS-SpeciesDiversity_v2013a/tmp", layer="land_gcs")

data <- read.csv("")
/var/data/ohi
git-annex
ingest
model
raw
stable


