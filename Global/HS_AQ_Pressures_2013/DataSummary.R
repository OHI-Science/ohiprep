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

# Region mollwide file: 
regions <- readOGR(dsn=".", layer="sp_mol")
regions@data[regions@data$sp_type=="ccamlr",]
table(regions@data$sp_type)
table(regions@data$rgn_type)
regions <- regions[regions@data$sp_type %in% c("ccamlr", "fao", "eez"), ]
regions@data$sp_id <- as.numeric(1:dim(regions@data)[1])  

# Region GCS file:
regions_gcs <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_gcs")
regions_gcs <- regions_gcs[regions_gcs@data$sp_type %in% c("ccamlr", "fao", "eez"), ]
regions_gcs@data$sp_id <- as.numeric(1:dim(regions_gcs@data)[1])  



############################
## SST ----
############################

# take a look at SST raster that I need to extract data from:
SST <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/sst_05_10-82_86i_mol.tif")
SST_oldData <- read.dbf("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/rgn_fao_mol_sst_05_10-82_86i_mol.dbf")

plot(SST)
plot(regions, add=TRUE)

#convert to raster
rasterize(regions, SST, 
          field="sp_id", 
          filename="sp_mol_raster_SST", overwrite=TRUE, 
          progress="text")


regions_raster <- raster("sp_mol_raster_SST")

regions_stats <- zonal(SST,  regions_raster, progress="text")

data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"SST_ZonalMean.csv", 
          row.names=FALSE)  
#NOTE: Need to standardize - but need to ask ben about a discrepancy between
# the model_rasters_output.csv and the data results in the final table (cc_sst_2013.csv)



############################
## ACID ----
############################
acid <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/masked_impacts_acid.tif")
#acid_oldData <- read.dbf("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/rgn_fao_mol_sst_05_10-82_86i_mol.dbf")

plot(acid)
plot(regions, add=TRUE)

#convert to raster
rasterize(regions, acid, 
          field="sp_id", 
          filename="sp_mol_raster_acid", overwrite=TRUE, 
          progress="text")


regions_ras hter <- raster("sp_mol_raster_acid")

regions_stats <- zonal(acid,  regions_raster, progress="text")

data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"acid_ZonalMean.csv", 
          row.names=FALSE)  


############################
## UV ----
############################
uv <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/impact_layers_2013_redo/impact_layers/work/uv/uv_baseline_anomaly/omi_aura_uv_anomaly_2008m01-2012m12_trans.tif")
#uv_oldData <- read.dbf("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/rgn_fao_mol_sst_05_10-82_86i_mol.dbf")

plot(uv)
plot(regions_gcs, add=TRUE)

#convert to raster
rasterize(regions_gcs, uv, 
          field="sp_id", 
          filename="sp_gcs_raster_uv", overwrite=TRUE, 
          progress="text")


regions_raster <- raster("sp_gcs_raster_uv")

regions_stats <- zonal(uv,  regions_raster, progress="text")

data <- merge(regions_gcs@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"uv_ZonalMean.csv", 
          row.names=FALSE)  

############################
## Commercial High Bycatch
############################
hb_dem <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/fp_com_hb_dem_2013_rescaled.tif")
hb_dem_nd <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/fp_com_hb_dem_nd_2013_rescaled.tif")
hb_pel <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/fp_com_hb_pel_2013_rescaled.tif")

plot(hb_dem)
plot(regions, add=TRUE)

#convert to raster
rasterize(regions, hb_dem, 
          field="sp_id", 
          filename="sp_mol_raster_ComHBC", overwrite=TRUE, 
          progress="text")


regions_raster <- raster("sp_mol_raster_ComHBC")

CommercialHBC <- stack(hb_dem, hb_dem_nd, hb_pel)

regions_stats <- zonal(CommercialHBC,  regions_raster, progress="text")

data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"CommHBC_ZonalMean.csv", 
          row.names=FALSE)  


############################
## Commercial Low Bycatch
############################
lb_dem <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/fp_com_lb_dem_nd_2013_rescaled.tif")
lb_pel <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/fp_com_lb_pel_2013_rescaled.tif")

plot(lb_dem)
plot(regions, add=TRUE)

#can use the same region raster as the high by-catch
regions_raster <- raster("sp_mol_raster_ComHBC")

CommercialHBC <- stack(lb_dem, lb_pel)

regions_stats <- zonal(CommercialHBC,  regions_raster, progress="text")

data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"CommLBC_ZonalMean.csv", 
          row.names=FALSE)  


############################
## SLR
############################
slr <- raster("/var/data/ohi/model/GL-NCEAS-Pressures_v2013a/tmp/impact_layers_2013_redo/impact_layers/work/sea_level_rise/final_products/slr_oct1992_dec2012_gcs_wgs84_normalized.tif")
slr <- rotate(slr) # scales the data to start at -180 rather than 0

plot(slr)
plot(regions_gcs, add=TRUE)
hist(slr)

#convert to raster
rasterize(regions_gcs, slr, 
          field="sp_id", 
          filename="sp_gcs_raster_slr", overwrite=TRUE, 
          progress="text")

regions_raster <- raster("sp_gcs_raster_slr")                                                                                                                                                                                                                                                                                                                                                                            

regions_stats <- zonal(slr,  regions_raster, progress="text")

data <- merge(regions@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 

write.csv(data,"slr_ZonalMean.csv", 
          row.names=FALSE)                               

test <- extract(slr, regions_gcs[25, ], fun=mean, na.rm=TRUE, weights=TRUE)
test <- extract(slr, regions_gcs[25, ], fun=mean, na.rm=TRUE, weights=TRUE)



2+2














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


