## Sea ice: Series of scripts to obtain sea ice data
## Feb 27 2014
## Update: Mar 6 2013: removed countries with small sea ice areas
##        These areas are stochastic and are being unfairly penalized

# relevant libraries:
require(sp)
library(raster)
library(rgdal)
library(maptools)
library(fields)
library(maps)
library(gpclib)
library(animation)
library(plyr)
library(reshape2)
rm(list = ls())

# Description ----
## arcGIS processes:
## transformed this file: N:\model\GL-NCEAS-OceanRegions_v2013a\data\rgn_fao_gcs
## to the coordinate systems used in the shp files located in the "Testing old data" 
## folder: n_type_rgns_pts and s_type_rgns_pts  (used these as the basemaps to ensure that
## the transformation was correct.)

setwd('N:\\model\\GL-NCEAS-SeaIce_v2013')

# final year of data:
final.year <- 2012

# Establish: CRS, website to collect data, data selection parameters
pixel = 25000    # pixel dimension in meters for both x and y 
# epsg projection 3411 - nsidc sea ice polar stereographic north (http://spatialreference.org/ref/epsg/3411/)
# epsg projection 3412 - nsidc sea ice polar stereographic south (http://spatialreference.org/ref/epsg/3412/)
prj.n = '+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'
prj.s = '+proj=stere +lat_0=-90 +lat_ts=-70 +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'
prj.mol = '+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'

# URL base (ub), filename format for final monthly data is nt_YYYYMM_SSS_vVV_R.bin
ub.n = 'ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/north/monthly'
ub.s = 'ftp://sidads.colorado.edu/pub/DATASETS/nsidc0051_gsfc_nasateam_seaice/final-gsfc/south/monthly'

poles = c('n','s')
years = c(1979:2012)  #Full range of data
months = 1:12
n.pym=length(poles)*length(years)*length(months)
i.pym = 0
t0 = Sys.time()


#Function 1: ----
## Collects the data for each month/year from the website
## and add to raster stack that is saved in tmp folder as: n_rasters_points.rdata  or s_rasters_points.rdata
## And, if it doesn't already exist, it converts the region shapefile into a raster points file

source("ObtainingData.R")

#Function 2: ----
## Using the data from the .rdata files created above calculates
## status and trend for shoreline ice and ice edge habitat.
## Data is saved in tmp folder: Habitat: n_IceEdgeHabitat.csv, s_IceEdgeHabitat.csv 
## Coastal Protection: n_IceShoreProtection.csv, s_IceShoreProtection.csv  
source("Status_Trend.R")

# Final calculations and organization: ----

## health calculations:
n_edge <- read.csv("tmp\\n_IceEdgeHabitat.csv")
s_edge <- read.csv("tmp\\s_IceEdgeHabitat.csv")
edge <- rbind(n_edge, s_edge)
edge <- subset(edge, rgn_typ=="eez")
edge_condition <- subset(edge, select=c(OHIregion_2013, pctdevR_2011))
edge_condition$habitat  <- "seaice_edge"
edge_condition$health  <- ifelse(edge_condition$pctdevR_2011 >1, 1, edge_condition$pctdevR_2011)
edge_condition <- subset(edge_condition, select=c(OHIregion_2013, habitat, health))
edge_condition <- rename(edge_condition, c("OHIregion_2013"="rgn_id"))

n_shore <- read.csv("tmp\\n_IceShoreProtection.csv")
s_shore <- read.csv("tmp\\s_IceShoreProtection.csv")
shore <- rbind(n_shore, s_shore)
shore <- subset(shore, rgn_typ=="eez")
shore_condition <- subset(shore, select=c(OHIregion_2013, pctdevR_2011))
shore_condition$habitat  <- "seaice_shoreline"
shore_condition$health  <- ifelse(shore_condition$pctdevR_2011 >1, 1, shore_condition$pctdevR_2011)
shore_condition <- subset(shore_condition, select=c(OHIregion_2013, habitat, health))
shore_condition <- rename(shore_condition, c("OHIregion_2013"="rgn_id"))
health <- rbind(edge_condition, shore_condition)
# remove a few anomolous regions:
health <- health[!(health$rgn_id %in% c(59, 141, 219, 4, 172, 94) & 
                   health$habitat=="seaice_edge"), ]
health <- health[!(health$rgn_id %in% c(59, 89, 177, 178) & 
                     health$habitat=="seaice_shoreline"), ]

write.csv(health, "data\\hab_seaice_health.csv", row.names=FALSE)


## extent calculations:
edge_extent <- subset(edge, select=c(OHIregion_2013, Reference_avg1979to2012monthlypixels))
edge_extent$km2 <-  edge_extent$Reference_avg1979to2012monthlypixels/12*(pixel/1000)^2
edge_extent$habitat <- "seaice_edge"
edge_extent <- subset(edge_extent, select=c(OHIregion_2013, habitat, km2))
edge_extent <- rename(edge_extent, c(OHIregion_2013="rgn_id"))

shore_extent <- subset(shore, select=c(OHIregion_2013, Reference_avg1979to2012monthlypixels))
shore_extent$km2 <-  shore_extent$Reference_avg1979to2012monthlypixels/12*(pixel/1000)^2
shore_extent$habitat <- "seaice_shoreline"
shore_extent <- subset(shore_extent, select=c(OHIregion_2013, habitat, km2))
shore_extent <- rename(shore_extent, c(OHIregion_2013="rgn_id"))
extent <- rbind(edge_extent, shore_extent)

# remove a few anomolous regions:
extent <- extent[!(extent$rgn_id %in% c(59, 141, 219, 4, 172, 94) & 
                     extent$habitat=="seaice_edge"), ]
extent <- extent[!(extent$rgn_id %in% c(59, 89, 177, 178) & 
                     extent$habitat=="seaice_shoreline"), ]
write.csv(extent, "data\\hab_seaice_extent.csv", row.names=FALSE) 

## trend calculations:
edge_trend <- subset(edge, select=c(OHIregion_2013, Trend_2006to2011_pctdevRperyr))
edge_trend$habitat <- "seaice_edge"
edge_trend <- rename(edge_trend, c(Trend_2006to2011_pctdevRperyr="trend", OHIregion_2013="rgn_id"))
edge_trend <- subset(edge_trend, select=c(rgn_id, habitat, trend))

shore_trend <- subset(shore, select=c(OHIregion_2013, Trend_2006to2011_pctdevRperyr))
shore_trend$habitat <- "seaice_shoreline"
shore_trend <- rename(shore_trend, c(Trend_2006to2011_pctdevRperyr="trend", OHIregion_2013="rgn_id"))
shore_trend <- subset(shore_trend, select=c(rgn_id, habitat, trend))
trend <- rbind(edge_trend, shore_trend)
# remove a few anomolous regions:
trend <- trend[!(trend$rgn_id %in% c(59, 141, 219, 4, 172, 94) & 
                     trend$habitat=="seaice_edge"), ]
trend <- trend[!(trend$rgn_id %in% c(59, 89, 177, 178) & 
                     trend$habitat=="seaice_shoreline"), ]
write.csv(trend, "data\\hab_seaice_trend.csv", row.names=FALSE)

# Reviewing data: ----
extent <- read.csv("data\\hab_seaice_extent.csv")
health <- read.csv("data\\hab_seaice_health.csv")
trend <- read.csv("data\\hab_seaice_trend.csv")
names <- read.csv("C:\\Users\\Melanie\\Desktop\\NCEAS\\data\\rgn_2013master.csv")
names <- subset(names, select=c("rgn_id_2013", "rgn_nam_2013"))

full <- merge(extent, health, by=c("rgn_id", "habitat"))
full <- merge(full, trend, by=c("rgn_id", "habitat"))
full_melt <- melt(full, id.vars=c("rgn_id", "habitat"))
full_melt$value <- round(full_melt$value, 2)
full_cast <- dcast(full_melt, rgn_id ~ habitat + variable, mean)
names(full_cast) <- gsub("seaice_", "", names(full_cast))

full_cast <- merge(full_cast, names, by.x="rgn_id", by.y="rgn_id_2013")
full_cast <- subset(full_cast, rgn_id != 255, select=c(rgn_id, rgn_nam_2013, edge_km2, edge_health, edge_trend,
                                                       shoreline_km2, shoreline_health, shoreline_trend))
write.csv(full_cast, "data\\seaice_summary.csv", row.names=FALSE)
