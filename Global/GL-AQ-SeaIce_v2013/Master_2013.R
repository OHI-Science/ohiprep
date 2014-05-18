## Sea ice: Series of scripts to obtain sea ice data
## Mar 11 2014
## Obtaining sea ice data for Antarctica

# relevant libraries:
library(ggplot2)
require(sp)
library(raster)
library(rgdal)
library(maptools)
library(fields)
library(maps)
library(gpclib)
library(plyr)
library(reshape2)
library(dplyr)
rm(list = ls())

 # Description ----
## arcGIS processes:
## transformed this shape file: /var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data/sp_gcs
## to the poloar coordinate systems used in the these files: 
## n_type_rgns_pts and s_type_rgns_pts. 
## For some annoying reason R messes up this conversion

setwd('/var/data/ohi/model/GL-AQ-SeaIce_v2013')

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

poles = c('s')
years = c(1979:2012)  #Full range of data
months = 1:12
n.pym=length(poles)*length(years)*length(months)
i.pym = 0
t0 = Sys.time()


#Function 1: ----
## Collects the data for each month/year from the website
## and add to raster stack that is saved in tmp folder as: n_rasters_points.rdata  or s_rasters_points.rdata
## And, if it doesn't already exist, it converts the region shapefile into a raster points file

source("~/ohiprep/Global/GL-AQ-SeaIce_v2013/ObtainingData.R")

#Function 2: ----
## Using the data from the .rdata files created above calculates
## status and trend for shoreline ice and ice edge habitat.
## Data is saved in tmp folder: Habitat: n_IceEdgeHabitat.csv, s_IceEdgeHabitat.csv 
## Coastal Protection: n_IceShoreProtection.csv, s_IceShoreProtection.csv  
## UPDATE 4/18/2014: We are currently using this script to calculate only Shore protection for Antarctica,
## We are using a different technique to calculate sea ice health in Antarctica.
## Sea ice health has been cut for now for the HS regions. 

source("~/ohiprep/Global/GL-AQ-SeaIce_v2013/Status_Trend.R")

#Following is all now in qa_qc folder:
#source("Status_Trend_10to20per.R") #exploring different definitions of sea ice edge
#source("Status_Trend_15per.R") #exploring different definitions of sea ice edge
#source("Status_Trend_Aug.R")  #exploring different months

#Function 3: ----
## Calculating the km of ice per month/year/region (output is a csv file in tmp: s_AQ_YearlyMonthlyIceCover.csv).
## This is currently configured only to calculate data for CCAMLR regions.  This can be easily modified, however.
## This was ultimately not included, but may be picked back up to explore anomolies.
## See (SampleSeaIce folder for example of example exploration of these data)
#source("~/ohiprep/Global/GL-AQ-SeaIce_v2013/MonthYearExtent.R")


############### Final calculations----

### shore protection ---- 
shore <- read.csv("tmp/s_IceShoreProtection.csv")
regions <- read.csv("/var/data/ohi/git-annex/Global/NCEAS-Antarctica-Other_v2014/rgn_labels_ccamlr.csv") %.%
  select(sp_id) %.%
  left_join(shore, by="sp_id")


# health
shore_condition <- subset(regions, select=c(sp_id, pctdevR_2011))
shore_condition$habitat  <- "seaice_shoreline"
shore_condition$health  <- ifelse(shore_condition$pctdevR_2011 >1, 1, shore_condition$pctdevR_2011)
shore_condition <- subset(shore_condition, select=c(sp_id, habitat, health))

## extent
shore_extent <- subset(regions, select=c(sp_id, Reference_avg1979to2012monthlypixels))
shore_extent$km2 <-  shore_extent$Reference_avg1979to2012monthlypixels/12*(pixel/1000)^2
shore_extent$habitat <- "seaice_shoreline"
shore_extent <- subset(shore_extent, select=c(sp_id, habitat, km2))

## trend 
shore_trend <- subset(regions, select=c(sp_id, Trend_2006to2011_pctdevRperyr))
shore_trend$habitat <- "seaice_shoreline"
shore_trend <- rename(shore_trend, c(Trend_2006to2011_pctdevRperyr="trend"))
shore_trend <- subset(shore_trend, select=c(sp_id, habitat, trend))


### habitat health ---- 

habitat <- read.csv("tmp/s_IceHabitat.csv")
# some regions don't really have enough ice to make a clear assessment:
# cut some regions due to minimal ice (<200 km2 per year - average of months)
# get monthly average of cover across years:
habitat_condition <- subset(habitat, !(habitat$sp_id %in% c("248300", "258510", "258520", "258600", "258700")))

regions <- read.csv("/var/data/ohi/git-annex/Global/NCEAS-Antarctica-Other_v2014/rgn_labels_ccamlr.csv") %.%
  select(sp_id) %.%
  left_join(habitat_condition, by="sp_id")


# health
habitat_condition <- subset(regions, select=c(sp_id, pctdevR_2011))
habitat_condition$habitat  <- "seaice_extent"
habitat_condition$health  <- ifelse(habitat_condition$pctdevR_2011 >1, 1, habitat_condition$pctdevR_2011)
habitat_condition <- subset(habitat_condition, select=c(sp_id, habitat, health))

## extent
habitat_extent <- subset(regions, select=c(sp_id, Reference_avg1979to2012monthlypixels))
habitat_extent$km2 <-  habitat_extent$Reference_avg1979to2012monthlypixels/12*(pixel/1000)^2
habitat_extent$habitat <- "seaice_extent"
habitat_extent <- subset(habitat_extent, select=c(sp_id, habitat, km2))

## trend 
habitat_trend <- subset(regions, select=c(sp_id, Trend_2006to2011_pctdevRperyr))
habitat_trend$habitat <- "seaice_extent"
habitat_trend <- rename(habitat_trend, c(Trend_2006to2011_pctdevRperyr="trend"))
habitat_trend <- subset(habitat_trend, select=c(sp_id, habitat, trend))


###Merging and saving data ----
write.csv(rbind(shore_condition, habitat_condition), "data//hab_habitat_health_2013.csv", row.names=FALSE)
write.csv(rbind(shore_extent, habitat_extent), "data//hab_habitat_extent_2013.csv", row.names=FALSE)
write.csv(rbind(shore_trend, habitat_trend), "data//hab_habitat_trend_2013.csv", row.names=FALSE)

### complete data for viewing ----
full <- merge(rbind(shore_condition, habitat_condition), rbind(shore_trend, habitat_trend),
              by=c("sp_id", "habitat"))
full <- merge(full, rbind(shore_extent, habitat_extent), by=c("sp_id", "habitat"))
full_melt <- melt(full, id.vars=c("sp_id", "habitat"))
full_melt$value <- round(full_melt$value, 2)
full_cast <- dcast(full_melt, sp_id ~ habitat + variable, mean)
names(full_cast) <- gsub("seaice_", "", names(full_cast))

##write.csv(full_cast, "data//seaice_summary.csv", row.names=FALSE)




############ Visualization #################################
library(RColorBrewer)
library(colorspace)
map <- readOGR(dsn="C:\\Users\\Melanie\\Desktop\\GL-NCEAS-Regions_v2014\\data", layer="eez_ccmlar_fao_gcs")
map <- map[map$rgn_type %in% c("CCAMLR", "fao"), ]
data <- read.csv("C:\\Users\\Melanie\\Desktop\\NCEAS\\Projects\\HS_Ant_SeaIce Mar112014\\data\\seaice_summary_2.csv")
map@data <- join(map@data, data, by="rgn_id")
spplot(map, c("edge_health", "shoreline_health"), col.regions=rev(diverge_hcl(100)))


