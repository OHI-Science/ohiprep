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
library(animation)
library(plyr)
library(reshape2)
library(dplyr)
rm(list = ls())

# Description ----
## arcGIS processes:
## transformed this file: N:\model\GL-NCEAS-Regions_v2014\data\eez_ccmlar_fao_gcs
## to the coordinate systems used in the shp files located in the "Testing old data" 
## folder: n_type_rgns_pts and s_type_rgns_pts  (used these as the basemaps to ensure that
## the transformation was correct). For some annoying reason R messes up this conversion

setwd('N:\\model\\GL-AQ-SeaIce_v2013')

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

source("ObtainingData.R")

#Function 2: ----
## Using the data from the .rdata files created above calculates
## status and trend for shoreline ice and ice edge habitat.
## Data is saved in tmp folder: Habitat: n_IceEdgeHabitat.csv, s_IceEdgeHabitat.csv 
## Coastal Protection: n_IceShoreProtection.csv, s_IceShoreProtection.csv  
## UPDATE 4/18/2014: We are currently using this script to calculate only Shore protection for Antarctica,
## We are using a different technique to calculate sea ice health in Antarctica.
## Sea ice health has been cut for now for the HS regions. 

source("Status_Trend.R")

#Following is all now in qa_qc folder:
#source("Status_Trend_10to20per.R") #exploring different definitions of sea ice edge
#source("Status_Trend_15per.R") #exploring different definitions of sea ice edge
#source("Status_Trend_Aug.R")  #exploring different months

#Function 3: ----
## Calculating the km of ice per month/year/region (output is a csv file in tmp: s_AQ_YearlyMonthlyIceCover.csv).
## This is currently configured only to calculate data for CCAMLR regions.  This can be easily modified, however.

source("MonthYearExtent.R")


# Calculate health ----
extent <- read.csv("tmp\\s_AQ_YearlyMonthlyIceCover.csv")
extent$rel_anomoly <- extent$anomoly/extent$all_years_avg

regions <- unique(extent$zone)
startYear <- 1979
refYear <- 2002
refMonth <- 6
Sea_ice <- data.frame(regions=regions, status=NA, trend=NA)
for(i in 1:length(regions)){
 # i <- 1 #testing
  region <- regions[i]
  
  tmp  <- extent %.%
  filter(zone==region) %.%
  arrange(year, monthCode)

tmp$timeSeries <- 1:dim(tmp)[1]
mod <- lm(rel_anomoly ~ timeSeries, data=tmp)
mod

#p <- ggplot(tmp, aes(x=timeSeries, y=rel_anomoly))
#p + geom_point() + geom_line() + geom_abline(intercept=mod$coefficients[1], slope=mod$coefficients[2], col="red") +
#  labs(title="Region 481: Proportional anomoly for each month from 1979-2012")

refTS <- (refYear-startYear)*12 + refMonth
curTS <- (final.year - startYear)*12 + refMonth

predictAnom <- predict(mod, data.frame(timeSeries=1:408))[which(extent481$timeSeries %in% c(refTS, curTS))]
# predict all June values:
#mean(predict(mod, data.frame(timeSeries=1:408))[which(tmp$month %in% "Jun")])

sea_ice$status[i] <- 1 - abs(predictAnom[[2]]-predictAnom[[1]])
sea_ice$trend[i] <- mod$coefficients[[2]]*12*5
}



p <- ggplot(extent481, aes(x=timeSeries, y=km2))
p + geom_point() + geom_line() + geom_abline(intercept=151628.1, slope=-105.2, col="red") +
  labs(title="ice cover decreases over time")

mod <- lm(km2 ~ timeSeries, data=extent481)

# predict 1979 and 2012:
predict(mod, data.frame(timeSeries=1:408))[which(extent481$timeSeries %in% c(6, 402))]
# predict all June values:
mean(predict(mod, data.frame(timeSeries=1:408))[which(extent481$month %in% "Jun")])
1-abs((109339-150997)/130168)
1-abs((109339-150997)/109339)

extent_year <- extent %.%
  group_by(zone, year) %.%
  summarize(yearly_avg_anomoly=mean(abs(anomoly)))

extent_year_lm <- dlply(extent_year, .(zone), function(x) lm(yearly_avg_anomoly ~ year, data=x))
extent_year_predict <- ldply(extent_year_lm, function(x) predict(x, data.frame(year=c(1979, 2012))))
extent_year_predict$ratio <-extent_year_predict[,2]/extent_year_predict[,3] 


mod <- lm(yearly_avg_anomoly~year, data=subset(extent_year, zone==481))


##test###
extent_year <- extent %.%
  group_by(zone, year) %.%
  summarize(yearly_avg_anomoly=mean(abs(rel_anomoly)))

extent_year_lm <- dlply(extent_year, .(zone), function(x) lm(yearly_avg_anomoly ~ year, data=x))
extent_year_predict <- ldply(extent_year_lm, function(x) predict(x, data.frame(year=c(1979, 2012))))
extent_year_predict$ratio <-extent_year_predict[,2]/extent_year_predict[,3] 


mod <- lm(yearly_avg_anomoly~year, data=subset(extent_year, zone==481))



p <- ggplot(subset(extent, zone==481), aes(x=year, y=anomoly, group=as.numeric(monthCode), col=as.numeric(monthCode))) 
p+geom_line() + 
  labs(title="Zone=481", colour="Month")

p <- ggplot(subset(extent_year, zone==481), aes(x=year, y=total_anomoly)) 
p+geom_line() + 
  geom_abline(intercept=730623.6, slope=-346.4) +  
  labs(title="Zone=481")


# Final calculations and organization: ----
# currently only using the CCAMLR regions.

## note: should be no shoreline in fao regions
shore <- read.csv("tmp\\s_IceShoreProtection.csv")
shore_condition <- subset(shore, select=c(OHIregion_2013, pctdevR_2011))
shore_condition$habitat  <- "seaice_shoreline"
shore_condition$health  <- ifelse(shore_condition$pctdevR_2011 >1, 1, shore_condition$pctdevR_2011)
shore_condition <- subset(shore_condition, select=c(OHIregion_2013, habitat, health))
shore_condition <- rename(shore_condition, c("OHIregion_2013"="rgn_id"))
health <- rbind(extent_anomoly_condition, shore_condition)

# remove a few anomolous regions 
# (reference has less than 10 cells on average of ice and 3 or fewer ice measurements in past 5 years):
health <- health[!(health$rgn_id %in% c(270, 277, 5852) ), ]
write.csv(health, "data\\hab_seaice_health.csv", row.names=FALSE)


## extent calculations:
shore_extent <- subset(shore, select=c(OHIregion_2013, Reference_avg1979to2012monthlypixels))
shore_extent$km2 <-  shore_extent$Reference_avg1979to2012monthlypixels/12*(pixel/1000)^2
shore_extent$habitat <- "seaice_shoreline"
shore_extent <- subset(shore_extent, select=c(OHIregion_2013, habitat, km2))
shore_extent <- rename(shore_extent, c(OHIregion_2013="rgn_id"))
extent <- rbind(extent_anomoly_extent, shore_extent)

# remove a few anomolous regions:
extent <- extent[!(extent$rgn_id %in% c(270, 277, 5852) ), ]
write.csv(extent, "data\\hab_seaice_extent.csv", row.names=FALSE) 

## trend calculations:
shore_trend <- subset(shore, select=c(OHIregion_2013, Trend_2006to2011_pctdevRperyr))
shore_trend$habitat <- "seaice_shoreline"
shore_trend <- rename(shore_trend, c(Trend_2006to2011_pctdevRperyr="trend", OHIregion_2013="rgn_id"))
shore_trend <- subset(shore_trend, select=c(rgn_id, habitat, trend))
trend <- rbind(extent_anomoly_trend, shore_trend)
# remove a few anomolous regions:
trend <- trend[!(trend$rgn_id %in% c(270, 277, 5852) ), ]
#write.csv(trend, "data\\hab_seaice_trend.csv", row.names=FALSE)

# Reviewing data: ----
extent <- read.csv("data\\hab_seaice_extent.csv")
health <- read.csv("data\\hab_seaice_health.csv")
trend <- read.csv("data\\hab_seaice_trend.csv")

full <- merge(extent, health, by=c("rgn_id", "habitat"))
full <- merge(full, trend, by=c("rgn_id", "habitat"))
full_melt <- melt(full, id.vars=c("rgn_id", "habitat"))
full_melt$value <- round(full_melt$value, 2)
full_cast <- dcast(full_melt, rgn_id ~ habitat + variable, mean)
names(full_cast) <- gsub("seaice_", "", names(full_cast))

#write.csv(full_cast, "data\\seaice_summary.csv", row.names=FALSE)

############ Visualization #################################
library(RColorBrewer)
library(colorspace)
map <- readOGR(dsn="C:\\Users\\Melanie\\Desktop\\GL-NCEAS-Regions_v2014\\data", layer="eez_ccmlar_fao_gcs")
map <- map[map$rgn_type %in% c("CCAMLR", "fao"), ]
data <- read.csv("C:\\Users\\Melanie\\Desktop\\NCEAS\\Projects\\HS_Ant_SeaIce Mar112014\\data\\seaice_summary_2.csv")
map@data <- join(map@data, data, by="rgn_id")
spplot(map, c("edge_health", "shoreline_health"), col.regions=rev(diverge_hcl(100)))



#### reading a .sav file ################
# save SPSS dataset in trasport format
get file='C:\Users\Melanie\Desktop\GL-HS-AQ-SeaIce_v2013\so_latlonarea.sav'.

export outfile='C:\\Users\\Melanie\\Desktop\\GL-HS-AQ-SeaIce_v2013\\so_latlonarea.por' 

# in R 
library(foreign)
mydata <- read.spss("C:\\Users\\Melanie\\Desktop\\GL-HS-AQ-SeaIce_v2013\\so_latlonarea.sav")
# last option converts value labels to R factors
tmp <- read.
