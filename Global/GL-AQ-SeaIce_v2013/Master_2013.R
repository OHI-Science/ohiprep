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

source("~/ohiprep/Global/GL-AQ-SeaIce_v2013/MonthYearExtent.R")


############### Final calculations----

### shore protection ---- 

# health
shore <- read.csv("tmp/s_IceShoreProtection.csv")
shore_condition <- subset(shore, select=c(sp_id, pctdevR_2011))
shore_condition$habitat  <- "seaice_shoreline"
shore_condition$health  <- ifelse(shore_condition$pctdevR_2011 >1, 1, shore_condition$pctdevR_2011)
shore_condition <- subset(shore_condition, select=c(sp_id, habitat, health))

## extent
shore_extent <- subset(shore, select=c(sp_id, Reference_avg1979to2012monthlypixels))
shore_extent$km2 <-  shore_extent$Reference_avg1979to2012monthlypixels/12*(pixel/1000)^2
shore_extent$habitat <- "seaice_shoreline"
shore_extent <- subset(shore_extent, select=c(sp_id, habitat, km2))
extent <- rbind(extent_anomoly_extent, shore_extent)

## trend 
shore_trend <- subset(shore, select=c(sp_id, Trend_2006to2011_pctdevRperyr))
shore_trend$habitat <- "seaice_shoreline"
shore_trend <- rename(shore_trend, c(Trend_2006to2011_pctdevRperyr="trend"))
shore_trend <- subset(shore_trend, select=c(sp_id, habitat, trend))


#### anomoly ----

extent <- read.csv("tmp/s_AQ_YearlyMonthlyIceCover.csv")

# some regions don't really have enough ice to make a clear assessment:
# cut some regions due to minimal ice (<200 km2 per year - average of months)
# get monthly average of cover across years:
year_avg <- extent %.%
  group_by(zone) %.%
  summarise(yearly_avg = mean(km2, na.rm=TRUE))
extent <- subset(extent, !(zone %in% c("248300", "258510", "258520", "258600", "258700")))

# get monthly average of cover across years:
month_avg <- extent %.%
  group_by(zone, monthCode, month) %.%
  summarise(all_years_monthly_avg = mean(km2, na.rm=TRUE))

# merge the month averages with data,
# calculate anomoly, 
# calculate relative anomoly for the "adjustment"
extent  <- extent %.%
  left_join(month_avg, by=c("zone", "monthCode", "month")) %.%
  mutate(anomoly = km2-all_years_monthly_avg) %.% 
  mutate(rel_anomoly = anomoly/all_years_monthly_avg)  ## This calculates the "anomaly_i/cover_i" portion of the adjustment. 

adjustment  <-  extent %.%
  group_by(zone) %.%
  summarise(adj=mean(abs(rel_anomoly), na.rm=TRUE))

startYear <- 1979
refYear <- 1979
refMonth <- 6
sea_ice <- expand.grid(regions=unique(extent$zone), year=c((final.year-5):final.year), status=NA)

for(i in 1:length(sea_ice$regions)){
#  i <- 8 #testing

  region <- sea_ice[i,1]
  year <- sea_ice[i,2]  
  tmp  <- extent %.%
  filter(zone==region) %.%
  arrange(year, monthCode) %.%
  mutate(timeSeries = 1:length(year))

mod <- lm(anomoly ~ timeSeries, data=tmp)
mod

p <- ggplot(tmp, aes(x=timeSeries, y=anomoly))
p + geom_point() + geom_line() + geom_abline(intercept=mod$coefficients[1], slope=mod$coefficients[2], col="red") +
  labs(title=sprintf("Region %s: Proportional anomoly for each month from 1979-2012", region))

refTS <- (refYear - startYear)*12 + refMonth
curTS <- (year - refYear)*12 + refMonth

predictAnom <- predict(mod, data.frame(timeSeries=1:length(tmp$year)))[which(tmp$timeSeries %in% c(refTS, curTS))]

delta <-(predictAnom[[2]] - predictAnom[[1]])/predictAnom[[1]]   
sea_ice$status[i] <- 1 - abs(delta) * adjustment$adj[adjustment$zone == region]
}

write.csv(sea_ice, "sea_ice_test_4_27_2014.csv", row.names=FALSE)
############ Visualization #################################
library(RColorBrewer)
library(colorspace)
map <- readOGR(dsn="C:\\Users\\Melanie\\Desktop\\GL-NCEAS-Regions_v2014\\data", layer="eez_ccmlar_fao_gcs")
map <- map[map$rgn_type %in% c("CCAMLR", "fao"), ]
data <- read.csv("C:\\Users\\Melanie\\Desktop\\NCEAS\\Projects\\HS_Ant_SeaIce Mar112014\\data\\seaice_summary_2.csv")
map@data <- join(map@data, data, by="rgn_id")
spplot(map, c("edge_health", "shoreline_health"), col.regions=rev(diverge_hcl(100)))


