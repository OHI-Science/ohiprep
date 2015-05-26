#### NOTE:  If Sea Ice (or any other habitat data) is updated
#### be sure to update the scripts in globalprep/hab_combined to 
#### merge the new data with the other habitat data!

## Sea ice: Series of scripts to obtain sea ice data
## Feb 27 2014
## Update: Mar 6 2014: removed countries with small sea ice areas
##        These areas are stochastic and are being unfairly penalized
## Update: Updating script for 2015 analysis

# relevant libraries:
library(sp)
library(raster)
library(rgdal)
#library(maptools)
library(fields) #colors in Status_Trend.R
#library(maps)
#library(gpclib)
#library(plyr)
#library(reshape2)

source("src/R/common.R")

# Description ----
## Check out PreparingSpatialFiles.R if spatial files change or to understand origination of spatial data files used here.

neptune <- file.path(dir_neptune_data, "git-annex/globalprep/NSIDC_SeaIce/v2015")
github <- "globalprep/NSIDC_SeaIce/v2015"


# final year of data:
final.year <- 2014

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
years = c(1979:final.year)  #Full range of data
months = 1:12
n.pym=length(poles)*length(years)*length(months)
i.pym = 0
t0 = Sys.time()

### NOTE: walk through the ObtainingData.R script if any of the spatial files have been modified.
### This will save the files as spatial points.


#Function 1: ----
## Collects the data for each month/year from the website
## and add to raster stack that is saved in tmp folder as: n_rasters_points.rdata  or s_rasters_points.rdata
## And, if it doesn't already exist, it converts the region shapefile into a raster points file

source(file.path(github, "ObtainingData.R"))

#Function 2: ----
## Using the data from the .rdata files created above calculates
## status and trend for shoreline ice and ice edge habitat.
## Data is saved in tmp folder: Habitat: n_IceEdgeHabitat.csv, s_IceEdgeHabitat.csv 
## Coastal Protection: n_IceShoreProtection.csv, s_IceShoreProtection.csv  
ref.years <- 1979:2000
source(file.path(github, "Status_Trend.R"))

# Final calculations and organization: ----

n_edge <- read.csv(file.path(github, "tmp/n_IceEdgeHabitat_ref1979to2000.csv"))
s_edge <- read.csv(file.path(github, "tmp/s_IceEdgeHabitat_ref1979to2000.csv"))
edge <- rbind(n_edge, s_edge)
edge  <- edge %>%
  filter(Reference_avg1979to2000monthlypixels != 0) %>%
  filter(!(rgn_id %in% c(59, 141, 219, 4, 172, 94))) %>%  #anomolous eez regions with very little ice cover
  filter(!(rgn_id %in% c("248300", "258510", "258520", "258600", "258700"))) %>% # ccamlr: cut some regions due to minimal ice (<200 km2 per year - average of months)
  filter(rgn_nam != "DISPUTED") %>%
  mutate(habitat="seaice_edge")

n_shore <- read.csv(file.path(github, "tmp/n_IceShoreProtection_ref1979to2000.csv"))
s_shore <- read.csv(file.path(github, "tmp/s_IceShoreProtection_ref1979to2000.csv"))
shore <- rbind(n_shore, s_shore)
shore <- shore %>%
filter(Reference_avg1979to2000monthlypixels != 0) %>%
  filter(!(rgn_id %in% c(59, 89, 177, 178))) %>%  #anomolous eez regions with very little ice cover
  filter(rgn_nam != "DISPUTED") %>%
  mutate(habitat="seaice_shoreline")

data <- rbind(edge, shore)
data  <- data %>%
  mutate(km2 = Reference_avg1979to2000monthlypixels/12 * (pixel/1000)^2)

write.csv(data, file.path(github, "tmp/sea_ice.csv"), row.names=FALSE)

## ice health data subset/format/save

iceHealth <- function(type ="eez", year=final.year){ 
  criteria1 <- ~rgn_typ == type
  dataYear <- paste("pctdevR", year, sep="_")
  filterdata <- data %>%
    filter_(criteria1) 
  
  filterdata <- subset(filterdata, select=c(rgn_id, habitat, get(dataYear)))
  names(filterdata) <- c('rgn_id', 'habitat', 'health')
  filterdata$health <- ifelse(filterdata$health>1, 1, filterdata$health)
  write.csv(filterdata, sprintf("globalprep/NSIDC_SeaIce/v2015/data/hab_ice_health_%s_%s.csv", type, year), row.names=FALSE)
}

## can also do fao and ccamlr by changing type (not sure if we will need this)
iceHealth(type="eez", year='2014') # 2015 analysis
iceHealth(type="eez", year='2013') # 2014 analysis
iceHealth(type="eez", year='2012') # 2013 analysis
iceHealth(type="eez", year='2011') # 2013 analysis


## ice trend data subset/format/save
iceTrend <- function(type ="eez", year=final.year){ 
  criteria1 <- ~rgn_typ == type
  dataYear <- sprintf("Trend_%sto%s_pctdevRperyr", year-4, year)
  filterdata <- data %>%
    filter_(criteria1) 
  
  filterdata <- subset(filterdata, select=c(rgn_id, habitat, get(dataYear)))
  names(filterdata) <- c('rgn_id', 'habitat', 'trend')
  filterdata$trend <- filterdata$trend*5
   filterdata$trend <- ifelse(filterdata$trend>1, 1, filterdata$trend)
   filterdata$trend <- ifelse(filterdata$trend<(-1), -1, filterdata$trend)
  write.csv(filterdata, sprintf("globalprep/NSIDC_SeaIce/v2015/data/hab_ice_trend_%s_%s.csv", type, year), row.names=FALSE)
}

## can also do fao and ccamlr by changing type (not sure if we will need this)
iceTrend(type="eez", year=2014) # 2015 analysis
iceTrend(type="eez", year=2013) # 2014 analysis
iceTrend(type="eez", year=2012) # 2013 analysis
iceTrend(type="eez", year=2011) # 2013 analysis

## ice extent data subset/format/save
iceExtent <- function(type ="eez"){ 
  criteria1 <- ~rgn_typ == type

  filterdata <- data %>%
    filter_(criteria1) 
  
  filterdata <- subset(filterdata, select=c(rgn_id, habitat, km2))
  write.csv(filterdata, sprintf("globalprep/NSIDC_SeaIce/v2015/data/hab_ice_extent_%s.csv", type), row.names=FALSE)
}

## can also do fao and ccamlr by changing type (not sure if we will need this)
iceExtent(type="eez") # all years

