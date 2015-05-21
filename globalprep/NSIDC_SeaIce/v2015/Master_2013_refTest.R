## Sea ice reference test: Previously, the reference period was 1979 to present
##  updating so the reference period will stay constant over time.
##  testing a few time periods and comparing.

# relevant libraries:
library(sp)
library(raster)
library(rgdal)
library(fields) #colors in Status_Trend.R

source("src/R/common.R")


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

#Function 2: ----
## Using the data from the .rdata files created above calculates
## status and trend for shoreline ice and ice edge habitat.
## Data is saved in tmp folder: Habitat: n_IceEdgeHabitat.csv, s_IceEdgeHabitat.csv 
## Coastal Protection: n_IceShoreProtection.csv, s_IceShoreProtection.csv  
ref.years <- 1979:2000
source(file.path(github, "Status_Trend_refTest.R"))

ref.years <- 1979:2014
source(file.path(github, "Status_Trend_refTest.R"))

ref.years <- 1979:2005
source(file.path(github, "Status_Trend_refTest.R"))

ref.years <- 1979:1995
source(file.path(github, "Status_Trend_refTest.R"))

# Comparison: ----

### STEP 1: Merge N and S poles and do a little additional formatting
refData <- c("_ref1979to2014", "_ref1979to1995", "_ref1979to2000", "_ref1979to2005")

for(refs in refData){
  
n_edge <- read.csv(file.path(github, sprintf("RefYearTest/n_IceEdgeHabitat%s.csv", refs)))
s_edge <- read.csv(file.path(github, sprintf("RefYearTest/s_IceEdgeHabitat%s.csv", refs)))
edge <- rbind(n_edge, s_edge)
edge  <- edge %>%
  filter(!(rgn_id %in% c(59, 141, 219, 4, 172, 94))) %>%  #anomolous eez regions with very little ice cover
  filter(!(rgn_id %in% c("248300", "258510", "258520", "258600", "258700"))) %>% # ccamlr: cut some regions due to minimal ice (<200 km2 per year - average of months)
  filter(rgn_nam != "DISPUTED") %>%
  mutate(habitat="seaice_edge")

n_shore <- read.csv(file.path(github, sprintf("RefYearTest/n_IceShoreProtection%s.csv", refs)))
s_shore <- read.csv(file.path(github, sprintf("RefYearTest/s_IceShoreProtection%s.csv", refs)))
shore <- rbind(n_shore, s_shore)
shore <- shore %>%
  filter(!(rgn_id %in% c(59, 89, 177, 178))) %>%  #anomolous eez regions with very little ice cover
  filter(rgn_nam != "DISPUTED") %>%
  mutate(habitat="seaice_shoreline")

data <- rbind(edge, shore)

write.csv(data, file.path(github, sprintf("RefYearTest/sea_ice%s.csv", refs)), row.names=FALSE)

}          
          
###################################################
#STEP 2: Combine the different scenarios
# set up fake dataframe

dataFrame <- read.csv(file.path(github, "RefYearTest/sea_ice_ref1979to2014.csv"))
data <- dataFrame %>%
  select(rgn_id, rgn_typ, rgn_nam, habitat)

refYears <- c("ref1979to2014", "ref1979to1995", "ref1979to2000", "ref1979to2005")

for(refs in refYears){
  #refs <- "ref1979to2014"  #testing

  tmp <- read.csv(file.path(github, sprintf("RefYearTest/sea_ice_%s.csv", refs)))

refData <- data.frame(x = tmp[, grep("Reference", names(tmp))]) #pull out the reference data and name
names(refData) <- refs

tmp <- tmp %>%  # pull out other relevant variables
  select(rgn_id, rgn_typ, rgn_nam, habitat, pctdevR_2014) %>%
  mutate(pctdevR_2014 = ifelse(pctdevR_2014 > 1, 1, pctdevR_2014))

tmp <- cbind(tmp, refData) # bind to reference values

# add reference dates to status variable
statLoc <- grep("pctdevR_2014", names(tmp))
names(tmp)[statLoc]  <- sprintf("pctdevR_2014_%s", refs)          

data <- left_join(data, tmp)
}          

data <- data %>%
  filter(ref1979to2014 >0 ) 
  
#write.csv(data, file.path(github, "RefYearTest/summaryData.csv"), row.names=FALSE)


##### Plot the data ----
library(ggplot2)          
 
data <- read.csv(file.path(github, "RefYearTest/summaryData.csv"))          


ggplot(data, aes(x= pctdevR_2014_ref1979to2014, y= pctdevR_2014_ref1979to2000, color=habitat)) +
  geom_point(shape=19) + 
  geom_abline(slope=1, intercept=0, color="red") +
  theme_bw()

ggplot(data, aes(x= pctdevR_2014_ref1979to2014, y= pctdevR_2014_ref1979to1995, color=habitat)) +
  geom_point(shape=19) + 
  geom_abline(slope=1, intercept=0, color="red") +
  theme_bw()

ggplot(data, aes(x= pctdevR_2014_ref1979to2014, y= pctdevR_2014_ref1979to2005, color=habitat)) +
  geom_point(shape=19) + 
  geom_abline(slope=1, intercept=0, color="red") +
  theme_bw()









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

