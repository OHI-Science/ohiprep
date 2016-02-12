## Calculating total sea ice converage in a region
## Mar 27 2014
## Antarctica and HS

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
library(dplyr)

rm(list = ls())

# Description ----
## arcGIS processes:
## transformed this file: N:\model\GL-NCEAS-Regions_v2014\data\eez_ccmlar_fao_gcs
## to the coordinate systems used in the shp files located in the "Testing old data" 
## folder: n_type_rgns_pts and s_type_rgns_pts  (used these as the basemaps to ensure that
## the transformation was correct.). For some annoying reason R messes up this conversion

setwd('C:\\Users\\Melanie\\Desktop\\NCEAS\\Projects\\HS_Ant_SeaIce Mar112014')

poles = c('n','s')

for (p in poles){ 
  #p='n'#testing
  
  
  # identify the reference rasters for the north and south pole to ID regions:
  if (p=='n'){
    l='n201203'; w=641; h=709
  } else {
    l='s201209'; w=940; h=671
  }
  
  # load the .rdata file created in the above function.
  # s = raster stack of the ice data for all years/months
  # pts = points data of OHI regions, NSIDC type of land cover, and data extracted from each NSIDC ice layer
  load(file=sprintf('tmp\\%s_rasters_points.rdata',p))
  
  # using the reference rasters ("l") for the north and south pole 
  r = raster(s,l) #select the "l" (i.e., reference) raster layer from the stack
  #create a NSIDC land cover types raster (used later on):
  r.typ = setValues(r, pts@data[['type_nsidc']]) 
  #create a region raster that excludes land and coast areas (used later on):
  r.rgn = setValues(r, pts@data[['rgn_id']]) #create a new raster with the regions 
  r.rgn[r.typ < 2] = NA # exclude: land(0), coast(1) from the regions
  r.rgn[r.rgn == 0] = NA #exclude: eezs
  
  ########################################################### 
  ### Converts the (s)tack of rasters to (i)ce concentration:
  ###   - exclude land/coast (previously excluded water, but we want the FAO stats)
  ###   - divide raster by 250 to get proportion ice coverage
  ###########################################################
  # convert (s)tack of rasters to (i)ce concentration
  si = s
  
  si[is.na(r.rgn) | r.rgn==0] = NA # converting land/coast regions to NA, otherwise these have weird values
  si[si>250] = 250      # convert hole (value=251) to max ice
  si = si/250           # to get 0 to 1 ice proportion
  names(si) = names(s)
      
  
  ################################################################ 
  # sum pixels across months for each year (results in N= 1 layer for each year)  
  ################################################################
  yrs.s = as.factor(substr(names(s),2,5))  # get reference to years by column  
  si.yr = stackApply(si, yrs.s, sum)
  names(si.yr) = levels(yrs.s)
  si.yr[is.na(r.rgn) | r.rgn==0] = NA # converting land/coast regions to NA, otherwise these have weird values
  
  
  ################################################################ 
  # summarize averaged data for each year/region
  # (output is a dataframe)
  ################################################################
  z.coverage = zonal(si.yr, r.rgn, sum)
  
  write.csv(z.coverage, sprintf("YearlyIceCover_%s.csv", p), row.names=FALSE)
  
}


### Analyzing the data once it's been generated ----
cover_n <- read.csv("cover\\YearlyIceCover_n.csv")
cover_s <- read.csv("cover\\YearlyIceCover_s.csv")

cover <- rbind(cover_n, cover_s)

cover <- subset(cover, select=c(-X))
cover <- rename(cover, c(zone="rgn_id"))
cover_melt <- melt(cover, id="rgn_id")
cover_melt <- rename(cover_melt, c(variable="Year", value="MonthlySum_PercentCover"))

cover_melt$Year <- as.numeric(as.character(gsub("X", "", cover_melt$Year)))

### some plots of the data:
p <- ggplot(subset(cover_melt, rgn_id=="260"), aes(x=Year, y=MonthlySum_PercentCover))
p+geom_point(size=4)+
  labs(title="Arctic (rgn = 260)")

ggsave("Arctic FAO.png")

