## Calculating total sea ice coverage in a region
## Mar 27 2014
library(dplyr)

# Description ----
## arcGIS processes:
## transformed this file: N:\model\GL-NCEAS-Regions_v2014\data\eez_ccmlar_fao_gcs
## to the coordinate systems used in the shp files located in the "Testing old data" 
## folder: n_type_rgns_pts and s_type_rgns_pts  (used these as the basemaps to ensure that
## the transformation was correct.). For some annoying reason R messes up this conversion

for (p in poles){ 
#  p='s' 
  
  
  # identify the reference rasters for the north and south pole to ID regions:
  if (p=='n'){
    l='n201203'; w=641; h=709
  } else {
    l='s201209'; w=940; h=671
  }
  
  # load the .rdata file created in the above function.
  # s = raster stack of the ice data for all years/months
  # pts = points data of OHI regions, NSIDC type of land cover, and data extracted from each NSIDC ice layer
  load(file=sprintf('tmp/%s_rasters_points.rdata',p))
  
  # using the reference rasters ("l") for the north and south pole 
  r = raster(s,l) #select the "l" (i.e., reference) raster layer from the stack
  #create a NSIDC land cover types raster (used later on):
  r.typ = setValues(r, pts@data[['type_nsidc']]) 
  #create a region raster that excludes land and coast areas (used later on):
  r.rgn = setValues(r, pts@data[['sp_id']]) #create a new raster with the regions 
  r.rgn[r.typ < 2] = NA # exclude: land(0), coast(1) from the regions
  r.rgn[r.rgn == 0] = NA #exclude: eezs
  
  ########################################################### 
  ### Converts the (s)tack of rasters to (i)ce concentration:
  ###   - exclude land/coast (previously excluded water, but we want the FAO stats)
  ###   - divide raster by 250 to get proportion ice coverage
  ###########################################################
  # convert (s)tack of rasters to (i)ce concentration
  si = s

  si[is.na(r.rgn)] = NA # converting land/coast regions to NA, otherwise these have weird values
  si[si>250] = 250      # convert hole (value=251) to max ice
  si = si/250           # to get 0 to 1 ice proportion
  names(si) = names(s)
      
   
  ################################################################ 
  # summarize averaged data for each year/region
  # (output is a dataframe)
  ################################################################
z.coverage <- zonal(si, r.rgn, sum, progress="text")
z.coverage <- data.frame(z.coverage)
z.coverage <- melt(z.coverage, id=c("zone"))  
z.coverage$year <- substr(z.coverage$variable, 2, 5)
z.coverage$monthCode <- substr(z.coverage$variable, 6, 7)
z.coverage$month <- z.coverage$monthCode
z.coverage$month <- revalue(z.coverage$month, c("01"="Jan", "02"="Feb", "03"="Mar", "04"="Apr", "05"="May", "06"="Jun", "07"="Jul", "08"="Aug", "09"="Sep", "10"="Oct", "11"="Nov", "12"="Dec"))
z.coverage$monthCode <- as.numeric(as.character(z.coverage$monthCode))

# convert value to km2
z.coverage$km2 <-  z.coverage$value*(pixel/1000)^2

# select relevant variables
z.coverage <- subset(z.coverage, select=c("zone", "year", "monthCode", "month", "km2"))

write.csv(z.coverage, sprintf("tmp/%s_AQ_YearlyMonthlyIceCover.csv", p), row.names=FALSE)
  
}


# p <- ggplot(subset(z.coverage, year==1979), aes(x=as.factor(monthCode), y=km2, group=zone, col=as.factor(zone))) 
# p+geom_line()+
#   labs(title="Year=1979", y="Ice extent, km2", x="Month", colour="CCAMLR")
# ggsave("IceExtent_yr1979.png")
# 
# p <- ggplot(subset(z.coverage, zone==485), aes(x=year, y=anomoly, group=as.numeric(monthCode), col=as.numeric(monthCode))) 
# p+geom_line() + 
#   labs(title="Zone=485", colour="Month")
# p <- ggplot(subset(z.coverage, zone==485), aes(x=year, y=km2, group=as.numeric(monthCode), col=as.numeric(monthCode))) 
# p+geom_line() 
# 
# 
#  p <- ggplot(subset(z.coverage, zone==484), aes(x=year, y=anomoly, group=as.numeric(monthCode), col=as.numeric(monthCode))) 
# p+geom_line() + 
#   labs(title="Zone=484", colour="Month")
# p <- ggplot(subset(z.coverage, zone==484), aes(x=year, y=km2, group=as.numeric(monthCode), col=as.numeric(monthCode))) 
# p+geom_line() 
# 
# 
# 
# p <- ggplot(subset(z.coverage, zone==486), aes(x=year, y=anomoly, group=as.numeric(monthCode), col=as.numeric(monthCode))) 
# p+geom_line() + 
#   labs(title="Zone=486", colour="Month")
# p <- ggplot(subset(z.coverage, zone==486), aes(x=year, y=km2, group=as.numeric(monthCode), col=as.numeric(monthCode))) 
# p+geom_line() 
# 
# 
