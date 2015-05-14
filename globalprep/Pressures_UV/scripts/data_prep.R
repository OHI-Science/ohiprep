# UV data from .nc to .tif

library(ncdf4)
library(raster)
library(maps)




# set working directory

setwd('/Volumes/data_edit/git-annex/globalprep/Pressures_UV')

nc_files = list.files('data/uv_omi_aura_2013_2014/nc',full.names=T)


nc_to_tif = function(file){

  nc = nc_open(file)
  long <- as.array(ncvar_get(nc,varid='lon'))
  nlon <- dim(long)
  
  lat <- ncvar_get(nc,varid='lat',verbose = F)
  nlat <- dim(lat)
  
  EDR <- ncvar_get(nc,varid='ErythemalDoseRate')
  fillvalue <- ncatt_get(nc,'ErythemalDoseRate', "_FillValue")
  
  EDR[EDR==fillvalue]<-NA 

  
  lonlat <- expand.grid(long, lat)
  edr.vec <- as.vector(EDR)
  length(edr.vec)
  
  
  df <- data.frame(cbind(lonlat, edr.vec))
  names(df)<-c('x','y','value')

  
  #set extent to lon/lat
  e <- extent(df[,1:2])
  r <- raster(e,ncol=359,nrow=180) #create empty raster with e extent
  
  
  #rasterize the dataframe
  out <- rasterize(df[,1:2],r,df[,3],fun=function(x,...)mean(x),progress='text') # i had to create a mean function here for "multiple points in a cell"
  extent(out)<-c(1,360,-89.5,89.5) #data extent 
  out <- rotate(out) #shifts data from 0-360 to -180-180
  
  
  # Define initial projection for out
  projection(out) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  
 

