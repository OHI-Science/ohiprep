# UV data from .nc to .tif

library(ncdf4)
library(raster)
library(maps)




# set working directory

setwd('/Volumes/data_edit/git-annex/globalprep/Pressures_UV')

nc_files = list.files('data/uv_omi_aura_2013_2014/nc',full.names=T)

od = nc_open('data/uv_omi_aura_2013_2014/nc_opendap/OMI-Aura_L3-OMUVBd_2014m0101_v003-2014m0105t093001.he5.nc')

nc_to_tif = function(file){

  nc = nc_open(file)
  long <- as.array(ncvar_get(od,varid='lon'))
  nlon <- dim(long)
  
  lat <- ncvar_get(od,varid='lat',verbose = F)
  nlat <- dim(lat)
  
  EDR <- ncvar_get(od,varid='ErythemalDoseRate')
  fillvalue <- ncatt_get(od,'ErythemalDoseRate', "_FillValue")
  
  EDR[EDR==fillvalue]<-NA 

  
  lonlat <- expand.grid(long, lat)
  edr.vec <- as.vector(EDR)
  length(edr.vec)
  
  
  df <- data.frame(cbind(lonlat, edr.vec))
  names(df)<-c('x','y','value')

  
  #set extent to lon/lat
  e <- extent(df[,1:2])
  r <- raster(e,ncol=360,nrow=180) #create empty raster with e extent
  
  
  #rasterize the dataframe
  out <- rasterize(df[,1:2],r,df[,3],fun=function(x,...)mean(x),progress='text') # i had to create a mean function here for "multiple points in a cell"
  extent(out)<-c(-180,180,-90,90) #data extent 
  #out <- rotate(out) #shifts data from 0-360 to -180-180
  
  
  # Define initial projection for out
  
  crs_ja = CRS("+init=epsg:4326")
  
  projection(out) <- crs_ja#("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  projectRaster(out,crs=crs_ja) #crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  

  p = raster('/Volumes/halpern2008_edit/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/work/uv/uv_og/uv_omi_aura_2004_2013/tif_from_nc/OMI-Aura_L3-OMUVBd_2004m1003_v003-2012m1219t083707.tif')

