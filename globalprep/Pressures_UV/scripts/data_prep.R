# UV data from .nc to .tif

library(ncdf4)
library(raster)
library(maps)




# set working directory

setwd(file.path(dir_N,'data_edit/git-annex/globalprep/Pressures_UV'))

#list all netcdf files
nc_files = list.files('data/uv_omi_aura_2013_2014/nc',full.names=T)

# function to turn netcdfs into tifs

nc_to_tif = function(file){

  nc = nc_open(file)
  
  yr = substr(file,50,58)
  print(yr)
  
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
  r <- raster(e,ncol=360,nrow=180) #create empty raster with e extent
  
  
  #rasterize the dataframe
  out <- rasterize(df[,1:2],r,df[,3],fun=function(x,...)mean(x),progress='text') # i had to create a mean function here for "multiple points in a cell"
  extent(out)<-c(-180,180,-90,90) #data extent 
  #out <- rotate(out) #shifts data from 0-360 to -180-180
  
  
  # Define initial projection for out
  
  crs_ja = CRS("+init=epsg:4326")
  
  projection(out) <- crs_ja#("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  
  
  writeRaster(out,filename=paste0('data/uv_omi_aura_2013_2014/tif_from_nc/OMI-Aura_L4-OMUVBd_',yr,sep=''),format='GTiff')
}

#create all the tifs
sapply(nc_files,nc_to_tif)