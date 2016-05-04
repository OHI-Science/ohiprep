# mapping global ocean acidification data

#JAfflerbach

#-------------------------------------
#               SETUP
#-------------------------------------

# set tmp directory
  tmpdir='~/big/R_raster_tmp'
  dir.create(tmpdir, showWarnings=F)
  rasterOptions(tmpdir=tmpdir)

# paths
  dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
            'Darwin'  = '/Volumes/data_edit',
            'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

#libraries

  library(raster)
  library(ncdf4)
  library(maps)


wd = file.path(dir_N,'git-annex/globalprep/Pressures_OceanAcidification/v2015')
setwd(wd)

# read in 1880-1889 data

      data_nc_18 = nc_open('input/cesm_co2sys_1880-1889.nc')
        print(data_nc_18)

# read in 2005-2014 data

      data_nc_20 = nc_open('input/cesm_co2sys_2005-2014.nc')
        print(data_nc_20)

# longitude values are stored in the variable 'TLONG'

      long <- ncvar_get(data_nc_18,varid='TLONG') #same lat and long for both data sets so just reading in from data_nc_18 works

# latitude values are stored in the variable 'TLAT'

      lat <- ncvar_get(data_nc_18,varid='TLAT')


# select the surface aragonite variable (OARG)

      arag_18 <- ncvar_get(data_nc_18,varid="OARG")
      arag_20 <- ncvar_get(data_nc_20,varid="OARG")


# land cells are given large, strange value, so set these to NA

      arag_18[arag_18==9969209968386869046778552952102584320]<-NA 
      arag_20[arag_20==9969209968386869046778552952102584320]<-NA 

#-----------------------------------------------------------

# Get annual averages for each year in the 2 decades

#-----------------------------------------------------------


#annual_arag is a function that calculates annual average surface aragonite saturation 

  annual_arag<-function(data,k){

for(i in seq(1,120,by=12)){
  
      print(i) # i is the first month of the year
      j  = i+11 #j is the last month of the year
  
      yr = data[,,i:j] #select the monthly layers for each year
      yr_mean = apply(yr,1:2,mean) # get the annual mean
  
      #create an array with long, lat, aragonite mean data
      A <- array(c(long,lat,yr_mean),dim=c(320,384,3))
      B <- apply(A, 3, cbind)
  
      #lon=x, lat=y
      x = B[,1]
      y = B[,2]
  
  
      C = as.data.frame(B)
      names(C)<-c('x','y','value')
  
      #set extent to lon/lat
      e <- extent(C[,1:2])
      r <- raster(e,ncol=320,nrow=384) #create empty raster with e extent
  
      #rasterize the dataframe
      out <- rasterize(C[,1:2],r,C[,3],fun=function(x,...)mean(x),progress='text') # i had to create a mean function here for "multiple points in a cell"
      extent(out)<-c(0,360,-80,90) #data extent is 0,360,-80, 90 (the -80 is not a typo)
      out <- rotate(out) #shifts data from 0-360 to -180-180
  
  
      plot(out)
      map('world',col='gray95',fill=T,border='gray80',add=T)
  
      #notice the empty cells, due to irregular grid, so plot using a different projection
  
      
      # Define initial projection for out
      projection(out) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      
  
      # define alternate projections
      mollCRS <- CRS('+proj=moll')
      #laeaCRS <- CRS('+proj=laea')
  
      # then convert to mollweide and other projections
      out_moll <- projectRaster(out, crs=mollCRS, over=T)
      #out_laea <- projectRaster(out,crs=laeaCRS)
  
  
      # plot results
      plot(out_moll)
      #plot(out_laea)
      
      print(cellStats(out_moll,stat='mean',na.rm=T))
  
      #Save rasters in LAEA projection for Arctic Options
      
  writeRaster(out_moll,paste0("working/annualmean_1880-1889/moll/global_arag_avg_moll",i,k,sep="_"), format='GTiff', overwrite=T)
  #writeRaster(out_laea,paste0("working/annualmean_2005-2014/laea/global_arag_avg_laea",i,k,sep="_"), format='GTiff', overwrite=T)
  
  #currently, the naming convention is a little wonky. The files are saved as follows:
  # For the year 1880: 'global_arag_avg_laea118th_.tif' (i = 1, k = 18th)
  # For the year 1881: 'global_arag_avg_laea1318th_.tif' (i=13, k = 18th)
  # I've been renaming these manually but suggest finding an alternate way to do this
    
  }    
}

#run the function

  annual_arag(arag_18,'18th')
  annual_arag(arag_20,'20th')


#------------------------------------------------------------

# Get the decadal mean (across all months/years)

#------------------------------------------------------------


decadal_arag<-function(data){
  
  dec_mean = apply(data, 1:2, mean) #gets the mean across all 120 data layers 
    
    #create an array with long, lat, aragonite mean data
    A <- array(c(long,lat,dec_mean),dim=c(320,384,3))
    B <- apply(A, 3, cbind)
    
    #lon=x, lat=y
    x = B[,1]
    y = B[,2]
    
    
    C = as.data.frame(B)
    names(C)<-c('x','y','value')
    
    #set extent to lon/lat
    e <- extent(C[,1:2])
    r <- raster(e,ncol=320,nrow=384) #create empty raster with e extent
    
    #rasterize the dataframe
    out <- rasterize(C[,1:2],r,C[,3],fun=function(x,...)mean(x),progress='text') # i had to create a mean function here for "multiple points in a cell"
    extent(out)<-c(0,360,-80,90) #data extent is 0,360,-8090 (the -80 is not a typo)
    out <- rotate(out) #shifts data from 0-360 to -180-180
    
    
    
    plot(out)
    map('world',col='gray95',fill=T,border='gray80',add=T)
    
    #notice the empty cells, due to irregular grid, so plot using a different projection
    
    
    # Define initial projection for out
    projection(out) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    
    
    # define alternate projections
    mollCRS <- CRS('+proj=moll')
    #laeaCRS <- CRS('+proj=laea')
    
    # then convert to mollweide and other projections
    out_moll <- projectRaster(out, crs=mollCRS, over=T)
    #out_laea <- projectRaster(out,crs=laeaCRS)
    
    
    # plot results
    plot(out_moll)
    #plot(out_laea)
 
  return(out_moll)
  
}

#get the decadal average for 1880 - 1889

  decadal_arag_18 = decadal_arag(arag_18)

  writeRaster(decadal_arag_18,filename='working/global_oa_1880_1889_arag_mean_moll',format='GTiff', overwrite=T)

#get the decadal average for 2005-2014

  decadal_arag_20 = decadal_arag(arag_20)

  writeRaster(decadal_arag_20,filename='working/global_oa_2005_2014_arag_mean_moll',format='GTiff', overwrite=T)


#-------------------------------------------------------------

# Calculate difference between 2 decadal averages

#-------------------------------------------------------------

diff = decadal_arag_18-decadal_arag_20 #although this doesn't make sense logically, this produces values that are easier to interpret for the sake of pressures. Higher values indicate increased acidification.


writeRaster(diff,filename='oa_arag_DecadalDifference_hist_minus_current_moll.tif',overwrite=T)


