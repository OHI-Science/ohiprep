# Create SST layers

library(raster)
library(RColorBrewer)
library(dplyr)

#paths

dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_N,'git-annex/globalprep/prs_sst'))

cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme

# set tmp directory

	tmpdir='~/big/R_raster_tmp'
	dir.create(tmpdir, showWarnings=F)
	rasterOptions(tmpdir=tmpdir)

# data

#ocean raster at 1km
	
ocean = raster(file.path(dir_N, 'model/GL-NCEAS-Halpern2008/tmp/ocean.tif'))

mollCRS=crs('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs')
	


# Look at all years

  l   <- list.files('v2015/tmp',pattern='annual_pos_anomalies',full.names=TRUE)
  
# Get 5 year aggregates

  yrs_1985_1989 <- stack(l[4:8])%>%sum(.) # This is the time period we are using for historical comparison
  
  
for(i in 2005:2008){ #i=2005
  print(i)
  
  yrs <- c(i,i+1,i+2,i+3,i+4)
  s   <- stack(l[substr(l,35,38)%in%yrs])%>%sum(.)
  
  diff = s - yrs_1985_1989
  
  projection(diff) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  out = projectRaster(diff,crs=mollCRS,progress='text',over=T)%>%
         resample(.,ocean,method='ngb',progress='text')%>%
          mask(.,ocean,filename=paste0('v2015/output/sst_',min(yrs),'_',max(yrs),'-1985_1989.tif',sep=""),overwrite=T)
  
  ref = quantile(out,prob=0.9999) # calculate the 99.99th quantile  
  
  sprintf('Rescaling')
  
  out_rescale = calc(out,fun=function(x){ifelse(x>0,ifelse(x>ref,1,x/ref),0)},progress='text',
                     filename=paste0('v2015/output/sst_',min(yrs),'_',max(yrs),'-1985_1989_rescaled.tif',sep=""),overwrite=T)
}

  
### creating a set of rescaled rasters that is scaled by the highest 99.99th quantile
    
  ## figure out the scaling value:
  rast_2012 <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/prs_sst/v2015/output/sst_2005_2009-1985_1989.tif'))
  quantile(rast_2012,prob=0.9999)  #118.4027
  rast_2013 <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/prs_sst/v2015/output/sst_2006_2010-1985_1989.tif'))
  quantile(rast_2013,prob=0.9999)   #133.0371 
  rast_2014 <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/prs_sst/v2015/output/sst_2007_2011-1985_1989.tif'))
  quantile(rast_2014,prob=0.9999)  #127.1995
  rast_2015 <- raster(file.path(dir_neptune_data, 'git-annex/globalprep/prs_sst/v2015/output/sst_2008_2012-1985_1989.tif'))
  quantile(rast_2015,prob=0.9999)  #130.5288
  
  # loop to rescale based on a constant reference point
  ref <- 133.0371 # maximum value across rasters
  
  for(i in 2005:2008){ #i=2005
    print(i)
    
    final_yr <- c(i+4)

  out <- raster(file.path(dir_neptune_data, 
                          sprintf('git-annex/globalprep/prs_sst/v2015/output/sst_%s_%s-1985_1989.tif', i, final_yr)))  
  out_rescale = calc(out,fun=function(x){ifelse(x>0,ifelse(x>ref,1,x/ref),0)},progress='text',
                     filename=paste0('v2015/output/sst_',i,'_',final_yr,'-1985_1989_rescaled_v2.tif',sep=""),overwrite=T)
  }