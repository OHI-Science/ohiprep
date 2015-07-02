# Create SST layers

library(raster)
library(RColorBrewer)
library(dplyr)

#paths

dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_N,'git-annex/globalprep/Pressures_SST'))

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
  
  
for(i in 2005:2008){
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
