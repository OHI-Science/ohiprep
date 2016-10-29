# Creating annual averages for each year

library(dplyr)

source("~/github/ohiprep/src/R/common.R")


# set tmp directory
tmpdir=file.path(dir_N,'home_big/afflerbach/R_raster_tmp')
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)


setwd(file.path(dir_M,'git-annex/globalprep/prs_fis/v2015/VGPM_primary_productivity'))

#global ocean raster at 1km

ocean = raster(file.path(dir_M,'model/GL-NCEAS-Halpern2008/tmp/ocean.tif'))


# set mollweide projection
moll_crs = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")


for (i in 2003:2014){
  print(i)
  
  l = list.files('working/rasterized_rawdata',pattern=as.character(i),full.names=T)
  
  s = stack(l)
  
  c = calc(s,fun=function(x){mean(x,na.rm=T)},progress='text')
  
  moll = projectRaster(c,crs=moll_crs,over=T,progress='text')
  
  res = resample(moll,ocean,method='ngb',filename=paste0('output/annual_mean_npp_moll_1km',i,'.tif',sep=''),overwrite=T)
  
}




