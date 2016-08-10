# Create the fishing pressure layers


#-----------------------------------------------------------------------------
#SETUP 
#-----------------------------------------------------------------------------

rm(list=ls())

library(raster)
library(dplyr)
library(RColorBrewer)
library(rgdal)
library(ggplot2)

cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme



dir_M             <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
                       'Darwin'  = '/Volumes/ohi',    ### connect (cmd-K) to smb://mazu/ohi
                       'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_M,'git-annex/globalprep/prs_fish'))


# set tmp directory
tmpdir=file.path(dir_M,'home_big/afflerbach/R_raster_tmp')
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)

# set mollweide projection
moll_crs = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")


ocean = raster(file.path(dir_M, 'model/GL-NCEAS-Halpern2008/tmp/ocean.tif'))
#-----------------------------------------------------------------------------
#READ IN DATA
#-----------------------------------------------------------------------------

# Fish catch at 1km

catch_06_10 = raster('v2015/catch_km_06_10.tif')
catch_05_09 = raster('v2015/catch_km_05_09.tif')
catch_04_08 = raster('v2015/catch_km_04_08.tif')
catch_03_07 = raster('v2015/catch_km_03_07.tif')

#Primary productivity at 1km - aggregate to time periods

npp = list.files(file.path(dir_M,'git-annex/globalprep/VGPM_primary_productivity/v_2015/output'),pattern='annual_mean_npp_2',full.names=T)

npp_06_10 = calc(stack(npp[substr(npp,113,116) %in% 2006:2010]),fun=function(x){mean(x,na.rm=T)},progress='text')%>%
             projectRaster(.,crs=moll_crs,progress='text',over=T)%>%
              resample(.,ocean,method='ngb',filename='v2015/npp/npp_06_10.tif')

npp_06_10 = raster('v2015/npp/npp_06_10.tif')

npp_05_09 = calc(stack(npp[substr(npp,113,116) %in% 2005:2009]),fun=function(x){mean(x,na.rm=T)},progress='text')%>%
               projectRaster(.,crs=moll_crs,progress='text',over=T)%>%
                resample(.,ocean,method='ngb',filename='v2015/npp/npp_05_09.tif')

npp_05_09 = raster('v2015/npp/npp_05_09.tif')

npp_04_08 = calc(stack(npp[substr(npp,113,116) %in% 2004:2008]),fun=function(x){mean(x,na.rm=T)},progress='text')%>%
             projectRaster(.,crs=moll_crs,progress='text',over=T)%>%
              resample(.,ocean,method='ngb',filename='v2015/npp/npp_04_08.tif')

npp_04_08 = raster('v2015/npp/npp_04_08.tif')

npp_03_07 = calc(stack(npp[substr(npp,113,116) %in% 2003:2007]),fun=function(x){mean(x,na.rm=T)},progress='text')%>%
             projectRaster(.,crs=moll_crs,progress='text',over=T)%>%
              resample(.,ocean,method='ngb',filename='v2015/npp/npp_03_07.tif')

npp_03_07 = raster('v2015/npp/npp_03_07.tif')

  
# gear proportions at 1 km

gear_hb = raster('v2015/gear_prop_hb_moll_1km_ocean.tif')
gear_lb = raster('v2015/gear_prop_lb_moll_1km_ocean.tif')

#---------------------------------------------------------------------------

# Catch standardized by primary productivity

catch_npp_06_10 = overlay(catch_06_10,npp_06_10,fun=function(x,y){x/y},progress='text',filename='v2015/catch_npp_06_10.tif')
catch_npp_05_09 = overlay(catch_05_09,npp_05_09,fun=function(x,y){x/y},progress='text',filename='v2015/catch_npp_05_09.tif')
catch_npp_04_08 = overlay(catch_04_08,npp_04_08,fun=function(x,y){x/y},progress='text',filename='v2015/catch_npp_04_08.tif')
catch_npp_03_07 = overlay(catch_03_07,npp_03_07,fun=function(x,y){x/y},progress='text',filename='v2015/catch_npp_03_07.tif')
#----------------------------------------------------------------------------
# Divide catch by primary productivity then multiply by gear proportions


out_06_10_hb = overlay(catch_npp_06_10,gear_hb,fun=function(x,y){x*y},progress='text',
                       filename='v2015/output/catch_06_10_npp_hb_raw.tif',overwrite=T)
out_06_10_lb = overlay(catch_npp_06_10,gear_lb,fun=function(x,y){x*y},progress='text',
                       filename='v2015/output/catch_06_10_npp_lb_raw.tif',overwrite=T)

out_05_09_hb = overlay(catch_npp_05_09,gear_hb,fun=function(x,y){x*y},progress='text',
                       filename='v2015/output/catch_05_09_npp_hb_raw.tif',overwrite=T)
out_05_09_lb = overlay(catch_npp_05_09,gear_lb,fun=function(x,y){x*y},progress='text',
                       filename='v2015/output/catch_05_09_npp_lb_raw.tif',overwrite=T)

out_04_08_hb = overlay(catch_npp_04_08,gear_hb,fun=function(x,y){x*y},progress='text',
                       filename='v2015/output/catch_04_08_npp_hb_raw.tif',overwrite=T)
out_04_08_lb = overlay(catch_npp_04_08,gear_lb,fun=function(x,y){x*y},progress='text',
                       filename='v2015/output/catch_04_08_npp_lb_raw.tif',overwrite=T)

out_03_07_hb = overlay(catch_npp_03_07,gear_hb,fun=function(x,y){x*y},progress='text',
                       filename='v2015/output/catch_03_07_npp_hb_raw.tif',overwrite=T)
out_03_07_lb = overlay(catch_npp_03_07,gear_lb,fun=function(x,y){x*y},progress='text',
                       filename='v2015/output/catch_03_07_npp_lb_raw.tif',overwrite=T)

#----------------------------------------------------------------------------

# Rescale using 99.99 quantile

fishing.pressures = list.files("v2015/output",full.names=T)

for (i in 1:length(fishing.pressures)){
  
  print(i)
  
  r    = raster(fishing.pressures[i])
  yrs  = substr(names(r),7,11)
  gear = substr(names(r),17,18)
  
  # look at logged data
  
  r_log = calc(r,fun=function(x){log(x+1)},progress='text')

  ref = quantile(r_log,prob=0.9999)
  
  resc_log =  calc(r_log,fun=function(x){ifelse(x>ref,1,x/ref)},progress='text')
  
  writeRaster(resc_log,filename=paste0('v2015/output/catch_',yrs,'_npp_',gear,'_rescaled.tif'),overwrite=T)
  
}
