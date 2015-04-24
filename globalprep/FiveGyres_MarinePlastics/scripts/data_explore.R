# look at marine plastics data


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
library(rgdal)
library(rasterVis)
library(RColorBrewer)

#set colors for plotting

cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme
#use my theme for levelplot
mytheme <- rasterTheme(region = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)))

data_wd = file.path(dir_N,'git-annex/globalprep/FiveGyres_MarinePlastics_CW/v2015')

setwd(data_wd)

ocean = raster(file.path(dir_N,'model/GL-NCEAS-Halpern2008/tmp/ocean.tif'))


# plastics data
count  = list.files(path='globalplastic_wd-cd_rasters',pattern='count_*',full.names=T)
weight = list.files(path='globalplastic_wd-cd_rasters',pattern='weight_*',full.names=T)


#Unlog data
# Data came to use logged (using base 10 log)
unlog = function(file){

  name = unlist(strsplit(file,'/','.'))[2] #split filename, grab second string to use in naming tif
  r = raster(file)
  r = shift(r,x=-20,y=0) #need to shift each raster, came in on a scale 20 to 380 (not 0 to 360)
  out = 10^r
  writeRaster(out,file=paste0('tmp/','unlog_',name,sep=''),overwrite=T)
  
}

sapply(count,unlog)
sapply(weight,unlog)

# stack, reproject, resample

weight = stack(list.files('tmp','unlog_weight_*',full.names=T))
count  = stack(list.files('tmp','unlog_count_*',full.names=T))


#sum

w_sum = calc(weight,fun=sum)
c_sum = calc(count,fun=sum)

#reproject

w_sum = rotate(w_sum)
c_sum = rotate(c_sum)

projection(w_sum) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"    #define initial projection. Helps avoid errors when reprojecting to mollweide
projection(c_sum) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"  
mollCRS <- CRS('+proj=moll')


w_moll <- projectRaster(w_sum, crs=mollCRS,over=T,progress='text',filename='tmp/weight_sum_moll.tif',overwrite=T)
resamp   <- resample(w_moll,ocean,progress='text',filename='tmp/weight_sum_moll_1km.tif',overwrite=T,method='ngb')

c_moll <- projectRaster(c_sum, crs=mollCRS,over=T,progress='text',filename='tmp/count_sum_moll.tif',overwrite=T)
resamp   <- resample(c_moll,ocean,progress='text',filename='tmp/count_sum_moll_1km.tif',overwrite=T,method='ngb')

# read in mollweide rasters
count_moll = raster('tmp/count_sum_moll.tif')
count_moll_1km =raster('tmp/count_sum_moll_1km.tif')
weight_moll = raster('tmp/weight_sum_moll_1km.tif')
weight_moll_1km = raster('tmp/weight_sum_moll_1km.tif')

# Mask out ocean

count_mask = mask(count_moll_1km,ocean,progress='text',filename = 'tmp/count_sum_moll_1km_clip.tif')
weight_mask = mask(weight_moll_1km,ocean,progress='text',filename='tmp/weight_sum_moll_1km_clip.tif')

#log transform (log(x+1))

w_log = calc(weight_mask,fun=function(x){log(x+1)},filename='tmp/weight_sum_moll_1km_clip_log.tif')
c_log = calc(count_mask,fun=function(x){log(x+1)},filename='tmp/count_sum_moll_1km_clip_log.tif')

#99.99th quantile

w_ref = quantile(w_sum_log,prob=c(0.001,0.01,0.1,0.25,0.5,0.75,0.9,0.99,0.999,0.9999))
c_ref = quantile(c_sum_log,prob=0.9999)

#rescale to 99.99th quantile

w_rescale = calc(w_sum_log,fun=function(x){ifelse(x>w_ref,1,x/w_ref)})

plot(w_rescale,main='Pressure layer: weight density (g/km2)',col=cols)

c_rescale = calc(c_sum_log,fun=function(x){ifelse(x>c_ref,1,x/c_ref)})
plot(c_rescale,main='Pressure layer: count density (pieces/km2)',col=cols)

