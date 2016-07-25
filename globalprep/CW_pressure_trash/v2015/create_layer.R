# Create marine plastics layer
#
# Jamie Afflerbach
#
# Used for Clean Waters in OHI 2015


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

#set working directory on neptune
data_wd = file.path(dir_N,'git-annex/globalprep/CW_pressure_trash')

setwd(data_wd)

#bring in ocean raster to clip out land
ocean = raster(file.path(dir_N,'model/GL-NCEAS-Halpern2008/tmp/ocean.tif'))


# plastics data
count  = list.files(path='v2015/globalplastic_wd_cd_rasters_180',pattern='count_*',full.names=T)
weight = list.files(path='v2015/globalplastic_wd_cd_rasters_180',pattern='weight_*',full.names=T)

# There are 3 extra weight files from the data source. For sizes 2-4, there is a file with a '2' at the end of it. These are the rasters
# that work, while the other three do not. As an example:

raster('v2015/globalplastic_wd_cd_rasters_180/weight_density_size2_180.tif') #gives an error
#but
raster('v2015/globalplastic_wd_cd_rasters_180/weight_density_size2_180_2.tif') #works! These are the ones we will use, along with weight size 1
# which appears to work

#--------------------------------------------------------------------------------------

# (1) Unlog data

#   Data came to use logged (using base 10 log) so need to 'unlog' the data first

    unlog = function(file){

  name = unlist(strsplit(file,'/','.'))[3] #split filename, grab second string to use in naming tif
  r = raster(file)
  out = 10^r
  writeRaster(out,filename=paste0('v2015/tmp/unlog/unlog_',name,sep=''),overwrite=T,format='GTiff')
  
}

    sapply(count,unlog)
    sapply(weight,unlog)

#--------------------------------------------------------------------------------------

# (2) stack all rasters within weight and count

    weight = stack(list.files('v2015/tmp/unlog','unlog_weight_*',full.names=T))
    count  = stack(list.files('v2015/tmp/unlog','unlog_count_*',full.names=T))

#--------------------------------------------------------------------------------------

# (3) sum across 4 datasets

    w_sum = calc(weight,fun=sum)
    c_sum = calc(count,fun=sum)

#--------------------------------------------------------------------------------------

# (4) reproject to mollweide

    projection(w_sum) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"    #define initial projection. Helps avoid errors when reprojecting to mollweide
    projection(c_sum) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"  
    mollCRS <- CRS('+proj=moll') #set mollweide CRS


    w_moll <- projectRaster(w_sum, crs=mollCRS,over=T,progress='text')#,filename='v2015/tmp/weight_sum_moll.tif',overwrite=T)

    c_moll <- projectRaster(c_sum, crs=mollCRS,over=T,progress='text')#,filename='v2015/tmp/count_sum_moll.tif',overwrite=T)


#--------------------------------------------------------------------------------------

# (5) Resample to 1km 

    resamp_w   <- resample(w_moll,ocean,progress='text',filename='v2015/tmp/weight_sum_moll_1km.tif',overwrite=T,method='ngb')

    resamp_c   <- resample(c_moll,ocean,progress='text',filename='v2015/tmp/count_sum_moll_1km.tif',overwrite=T,method='ngb')

#--------------------------------------------------------------------------------------

# (6) read in mollweide rasters

count_moll = raster('v2015/tmp/count_sum_moll.tif')
count_moll_1km =raster('v2015/tmp/count_sum_moll_1km.tif')
weight_moll = raster('v2015/tmp/weight_sum_moll_1km.tif')
weight_moll_1km = raster('v2015/tmp/weight_sum_moll_1km.tif')

#--------------------------------------------------------------------------------------

# (7) Mask out ocean

    count_mask = mask(count_moll_1km,ocean,progress='text',filename = 'v2015/tmp/count_sum_moll_1km_clip.tif',overwrite=T)
    weight_mask = mask(weight_moll_1km,ocean,progress='text',filename='v2015/tmp/weight_sum_moll_1km_clip.tif',overwrite=T)

    plot(count_mask,col=cols,main='Count density (pieces/km2)')
    plot(weight_mask,col=cols,main='Weight density (g/km2)')

#--------------------------------------------------------------------------------------

# (8) log transform (log(x+1))

    w_log = calc(weight_mask,fun=function(x){log(x+1)},filename='v2015/tmp/weight_sum_moll_1km_clip_log.tif',overwrite=T)
    c_log = calc(count_mask,fun=function(x){log(x+1)},filename='v2015/tmp/count_sum_moll_1km_clip_log.tif',overwrite=T)

    plot(w_log,col=cols,main='Weight density\nlog(g/km2)')
    plot(c_log,col=cols,main='Count density\nlog(pieces/km2)')

#--------------------------------------------------------------------------------------

# (9) Get 99.99th quantile

w_ref = quantile(w_log,prob=c(0.001,0.01,0.1,0.25,0.5,0.75,0.9,0.99,0.999,0.9999))
c_ref = quantile(c_log,prob=c(0.001,0.01,0.1,0.25,0.5,0.75,0.9,0.99,0.999,0.9999))

w_99 = w_ref[10]
c_99 = c_ref[10]

histogram(w_log,main='Weight density (log(g/km2))')
histogram(c_log,main='Count density (log(pieces/km2))')

#--------------------------------------------------------------------------------------

# (10) rescale to 99.99th quantile

w_rescale = calc(w_log,fun=function(x){ifelse(x>w_99,1,x/w_99)},filename='v2015/output/weight_rescale.tif',overwrite=T)

plot(w_rescale,main='Pressure layer: weight density (g/km2)',col=cols)

c_rescale = calc(c_log,fun=function(x){ifelse(x>c_99,1,x/c_99)},filename='v2015/output/count_rescale.tif',overwrite=T)
plot(c_rescale,main='Pressure layer: count density (pieces/km2)',col=cols)


#--------------------------------------------------------------------------------------

#Compare count vs weight

# This provides some general statistics and visualizations:
compare <- stack(w_rescale, c_rescale) 
pairs(compare)    

## Here is a scatter plot comparison:
raster:plot(w_rescale, c_rescale, ylab="Count", xlab="Weight", maxpixels=10000000, col=rgb(0,0,0,0.2))   

