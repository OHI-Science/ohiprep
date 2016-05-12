# UV Pressures Layer

#JAfflerbach

# libraries

library(raster)


# set tmp directory

tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)


# paths

dir_M             <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
                       'Darwin'  = '/Volumes/ohi',    ### connect (cmd-K) to smb://mazu/ohi
                       'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_M,'git-annex/globalprep/prs_uv'))



#bring in ocean raster to clip out land
ocean = raster(file.path(dir_M,'model/GL-NCEAS-Halpern2008/tmp/ocean.tif'))


uv_current = raster('data/uv_omi_aura_2013_2014/uv_baseline_anomaly/omi_aura_uv_anomaly_2010m01-2014m12_raw.tif')
uv_past    = raster('data/uv_omi_aura_2013_2014/uv_baseline_anomaly/toms_ep_uv_anomaly_1997m01-2001m12_raw.tif')
data       = raster('data/uv_omi_aura_2013_2014/uv_baseline_anomaly/uv_anomaly_difference_2010m01-2014m12_minus_1997m01-2001m12_raw.tif')


# Reproject to mollweide

projection(data) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"    #define initial projection. Helps avoid errors when reprojecting to mollweide
mollCRS <- CRS('+proj=moll') #set mollweide CRS

diff_moll <- projectRaster(data, crs=mollCRS,over=T,progress='text',filename='working/uv_anomaly_diff_moll.tif',overwrite=T)


#set all negative values to 0

diff_moll[diff_moll<0]<-0


# Resample to 1km 

resamp   <- resample(diff_moll,ocean,progress='text',filename='working/uv_anomaly_diff_moll_1km.tif',overwrite=T,method='ngb')


# log transform

uv_log = calc(resamp,fun=function(x){log(x+1)},filename='working/uv_anomaly_diff_moll_1km_log.tif')

#mask out land

uv_log_mask = mask(uv_log,ocean,progress='text')


# get 99.99th quantile

ref_log = quantile(uv_log_mask,prob=0.9999)


#rescale

resc_log = calc(uv_log_mask,fun=function(x){ifelse(x>ref_log,1,x/ref_log)},filename='output/uv_anomaly_diff_moll_1km_log_resc.tif',overwrite=T)






