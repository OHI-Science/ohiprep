# Getting global OA values 

# Jamie Afflerbach

#-----------------------------------------

# This script takes prepped Ocean Acidification input raster layers (created by oa_dataprep.R) and does the following:

#     1. Calculates the historical global mean for the decade 1880-1889 (1 raster layer as output)
#     2. Calculates the annual mean for each of the 10 years in 2005-2014 (10 raster layers as output)
#     3. Takes each of the 10 raster layers produced in step 2, and subtract the historical global mean (produced in step 1) to create 10 new raster layers (one for each year) with values equal to the change in aragonite saturation state
#     4. All values that are 0 and below are set to 0
#     5. Finds the maximum value across all 10 raster layers produced in step 3
#     6. Multiply this maximum value by 110%
#     7. Divide all raster layers produced in step 3 by value produced in step 6
#     8. Interpolates values to the coast (for all 10 raster layers)
#     9. Resamples to 1km for the final output raster layer (for all 10 raster layers)

# Steps 1 and 2 were done in 'oa_dataprep.R'

# NOTE: Interpolation was done in ArcGIS

# Since the input data does not have a consistent 'square' cell resolution, interpolation was having difficulty in ArcGIS. The noninterpolated raster layers
# were resampled to square cells (~ half degree) in ArcGIS in step 

#------------------------------------------


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


wd = file.path(dir_N,'git-annex/globalprep/Pressures_acid/v2015')
setwd(wd)



# read in data

    hist_mean = raster('working/global_oa_1880_1889_arag_mean_moll.tif') # historical decadal mean of aragonite saturation state from 1880-1889

    files = list.files('working/annualmean_2005-2014/moll',full.names=TRUE,recursive=TRUE) # list the annual mean raster files for each year in 2005-2014

#-------------------------------------------------------------------------------------------------

# (Step 3): function that subtracts annual mean from historical decadal mean and outputs raster to specified folder

    annual_change = function(file){
  yr = substr(file,56,59)
  out = hist_mean - raster(file) #subtract current from historical. Although this is counterintuitive, it results in the correct scaling of values (higher values = more acidification)
  writeRaster(out,filename=paste0('working/annualchange_2005-2014/difference_from_hist_mean_',yr,sep=""),format='GTiff',overwrite=T)
}


    sapply(files,annual_change) # apply function across all files using sapply


    newfiles = list.files('working/annualchange_2005-2014',full.names=TRUE,recursive=TRUE) #list the new raster files (difference from historical mean)

#-------------------------------------------------------------------------------------------------

# (Step 4): reclassify all values less than 0 to 0

    zero = function(file){
  yr = substr(file,58,61)
  r  = raster(file)
  sub = reclassify(r,c(-Inf,0,0))
  writeRaster(sub,filename=paste0('working/annualchange_reclassify/annualchange_reclass_',yr,sep=""),format='GTiff',overwrite=T)
}

    sapply(newfiles,zero) #apply to all newfiles


#-------------------------------------------------------------------------------------------------


# (Step 5): find max value across all reclassified rasters

    allfiles = list.files('working/annualchange_reclassify',full.names=TRUE,recursive=TRUE)

    s <- stack(allfiles) #put all 10 rasters into a stack
    m = max(cellStats(s,stat='max')) #get the max value across all 10 layers


#-------------------------------------------------------------------------------------------------

# (Step 6): Multiply max by 110% to create buffered reference point

    max = m*1.1


#-------------------------------------------------------------------------------------------------

# (Step 7): Rescale rasterized values by dividing all reclassified rasters by max

    rescale = function(file){
  yr = substr(file,54,57)
  r = raster(file)
  out = calc(r,fun=function(x){x/max})
  writeRaster(out,file=paste0('working/annual_oa_rescaled_nonint_1deg/annual_oa_rescaled_nonint_1deg_',yr,sep=''),format='GTiff',overwrite=T)
}

    sapply(allfiles,rescale)

    files = list.files('working/annual_oa_rescaled_nonint_1deg',full.names=TRUE,recursive=TRUE)


#-------------------------------------------------------------------------------------------------


# (Step 8): Interpolate to coast. This was done manually in ArcGIS (Jamie Afflerbach)

#     Interpolation will be done in ArcGIS. In order to accurately turn rasters to points (to then be interpolated back to raster) the input raster needs to have square cell sizes.
#     All files in 'annual_oa_rescaled_nonint_1deg' have rectangular cells that do not have the same height and width. Within Arc, a template raster that splits these
#     rectangles into squares was created, labeled 't' below. Using this, resample all 10 years to half-degree (these aren't exactly half degree but close).

    t <-raster('working/annual_oa_rescaled_nonint_halfdeg/oa_halfdeg.tif') #use this layer created in arc as the structure for all raster layers to be resampled to

    resample = function(file){
  yr  = substr(file,71,74)
  r   = raster(file)
  out = raster::resample(r,t)
  writeRaster(out,file=paste0('working/annual_oa_rescaled_nonint_halfdeg/annual_oa_rescaled_nonint_halfdeg_',yr,sep=''),format='GTiff',overwrite=T)
}

    sapply(files,resample)


# interpolation of the half-degree raster layers was done in ArcGIS using the natural neighbor method. 
# The outputs for each year are saved in 'annual_oa_rescaled_int_halfdeg', and are used below.


    ras_files = list.files('working/annual_oa_rescaled_int_halfdeg',pattern='[.]tif$',full.names=TRUE,recursive=TRUE)



#-------------------------------------------------------------------------------------------------


# (Step 9) Resample these interpolated, half-degree rasters to 1 km

    # create dummy 1 km raster layer in ArcGIS to use as template in function below
    samp = raster('working/annual_oa_1km/oa_1km.tif')


    resample_1km = function(file){
  yr  = substr(file,43,46)
  r   = raster(file)
  print(yr)
  out = raster::resample(r,samp,method='ngb',progress='text')
  writeRaster(out,file=paste0('working/annual_oa_1km/oa_1km_',yr,sep=''),format='GTiff',overwrite=T)
}

    sapply(ras_files,resample_1km)
