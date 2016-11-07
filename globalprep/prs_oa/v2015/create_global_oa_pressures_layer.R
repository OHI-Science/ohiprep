# Creating global ocean acidification pressure layers

# Jamie Afflerbach

# This is the same code as in 'oa_methods_2015.Rmd'. Keeping both this script and the Rmd in this folder for now. In the future for 
#OHI we will only use .Rmds as scripts for dataprep/layer creation
#-----------------------------------------

#   'oa_dataprep.R' created the following:
#     a. Calculates the historical global mean for the decade 1880-1889 (1 raster layer as output)
#     b. Calculates the annual mean for each of the 10 years in 2005-2014 (10 raster layers as output)

# This script takes prepped Ocean Acidification input raster layers (created by oa_dataprep.R) and does the following:

#     1. Takes each of the 10 raster layers produced in (b) above, and subtracts the historical global mean (produced in step 1) 
#        to create 10 new raster layers (one for each year) with values equal to the change in aragonite saturation state
#     2. RESCALE: For each year between 2005 and 2014, look at the mean annual aragonite saturation state rasters (annualmean_2005-2014). 
#        All values at or below the threshold (<=1) are set to 1 (highest pressure value). All cells with aragonite saturation state values >1 
#        will be scaled based on their change relative to historical levels (calculated in step 2 above). All cells that have a negative change 
#        (indicating a decrease in acidification) are assigned 0    
#     3. Resamples each raster to 1km
#     4. Using ArcGIS through arcpy in python, NA cells are interpolated using nearest neighbor to create final output raster layer


# NOTE: Interpolation was done in ArcGIS using OA_interpolation.py

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
    library(maps)


    # set working directory
    wd = file.path(dir_N,'git-annex/globalprep/Pressures_OceanAcidification/v2015')
    setwd(wd)



# read in data

    hist_mean = raster('working/global_oa_1880_1889_arag_mean_moll.tif') # historical decadal mean of aragonite saturation state from 1880-1889

    files = list.files('working/annualmean_2005-2014/moll',full.names=TRUE,recursive=TRUE) # list the annual mean raster files for each year in 2005-2014

    #ocean is a raster with all land clipped out - at 1km with value of 1
    ocean = raster(file.path(dir_N,'model/GL-NCEAS-Halpern2008/tmp/ocean.tif'))


#-------------------------------------------------------------------------------------------------

# (Step 1): function that subtracts annual mean from historical decadal mean and outputs raster to specified folder

    annual_change = function(file){
    
      yr = substr(file,56,59)         #use substr to grab the year out of the filename
      out = hist_mean - raster(file)  #subtract current from historical. Although this is counterintuitive, it results in the 
                                      #correct scaling of values (higher values = more acidification)
      writeRaster(out,filename=paste0('working/annualchange_2005-2014/difference_from_hist_mean_',yr,sep=""),format='GTiff',overwrite=T)

    }


    sapply(files,annual_change) # apply function across all files using sapply


    newfiles = list.files('working/annualchange_2005-2014',full.names=TRUE,recursive=TRUE) #list the new raster files (difference from historical mean)


#-------------------------------------------------------------------------------------------------

# Step (2): Rescale values

#     For each year between 2005 and 2014, look at the mean annual aragonite saturation state rasters (annualmean_2005-2014). All values at or below
#     the threshold (<=1) are set to 1 (highest pressure value). All cells with aragonite saturation state values >1, will be scaled based on their change
#     relative to historical levels (calculated in step 2 above). 


    rescale = function(file){
  
        yr   = substr(file,56,59) #get year of file
        mean = raster(file)       #get annual mean aragonite raster for given year
        diff = raster(newfiles[substr(newfiles,58,61)==yr])  #get the change raster for same year ((current-historical)/historical)
        mean[mean<=1]<-1    #all values at or less than 1 are given a value of 1
        mean[mean>1] = diff[mean>1]  # all cells with values greater than 1 are swapped out with their amount of change 
        mean[mean<0]<-0   #all values less than 0 (indicating a decrease in acidity) are capped at 0
        
        writeRaster(mean,filename=paste0('working/annual_oa_rescaled/oa_rescaled_',yr,sep=""),format='GTiff',overwrite=T)
        
}

  sapply(files,rescale)

  rescaled = list.files('working/annual_oa_rescaled',full.names=T)

#-------------------------------------------------------------------------------------------------

# (Step 3): Resample to 1km

    resample = function(file){
  
        yr  = substr(file,40,43)
        r   = raster(file)
        out = raster::resample(r,ocean,method='ngb',progress='text') # resample r to the resolution of 'ocean' (~1km)
    
        writeRaster(out,filename=paste0('working/annual_oa_rescaled_1km/annual_oa_rescaled_1km_',yr,sep=''),format='GTiff',overwrite=T)
  
}

  sapply(rescaled,resample)

#  ras_files = list.files('working/annual_oa_rescaled_1km',full.names=TRUE,recursive=TRUE)



#-------------------------------------------------------------------------------------------------

# (Step 4): Interpolate to coast. This was done manually in ArcGIS (Jamie Afflerbach)

#     Interpolation to fill in NA cells with values of the nearest neighbor 
#     is done within the 'OA_interpolation.py' python script, which relies on arcpy (ArcGIS)

interpolated = list.files('working/annual_oa_rescaled_1km_int',full.names=T)


#-------------------------------------------------------------------------------------------------

# (Step 5): Clip out ocean

# Each interpolated raster needs to have all land cells clipped out. Using the ocean raster again, mask the interpolated
# rasters to select just those in the oceanic regions.

    ocean_clip = function(file){
  
          yr  = substr(file,59,62)
          r   = raster(file)
          out = mask(r,ocean,progress='text')
  
          writeRaster(out,filename=paste0('output/annual_oa_rescaled_1km_int_clip_',yr,sep=''),format='GTiff',overwrite=T)
  
    }

    sapply(interpolated,ocean_clip)

#-------------------------------------------------------------------------------------------------

# (Step 6): Create a raster showing what cells were interpolated - just creating ONE raster. All ten output OA rasters have the same cells interpolated

    #original data rescaled and resampled, before interpolation

      r_1km = raster('working/annual_oa_rescaled_1km/annual_oa_rescaled_1km_2014.tif')

    #after interpolation, and after land clipped ou

      r_c = raster('output/annual_oa_rescaled_1km_int_clip_2014.tif')


    interp_cells = mask(r_c,r_1km,progress='text',inverse=TRUE,filename='output/oa_interpolated_cells.tif')

