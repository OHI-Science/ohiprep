# Creating sea level rise pressure layer

# updated with data through 2014

#JAfflerbach

# 1. Create raster of original data (came as .nc)
# 2. Reproject to mollweide
# 3. Calculate cumulative sea level rise over all years
# 4. Clip all negative values (values that indicate decreasing sea level)
# 5. resample to 1km
# 6. rescale using 99.99 percentile
# 7. Interpolate and replace NA cells with interpolated values (use Python - arcpy for this)
# 8. Clip out ocean using ocean raster at 1km cell size
# 9. Create raster of just the interpolated cells

#libraries

library(raster)
library(rgdal)
library(rasterVis)
library(RColorBrewer)
library(maptools)


# set tmp directory

tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)


# paths

    dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
              'Darwin'  = '/Volumes/data_edit',
              'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

    setwd(file.path(dir_N,'git-annex/globalprep/prs_slr'))


#set colors for plotting

    cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme


# The following 3 lines were done once to create raster from downloaded NetCDF. No longer need to run these

    #library(ncdf4)

    #r <- raster('raw/MSL_Map_MERGED_Global_IB_RWT_NoGIA_Adjust.nc')
    #writeRaster(r,'tmp/MSL_Map_MERGED_Global_IB_RWT_NoGIA_Adjust.tif')


# Read in raw data

    r <- raster('tmp/MSL_Map_MERGED_Global_IB_RWT_NoGIA_Adjust.tif')

    plot(r,col=cols,main='Mean Annual Sea Level Rise (mm)\n1993-2014')
    

#--------------------------------------------------

# (1) Create raster of original data 

  #  writeRaster(r,filename='tmp/slr_Jan1993_Jun2014_gcs_wgs84_rate_mm_per_year.tif')


#--------------------------------------------------

# (2) Reproject raster to mollweide

  r = rotate(r) #rotate raster: -180 to 180
  
  projection(r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"   #define initial projection. Helps avoid errors when reprojecting to mollweide

  mollCRS <- CRS('+proj=moll')

  slr_moll <- projectRaster(r, crs=mollCRS, over=T,progress='text',filename='tmp/slr_moll.tif',overwrite=T)  


#--------------------------------------------------

# (3) Multiply annual rate by all years in dataset

    # data used to create r comes from January 1993 - June 2014 (21.5 years)
    # Multiply values by 21+5/12 = 21.41667

      slr_moll <- slr_moll*21.41667

      plot(slr_moll,col=cols,main='Aggregate sea level rise (mm)\n1993-2014')

#--------------------------------------------------


# (4) Clip all negative values to 0

      slr_moll[slr_moll<0]<-0

#--------------------------------------------------


# (5) convert to 1km

#sample 1 km raster

    #ocean is a raster with all land clipped out - at 1km with value of 1
    ocean = raster(file.path(dir_N,'model/GL-NCEAS-Halpern2008/tmp/ocean.tif'))

    slr_1km = resample(slr_moll,ocean,method='ngb',progress='text',filename='tmp/slr_moll_1km.tif') 
    #slr_1km = resample(slr_moll,ocean,method='ngb',progress='text',filename='tmp/slr_moll_nonlog_1km_rate.tif',overwrite=T)
    #^second one is for rate of change in slr per year

#--------------------------------------------------


#(6) rescale using the reference point (99.99 quantile)
    #get reference point
   
    ref = quantile(slr_1km,prob=0.9999) #512.7999

    #normalize by the reference point - cap all values greater than 1 to 1
    r_resc <- calc(slr_1km,fun=function(x){ifelse(x>ref,1,x/ref)},progress='text',filename='tmp/slr_moll_nonlog_1km_resc.tif',overwrite=T)
    #r_resc <- calc(slr_1km,fun=function(x){ifelse(x>ref,1,x/ref)},progress='text',filename='tmp/slr_moll_nonlog_1km_rescaled_rate.tif',overwrite=T)
    #^use for the rate (mm/year) data

    plot(r_resc,main='Rescaled Sea Level Rise Pressure',col=cols)

    histogram(r_resc)
#--------------------------------------------------


# (7) Interpolation


# need to do this in arcgis likely

#   script used to do this 'scripts/SLR_interpolation.py'

  r_int = raster('tmp/slr_moll_nonlog_1km_resc_int.tif')

#--------------------------------------------------

# (8) Clip out ocean


r_final = mask(r_int,ocean,progress='text',filename='output/slr_nonlog_final.tif')

#---------------------------------------------------

# (9) Create interpolated cells raster

interp = mask(r_final,slr_1km,progress='text',inverse=TRUE,filename='output/slr_interpolated_cells.tif')
