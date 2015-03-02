# sea level rise

#JAfflerbach

#1/12/2014



# set tmp directory

tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)

# paths

dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_N,'git-annex/globalprep/AVISO-SeaLevelRise_v2015'))

dir_git = file.path('../ohiprep/globlaprep/AVISO-SeaLevelRise_v2015')

#set colors for plotting
cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme

library(raster)
library(rgdal)
library(rasterVis)
library(RColorBrewer)


# The following 3 lines were done once to create raster from downloaded NetCDF. No longer need to run these

    #library(ncdf4)

    #r <- raster('raw/MSL_Map_MERGED_Global_IB_RWT_NoGIA_Adjust.nc')
    #writeRaster(r,'tmp/MSL_Map_MERGED_Global_IB_RWT_NoGIA_Adjust.tif')


# Read in raw data

    r<-raster('tmp/MSL_Map_MERGED_Global_IB_RWT_NoGIA_Adjust.tif')

    plot(r,col=cols,main='Mean Annual Sea Level Rise (mm)\n1993-2014')


# (1) Create raster of original data 

    writeRaster(r,filename='tmp/slr_Jan1993_Jun2014_gcs_wgs84_rate_mm_per_year.tif')



# (2) Multiply annual rate by all years in dataset

    # data used to create r comes from January 1993 - June 2014 (21.5 years)
    # Multiply values by 21+5/12 = 21.41667

      r_years <- r*21.41667
      plot(r_years,col=cols,main='Aggregate sea level rise (mm)\n1993-2014')

      writeRaster(r_years,filename='tmp/slr_Jan1993_Jun2014_gcs_wgs84_change_mm.tif',overwrite=T)



# (3) Get reference point

#   The reference point is the 99.99th percentile with a 10% buffer

    quantile(r,probs=c(0,0.0001,0.001,0.01,0.1,0.25,0.5,0.75,0.9,0.99,0.999,0.9999))

#     99.99th percentile is a value of 24.0321697 mm per year




# (4) Clip all data <0 to 0 and then normalize to the reference point

     r[r<0]<-0   

    #get reference point
    ref = 24.0321697*1.1

    #normalize by the reference point
    r_norm <- r/ref

    # since some values are still above the reference point (greater than 1) cap these to 1.
    r_norm[r_norm>1]<-1

    #rotate so extent is -180 to 180
    r_norm = rotate(r_norm)

    plot(r_norm,main='Normalized Sea Level Rise Pressure',col=cols)


  
writeRaster(r_norm,filename='output/slr_Jan1993_Jun2014_gcs_wgs84_rate_per_year_normalized.tif',overwrite=T)
