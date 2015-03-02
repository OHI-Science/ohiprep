# sea level rise

#JAfflerbach

#1/12/2014

setwd('N:/git-annex/Global/AVISO-SeaLevelRise_v2015')

library(raster)
library(rgdal)

# The following 3 lines were done once to create raster from downloaded NetCDF. No longer need to run these
#library(ncdf4)

#r <- raster('raw/MSL_Map_MERGED_Global_IB_RWT_NoGIA_Adjust.nc')
#writeRaster(r,'working/MSL_Map_MERGED_Global_IB_RWT_NoGIA_Adjust.tif')

# Read in data

r<-raster('working/MSL_Map_MERGED_Global_IB_RWT_NoGIA_Adjust.tif')

plot(r)


# (1) Create raster of original data 

writeRaster(r,filename='working/slr_Jan1993_Jun2014_gcs_wgs84_rate_mm_per_year.tif')


#(2) Multiply annual rate by all years in dataset

# data used to create r comes from January 1993 - June 2014 (21.5 years)
# Multiply values by 21+5/12 = 21.41667

r_years <- r*21.41667
plot(r_years)

writeRaster(r_years,filename='working/slr_Jan1993_Jun2014_gcs_wgs84_change_mm.tif',overwrite=T)


# (3) normalize data from #1

max = cellStats(abs(r),stat='max')

#normalize by taking the maximum absolute value
r_norm <- r/max

r_norm = rotate(r_norm)

writeRaster(r_norm,filename='working/slr_Jan1993_Jun2014_gcs_wgs84_rate_per_year_normalized.tif',overwrite=T)
