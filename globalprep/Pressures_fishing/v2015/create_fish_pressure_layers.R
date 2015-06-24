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



dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_N,'git-annex/globalprep/Pressures_fishing'))

#-----------------------------------------------------------------------------
#READ IN DATA
#-----------------------------------------------------------------------------

# Fish catch at 1km

catch_06_10 = raster('v2015/catch_06_10_1km.tif')
catch_05_09 = raster('v2015/catch_05_09_1km.tif')
catch_04_08 = raster('v2015/catch_04_08_1km.tif')

#Primary productivity at 1km - aggregate to time periods


# gear proportions at 1 km

gear_hb = raster('v2015/gear_prop_hb_moll_1km_ocean.tif')
gear_lb = raster('v2015/gear_prop_lb_moll_1km_ocean.tif')


#----------------------------------------------------------------------------
# Divide catch by primary productivity then multiply by gear proportions


out_06_10_hb = (catch_06_10/npp_06_10)*gear_hb
out_06_10_lb = (catch_06_10/npp_06_10)*gear_lb

out_05_09_hb = (catch_05_09/npp_05_09)*gear_hb
out_05_09_lb = (catch_05_09/npp_05_09)*gear_lb

out_04_08_hb = (catch_04_08/npp_04_08)*gear_hb
out_04_08_lb = (catch_04_08/npp_04_08)*gear_lb

out_03_07_hb = (catch_03_07/npp_03_07)*gear_hb
out_03_07_lb = (catch_03_07/npp_03_07)*gear_lb
