# sea surface temperature

# Jamie Afflerbach

#4.13.2015

library(ncdf4)
library(raster)

#paths

dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

dir_halpern2008 = c('Windows' = '//neptune.nceas.ucsb.edu/halpern2008_edit',
                    'Darwin'  = '/Volumes/halpern2008_edit',
                    'Linux'   = '/var/cache/halpern-et-al')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_N,'git-annex/globalprep/Pressures_SST/data'))

# data

sst = raster('freq_annual_sst_anomalies_82_2012.nc')

sst = nc_open('freq_sst_anomalies_2010_2012/freq_sst_2010_2012.nc')

