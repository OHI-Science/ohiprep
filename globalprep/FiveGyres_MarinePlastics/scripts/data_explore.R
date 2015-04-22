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

data_wd = file.path(dir_N,'git-annex/globalprep/FiveGyres_MarinePlastics_CW/v2015/globalplastic_wd-cd_rasters')

# plastics data

count_1 = raster(file.path(data_wd,'count_density_size1.tif'))

# want to check how good the coastal coverage is

# bring in shapefile

eez = readOGR(dsn=file.path(dir_N,'git-annex/Global/NCEAS-Regions_v2014/data'),layer='rgn_gcs')


ocean = raster(file.path(dir_N,'model/GL-NCEAS-Landsea_v2013a/data/ocean.tif'))
