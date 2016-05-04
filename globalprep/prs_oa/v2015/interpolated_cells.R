# Creating interpolated cell map for OA

# 2/26/2015

# Jamie Afflerbach

#libraries

library(raster)
library(rgdal)

# set tmp directory

tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)

# paths

dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]


# we just need one interpolated raster (final output) and the original data before interpolation

dat = raster(file.path(dir_N,'git-annex/globalprep/Pressures_OceanAcidification/v2015/working/annualmean_2005-2014/moll/global_arag_avg_moll_2005.tif'))

out = raster(file.path(dir_N,'git-annex/globalprep/Pressures_OceanAcidification/v2015/working/annual_oa_1km/oa_1km_2005.tif'))

#resample dat to out resolution

dat_1km = resample(dat,out,method='ngb',progress='text')

dat_na=calc(dat_1km,fun=function(x){is.na(x)},progress='text')

int_cells = overlay(out,dat_na,fun=function(x,y){x*y},progress='text')


#writeRaster(int_cells,filename=file.path(dir_N,'git-annex/globalprep/Pressures_OceanAcidification/v2015/working/interpolated_cells.tif'),progress='text',overwrite=T)

r <- raster(file.path(dir_N,'git-annex/globalprep/Pressures_acid/v2015/output/interpolated_cells_clip.tif'))




