# need to align rasters in model/GL-NCEAS-Halpern2008 to new regions:
# 
# Switching to ArcGIS on Win, b/c in R on Mac:
# ```
# z = zonal(r,  rgn_offshore_mol, progress="text")
# Error in compareRaster(c(x, z)) : different extent
# ```

library(raster)
library(rgdal) # on Mac with R 3.1: http://cran.r-project.org/bin/macosx/contrib/3.1/rgdal_0.8-16.tgz

source('src/R/common.R')

rgn_offshore_mol = raster(file.path(dir_neptune_data,'git-annex/Global/NCEAS-Regions_v2014/data/rgn_offshore_mol.tif'))

list.files(file.path(dir_neptune_data, 'model/GL-NCEAS-Halpern2008/data'), pattern=glob2rx('*.tif'))

r = raster(file.path(dir_neptune_data, 'model/GL-NCEAS-Halpern2008/data/masked_ecosystems_coral_reef.tif'))

extent(rgn_offshore_mol)
extent(r)

#a = alignExtent(r, extent(rgn_offshore_mol))
r_r = resample(r, rgn_offshore_mol, method='bilinear') # 10:24

z = zonal(r,  rgn_offshore_mol, progress="text")