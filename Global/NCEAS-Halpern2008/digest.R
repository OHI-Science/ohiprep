# get zonal stats on rasters

library(raster)
library(rgdal) # on Mac with R 3.1: http://cran.r-project.org/bin/macosx/contrib/3.1/rgdal_0.8-16.tgz

source('src/R/common.R')

r = raster('C:/tmp/Global/NCEAS-Regions_v2014/data/rgn_offshore_mol.tif')
p = raster('C:/tmp/Global/NCEAS-Halpern2008/tmp/masked_ecosystems_coral_reef.tif') # file.path(dir_neptune_data, 'model/GL-NCEAS-Halpern2008/data
m = raster('C:/tmp/Global/NCEAS-Halpern2008/tmp/masked_model.tif') # copied from N:/model/GL-NCEAS-Halpern2008/data/

tifs = list.files(file.path(dir_neptune_data, 'model/GL-NCEAS-Halpern2008/data'), pattern=glob2rx('*.tif'))
for (i in 1:length(tifs)){
  p = tifs[i]
  cat(sprintf('%02d of %d: %s (%s)', i, basename(p), )))
  print(system.time({
    z = zonal(r, p, progress="text") }))
  
  data <- merge(regions_mol@data, regions_stats, all.y=TRUE, by.x="sp_id", by.y="zone") 
  
  write.csv(data,"SST_ZonalMean.csv", 
            row.names=FALSE)  
  
}

