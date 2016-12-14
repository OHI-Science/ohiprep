# get zonal stats on rasters

# much faster on https://neptune.nceas.ucsb.edu/rstudio/, ~ 15 min ea
# started ~ 3am -> , 24 of 38: masked_impacts_dem_nd_hb.tif (2014-06-23 09:22:03), ended 15:43:19

library(raster)
library(rgdal) # on Mac with R 3.1: http://cran.r-project.org/bin/macosx/contrib/3.1/rgdal_0.8-16.tgz
library(stringr)
library(reshape2)
library(plyr)
library(dplyr)

source('src/R/common.R')

dir_out = 'Global/NCEAS-Halpern2008_v2014/data'

r = raster(file.path(dir_neptune_data, 'git-annex/Global/NCEAS-Regions_v2014/data/rgn_offshore_mol.tif'))
p = raster(file.path(dir_neptune_data, 'model/GL-NCEAS-Halpern2008/data/masked_ecosystems_coral_reef.tif'))

if (!file.exists(dir_out)) dir.create(dir_out, showWarnings=F)

tifs = list.files(file.path(dir_neptune_data, 'model/GL-NCEAS-Halpern2008/data'), pattern=glob2rx('*.tif'))

for (i in 1:3){ #length(tifs)){ # i=2
  
  f = tifs[i]
  csv = sprintf('%s/%s.csv', dir_out, str_replace(tools::file_path_sans_ext(f), 'masked_', '') )
  cat(sprintf('%02d of %d: %s (%s)\n', i, length(tifs), f, Sys.time()))
  
  p = raster(sprintf('%s/model/GL-NCEAS-Halpern2008/data/%s', dir_neptune_data, tifs[i]))
  
  if (str_detect(f, '^masked_ecosystems_')){
    
    z = zonal(p, r, fun='sum', progress='text') %>%
      as.data.frame() %>%
      mutate(area_km2 = sum * res(p)[1]^2 / 1000^2 ) %>%  # convert ncells to km^2, cellsize of 934.48 m 
      select(rgn_id=zone, area_km2) %>%
      filter(area_km2 > 0) %>%
      arrange(rgn_id)
        
  } else {
    
    z = zonal(p, r, fun='mean', progress='text') %>%
      as.data.frame() %>%
      select(rgn_id=zone, mean) %>%
      arrange(rgn_id)
    
   }

  write.csv(z, csv, na='', row.names=F)

}