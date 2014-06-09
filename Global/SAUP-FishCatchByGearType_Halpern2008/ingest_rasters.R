library(rgdal)
library(raster)

source('src/R/common.R')

wd = 'Global/SAUP-FishCatchByGearType_Halpern2008'
lu_csv = sprintf('%s/ingest_fish_lookup.csv', wd)

pfx = c('catch'    = sprintf('%s/mnt/storage/marine_threats/work/fisheries/catch_rasters/catch', dir_halpern2008),
        'fishprod' = sprintf('%s/mnt/storage/marine_threats/work/fisheries/normalized/fishprod', dir_halpern2008))
dir_to = sprintf('%s/git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data', dir_neptune_data)
redo = F

lu = read.csv(lu_csv)
for (i in 1:nrow(lu)){ # i=5
  for (g in c('catch','fishprod')){ # g='catch'
    
    # paths from and to
    p_fro = sprintf('%s%s', pfx[[g]], as.character(lu[i, sprintf('id_%s', g)]))
    p_to = sprintf('%s/%s_%s_gcs.tif', dir_to, g, lu$code[i])
    
    # delete if exists and redoing
    if (file.exists(p_to) & redo) file.remove(p_to)
    
    # if doesn't exist, read from and write to
    if (!file.exists(p_to)){ 
      r = raster(p_fro)
      # table(getValues(r)) # check raster for all 0s
      writeRaster(r, p_to)
    }
    
    if (i==1){
      p_a = sprintf('%s/%s_area_gcs.tif', dir_to, g)
      if (file.exists(p_to) & redo) file.unlink(p_a)
      if (!file.exists(p_a)){
        r = raster(p_fro)
        a = area(raster(r))      
        writeRaster(a, p_a)
      }
    }
  }
}
