library(rgdal)
library(raster)

source('src/R/common.R')

wd = 'Global/SAUP-FishCatchByGearType_Halpern2008'
lu_csv = sprintf('%s/ingest_fish_lookup.csv', wd)

pfx = c('catch'    = sprintf('%s/mnt/storage/marine_threats/work/fisheries/catch_rasters/catch', dir_halpern2008),
        'fishprod' = sprintf('%s/mnt/storage/marine_threats/work/fisheries/normalized/fishprod', dir_halpern2008))
dir_to = sprintf('%s/git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data', dir_neptune_data)

lu = read.csv(lu_csv)
for (i in 1:nrow(lu)){ # i=1
  for (g in c('catch','fishprod')){ # g='catch'
    p_fro = sprintf('%s%d', pfx[[g]], lu$id[i])
    p_to = sprintf('%s/%s_%s_gcs.tif', dir_to, g, lu$code[i])
    r = raster(p_fro)
    writeRaster(r, p_to)
    if (i==1){
      a = area(raster(r))
      p_a = sprintf('%s/%s_area_gcs.tif', dir_to, g)
      writeRaster(a, p_a)
    }
  }
}



