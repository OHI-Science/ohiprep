source('src/R/common.R') # dir_neptune_data
library(raster)
select = dplyr::select

# vars
dir_d = 'Global/WRI-ReefsAtRisk_v2013'
r13 = raster(file.path(dir_neptune_data, 'model/GL-NCEAS-OceanRegions_v2013a/data/rgn_mol.tif'))
lyrs = list(blast   =list(tif='gl_thr_blast_3nm.tif'),
            poision =list(tif='gl_thr_blast_3nm.tif'))

dir.create(file.path(dir_d, 'data'), showWarnings=F)

for (i in 1:length(lyrs)){ # i=1
  
  # vars
  lyr = names(lyrs)[i]  
  tif = file.path(dir_neptune_data, 'model/GL-WRI-ReefsAtRisk_Distance/data', lyrs[[i]][['tif']])
  csv = sprintf('%s/data/%s_rgn2013.csv', dir_d,  tools::file_path_sans_ext(basename(tif)))
  
  # zonal
  r = raster(tif)
  z = zonal(r, r13, 'mean')

  if (lyr=='blast'){
    z = b_z
  } else {
    z = p_z
  }
  
  # get score
  d = data.frame(z) %.%    
    filter(mean > 1) %.%
    mutate(score = mean - 1) %.%
    select(rgn_id=zone, score)
  
  write.csv(d, csv, row.names=F, na='')  
}