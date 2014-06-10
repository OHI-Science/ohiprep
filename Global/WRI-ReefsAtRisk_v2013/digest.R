library(raster) # overwrites dplyr select

source('src/R/common.R') # dir_neptune_data
dir_d = 'Global/WRI-ReefsAtRisk_v2013'

# read in rasters
r13 = raster(file.path(dir_neptune_data, 'model/GL-NCEAS-OceanRegions_v2013a/data/rgn_mol.tif'))
b = raster(file.path(dir_neptune_data, 'model/GL-WRI-ReefsAtRisk_Distance/data/gl_thr_blast_3nm.tif'))
p = raster(file.path(dir_neptune_data, 'model/GL-WRI-ReefsAtRisk_Distance/data/gl_thr_poison_3nm.tif'))

# calculate means per region (SLOW: takes 9.5 min each)
b_z = zonal(b, r13, 'mean')
p_z = zonal(p, r13, 'mean')

# output
dir.create(file.path(dir_d, 'data'))
write.csv(dplyr::select(data.frame(b_z), rgn_id=zone, p_rank=mean), 
          file.path(dir_d, 'data/gl_thr_blast_3nm_rgn2013.csv'), 
          row.names=F, na='')
write.csv(dplyr::select(data.frame(p_z), rgn_id=zone, p_rank=mean), 
          file.path(dir_d, 'data/gl_thr_poison_3nm_rgn2013.csv'),
          row.names=F, na='')
