source('src/R/common.R') # dir_neptune_data
library(raster)
select = dplyr::select

# vars
tifs = sprintf('%s/model/GL-WRI-ReefsAtRisk_Distance/data/%s', 
               dir_neptune_data,  
               c('gl_thr_blast_3nm.tif', 'gl_thr_poison_3nm.tif'))
dir_out = 'Global/WRI-ReefsAtRisk_v2013'

r13 = raster(file.path(dir_neptune_data, 'model/GL-NCEAS-OceanRegions_v2013a/data/rgn_mol.tif'))
dir.create(file.path(dir_o, 'data'), showWarnings=F)

for (tif in tifs){ # tif = tifs[2]
  
  # zonal
  r = raster(tif)
  z = zonal(r, r13, 'mean') # SLOW: ~9 min ea 

  # score
  d = data.frame(z) %.%    
    filter(mean > 1) %.%
    mutate(score = mean - 1) %.%
    select(rgn_id=zone, score)
  
  # write
  csv = sprintf('%s/data/%s_rgn2013.csv', dir_out,  tools::file_path_sans_ext(basename(tif)))
  write.csv(d, csv, row.names=F, na='')  
}


### Combining data for combined pressure

p <- read.csv("globalprep/np_prs_poison_blast_fishing/v2013/data/gl_thr_poison_3nm_rgn2013.csv")
b <- read.csv("globalprep/np_prs_poison_blast_fishing/v2013/data/gl_thr_blast_3nm_rgn2013.csv")

p_b <- rbind(p, b) %>%
  group_by(rgn_id) %>%
  summarize(score=sum(score)) %>%
  mutate(score = ifelse(score > 1, 1, score)) %>%
  ungroup() %>%
  select(rgn_id, pressure_score = score)

write.csv(p_b, "globalprep/np_prs_poison_blast_fishing/v2013/data/blast_poison_3nm.csv", row.names=FALSE)
