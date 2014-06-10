source('src/R/common.R')

library(raster)

r.tif = file.path(dir_neptune_data, 'model/GL-NCEAS-OceanRegions_v2013a/data/rgn_mol.tif')
b.tif = file.path(dir_neptune_data, 'model/GL-WRI-ReefsAtRisk_Distance/data/gl_thr_blast_3nm.tif')
p.tif = file.path(dir_neptune_data, 'model/GL-WRI-ReefsAtRisk_Distance/data/gl_thr_poison_3nm.tif')

# raster functions: raster, rasterize, extract, rasterToPoints, zonal
r = raster(r.tif)
b = raster(b.tif)
p = raster(b.tif)

#r_p = rasterToPoints(r) # SLOW!
system.time({ b_z = zonal(b, r, 'mean') })
system.time({ p_z = zonal(p, r, 'mean') })

zonal_dt = function (x, z, stat = "mean", digits = 0, na.rm = TRUE, ...) {
  # zonal function using 10x faster data.table package
  #   source: http://r-sig-geo.2731867.n2.nabble.com/Alternative-to-zonal-for-large-images-tt7582580.html#a7582593
  library(data.table)
  fun   = match.fun(stat) 
  vals  = getValues(x) 
  zones = round(getValues(z), digits = digits) 
  rDT   = data.table(vals, z=zones) 
  setkey(rDT, z) 
  rDT[, lapply(.SD, fun), by=z] 
} 

system.time({ b_z_dt = zonal_dt(b, r, 'mean') })
system.time({ p_z_dt = zonal_dt(p, r, 'mean') })
