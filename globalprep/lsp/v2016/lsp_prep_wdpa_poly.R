### lsp_prep_wdpa_poly.R
### created 6/20/2016 by Casey O'Hara
### supporting script for lsp_goal_prep.Rmd
###
### setup WDPA shapefile for lasting special places by
### filtering out non-"Designated" protected areas, and filtering out
### "non-MPA programmatic management plans" (US protected areas that distort scores)

shp_raw     <- file.path(dir_data_wdpa, 'WDPA_May2016-shapefile', 'WDPA_May2016-shapefile-polygons')
shp_reorder <- file.path(dir_data_wdpa, 'shps', 'WDPA_May2016_shp_ordered')
shp_xformed <- file.path(dir_data_wdpa, 'shps', 'WDPA_May2016_shp_xformed')

if(!file.exists(paste0(shp_xformed, '.shp'))) {
  message('No shp found for filtered/reordered/transformed WDPA database')
  if(!file.exists(paste0(shp_reorder, '.shp'))) {
    message('No shp found for filtered/reordered WDPA database')
    ### Read in the raw shapefile (3.2 GB)
    message('Reading in raw shapefile: \n  ', shp_raw)
    ptm <- proc.time()
    wdpa_poly <- readOGR(dsn = dirname(shp_raw), layer = basename(shp_raw))
    message('elapsed: ', (proc.time() - ptm)[3])
    
    ### filter polygons
    wdpa_poly <- wdpa_poly[wdpa_poly@data$STATUS == 'Designated', ]
    wdpa_poly <- wdpa_poly[!str_detect(tolower(wdpa_poly@data$MANG_PLAN), 'non-mpa program'), ]
    
    ### reorder polygons (oldest last) and save shapefile
    reorder_vec <- order(wdpa_poly@data$STATUS_YR, decreasing = TRUE)
    wdpa_poly1 <- wdpa_poly[reorder_vec, ]
    
    message('Writing filtered/reordered WDPA polygons to: \n  ', shp_reorder)
    writeOGR(wdpa_poly1, dsn = dirname(shp_reorder), layer = basename(shp_reorder),
             driver = 'ESRI Shapefile')
    ### lots of warnings similar to:
    ###   "Warning 1: Value 555593627 of field WDPAID of feature 507 not successfully written."
    ### warning ignored: WDPAID field not used in analysis
    
    rm('wdpa_poly') ### clean up memory
    
  } else {
    ### reordered shapefile exists; read it in
    message('Reading in filtered/re-ordered shapefile: \n  ', shp_reorder)
    wdpa_poly1 <- readOGR(dsn = dirname(shp_reorder), layer = basename(shp_reorder))
  }
  
  message('Spatial transforming WDPA polygons to Mollweide')
  crs_mol <- CRS('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs')
  ptm <- proc.time()
  wdpa_poly2 <- spTransform(wdpa_poly1, crs_mol)
  message('elapsed: ', (proc.time() - ptm)[3])  ### elapsed: 485.793s on Mazu
  
  message('Writing filtered/reorderedtransformed WDPA polygons to: \n  ', shp_xformed)
  ptm <- proc.time()
  writeOGR(wdpa_poly2, dsn = dirname(shp_xformed), layer = basename(shp_xformed),
           driver = 'ESRI Shapefile')
  message('elapsed: ', (proc.time() - ptm)[3])
  ### elapsed: 1445.093 s on Mazu
  
  rm(c('wdpa_poly1', 'wdpa_poly2')) ### clean up the memory
  
} else {
  ### transformed shapefile exists
  message('Transformed WDPA shapefile exists at \n  ', shp_xformed)
  message('Go inspect it to make sure it looks good!')
  message('Then: use rasterize_wdpa.py to rasterize the polygons')
}
