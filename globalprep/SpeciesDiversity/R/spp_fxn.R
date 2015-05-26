# spp_fxn.R

create_loiczid_raster <- function(am_cells, dir_anx, reload = FALSE) {
### Generate half-degree raster using am_cells to assign LOICZID to CenterLong x CenterLat.
### * Template and output in <dir_anx>/rgns
### * rasterize takes about 10 seconds...
##############################################################################=
  stopifnot(sum(duplicated(am_cells$LOICZID)) == 0) #key numeric ID for raster generation.... check for dups
  
  loiczid_raster_file  <- file.path(dir_anx, 'rgns/loiczid_raster.grd')
  raster_template_file <- file.path(dir_anx, 'rgns/am_cells_template.tif')
  
  if(!file.exists(loiczid_raster_file) | reload == TRUE) {
    template_raster <- raster(raster_template_file)
    
    coordinates(am_cells) <- ~ CenterLong + CenterLat
    proj4string(am_cells) <- CRS(projection(template_raster))
    
    cat(sprintf('Writing LOICZID 0.5° raster to: \n  %s', rgn_prop_file))
    rasterize(am_cells, template_raster, field = 'LOICZID', progress = 'text', 
              filename = loiczid_raster_file,
              overwrite = TRUE)
  } else {
    cat(sprintf('Reading LOICZID 0.5° raster from: \n  %s', loiczid_raster_file))
  }
  loiczid_raster <- raster(loiczid_raster_file)
  return(invisible(loiczid_raster))
}



extract_loiczid_per_region <- function(dir_anx, ogr_location = file.path(dir_neptune_data, 'git-annex/Global/NCEAS-Regions_v2014/data'), reload = FALSE) {
### Determines proportional area of each cell covered by region polygons.  Returns data frame
### of sp_id, LOICZID, and proportional area of LOICZID cell covered by the sp_id region.
### Should not be year-specific, so leave files in SpeciesDiversity/rgns.
### ??? TO DO: compare LOICZID regions <-> CenterLong and CenterLat to last year's table, to make sure consistent from year to year.
### TO DO: update to translate sp_id directly into rgn_id using Melanie's code.
##############################################################################=
  
  rgn_prop_file <- file.path(dir_anx, 'rgns/region_prop_df.csv')
  
  if(!file.exists(rgn_prop_file) | reload == TRUE) {
    
    cat(sprintf('Reading regions shape file - come back in about 4 minutes.\n  %s\n', ogr_location))
    regions <- readOGR(dsn = ogr_location, layer='sp_gcs')
    # slow command... ~ 4 minutes
    regions <- regions[regions@data$rgn_type %in% c('eez', 'fao', 'eez_ccamlr', 'eez-disputed', 'eez-inland'), ]
    
    cat('Extracting proportional area of LOICZID cells per region polygon.  Come back in 15-20 minutes.')
    loiczid_raster <- raster(file.path(dir_anx, 'rgns/loiczid_raster'))
    region_prop <- raster::extract(loiczid_raster,  regions, weights = TRUE, normalizeWeights = FALSE, progress = 'text') 
    # small = TRUE returns 1 for sp_id 232, not what we want.
    # slow command... ~15 minutes (even with the small = TRUE)
    
    ### assign sp_id identifiers (from `regions`) to region_prop, convert to data.frame
    names(region_prop) <- regions@data$sp_id
    region_prop_df     <- plyr::ldply(region_prop, rbind) # ??? still a plyr function.  can we get out of plyr? put into function, does library just stay in function?
    # length(unique(region_prop_df$.id)) 
    #   WAS: less than 254 due to repeats of Canada and one small region (232: Bosnia/Herzegovina) with no rasters identified
    #   IS:  278, including a cell for #232.
    
    # ??? consider converting sp_id into rgn_id code from Melanie
    region_prop_df <- region_prop_df %>%
      rename(sp_id = .id, 
             LOICZID = value, 
             proportionArea = weight)
    # confusion between dplyr and plyr... this needs dplyr version.
    
    ### ??? add in this region -  Bosnia/Herzegovina (BIH), which appears to be too small to catch using this method (<1% of area)
    ### ??? SKIPPING THIS FOR NOW!!!
    # cells_2013[cells_2013$rgn_id==232, ]
    # cells_2013[cells_2013$csq=='1401:227:4', ]
    # am_cells[am_cells$CsquareCode == '1401:227:4', ]
    # 6.034664/2269.83
    # bih <- data.frame(sp_id=232, LOICZID=68076, proportionArea=0.002658641)
    # region_prop_df <- rbind(region_prop_df, bih)
    
    cat(sprintf('Writing LOICZID cell proportions by region to: \n  %s', rgn_prop_file))
    write.csv(region_prop_df, rgn_prop_file, row.names = FALSE)
  } else {
    cat(sprintf('Reading LOICZID cell proportions by region from: \n  %s', rgn_prop_file))
    region_prop_df <- read.csv(rgn_prop_file)
  }
  
  return(invisible(region_prop_df))
}
