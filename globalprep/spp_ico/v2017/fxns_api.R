# spp_fxn.R
# created Jun2015 by Casey O'Hara
# updated May2017 by Casey O'Hara for streamlined process
# functions to support calculations of the species diversity subgoal

message('NOTE: spp_fxn.R requires that the following variables be set in the global environment (main script):')
message(sprintf('dir_goal_anx:  currently set to "%s"', dir_goal_anx))

##############################################################################=
get_loiczid_raster <- function(reload = FALSE) {
  ### Generate half-degree raster using am_cells to assign LOICZID to CenterLong x CenterLat.
  ### * Template and output in <dir_anx>/rgns
  ### * rasterize takes about 10 seconds...
  ##############################################################################=

  loiczid_raster_file  <- file.path(dir_goal_anx, '../rgns/loiczid_raster.grd')
  raster_template_file <- file.path(dir_goal_anx, '../rgns/am_cells_template.tif')

  if(!file.exists(loiczid_raster_file) | reload) {
    # load and format AquaMaps half-degree cell authority file
    hcaf_file <- file.path(dir_data_am, 'tables/hcaf.csv')
    message(sprintf('Loading AquaMaps cell data.  Less than 1 minute.\n  %s ', file_loc))
    am_cells <- fread(hcaf_file, header = TRUE, stringsAsFactors = FALSE) %>%
      dplyr::select(csq = CsquareCode, LOICZID, CenterLat, CenterLong, CellArea)

    stopifnot(sum(duplicated(am_cells$LOICZID)) == 0) #key numeric ID for raster generation.... check for dups

    template_raster <- raster::raster(raster_template_file)

    coordinates(am_cells) <- ~ CenterLong + CenterLat
    proj4string(am_cells) <- CRS(projection(template_raster))

    message(sprintf('Writing LOICZID 0.5° raster to: \n  %s', rgn_prop_file))
    raster::rasterize(am_cells, template_raster, field = 'LOICZID', progress = 'text',
                      filename = loiczid_raster_file,
                      overwrite = TRUE)
  } else {
    message(sprintf('Reading LOICZID 0.5° raster from: \n  %s', loiczid_raster_file))
  }
  loiczid_raster <- raster::raster(loiczid_raster_file)
  return(invisible(loiczid_raster))
}

##############################################################################=
extract_cell_id_per_region <- function(reload       = FALSE, 
                                       ogr_location = file.path(dir_M, 'git-annex/globalprep/spatial/v2015/data'),
                                       rgn_layer    = 'regions_gcs', 
                                       ohi_type     = 'global') {   # ohi_type = 'HS'    ohi_type = 'AQ'
  ### Determines proportional area of each cell covered by region polygons.  Returns data frame
  ### of rgn_id, loiczid, csq, and proportional area of loiczid cell covered by the rgn_id region.
  ### * reload: re-extract region IDs to cell IDs from indicated shape file?
  ### * ogr_location: where is the region vector layer information (without layer name)
  ### * rgn_layer:    which vector layer to use (no file extension, e.g. .shp)
  ### * ohi_type:     what type of assessment: global, HS, AQ
  ###
  ### Should not be year-specific, so leave prepped files in SpeciesDiversity/rgns, or change reload to TRUE.
  ### ??? TO DO: compare loiczid regions <-> CenterLong and CenterLat to last year's table, to make sure consistent from year to year.
  ##############################################################################=
  
  message(sprintf('Getting cell ID per %s region based upon region file: %s\n  %s', ohi_type, rgn_layer, file.path(ogr_location, rgn_layer)))
  
  rgn_prop_file <- file.path(dir_goal_anx, sprintf('../rgns/cellID_%s_%s.csv', rgn_layer, ohi_type))
  
  if(!file.exists(rgn_prop_file) | reload) {
    
    message(sprintf('Reading regions shape file %s - come back in about 4 minutes.\n  %s/%s', rgn_layer, ogr_location, rgn_layer))
    regions        <- readOGR(dsn = ogr_location, layer = rgn_layer)
    # slow command... ~ 4 minutes
    
    regions <- switch(ohi_type,
                      global = regions[regions@data$rgn_typ %in% c('eez', 'eez-disputed', 'eez-inland'), ],
                      HS     = regions[regions@data$rgn_typ %in% c('fao'), ],
                      AQ     = regions[regions@data$ant_typ %in% c('eez-ccamlr'), ],
                      regions[regions@data$rgn_typ %in% c('eez', 'eez-disputed', 'eez-inland'), ]) #default to same as global
    
    raster_file    <- file.path(dir_anx, '../rgns/loiczid_raster')
    loiczid_raster <- get_loiczid_raster(reload = FALSE)
    
    message('Extracting proportional area of LOICZID cells per region polygon.  Come back in 15-20 minutes.')
    region_prop <- raster::extract(loiczid_raster,  regions, weights = TRUE, normalizeWeights = FALSE, progress = 'text') 
    # small = TRUE returns 1 for rgn_id 232, not what we want.
    # slow command... ~15 minutes (even with the small = TRUE)
    
    
    ### assign rgn_id and rgn_name identifiers (from `regions`) to region_prop, convert to data.frame
    if(ohi_type == 'AQ') {
      rgn_id_name <- data.frame(regions@data$ant_id, regions@data$rgn_nam) %>%
        unite(combo, regions.data.ant_id, regions.data.rgn_nam, sep = '_')
    } else {
      rgn_id_name <- data.frame(regions@data$rgn_id, regions@data$rgn_nam) %>%
        unite(combo, regions.data.rgn_id, regions.data.rgn_nam, sep = '_')
    }
    
    names(region_prop) <- rgn_id_name$combo
    region_prop_df     <- plyr::ldply(region_prop, rbind) # ??? still a plyr function.
    # length(unique(region_prop_df$.id)) 
    #   WAS: less than 254 due to repeats of Canada and one small region (232: Bosnia/Herzegovina) with no rasters identified
    #   IS:  278, including a cell for #232.
    region_prop_df <- region_prop_df %>%
      separate(.id, c('rgn_id', 'rgn_name'), sep = '_') %>%
      rename(loiczid = value, 
             proportionArea = weight)
    
    
    ### ??? add in this region -  Bosnia/Herzegovina (BIH), which appears to be too small to catch using this method (<1% of area)
    ### ??? SKIPPING THIS FOR NOW!!!
    # cells_2013[cells_2013$rgn_id==232, ]
    # cells_2013[cells_2013$csq=='1401:227:4', ]
    # am_cells[am_cells$CsquareCode == '1401:227:4', ]
    # 6.034664/2269.83
    # bih <- data.frame(rgn_id=232, LOICZID=68076, proportionArea=0.002658641)
    # region_prop_df <- rbind(region_prop_df, bih)
    
    file_loc <- file.path(dir_data_am, 'tables/hcaf.csv')
    message(sprintf('Loading AquaMaps half-degree cell authority file.  Less than 1 minute.\n  %s ', file_loc))
    am_cells <- fread(file_loc, header = TRUE, stringsAsFactors = FALSE) %>%
      as.data.frame() %>%
      dplyr::select(csq = CsquareCode, loiczid = LOICZID, cell_area = CellArea)
    stopifnot(sum(duplicated(am_cells$csq)) == 0)
    
    message('Joining csq values and cell areas to loiczid values.')
    region_prop_df <- region_prop_df %>%
      left_join(am_cells, by = 'loiczid')
    
    message(sprintf('Writing loiczid/csq/cell proportions/cell areas by region to: \n  %s', rgn_prop_file))
    write_csv(region_prop_df, rgn_prop_file)
  } else {
    message(sprintf('Reading loiczid cell proportions by region from: \n  %s', rgn_prop_file))
    region_prop_df <- read_csv(rgn_prop_file, col_types = 'dcdd_d')
  }
  
  return(invisible(region_prop_df))
}

##############################################################################=
process_am_summary_per_cell <- function(spp_all, 
                                        spp_cells,
                                        fn_tag = '',
                                        reload = FALSE) {
  # Calculate category and trend scores per cell for Aquamaps species.
  # * summarize by loiczid - mean cat_score, mean trend_score, count
  
  am_cells_spp_sum_file <- file.path(dir_goal, 'summary', sprintf('spp_sum_am_cells%s.csv', fn_tag))
  
  if(!file.exists(am_cells_spp_sum_file) | reload) {
    message('Generating cell-by-cell summary for Aquamaps species.')
    
    # filter species info to just Aquamaps species with category info, and bind to 
    # am_cells_spp to attach cat_score and trend_score.
    spp_am_info <- spp_all %>% 
      filter(str_detect(spatial_source, 'am')) %>%
      filter(!is.na(cat_score)) %>%
      dplyr::select(am_sid, cat_score, trend_score) %>%
      unique()
    message(sprintf('Length of Aquamaps valid species list: %d', nrow(spp_am_info)))
    
    message('Keyed data frame join cell/species IDs to master species list',
            ' (filtered for just spatial_source == am or am_parent).')
    acs_keyed <- data.table(am_cells_spp, key = "am_sid") 
    sai_keyed <- data.table(spp_am_info,  key = "am_sid")
    am_cells_spp1 <- acs_keyed[sai_keyed] %>%
      setkey(NULL)
    # z <- x[y] is analogous to z <- left_join(y, x, by = 'key'), so the y variable determines which
    # rows to keep (non-matching rows in x will be discarded).  In this case, all species must be on the spp_am_info list
    # (to learn IUCN cat/trend info); and species not on the list will be discarded.  So: acs_keyed is x, and sai_keyed is y.
    # The setkey(NULL) removes the key so the unique() can work properly (otherwise, just selects rows with unique values 
    # of the key)

    message('Grouping by cell and summarizing by mean category, mean trend, ', 
            'and n_spp for each, for AM spatial info.')
    am_cells_spp_sum <- am_cells_spp1 %>%
      as.data.frame() %>%
      distinct() %>%
      group_by(loiczid) %>%
      summarize(mean_cat_score        = mean(cat_score, na.rm = TRUE),  
                mean_pop_trend_score  = mean(trend_score, na.rm = TRUE), 
                n_cat_species         = sum(!is.na(cat_score)),
                n_trend_species       = sum(!is.na(trend_score))) %>%
      mutate(source = 'aquamaps')
    
    message('Writing cell-by-cell summary for Aquamaps species to:\n  ', 
            am_cells_spp_sum_file)
    write_csv(am_cells_spp_sum, am_cells_spp_sum_file)
  } else {
    message('Cell-by-cell summary for Aquamaps species already exists:\n  ', 
            am_cells_spp_sum_file)
  }
  return(am_cells_spp_sum_file)
}

##############################################################################=
get_am_cells_spp <- function(prob_filter = 0, reload = TRUE) {
  am_cells_spp_file <- file.path(dir_goal_anx, sprintf('int/am_cells_spp_prob%s.csv', prob_filter))
  if(!file.exists(am_cells_spp_file) | reload) {
    message('Creating Aquamaps species per cell file')
    ### Load Aquamaps species per cell table
    spp_cell_file <- file.path(dir_data_am, 'hcaf_species_native.csv')
    message(sprintf('Loading AquaMaps cell-species data.  Large file! \n  %s ', spp_cell_file))
    am_cells_spp <- read_csv(spp_cell_file) %>%
      setNames(tolower(names(.))) %>%
      rename(am_sid = speciesid, prob = probability)
    
    if(prob_filter > 0) {
      am_cells_spp <- am_cells_spp %>%
        filter(prob >= prob_filter)
    }
    
    if(!'loiczid' %in% names(am_cells_spp)) {
      ### replace C Square Code with LOICZID... use keyed data.table for speed
      library(data.table)
      csq_loiczid <- read_csv(file.path(dir_data_am, 'hcaf_v2015.csv'),
                              col_types = c('cd________')) %>%
        data.table(key = 'csquarecode')
      am_cells_spp_dt <- am_cells_spp %>%
        data.table(key = 'csquarecode')
      
      am_cells_spp_dt1 <- csq_loiczid[am_cells_spp_dt]
      
      am_cells_spp1 <- am_cells_spp_dt1 %>%
        as.data.frame()
    }
    
    am_cells_spp <- am_cells_spp1 %>%
      select(am_sid, loiczid)
    
    message(sprintf('Writing Aquamaps species per cell file to: \n  %s', am_cells_spp_file))
    write_csv(am_cells_spp, am_cells_spp_file)
    
  } else {
    
    message(sprintf('Aquamaps species per cell file already exists: \n  %s', am_cells_spp_file))
    am_cells_spp <- read_csv(am_cells_spp_file, col_types = 'cd')
    
  }
  
  return(am_cells_spp)
}

##############################################################################=
get_iucn_cells_spp <- function(reload = FALSE) {
  iucn_cells_file <- file.path(dir_goal_anx, 'int/iucn_cells_spp.csv')
  if(!file.exists(iucn_cells_file) | reload) {
    message(sprintf('Building IUCN species to cell table.  This might take a few minutes.'))
    dir_intsx <- file.path(dir_goal_anx, 'iucn_intersections')
    iucn_map_files      <- file.path(dir_intsx, list.files(dir_intsx))
    message('Binding rows from intersection files: \n  ', paste(basename(iucn_map_files), collapse = '\n  '))
    iucn_cells_spp_list <- lapply(iucn_map_files, utils::read.csv) # read each into dataframe within a list
    iucn_cells_spp      <- bind_rows(iucn_cells_spp_list)   # combine list of dataframes to single dataframe
    names(iucn_cells_spp) <- tolower(names(iucn_cells_spp))
    message('Writing IUCN species-cell file to ', iucn_cells_file)
    write_csv(iucn_cells_spp, iucn_cells_file)
  } else {
    message('IUCN species-cell file already exists: \n  ', iucn_cells_file)
    iucn_cells_spp <- read_csv(iucn_cells_file)
  }

  return(iucn_cells_spp)
}

##############################################################################=
process_iucn_summary_per_cell <- function(spp_all, spp_cells, fn_tag = '', reload = FALSE) {
  # Calculate mean category and trend scores per cell for IUCN species.
  # * spp_all is df filtered to desired species (e.g. no DD? no subpops?)
  # * load IUCN species <-> cell lookup
  # * join species info: category score and trend score
  # * filter by cat score != NA
  # * summarize by loiczid:  mean cat_score, mean trend_score, count
  
  iucn_cells_spp_sum_file <- file.path(dir_goal, sprintf('summary/spp_sum_iucn_cells%s.csv', fn_tag))
  
  if(!file.exists(iucn_cells_spp_sum_file) | reload) {
    message('Generating cell-by-cell summary for IUCN range-map species.')
    
    # bind to iucn_cells_spp to attach cat_score and trend_score.
    spp_iucn_info <- spp_all %>% 
      filter(str_detect(spatial_source, 'iucn')) %>% 
      dplyr::select(iucn_sid, cat_score, trend_score) %>%
      filter(!is.na(cat_score)) %>%
      unique()
    message(sprintf('Length of IUCN species list: %d', nrow(spp_iucn_info)))
    
    message('Keyed joining to species master list (filtered for spatial_source == iucn).')
    ics_keyed <- data.table(iucn_cells_spp,
                            key = "iucn_sid") 
    sii_keyed <- data.table(spp_iucn_info,  
                            key = "iucn_sid")
    iucn_cells_spp1 <- ics_keyed[sii_keyed] %>%
      setkey(NULL)
    # z <- x[y] is analogous to z <- left_join(y, x, by = 'key'), so the y variable determines which
    # rows to keep (non-matching rows in x will be discarded).  In this case, all species must be on the spp_iucn_info list
    # (to learn IUCN cat/trend info); and species not on the list will be discarded.  So: ics_is x, and sii_keyed is y.
    
    ### assign proper values for extinct polygons based on presence = 5; category becomes EX
    ### and trend becomes NA
    message('Setting category score to "extinct" for polygons with presence == 5.')
    iucn_cells_spp1 <- iucn_cells_spp1 %>%
      as.data.frame() %>%
      mutate(cat_score   = ifelse(presence == 5, 1, cat_score),
             trend_score = ifelse(presence == 5, NA, trend_score))
    
    message('Eliminating IUCN species double-counting due to overlapping polygons in IUCN shapefiles.')
    # this next part to collapse any duplicated cells - summarize to max proportional area
    iucn_cells_spp2 <- iucn_cells_spp1 %>%
      group_by(iucn_sid, loiczid) %>%
      summarize(cat_score   = mean(cat_score), 
                trend_score = mean(trend_score, na.rm = TRUE), 
                prop_area   = max(prop_area))
    message('Collapsed ', nrow(iucn_cells_spp1) - nrow(iucn_cells_spp2), ' double-counted cells...')
    ### DEBUG:  Check species counts again!
    # length(unique(iucn_cells_spp1$iucn_sid)); length(unique(iucn_cells_spp2$iucn_sid))
    # 4087; 4087.  Still no species dropped!
    
    message('Grouping by cell and summarizing mean category/trend and n_spp for each, for IUCN spatial info.')
    iucn_cells_spp_sum <- iucn_cells_spp2 %>%
      group_by(loiczid) %>%
      summarize(mean_cat_score = mean(cat_score),      # no na.rm needed; already filtered.
                mean_pop_trend_score = mean(trend_score, na.rm = TRUE), 
                n_cat_species = n(),
                n_trend_species = sum(!is.na(trend_score))) %>% # no na.rm needed; count all with cat_score
      mutate(source = 'iucn') %>%
      filter(!is.na(mean_cat_score))
    ### DEBUG:  Any lost cells?  Compare total of n_spp per cells to the number of species-cell pairs
    ### iucn_cells_spp2 is 17124438 species-cell pairs
    # sum(iucn_cells_spp_sum$n_cat_species) 
    ### --> this returns 17124385; a few dropped, why?
    ### come back to this later...
    
    message(sprintf('Writing cell-by-cell summary for IUCN species to:\n  %s', iucn_cells_spp_sum_file))
    write_csv(iucn_cells_spp_sum, iucn_cells_spp_sum_file)
  } else {
    message(sprintf('Cell-by-cell summary for IUCN species already exists:\n  %s', iucn_cells_spp_sum_file))
  }
  return(iucn_cells_spp_sum_file)
}

##############################################################################=
process_means_per_cell <- function(am_cell_summary, iucn_cell_summary, fn_tag = '', reload = FALSE) { 
  ### 2 input data frames:
  ### loiczid | mean_cat_score | mean_pop_trend_score | n_cat_species | n_trend_species | source
  ### calcs weighted score for each cell (by loiczid) from:
  ###   (mean IUCN category value * # of species) for both IUCN and AM data, divided by total species.
  ###   (same for trend)
  
  cell_sum_file <- file.path(dir_goal, 'summary', 
                             sprintf('cell_spp_summary_by_loiczid%s.csv', fn_tag))
  
  if(!file.exists(cell_sum_file) | reload) {
    
    if(sum(is.na(am_cell_summary$mean_cat_score)) > 0) {
      message('NA values for mean category score in aquamaps cell summary df')
      stop()
    }
    if(sum(is.na(iucn_cell_summary$mean_cat_score)) > 0) {
      message('NA values for mean category score in iucn cell summary df')
      stop()
    }
    sum_by_loiczid <- bind_rows(am_cell_summary, iucn_cell_summary) %>%
      group_by(loiczid) %>%
      summarize(weighted_mean_cat    = sum(n_cat_species   * mean_cat_score)/sum(n_cat_species),
                weighted_mean_trend  = sum(n_trend_species * mean_pop_trend_score, na.rm = TRUE)/sum(n_trend_species),
                n_cat_spp = sum(n_cat_species),
                n_tr_spp  = sum(n_trend_species)) %>%
      arrange(loiczid)
    
    write_csv(sum_by_loiczid, cell_sum_file)
  } else {
    message('Summary by loiczid file exists at: ', cell_sum_file)
  }
  
  return(cell_sum_file)
}

##############################################################################=
get_means_per_rgn <- function(summary_by_loiczid, rgn_cell_lookup, rgn_note = '', reload = FALSE) {  
  ### Joins region-cell info to mean cat & trend per cell.
  ### Groups by region IDs, and calcs area-weighted mean category and trend values
  ### for all cells across entire region.  Cells only partly within a region are
  ### accounted for by multiplying cell area * proportionArea from rgn_cell_lookup.
  
  region_summary_file <- file.path(dir_goal, 
                                   sprintf('summary/rgn_summary%s.csv', ifelse(rgn_note == '', '',
                                                                               sprintf('_%s', rgn_note))))
  if(!file.exists(region_summary_file) | reload) {
    if(sum(is.na(summary_by_loiczid$weighted_mean_cat)) > 0) {
      message('NA values for mean category score in cell summary df')
      stop()
    }
    
    rgn_weighted_sums <- summary_by_loiczid %>%
      inner_join(rgn_cell_lookup,
                 by = 'loiczid') %>%
      # mutate(rgn_area = cell_area * proportionArea,
      #        area_weighted_mean_cat   = weighted_mean_cat   * rgn_area,
      #        area_weighted_mean_trend = weighted_mean_trend * rgn_area) %>%
      mutate(cell_area_weight_cat     = cell_area * n_cat_spp * proportionArea,
             cell_area_weight_trend   = cell_area * n_tr_spp * proportionArea,
             area_weighted_mean_cat   = weighted_mean_cat   * cell_area_weight_cat,
             area_weighted_mean_trend = weighted_mean_trend * cell_area_weight_trend) %>%
      arrange(loiczid)
    
    region_sums <- rgn_weighted_sums %>%
      group_by(rgn_id) %>%
      summarize(rgn_mean_cat   = sum(area_weighted_mean_cat)/sum(cell_area_weight_cat),
                rgn_mean_trend = sum(area_weighted_mean_trend, na.rm = TRUE)/sum(cell_area_weight_trend))
    
    region_sums <- region_sums %>%
      mutate(status = ((1 - rgn_mean_cat) - 0.25) / 0.75)
    
    
    message(sprintf('Writing summary file of area-weighted mean category & trend per region:\n  %s', region_summary_file))
    write_csv(region_sums, region_summary_file)
  } else {
    message(sprintf('Reading summary file of area-weighted mean category & trend per region:\n  %s', region_summary_file))
    region_sums <- read_csv(region_summary_file)
    
  }
  
  return(region_sums)
}

##############################################################################=
show_dupes <- function(x, y) {
  # x is data frame, y is field within that dataframe
  z <- x %>% filter(x[[y]] %in% x[[y]][duplicated(x[[y]])])
}

##############################################################################=
calc_rgn_spp <- function(ics_keyed, acs_keyed, rgn_keyed, spp_all) {
  iucn_rgn_spp <- ics_keyed[rgn_keyed] %>%
    setkey(NULL) %>%
    group_by(rgn_id, rgn_name, iucn_sid, presence) %>%
    summarize(n_cells = n())
  
  am_rgn_spp <- acs_keyed[rgn_keyed] %>%
    setkey(NULL) %>%
    dplyr::select(am_sid, loiczid, rgn_id, rgn_name) %>%
    unique() %>%
    group_by(rgn_id, rgn_name, am_sid) %>%
    summarize(n_cells = n())
  
  spp_all_trunc <- spp_all %>%
    dplyr::select(iucn_sid, am_sid, sciname, cat_code, pop_trend, spatial_source) %>%
    unique()
  
  iucn_rgn_spp1 <- spp_all_trunc %>%
    filter(str_detect(spatial_source, 'iucn')) %>%
    left_join(iucn_rgn_spp, by = 'iucn_sid') %>%
    filter(!is.na(rgn_id))
  
  am_rgn_spp1 <- spp_all_trunc %>%
    filter(str_detect(spatial_source, 'am')) %>%
    left_join(am_rgn_spp, by = 'am_sid') %>%
    mutate(presence = NA) %>%
    filter(!is.na(rgn_id))
  
  message('binding IUCN cells/info with AM cells/info')
  rgn_spp_all <- bind_rows(am_rgn_spp1, iucn_rgn_spp1) %>%
    group_by(rgn_id) %>%
    mutate(n_spp_rgn = n()) %>%
    ungroup()
}

##############################################################################=
library(parallel)
library(jsonlite)

### api_key stored on git-annex so outside users can use their own key
api_key <- scan(file.path(dir_goal_anx, '../api_key.csv'), what = 'character')

get_from_api <- function(url, param, api_key, delay) {
  
  i <- 1; tries <- 5; success <- FALSE
  
  while(i <= tries & success == FALSE) {
    pause_time <- delay * i + runif(1, 0, delay)
    # message('try #', i, '... pausing ', pause_time, ' seconds')
    # cat('try #', i, '... pausing ', pause_time, ' seconds\n')
    Sys.sleep(pause_time) ### be nice to the API server? later attempts wait longer
    api_info <- fromJSON(sprintf(url, param, api_key)) 
    if (class(api_info) != 'try-error') {
      success <- TRUE
    } else {
      warning(sprintf('try #%s: class(api_info) = %s\n', i, class(api_info)))
    }
    message('... successful? ', success)
    i <- i + 1
  }
  
  if (class(api_info) == 'try-error') { ### multi tries and still try-error
    api_return <- data.frame(param_id  = param,
                             api_error = 'try-error after multiple attempts')
  } else if (class(api_info$result) != 'data.frame') { ### result isn't data frame for some reason
    api_return <- data.frame(param_id  = param,
                             api_error = paste('non data.frame output: ', class(api_info$result), ' length = ', length(api_info$result)))
  } else if (length(api_info$result) == 0) { ### result is empty
    api_return <- data.frame(param_id  = param,
                             api_error = 'zero length data.frame')
  } else {
    api_return <- api_info %>%
      data.frame(stringsAsFactors = FALSE)
  }
  
  return(api_return)
}

mc_get_from_api <- function(url, param_vec, api_key, cores = NULL, delay = 0.5) {
  
  if(is.null(cores)) 
    numcores <- ifelse(Sys.info()[['nodename']] == 'mazu', 12, 1)
  else 
    numcores <- cores
  
  out_list <- parallel::mclapply(param_vec, 
                                 function(x) get_from_api(url, x, api_key, delay),
                                 mc.cores   = numcores,
                                 mc.cleanup = TRUE) 
  
  if(any(sapply(out_list, class) != 'data.frame')) {
    error_list <- out_list[sapply(out_list, class) != 'data.frame']
    message('List items are not data frame: ', paste(sapply(error_list, class), collapse = '; '))
    message('might be causing the bind_rows() error; returning the raw list instead')
    return(out_list)
  }
  
  out_df <- out_list %>%
    bind_rows()
  out_df <- out_df %>%
    setNames(names(.) %>%
               str_replace('result.', ''))
  return(out_df)
}
