# spp_fxn.R
# created Jun2015 by Casey O'Hara
# functions to support calculations of the species diversity subgoal

message('NOTE: spp_fxn.R requires that the following variables be set in the global environment (main script):\n')
message(sprintf('dir_anx:  currently set to \'%s\'\n', dir_anx))
message(sprintf('scenario: currently set to \'%s\'\n\n', scenario))

##############################################################################=
get_loiczid_raster <- function(reload = FALSE) {
  ### Generate half-degree raster using am_cells to assign LOICZID to CenterLong x CenterLat.
  ### * Template and output in <dir_anx>/rgns
  ### * rasterize takes about 10 seconds...
  ##############################################################################=
  
  loiczid_raster_file  <- file.path(dir_anx, 'rgns/loiczid_raster.grd')
  raster_template_file <- file.path(dir_anx, 'rgns/am_cells_template.tif')
  
  if(!file.exists(loiczid_raster_file) | reload) {
    # load and format AquaMaps half-degree cell authority file
    hcaf_file <- file.path(dir_data_am, 'tables/hcaf.csv')
    message(sprintf('Loading AquaMaps cell data.  Less than 1 minute.\n  %s \n', file_loc))
    am_cells <- fread(hcaf_file, header = TRUE, stringsAsFactors = FALSE) %>%
      dplyr::select(csq = CsquareCode, LOICZID, CenterLat, CenterLong, CellArea)
    
    stopifnot(sum(duplicated(am_cells$LOICZID)) == 0) #key numeric ID for raster generation.... check for dups
    
    template_raster <- raster(raster_template_file)
    
    coordinates(am_cells) <- ~ CenterLong + CenterLat
    proj4string(am_cells) <- CRS(projection(template_raster))
    
    message(sprintf('Writing LOICZID 0.5° raster to: \n  %s\n', rgn_prop_file))
    rasterize(am_cells, template_raster, field = 'LOICZID', progress = 'text', 
              filename = loiczid_raster_file,
              overwrite = TRUE)
  } else {
    message(sprintf('Reading LOICZID 0.5° raster from: \n  %s\n', loiczid_raster_file))
  }
  loiczid_raster <- raster(loiczid_raster_file)
  return(invisible(loiczid_raster))
}


##############################################################################=
extract_cell_id_per_region <- function(reload       = FALSE, 
                                       ogr_location = file.path(dir_neptune_data, 'git-annex/globalprep/spatial/v2015/data'),
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
  
  message(sprintf('Getting cell ID per %s region based upon region file: %s\n  %s\n', ohi_type, rgn_layer, file.path(ogr_location, rgn_layer)))
  
  rgn_prop_file <- file.path(dir_anx, sprintf('rgns/cellID_%s_%s.csv', rgn_layer, ohi_type))
  
  if(!file.exists(rgn_prop_file) | reload) {
    
    message(sprintf('Reading regions shape file %s - come back in about 4 minutes.\n  %s/%s\n', rgn_layer, ogr_location, rgn_layer))
    regions        <- readOGR(dsn = ogr_location, layer = rgn_layer)
    # slow command... ~ 4 minutes
    
    regions <- switch(ohi_type,
                      global = regions[regions@data$rgn_typ %in% c('eez', 'eez-disputed', 'eez-inland'), ],
                      HS     = regions[regions@data$rgn_typ %in% c('fao'), ],
                      AQ     = regions[regions@data$ant_typ %in% c('eez-ccamlr'), ],
                      regions[regions@data$rgn_typ %in% c('eez', 'eez-disputed', 'eez-inland'), ]) #default to same as global
    
    raster_file    <- file.path(dir_anx, 'rgns/loiczid_raster')
    loiczid_raster <- get_loiczid_raster(reload = FALSE)
    
    message('Extracting proportional area of LOICZID cells per region polygon.  Come back in 15-20 minutes.\n')
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
    message(sprintf('Loading AquaMaps half-degree cell authority file.  Less than 1 minute.\n  %s \n', file_loc))
    am_cells <- fread(file_loc, header = TRUE, stringsAsFactors = FALSE) %>%
      as.data.frame() %>%
      dplyr::select(csq = CsquareCode, loiczid = LOICZID, cell_area = CellArea)
    stopifnot(sum(duplicated(am_cells$csq)) == 0)
    
    message('Joining csq values and cell areas to loiczid values.\n')
    region_prop_df <- region_prop_df %>%
      left_join(am_cells, by = 'loiczid')
    
    message(sprintf('Writing loiczid/csq/cell proportions/cell areas by region to: \n  %s\n', rgn_prop_file))
    write.csv(region_prop_df, rgn_prop_file, row.names = FALSE)
  } else {
    message(sprintf('Reading loiczid cell proportions by region from: \n  %s\n', rgn_prop_file))
    region_prop_df <- read.csv(rgn_prop_file, stringsAsFactors = FALSE)
  }
  
  return(invisible(region_prop_df))
}


##############################################################################=
iucn_shp_info <- function(reload = TRUE) {
  ### Queries shapefiles and extracts column headers.  Use to help identify which
  ### species groups have no id_no field, and possibly other fields to help 
  ### differentiate.
  iucn_shp_info_file <- file.path(dir_anx, scenario, 'int/iucn_shp_info.csv')
  if(!file.exists(iucn_shp_info_file) | reload) {
    if(!file.exists(iucn_shp_info_file)) message('No file found for IUCN shapefile information.  ')
    message('Generating new list of available IUCN range maps.\n')
    
    dir_iucn_shp <- file.path(dir_data_iucn, 'iucn_shp')
    groups_list <- as.data.frame(list.files(dir_iucn_shp)) %>%
      rename(shp_fn = `list.files(dir_iucn_shp)`) %>%
      filter(tools::file_ext(shp_fn) == 'shp') %>%
      mutate(shp_fn = str_replace(shp_fn, '.shp', ''))
    
    iucn_shp_info <- data.frame()
    
    for (spp_gp in groups_list$shp_fn) { # spp_gp <- groups_list$shp_fn[30]
      # determine size;
      fsize <- round(file.size(file.path(dir_iucn_shp, sprintf('%s.shp', spp_gp)))/1e6, 2)
      message(sprintf('Species group shapefile %s, %.2f MB\n  %s/%s\n', spp_gp, fsize, dir_iucn_shp, spp_gp))
      # spp_group | .shp size | # species | binomial TF | id_no TF | col_names
      spp_dbf <- read.dbf(file.path(dir_iucn_shp, sprintf('%s.dbf', spp_gp)))
      message('file read successfully... ')
      spp_dbf <- as.data.frame(spp_dbf)
      message('converted to data frame... ')
      # determine number of species;
      n_spp    <- nrow(spp_dbf)
      name_tmp <- names(spp_dbf)
      # determine whether binomial and id_no are present;
      binom  <- ifelse('binomial' %in% tolower(names(spp_dbf)), T, F)
      id_no  <- ifelse('id_no'    %in% tolower(names(spp_dbf)), T, F)
      subpop <- ifelse('subpop'   %in% tolower(names(spp_dbf)), T, F)
      # determine column names
      fields <- paste(name_tmp, collapse = ' | ')
      spp_dbf_tmp <- data.frame('spp_gp' = spp_gp, 'shp_MB' = fsize, 'n_spp' = n_spp, 
                                'binomialTF' = binom, 'id_noTF' = id_no, 'subpopTF' = subpop,
                                'fields' = fields)
      iucn_shp_info <- bind_rows(iucn_shp_info, spp_dbf_tmp)
      message('binding to list...\n')
    }
    
    message(sprintf('Writing list of IUCN range map info to: \n  %s\n', iucn_shp_info_file))
    write.csv(iucn_shp_info, iucn_shp_info_file, row.names = FALSE)
  } else {
    message(sprintf('Reading list of IUCN range map from: \n  %s\n', iucn_shp_info_file))
    iucn_shp_info <- read.csv(iucn_shp_info_file, stringsAsFactors = FALSE)
  }
  return(iucn_shp_info)
}

##############################################################################=
generate_iucn_map_list <- function(reload = FALSE) {
  ### support function for create_spp_master_lookup.  Interrogates each shapefile
  ### in raw/iucn_shp/ (for each species group) to determine which species are
  ### present.  Creates and returns output of:
  ###    spp_group | id_no | iucn_subpop | cat_map | spatial_source | sciname | name_verified
  
  iucn_map_list_file <- file.path(dir_anx, scenario, 'int/spp_iucn_maps_all.csv')
  if(!file.exists(iucn_map_list_file) | reload) {
    if(!file.exists(iucn_map_list_file)) message('No file found for list of available IUCN range maps.  ')
    message('Generating new list of available IUCN range maps.\n')
    
    dir_iucn_shp <- file.path(dir_data_iucn, 'iucn_shp')
    groups_list <- as.data.frame(list.files(dir_iucn_shp)) %>%
      rename(shp_fn = `list.files(dir_iucn_shp)`) %>%
      filter(tools::file_ext(shp_fn) == 'shp') %>%
      mutate(shp_fn = str_replace(shp_fn, '.shp', ''))
    
    spp_iucn_maps <- data.frame()
    
    for (spp_group in groups_list$shp_fn) { # spp_group <- groups_list$shp_fn[1]
      message(sprintf('Processing species group: %s...', tolower(spp_group)))
      spp_dbf <- read.dbf(file.path(dir_iucn_shp, sprintf('%s.dbf', spp_group)))
      
      # convert to data frame, lower-case the column names
      spp_dbf <- as.data.frame(spp_dbf)
      names(spp_dbf) <- tolower(names(spp_dbf))
      
      # add group name to the database for future reference
      spp_dbf <- data.frame(spp_group, spp_dbf, stringsAsFactors = FALSE)
      
      spp_dbf <- spp_dbf %>% 
        dplyr::select(spp_group, 
                      id_no, 
                      # sciname = binomial, 
                      # iucn_subspecies = subspecies, iucn_class = class_name, iucn_order = order_name, iucn_family = family_nam,
                      iucn_subpop = subpop,
                      # cat_map = code,
                      presence) %>% 
        mutate(id_no       = as.integer(id_no),
               spp_group   = as.character(spp_group),
               iucn_subpop = as.character(iucn_subpop),
               presence    = as.integer(presence)) # other fields?
      
      spp_iucn_maps <- bind_rows(spp_iucn_maps, spp_dbf)
    }
    spp_iucn_maps <- spp_iucn_maps %>%
      mutate(spatial_source = 'iucn') %>%
      unique()
    ### NOTE: This includes all terrestrial mammals(?) and reptiles(?),
    ### not just those in marine species list.
    
    message(sprintf('Writing list of available IUCN range maps to: \n  %s', iucn_map_list_file))
    write.csv(spp_iucn_maps, iucn_map_list_file, row.names = FALSE)
  } else {
    message(sprintf('Reading list of available IUCN range maps from: \n  %s', iucn_map_list_file))
    spp_iucn_maps <- read.csv(iucn_map_list_file, stringsAsFactors = FALSE)
  }
  return(spp_iucn_maps)
}

##############################################################################=
read_spp_am <- function(reload = FALSE) {
  spp_am_processed_file <- file.path(dir_anx, scenario, 'int/spp_am_cleaned.csv')
  if(!file.exists(spp_am_processed_file) | reload) {
    spp_am_raw_file <- file.path(dir_data_am, 'csv/speciesoccursum.csv')
    message(sprintf('Reading raw AquaMaps species list from: \n  %s\n', spp_am_raw_file))
    
    ### Read AquaMaps list, and fix improper popn categories
    spp_am <- read_csv(spp_am_raw_file) %>%
      dplyr::select(am_sid = speciesid, genus, species, am_category = iucn_code, reviewed) %>%
      mutate(genus = str_trim(genus),
             species = str_trim(species)) %>%
      unite(sciname, genus, species, sep = ' ') %>%
      mutate(am_category = ifelse(am_category ==  'N.E.',   NA, am_category), # not evaluated -> NA
             am_category = ifelse(am_category == 'LR/nt', 'NT', am_category), # update 1994 category
             am_category = ifelse(am_category == 'LR/lc', 'LC', am_category), # update 1994 category
             am_category = ifelse(am_category ==   '\\N',   NA, am_category)) %>%
      as.data.frame()
    
    ### Check names against databases via taxize package.
    am_names_good <- verify_scinames(spp_am %>% select(sciname, am_sid), fn_tag = 'am')
    
    ### ditch unverified names for am_sids with verified names
    am_duped_sid  <- am_names_good$am_sid[duplicated(am_names_good$am_sid)] %>%
      unique()
    am_names_good1 <- am_names_good %>%
      group_by(am_sid) %>%
      mutate(any_verified = any(name_verified)) %>%
      ungroup() %>%
      filter(!am_sid %in% am_duped_sid | name_verified == TRUE | !any_verified) %>%
      select(-any_verified)
    
    spp_am <- spp_am %>%
      select(-sciname) %>%
      unique() %>%
      left_join(am_names_good1, by = c('am_sid'))
    
    message(sprintf('Writing processed AquaMaps species list to: \n  %s\n', spp_am_processed_file))
    write_csv(spp_am, spp_am_processed_file)
    
  } else {
    message(sprintf('Reading processed AquaMaps species list from: \n  %s\n', spp_am_processed_file))
    spp_am <- read_csv(spp_am_processed_file)
  }
  return(spp_am)
}

##############################################################################=
read_spp_iucn <- function(reload = FALSE) {
  spp_iucn_processed_file <- file.path(dir_anx, scenario, 'int/spp_iucn_cleaned.csv')
  if(!file.exists(spp_iucn_processed_file) | reload) {
    ### pull the IUCN data from the git-annex file for this year - output from ingest_iucn.R
    iucn_marine_raw_file <- file.path(dir_anx, scenario, 'int/spp_iucn_marine.csv')
    
    message(sprintf('Reading IUCN marine species list (generated by ingest_iucn.R) from: \n  %s\n', iucn_marine_raw_file))
    spp_iucn <- read_csv(iucn_marine_raw_file) %>%
      dplyr::select(sciname, iucn_sid, iucn_category = category, popn_trend, parent_sid, subpop_sid) 
    
    iucn_names_good   <- verify_scinames(spp_iucn %>% select(sciname, iucn_sid), fn_tag = 'iucn')
    ### duplicate iucn_sid with different names.  For duped iucn_sid, select
    ### only verified scinames (or all, for iucn_sid with no verified names at all)
    iucn_duped_sid  <- iucn_names_good$iucn_sid[duplicated(iucn_names_good$iucn_sid)] %>%
      unique()
    iucn_names_good1 <- iucn_names_good %>%
      group_by(iucn_sid) %>%
      mutate(any_verified = any(name_verified)) %>%
      ungroup() %>%
      filter(!iucn_sid %in% iucn_duped_sid | name_verified == TRUE | !any_verified) %>%
      select(-any_verified)
    spp_iucn <- spp_iucn %>%
      select(-sciname) %>%
      unique() %>%
      left_join(iucn_names_good1, by = c('iucn_sid'))
    
    message(sprintf('Writing processed IUCN marine species list to: \n  %s\n', spp_iucn_processed_file))
    write_csv(spp_iucn, spp_iucn_processed_file)
    
  } else {
    message(sprintf('Reading processed IUCN species list from: \n  %s\n', spp_iucn_processed_file))
    spp_iucn <- read_csv(spp_iucn_processed_file)
  }
  return(spp_iucn)
}

##############################################################################=
create_spp_master_lookup <- function(source_pref = 'iucn', fn_tag = '', reload = FALSE) {
  ### Create lookup: species <-> popn_category/popn_trend and spatial_source.
  ### Output is data frame with these fields:
  ### * am_sid | am_category | reviewed | sciname | name_verified |
  ###   iucn_sid |iucn_category | popn_trend | parent_sid | subpop_sid |
  ###   popn_category | info_source | spp_group | id_no | iucn_subpop |
  ###   presence | spatial_source | category_score | trend_score
  ##############################################################################=
  
  spp_all_file <- file.path(dir_anx, scenario, sprintf('int/spp_all_raw%s.csv', fn_tag))
  
  if(!file.exists(spp_all_file) | reload) {
    
    spp_am <- read_spp_am(reload) 
    
    spp_iucn <- read_spp_iucn(reload)
    
    
    ### To the AquaMaps list (speciesoccursum) bind the IUCN marine species list
    spp_all <- spp_am %>%
      full_join(spp_iucn, by = c('sciname', 'name_verified'))
    
    spp_all <- spp_all %>%
      # create single 'category' field, and flag 'info_source' to indicate iucn or am.
      # For species with subpopulations and both AM and IUCN categories - use the IUCN.  Later, if using AM
      #   spatial data, we'll just use the parent IUCN info.
      dplyr::mutate(
        popn_category = ifelse(!is.na(iucn_category), iucn_category, am_category),
        info_source   = ifelse(!is.na(iucn_category), 'iucn',
                               ifelse(!is.na(am_category), 'am',   NA)))
    
    spp_iucn_maps <- generate_iucn_map_list(reload = reload) %>%
      filter(!str_detect(spp_group, 'MARINEFISH')) ### these items are duped in marine mammals
    ### This function returns a dataframe with the following columns: 
    ### | spp_group | id_no | sciname | iucn_subpop | spatial_source
    ### where iucn_subpop is a text name for the subpop (why not ID? dammit) and
    ### spatial_source is 'iucn'
    
    
    ### join spp_iucn_maps info to spp_all dataframe, using the 
    ### map ID number (rather than sciname as in the past)
    if(nrow(spp_iucn_maps %>% filter(is.na(id_no))) > 0)
      message(sprintf('Species %s range map has no ID number', spp_iucn_maps$sciname))
    spp_all <- spp_all %>% 
      left_join(spp_iucn_maps %>%
                  mutate(iucn_sid = id_no),     ### want both iucn_sid and id_no separate - helps to later determine IUCN aliases and subpops
                by = 'iucn_sid')
    
    ### query birdlife international .dbf, using the ID number, left join
    ### to spp_all which already filters to marine species.
    spp_birds <- foreign::read.dbf(file.path(dir_data_bird, 'BOTW.dbf')) %>%
      setNames(tolower(names(.))) %>%
      select(iucn_sid = sisid, sciname, presence) %>%
      unique()
    ### NOTE: this loses info on 'origin' and 'seasonal' fields
    
    ### toggle the spatial_source variable for bird species; set to 'iucn-bli' for
    ### same handling as IUCN generally, but identifiable as a birdlife int'l
    spp_all <- spp_all %>%
      mutate(spatial_source = ifelse(!is.na(iucn_sid) & iucn_sid %in% spp_birds$iucn_sid, 'iucn-bli', spatial_source),
             spp_group      = ifelse(str_detect(spatial_source, 'bli'), 'BIRDS', spp_group))
    
    ### toggle the spatial_source variable for aquamaps, based on source_pref
    ### and whether there is already an IUCN range map (or birdlife range map) available.
    if(source_pref == 'am') {
      spp_all <- spp_all %>%
        mutate(spatial_source = ifelse(!is.na(am_sid), 'am', spatial_source))
    } else {
      spp_all <- spp_all %>%
        mutate(spatial_source = ifelse(is.na(spatial_source) & !is.na(am_sid), 'am', spatial_source))
    }
    # is.na(spatial_source) means it wasn't in the IUCN maps list;
    # !is.na(am_sid) means it is an aquamaps species.
    
    # duplicates of sciname need to be dealt with. Some cases:
    # * sciname duped, due to same sciname/id_no polygons within multiple IUCN shapefiles 
    #   (e.g. REPTILES, SEASNAKES)
    dupes <- spp_all %>% 
      arrange(spp_group) %>%      ### REPTILES before SEASNAKES
      dplyr::select(sciname, iucn_sid, id_no, iucn_category, iucn_subpop) %>% 
      duplicated(fromLast = TRUE) ### This will identify the REPTILES instance as the duplicated row
    spp_all <- spp_all[!dupes, ]
    
    # to overall lookup table, join scores for population category and trend.
    popn_cat    <- data.frame(popn_category  = c("LC", "NT", "VU", "EN", "CR", "EX"), 
                              category_score = c(   0,  0.2,  0.4,  0.6,  0.8,   1))
    popn_trend  <- data.frame(popn_trend  = c("Decreasing", "Stable", "Increasing"), 
                              trend_score = c(-0.5, 0, 0.5))
    
    spp_all <- spp_all %>%
      left_join(popn_cat,   by = 'popn_category') %>%
      left_join(popn_trend, by = 'popn_trend') 
    
    message(sprintf('Writing full species lookup table to: \n  %s', spp_all_file))
    write_csv(spp_all, spp_all_file)
  } else {
    message(sprintf('Full species lookup table already exists.  Reading from: \n  %s', spp_all_file))
    spp_all <- read.csv(spp_all_file, stringsAsFactors = FALSE)
  }
  
  return(invisible(spp_all))
}


##############################################################################=
extract_loiczid_per_spp <- function(map_list, 
                                    shp_dir = file.path(dir_data_iucn, 'iucn_shp'), 
                                    fn_tag = NULL, reload = FALSE, use_readOGR = FALSE) {
  ### Determine intersections of IUCN maps with Aquamaps half-degree cells
  ### * map_list is dataframe of mapped species, with variables:
  ###     sciname | iucn_sid | spp_group
  ### * when finished with each spp_group, save a .csv for that group:
  ###     sciname | iucn_sid | LOICZID | prop_area
  
  ### Import LOICZID raster
  raster_file <- file.path(dir_anx, 'rgns/loiczid_raster')
  loiczid_raster <- get_loiczid_raster(reload = FALSE)
  
  spp_gp_list <- unique(map_list$spp_group)
  
  for(spp_gp in spp_gp_list) { # spp_gp <- spp_gp_list[1]
    maps_in_group <- map_list %>%
      filter(spp_group == spp_gp)
    
    ### set file path for output file from this species group.
    if(is.null(fn_tag)) {
      cache_file <- file.path(dir_anx, 'iucn_intersections', sprintf('%s.csv', spp_gp))
    } else {
      cache_file <- file.path(dir_anx, 'iucn_intersections', sprintf('%s_%s.csv', spp_gp, fn_tag))
    }
    
    ### if reload == FALSE, and the file exists, don't waste your friggin' time here, move on to next group.
    if(file.exists(cache_file) & reload == FALSE) {   
      message(sprintf('IUCN <-< LOICZID lookup file already exists for species group %s; file location:\n  %s', spp_gp, cache_file))
    } else {
      ptm <- proc.time()
      fsize <- round(file.size(file.path(shp_dir, sprintf('%s.shp', spp_gp)))/1e6, 2)
      message(sprintf('Reading species group shapefile %s, %.2f MB\n  %s/%s', spp_gp, fsize, shp_dir, spp_gp))
      
      ### Because the IUCN metadata states that shapefiles are unprojected lat-long with WGS84,
      ### use readShapePoly (rather than readOGR) and manually tell it the projection...
      if (use_readOGR) spp_shp <- readOGR(dsn = path.expand(shp_dir), layer = spp_gp)
      else  spp_shp <- readShapePoly(fn = file.path(shp_dir, spp_gp), 
                                     proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'),
                                     delete_null_obj = TRUE)
      message(sprintf('Elapsed read time: %.2f seconds\n', (proc.time() - ptm)[3]))
      
      names(spp_shp@data) <- tolower(names(spp_shp@data))
      
      if(spp_gp == 'BOTW') {
        ### fix column names
        spp_shp@data <- spp_shp@data %>%
          rename(id_no = sisid,
                 binomial = sciname) %>%
          mutate(subpop = NA)
      }
      
      message(sprintf('Filtering features by id_no field in %s.\n', spp_gp))
      spp_shp_filter <- spp_shp@data$id_no %in% maps_in_group$iucn_sid
      message(sprintf('... selecting %s species out of %s', 
                      length(unique(spp_shp@data$id_no[spp_shp_filter])), 
                      length(unique(spp_shp@data$id_no))))
      spp_shp <- spp_shp[spp_shp_filter, ]
      
      message('Extracting polygons to LOICZID cells...')
      ptm <- proc.time()
      spp_shp_prop <- raster::extract(loiczid_raster, spp_shp, weights = TRUE, normalizeWeights = FALSE, progress = 'text')
      message(sprintf('Elapsed process time: %.2f minutes\n', (proc.time() - ptm)[3]/60))
      
      ### combine sciname, iucn_sid, presence, and subpop code for a single unique identifier
      shp_id <- data.frame('sciname'  = spp_shp@data$binomial, 
                           'iucn_sid' = spp_shp@data$id_no, 
                           'presence' = spp_shp@data$presence,
                           'subpop'   = spp_shp@data$subpop) %>%
        unite(name_id, sciname, iucn_sid, presence, subpop, sep = '_')
      
      names(spp_shp_prop) <- shp_id$name_id
      
      ### convert list to data frame.
      spp_shp_prop_df <- plyr::ldply(spp_shp_prop, rbind)
      spp_shp_prop_df <- spp_shp_prop_df %>%
        rename(name_id   = .id,
               LOICZID   = value, 
               prop_area = weight) %>%
        separate(name_id, c('sciname', 'iucn_sid', 'presence', 'subpop'), sep = '_')
      
      ### save .csv for this species group
      message(sprintf('%s: %s species maps, %s total cells in output file', 
                      spp_gp, length(unique(spp_shp_prop_df$iucn_sid)), 
                      nrow(spp_shp_prop_df)))
      message(sprintf('Writing IUCN<->LOICZID intersection file for %s to:\n  %s\n', spp_gp, cache_file))
      write_csv(spp_shp_prop_df, cache_file)
    }
  }
}


##############################################################################=
process_am_summary_per_cell <- function(spp_all, fn_tag = '', prob_filter = .40, reload = FALSE) {
  # Calculate category and trend scores per cell for Aquamaps species.
  # * load AM species <-> cell lookup
  # * filter to appropriate cells (in regions, meets probability threshold)
  # * join spatial info: loiczid, region ID, cell area
  # * join species info: category score and trend score
  # * filter by cat score != NA
  # * summarize by loiczid - mean category_score, mean trend_score, count
  
  am_cells_spp_sum_file <- file.path(dir_git, scenario, sprintf('summary/spp_sum_am_cells%s.csv', fn_tag))
  
  if(!file.exists(am_cells_spp_sum_file) | reload) {
    message('Generating cell-by-cell summary for Aquamaps species.\n')
    
    am_cells_spp <- get_am_cells_spp(prob_filter = prob_filter, reload = FALSE)
    
    # filter species info to just Aquamaps species with category info, and bind to 
    # am_cells_spp to attach category_score and trend_score.
    message('Filtering to just species with non-NA IUCN category scores.\n')
    spp_am_info <- spp_all %>% 
      filter(str_detect(spatial_source, 'am')) %>%
      ### NOTE: as of 2015, AM data does not include spatially distinct subpops, so OK to cut 'am_subpops'
      filter(!is.na(category_score) & !(category_score == 'DD')) %>%
      dplyr::select(am_sid, category_score, trend_score) 
    message(sprintf('Length of Aquamaps species list: %d\n', nrow(spp_am_info)))
    spp_am_info <- spp_am_info %>% unique()
    message(sprintf('Length of Aquamaps species list, unique: %d\n', nrow(spp_am_info)))
    
    message('Keyed data frame join cell/species IDs to master species list (filtered for just spatial_source == am or am_parent).\n')
    acs_keyed <- data.table(am_cells_spp, key = "am_sid") 
    sai_keyed <- data.table(spp_am_info, key = "am_sid")
    am_cells_spp1 <- acs_keyed[sai_keyed] %>%
      as.data.frame(am_cells_spp1)
    # z <- x[y] is analogous to z <- left_join(y, x, by = 'key'), so the y variable determines which
    # rows to keep (non-matching rows in x will be discarded).  In this case, all species must be on the spp_am_info list
    # (to learn IUCN cat/trend info); and species not on the list will be discarded.  So: acs_keyed is x, and sai_keyed is y.
    # Somehow, after this step, there is one instance of Clupea harengus (Fis-29344) that has no LOICZID.  It appears on
    # the spp_all list, but has no associated cells.  Weird.    
    
    message('Grouping by cell and summarizing by mean category, mean trend, and n_spp for each, for AM spatial info.\n')
    am_cells_spp_sum <- am_cells_spp1 %>%
      group_by(loiczid) %>%
      summarize(mean_cat_score        = mean(category_score),     # no na.rm needed; already filtered
                mean_popn_trend_score = mean(trend_score, na.rm = TRUE), 
                n_cat_species         = n(),
                n_trend_species       = sum(!is.na(trend_score))) %>% # no na.rm needed; count all with cat_score
      mutate(source = 'aquamaps')
    
    message(sprintf('Writing cell-by-cell summary for Aquamaps species to:\n  %s\n', am_cells_spp_sum_file))
    write_csv(am_cells_spp_sum, am_cells_spp_sum_file)
  } else {
    message(sprintf('Cell-by-cell summary for Aquamaps species already exists.  Reading from:\n  %s\n', am_cells_spp_sum_file))
    am_cells_spp_sum <- read.csv(am_cells_spp_sum_file, stringsAsFactors = FALSE)
  }
  return(invisible(am_cells_spp_sum))
}


##############################################################################=
get_am_cells_spp <- function(n_max = -1, prob_filter = .40, reload = TRUE) {
  am_cells_spp_file <- file.path(dir_anx, scenario, sprintf('int/am_cells_spp_prob%s.csv', prob_filter))
  if(!file.exists(am_cells_spp_file) | reload) {
    message('Creating Aquamaps species per cell file\n')
    ### Load Aquamaps species per cell table
    spp_cell_file <- file.path(dir_data_am, 'csv/hcaf_sp_native_trunc.csv')
    message(sprintf('Loading AquaMaps cell-species data.  Large file! \n  %s \n', spp_cell_file))
    am_cells_spp <- read_csv(spp_cell_file, n_max = n_max) %>%
      rename(am_sid = speciesid, prob = probability)
    
    # filter out below probability threshold; 78 M to 56 M observations.
    am_cells_spp <- am_cells_spp %>%
      filter(prob >= prob_filter) %>%
      dplyr::select(-prob)
    
    message(sprintf('Writing Aquamaps species per cell file to: \n  %s\n', am_cells_spp_file))
    write_csv(am_cells_spp, am_cells_spp_file)
  } else {
    message(sprintf('Reading Aquamaps species per cell file from: \n  %s\n', am_cells_spp_file))
    am_cells_spp <- read_csv(am_cells_spp_file)
  }
  
  return(am_cells_spp)
}


##############################################################################=
get_iucn_cells_spp <- function(reload = FALSE) {
  iucn_cells_file <- file.path(dir_data_iucn, sprintf('iucn_cells_%s.csv', scenario))
  if(!file.exists(iucn_cells_file) | reload) {
    message(sprintf('Building IUCN species to cell table.  This might take a few minutes.\n'))
    iucn_map_files      <- file.path(dir_anx, 'iucn_intersections', list.files(file.path(dir_anx, 'iucn_intersections')))
    iucn_cells_spp_list <- lapply(iucn_map_files, read.csv) # read each into dataframe within a list
    iucn_cells_spp      <- bind_rows(iucn_cells_spp_list)   # combine list of dataframes to single dataframe
    names(iucn_cells_spp) <- tolower(names(iucn_cells_spp))
    message('Writing IUCN species-cell file to ', iucn_cells_file)
    write_csv(iucn_cells_spp, iucn_cells_file)
  } else {
    iucn_cells_spp <- read_csv(iucn_cells_file, col_types = 'cddddc') %>%
      as.data.frame(stringsAsFactors = FALSE) 
  }
  
  return(iucn_cells_spp)
}

##############################################################################=
process_iucn_summary_per_cell <- function(spp_all, fn_tag = '', reload = FALSE) {
  # Calculate mean category and trend scores per cell for IUCN species.
  # * spp_all is df filtered to desired species (e.g. no DD? no subpops?)
  # * load IUCN species <-> cell lookup
  # * join species info: category score and trend score
  # * filter by cat score != NA
  # * summarize by loiczid:  mean category_score, mean trend_score, count

  iucn_cells_spp_sum_file <- file.path(dir_git, scenario, sprintf('summary/spp_sum_iucn_cells%s.csv', fn_tag))
  
  if(!file.exists(iucn_cells_spp_sum_file) | reload) {
    message('Generating cell-by-cell summary for IUCN range-map species.')
    
    ### Load IUCN species per cell tables
    iucn_cells_spp <- get_iucn_cells_spp(reload = reload) %>%
      select(-subpop)
    
    # bind to iucn_cells_spp to attach category_score and trend_score.

    spp_iucn_info <- spp_all %>% 
      filter(str_detect(spatial_source, 'iucn')) %>% 
      dplyr::select(iucn_sid, category_score, trend_score) %>%
      unique()
    message(sprintf('Length of IUCN species list: %d\n', nrow(spp_iucn_info)))
    
    message('Keyed joining to species master list (filtered for spatial_source == iucn).')
    ics_keyed <- data.table(iucn_cells_spp,
                            key = "iucn_sid") 
    sii_keyed <- data.table(spp_iucn_info,  
                            key = "iucn_sid")
    iucn_cells_spp1 <- ics_keyed[sii_keyed] 
    # z <- x[y] is analogous to z <- left_join(y, x, by = 'key'), so the y variable determines which
    # rows to keep (non-matching rows in x will be discarded).  In this case, all species must be on the spp_iucn_info list
    # (to learn IUCN cat/trend info); and species not on the list will be discarded.  So: ics_keyed is x, and sii_keyed is y.
    # The following species are dropped; they have shapefile info but no IUCN ID number according to this analysis:
    #   Alopex lagopus, Conus eduardi, Conus evansi, Conus hypochlorus, Conus luteus, Conus moncuri, Conus moylani,
    #   Conus sartii, Conus subulatus, Diplodus argenteus, Diplodus sargus, Eptatretus fernholmi, Halichoeres bleekeri,
    #   Holothuria squamifera, Sarda chiliensis chiliensis, Scyphiphora hydrophyllacea, Tetrapturus albidus
    
    x <- ics_keyed %>% select(iucn_sid) %>% unique()
    y <- spp_iucn_info %>% filter(!iucn_sid %in% x$iucn_sid)
    z <- spp_all %>% filter(iucn_sid %in% y$iucn_sid) %>% filter(popn_category != 'DD')
    
    ### assign proper values for extinct polygons based on presence = 5
    iucn_cells_spp1 <- iucn_cells_spp1 %>%
      mutate(category_score = ifelse(presence == 5, 1, category_score),
             trend_score    = ifelse(presence == 5, 0, trend_score))
    
    message('Eliminating species double-counting due to overlapping polygons in IUCN shapefiles.')
    iucn_cells_spp1 <- iucn_cells_spp1 %>%
      as.data.frame() %>%
      # this next part to collapse any duplicated cells (overlapping polygons)
      group_by(iucn_sid, loiczid, category_score, trend_score) %>%
      summarize(prop_area = max(prop_area))
    
    message('Eliminating NA category scores (DD, etc)')
    iucn_cells_spp1 <- iucn_cells_spp1 %>%
      filter(!is.na(category_score))
    
    message('Grouping by cell and summarizing mean category/trend and n_spp for each, for IUCN spatial info.')
    iucn_cells_spp_sum <- iucn_cells_spp1 %>%
      group_by(loiczid) %>%
      summarize(mean_cat_score = mean(category_score),      # no na.rm needed; already filtered.
                mean_popn_trend_score = mean(trend_score, na.rm = TRUE), 
                n_cat_species = n(),
                n_trend_species = sum(!is.na(trend_score))) %>% # no na.rm needed; count all with cat_score
      mutate(source = 'iucn')
    
    message(sprintf('Writing cell-by-cell summary for IUCN species to:\n  %s', iucn_cells_spp_sum_file))
    write_csv(iucn_cells_spp_sum, iucn_cells_spp_sum_file)
  } else {
    message(sprintf('Cell-by-cell summary for IUCN species already exists.  Reading from:\n  %s', iucn_cells_spp_sum_file))
    iucn_cells_spp_sum <- read.csv(iucn_cells_spp_sum_file, stringsAsFactors = FALSE)
  }
  return(invisible(iucn_cells_spp_sum))
}


##############################################################################=
process_means_per_cell <- function(am_cell_summary, iucn_cell_summary, fn_tag = '') { 
  ### 2 input data frames:
  ### loiczid | mean_cat_score | mean_popn_trend_score | n_cat_species | n_trend_species | source
  ### calcs weighted score for each cell (by loiczid) from:
  ###   (mean IUCN category value * # of species) for both IUCN and AM data, divided by total species.
  ###   (same for trend)
  if(sum(is.na(am_cell_summary$mean_cat_score)) > 0) {
    message('NA values for mean category score in aquamaps cell summary df')
    stop()
  }
  if(sum(is.na(iucn_cell_summary$mean_cat_score)) > 0) {
    message('NA values for mean category score in iucn cell summary df')
    stop()
  }
  summary_by_loiczid <- bind_rows(am_cell_summary, iucn_cell_summary) %>%
    group_by(loiczid) %>%
    summarize(weighted_mean_cat   = sum(n_cat_species   * mean_cat_score)/sum(n_cat_species),
              weighted_mean_trend = sum(n_trend_species * mean_popn_trend_score, na.rm = TRUE)/sum(n_trend_species)) %>%
    arrange(loiczid)
  
  write_csv(summary_by_loiczid, file.path(dir_git, scenario, sprintf('summary/cell_spp_summary_by_loiczid%s.csv', fn_tag)))
  return(summary_by_loiczid)
}


##############################################################################=
process_means_per_rgn <- function(summary_by_loiczid, rgn_cell_lookup, rgn_note = '') {  
  ### Joins region-cell info to mean cat & trend per cell.
  ### Groups by region IDs, and calcs area-weighted mean category and trend values
  ### for all cells across entire region.  Cells only partly within a region are
  ### accounted for by multiplying cell area * proportionArea from rgn_cell_lookup.
  
  if(sum(is.na(summary_by_loiczid$weighted_mean_cat)) > 0) {
    message('NA values for mean category score in cell summary df')
    stop()
  }
  
  rgn_weighted_sums <- summary_by_loiczid %>%
    inner_join(rgn_cell_lookup %>% dplyr::select(-csq),
               by = 'loiczid') %>%
    mutate(rgn_area = cell_area * proportionArea,
           area_weighted_mean_cat   = weighted_mean_cat   * rgn_area,
           area_weighted_mean_trend = weighted_mean_trend * rgn_area) %>%
    arrange(loiczid)
  
  region_sums <- rgn_weighted_sums %>%
    group_by(rgn_id) %>%
    summarize(rgn_mean_cat   = sum(area_weighted_mean_cat)/sum(rgn_area),
              rgn_mean_trend = sum(area_weighted_mean_trend)/sum(rgn_area))
  
  region_sums <- region_sums %>%
    mutate(status = ((1 - rgn_mean_cat) - 0.25) / 0.75)
  
  region_summary_file <- file.path(dir_git, 
                                   scenario, 
                                   sprintf('summary/rgn_summary%s.csv', ifelse(rgn_note == '', '',
                                                                               sprintf('_%s', rgn_note))))
  
  message(sprintf('Writing summary file of area-weighted mean category & trend per region:\n  %s\n', region_summary_file))
  write_csv(region_sums, region_summary_file)
  
  return(region_sums)
}


##############################################################################=
show_dupes <- function(x, y) {
  # x is data frame, y is field within that dataframe
  z <- x %>% filter(x[[y]] %in% x[[y]][duplicated(x[[y]])])
}

##############################################################################=
verify_scinames <- function(names_df, fn_tag) {
  ### First: check to see if there's already a non-match file.
  ### If so, see if there is a 'force_match' column.
  ### If not, run against gnr_resolve and message that the user
  ###   should check that list and tell which matches to make.
  ### If so, skip gnr_resolve and update the names based on
  ### the force_match column; use original otherwise.
  nm_chk_file <- file.path(dir_anx, scenario, sprintf('int/namecheck_%s.csv',  fn_tag))
  if(file.exists(nm_chk_file)) {
    nm_chk <- read.csv(nm_chk_file, stringsAsFactors = FALSE)
    if(!'force_match' %in% names(nm_chk)) {
      stop(sprintf('Check name file %s: create a "force_match" column;\n', nm_chk_file),
           '  then set "force_match" to TRUE to use suggested name for unmatched names.') 
      return(NULL)
    } else {
      ### use nm_chk to assign matched = TRUE to good matches and forced matches, FALSE for names left as default
      message(sprintf('Found "force_match" column in %s;\n', nm_chk_file),
              '  using this as verified name list')
      nm_chk <- nm_chk %>%
        mutate(force_match = ifelse(is.na(force_match) | !force_match, FALSE, TRUE),
               sciname = ifelse(force_match, sciname2, sciname),
               name_verified = (matched | force_match)) %>%
        select(-data_source_title, -score, -sciname2, -matched, -force_match) %>%
        unique()
      
      return(nm_chk)
    }
  } else {
    ### create the file of non-matched names
    message('No verified name list found.  Generating using taxize::gnr_resolve()')
    nm_chk <- taxize::gnr_resolve(names = names_df$sciname,
                                  canonical = TRUE,
                                  best_match_only = TRUE,
                                  preferred_data_sources = c(12, 4),
                                  http = "post") %>%
      mutate(match = (submitted_name == matched_name2))
    nm_chk <- names_df %>%
      left_join(nm_chk %>%
                  rename(sciname = submitted_name,
                         sciname2 = matched_name2),
                by = 'sciname') %>%
      group_by(sciname) %>%
      mutate(match_count = sum(match, na.rm = TRUE)) %>%
      ungroup()
    
    nm_res_good <- nm_chk %>%
      filter(match_count > 0 & match == TRUE) %>%
      mutate(data_source_title = NA,
             score = NA) %>%
      mutate(matched = TRUE) %>%
      unique()
    nm_res_bad  <- nm_chk %>%
      filter(match_count == 0) %>%
      mutate(matched = FALSE)
    nm_res <- rbind(nm_res_good, nm_res_bad) %>%
      select(-match, -match_count)
    message(sprintf('Found %s instances of unrecognized sci names', length(unique(nm_res_bad$sciname))))
    message('Writing list to: ', nm_chk_file)
    write_csv(nm_res,  nm_chk_file)
    stop('Check non-matched names in name check file, then run again...\n',
         sprintf('  %s', nm_chk_file))
    return(NULL)
  }
  
}

