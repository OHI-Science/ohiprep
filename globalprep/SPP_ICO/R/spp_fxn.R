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
  ###     spp_group | id_no | binomial | spatial_source
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
      message(sprintf('Processing species group: %s... \n', tolower(spp_group)))
      spp_dbf <- read.dbf(file.path(dir_iucn_shp, sprintf('%s.dbf', spp_group)))
      message('file read successfully... ')
      
      # convert to data frame, lower-case the column names
      spp_dbf <- as.data.frame(spp_dbf)
      names(spp_dbf) <- tolower(names(spp_dbf))
      message('converted to data frame... ')
      
      # add group name to the database for future reference
      spp_dbf <- data.frame(spp_group, spp_dbf, stringsAsFactors = FALSE)
      
      spp_dbf <- spp_dbf %>% 
        dplyr::select(spp_group, id_no, 
                      sciname = binomial, 
                      #iucn_subspecies = subspecies, iucn_class = class_name, iucn_order = order_name, iucn_family = family_nam,
                      iucn_subpop = subpop) %>% 
        mutate(id_no       = as.integer(id_no),
               spp_group   = as.character(spp_group),
               sciname     = as.character(sciname),
               iucn_subpop = as.character(iucn_subpop))# other fields?
        
      spp_iucn_maps <- bind_rows(spp_iucn_maps, spp_dbf)
      message('binding to list...\n')
    }
    spp_iucn_maps <- spp_iucn_maps %>%
      mutate(spatial_source = 'iucn') %>%
      unique()
    ### NOTE: This includes all terrestrial mammals(?) and reptiles(?),
    ### not just those in marine species list.
    
    ### Compare to name check file for IUCN (based on IUCN marine list)
    
    nm_chk_file <- file.path(dir_anx, scenario, 'int/namecheck_iucn.csv')
    nm_chk <- read.csv(nm_chk_file, stringsAsFactors = FALSE)
    if(!'force_match' %in% names(nm_chk)) {
      stop(sprintf('Check name file %s: create a "force_match" column;\n', nm_chk_file),
           '  then set "force_match" to TRUE to use suggested name for unmatched names.') 
    } else {
      ### use nm_chk to assign matched = TRUE to good matches and forced matches, FALSE for names left as default
      message(sprintf('Verifying names against %s.\n', nm_chk_file))
      nm_chk <- nm_chk %>%
        mutate(force_match = ifelse(is.na(force_match) | !force_match, FALSE, TRUE),
               sciname = ifelse(force_match, sciname2, sciname),
               name_verified = (matched | force_match)) %>%
        select(-data_source_title, -score, -sciname2, -matched, -force_match) %>%
        unique()
    }
      
    iucn_duped_sid  <- nm_chk$iucn_sid[duplicated(nm_chk$iucn_sid)] %>%
      unique()
    nm_chk1 <- nm_chk %>%
      group_by(iucn_sid) %>%
      mutate(any_verified = any(name_verified)) %>%
      ungroup() %>%
      filter(!iucn_sid %in% iucn_duped_sid | name_verified == TRUE | !any_verified) %>%
      select(-any_verified)
    spp_iucn_maps1 <- spp_iucn_maps %>%
      select(-sciname) %>%
      unique() %>%
      inner_join(nm_chk1 %>% rename(id_no = iucn_sid), by = c('id_no'))
    ### using inner_join, so species NOT on the nm_chk list (which is marine species)
    ### get dropped - lose the terrestrial critters
      
    message(sprintf('Writing list of available IUCN range maps to: \n  %s\n', iucn_map_list_file))
    write.csv(spp_iucn_maps1, iucn_map_list_file, row.names = FALSE)
  } else {
    message(sprintf('Reading list of available IUCN range maps from: \n  %s\n', iucn_map_list_file))
    spp_iucn_maps1 <- read.csv(iucn_map_list_file, stringsAsFactors = FALSE)
  }
  return(spp_iucn_maps1)
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
  ### * sciname  iucn_sid  am_sid  popn_category  popn_trend info_source  spatial_source
  ### Aquamaps data is 'ohi_speciesoccursum.csv' with: 
  ### * SPECIESID   SpecCode   sciname   am_category
  ### IUCN input is 'spp_iucn_marine_global.csv' with:
  ### * sid(Red.List.Species.ID)  sciname  iucn_category  popn_trend ??? need 'subpop' field for parent/subpop ('p', 's', NA)
  ### Output details:
  ### * popn_category: IUCN category. Prioritize iucn_category then am_category; otherwise NA
  ### * popn_trend: for species with IUCN popn_trend info: 'Increasing', 'Decreasing', 'Stable'
  ### * info_source: source of IUCN category info (and popn trend if applicable): am, iucn, NA
  ### * spatial_source: prioritize IUCN range maps, then AquaMaps: iucn, am, NA
  ### * Filter out anything with spatial_source NA, and anything with category NA? 
  ###   Or leave in for future filtering?
  ###   - ??? test how many get filtered out and how big is the file.
  ##############################################################################=
  spp_all_file <- file.path(dir_anx, scenario, sprintf('int/spp_all%s.csv', fn_tag))
  
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
    
    spp_iucn_maps <- generate_iucn_map_list(reload = reload)
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
                by = c('iucn_sid', 'sciname', 'name_verified'))
    
    
    ### toggle the spatial_source variable for aquamaps, based on source_pref
    ### and whether there is already an IUCN range map available.
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
    #   (e.g. REPTILES, SEASNAKES, and non-homolopsids)
    #   - choose one and drop the others.  Which to choose? let R decide.
    dupes <- spp_all %>% 
      arrange(spp_group) %>%      ### REPTILES before SEASNAKES
      dplyr::select(sciname, iucn_sid, id_no, iucn_category, iucn_subpop) %>% 
      duplicated(fromLast = TRUE) ### This will identify the REPTILES instance as the duplicated row
    spp_all <- spp_all[!dupes, ]
    
    
    # to overall lookup table, join scores for population category and trend.
    popn_cat    <- data.frame(popn_category  = c("LC", "NT", "VU", "EN", "CR", "EX"), 
                              category_score = c(   0,  0.2,  0.4,  0.6,  0.8,   1))
    popn_trend  <- data.frame(popn_trend=c("Decreasing", "Stable", "Increasing"), 
                              trend_score=c(-0.5, 0, 0.5))
    
    spp_all <- spp_all %>%
      left_join(popn_cat,   by = 'popn_category') %>%
      left_join(popn_trend, by = 'popn_trend') 
    
    spp_all <- spp_all %>%
      fix_am_subpops() %>%
      fix_iucn_subpops(spp_iucn_maps) 
    
    message(sprintf('Writing full species lookup table to: \n  %s\n', spp_all_file))
    write_csv(spp_all, spp_all_file)
  } else {
    message(sprintf('Full species lookup table already exists.  Reading from: \n  %s\n', spp_all_file))
    spp_all <- read.csv(spp_all_file, stringsAsFactors = FALSE)
  }
  
  return(invisible(spp_all))
}


##############################################################################=
fix_am_subpops <- function(spp_all, use_am_subpops = FALSE) {
  if(use_am_subpops) {
    ### Identify and create 
    ### Filter out just the species with parent or subpop IDs according to IUCN:
    am_subpops <- spp_all %>% filter(spatial_source == 'am' & (!is.na(parent_sid) | !is.na(subpop_sid)))
    am_subpop_spp <- unique(am_subpops$sciname)
    
    ### Loop over all subpop/parent records, one species at a time:
    for (spp in am_subpop_spp) {
      ### Parents will have a non-NA subpop ID number, basically the parent's child.
      ### Subpops will have a non-NA parent ID number
      spp_parent <- spp_all %>% 
        filter(sciname == spp & !is.na(subpop_sid))
      spp_subpop <- spp_all %>% 
        filter(sciname == spp & !is.na(parent_sid))
      
      
      if(nrow(spp_parent) == 0) {
        ### This situation includes orphan subpopulations (subpops but no parent)
        message(sprintf('No parent info found for %s:  Parent id: %d, subpops = %s.\n', 
                        spp, spp_subpop$parent_sid[1], 
                        paste(unlist(spp_subpop$iucn_sid), collapse = ', ')))
        
        ### create a parent entry from the subpop entries, then bind to master list.  Because
        ### we don't know the info, assign it values of DD and unknown.
        spp_no_parent <- spp_all %>%
          filter(sciname == spp) %>%
          mutate(subpop_sid = iucn_sid,
                 iucn_sid   = parent_sid,
                 spatial_source = 'am_parent_orphan',
                 parent_sid = NA,
                 iucn_category = 'fix manually',
                 popn_category = 'fix manually',
                 popn_trend    = 'fix manually', 
                 category_score = NA,
                 trend_score   = NA)
        print(spp_no_parent %>% dplyr::select(sciname:popn_category, spatial_source))
        ### See list below: all parent populations are DD/unknown.
        #                      sciname am_category iucn_sid iucn_category popn_trend parent_sid
        #   1     Polyprion americanus          DD    43973            CR Decreasing      43972: Brazilian subpop.        Parent pop is DD/unknown.
        #   2 Carcharhinus amboinensis          DD    39543            NT    Unknown      39366: SW Indian Ocean subpop.  Parent pop is DD/unknown.
        #   3   Ginglymostoma cirratum          DD    60224            NT Decreasing      60223: W. Atlantic subpop.      Parent pop is DD/unknown.
        #   4   Notorynchus cepedianus          DD    39541            NT    Unknown      39324: E. Pacific subpop.       Parent pop is DD/unknown.
        #   5     Eurypegasus draconis          DD     8409            VU Decreasing       8407: Philippines subpop.      Parent pop is DD/unknown.
        #   6 Centrophorus moluccensis          DD 16727330            LC     Stable      42838: E/W Australian subpop.   Parent pop is DD/unknown.
        #   7         Pegasus volitans          DD    16477            VU Decreasing      16476: Phil, S. China subpops.  Parent pop is DD/unknown.
        
        spp_all <- rbind(spp_all, spp_no_parent)
      } # end 'if no parent' section
      
      spp_all <- spp_all %>%
        mutate(spatial_source = ifelse(sciname == spp & !is.na(parent_sid) & !str_detect(spatial_source, 'orphan'), 'am_subpop', spatial_source),
               spatial_source = ifelse(sciname == spp & !is.na(subpop_sid) & !str_detect(spatial_source, 'orphan'), 'am_parent', spatial_source))
    }   # end for loop
    ### for all species within the list (aquamaps w/parent or subpop), amend spatial source for subpops - so subpops can be ignored for spatial analysis.
    print(head(spp_all %>% filter(sciname %in% am_subpop_spp)))
    
  }
  return(spp_all)
}


##############################################################################=
fix_iucn_subpops <- function(spp_all, spp_iucn_maps) {
  ### Identify IUCN subpops and parents, and note in spatial_source.
  
  ### Any mismatch between IUCN species ID and shapefile ID number gets
  ### labeled as an alias first;
  spp_all_dupes <- show_dupes(spp_all, 'sciname')
  spp_all <- spp_all %>%
    mutate(spatial_source = ifelse((sciname %in% spp_all_dupes$sciname) &   ### duplicated scientific name
                                     (sciname %in% spp_iucn_maps$sciname) & ### sciname in species map list
                                     (is.na(id_no)),                  ### mismatch between iucn_sid and id_no
                                   ### New data (as of Dec 2015) should have id_no for all species?
                                   'iucn_alias',
                                   spatial_source))
  print(head(spp_all %>% filter(sciname %in% spp_all_dupes$sciname)))
  
  ### ID subpops and parents:
  iucn_subpops    <- spp_all %>% filter(str_detect(spatial_source, 'iucn') & (!is.na(parent_sid) | !is.na(subpop_sid)))
  iucn_subpop_spp <- unique(iucn_subpops$sciname)
  
  ### Loop over list of parent/subpop species names
  for (spp in iucn_subpop_spp) { ### spp = 'Tursiops truncatus'
    spp_parent <- spp_all %>% 
      filter(sciname == spp & !is.na(subpop_sid))
    spp_subpop <- spp_all %>% 
      filter(sciname == spp & !is.na(parent_sid))
    
    if(nrow(spp_parent) == 0) {
      message(sprintf('No parent info found for %s:  Parent id: %d, subpops = %s.\n', spp, spp_subpop$parent_sid[1], paste(unlist(spp_subpop$iucn_sid), collapse = ', ')))
      ### No instances of subpops without parent info, for IUCN listings.  But for future reference:
      ### create a parent entry from the subpop entries, then bind to master list
      spp_no_parent <- spp_all %>% 
        ### can this be done by looking up species on IUCN site? maybe manually
        ### - would need to flag this added row somehow
        filter(sciname == spp) %>%
        mutate(subpop_sid = iucn_sid,
               iucn_sid   = parent_sid,
               spatial_source = 'iucn_parent_orphan',
               parent_sid = NA,
               iucn_category = 'fix manually',
               popn_category = 'fix manually',
               popn_trend    = 'fix manually', 
               category_score = NA,
               trend_score   = NA)
      spp_all <- rbind(spp_all, spp_no_parent)
    } # end 'if no parent' section
    ### For all species within the list (IUCN w/parent or subpop), amend spatial source for subpops.
    ### Most subpops here are in MAMMMARINE group, with a few in Tunas_and_billfishes2, GROUPERS, and SEABREAMS_PORGIES.
    ### Of these groups, no id_no available for spatially differentiated shapefile extraction.
    ### Exception is two species in SEABREAMS_PORGIES, noted as parents, and whose subpops are actually subspecies,
    ### which have been filtered out.  So not a problem at this time.
    spp_all <- spp_all %>%
      mutate(spatial_source = ifelse(sciname == spp & !is.na(parent_sid) & !str_detect(spatial_source, 'orphan'), 'iucn_subpop', spatial_source),
             spatial_source = ifelse(sciname == spp & !is.na(subpop_sid) & !str_detect(spatial_source, 'orphan'), 'iucn_parent', spatial_source))
  }   # end for loop
  
  print(head(spp_all %>% filter(sciname %in% iucn_subpop_spp)))
  ### There are a large number of duplicated scinames still - some of these seem to be IUCN ID#s with multiple
  ### aliases.  Example: 
  #        am_sid          sciname iucn_sid  id_no popn_category popn_trend
  #   1 ITS-75291  Conus jaspideus   192338 192637            LC    Unknown
  #   2 ITS-75291  Conus jaspideus   192637 192637            LC    Unknown
  #   3 ITS-75352 Conus granulatus   192590 192624            LC    Unknown
  #   4 ITS-75352 Conus granulatus   192624 192624            LC    Unknown
  #   5 ITS-75376     Conus regius   192869 192869            LC    Unknown
  #   6 ITS-75376     Conus regius   192452 192869            LC       <NA>
  ### This may be an artifact of matching by sciname rather than species IDs - how to get around it?
  ### For observations where iucn_sid and id_no are both present, and match, use the spatial data.
  ### For obs where both are present but don't match, don't use spatial data - mutate the spatial_source field
  ### to something that can be filtered out later.
  
  return(spp_all)
}



##############################################################################=
extract_loiczid_per_spp <- function(spp_all, groups_override = NULL, fn_tag = NULL, reload = FALSE) {
  ### Determine intersections of IUCN maps with Aquamaps half-degree cells
  ### 
  ### * from the spp_all.csv, extract the species with IUCN range maps.  Use this file
  ###   rather than the spp_iucn_maps_all because it is truncated to just species on spp_iucn_marine_global.csv list.
  ### * for each species group (spp_group), open parent shape file
  ### * select by attribute - binomial == sciname in the list
  ### * use raster::extract, lay the selected polygons over the LOICZID raster.
  ###     * NOTE: this takes a long time - 15-20 minutes for the region shapefile, with ~200 fairly simple polygons.
  ###       We're looking at ~ 4000 complicated polygons.
  ### * when finished with each spp_group, save a file of sciname | id_no | LOICZID | prop_area for that group.
  ### * Use groups_override argument to pass a partial list of species groups, for testing/debugging.
  
  ### create list of all marine species with IUCN maps.
  iucn_range_maps <- spp_all %>%
    filter(str_detect(spatial_source, 'iucn')) %>%
    dplyr::select(sciname, iucn_sid, id_no, spp_group)
  
  ### Import LOICZID raster
  raster_file <- file.path(dir_anx, 'rgns/loiczid_raster')
  loiczid_raster <- get_loiczid_raster(reload = FALSE)
  
  ### create list of groups (i.e. shape files) to be analyzed
  if(is.null(groups_override)) {
    spp_gp_list <- unique(iucn_range_maps$spp_group)
  } else {
    spp_gp_list <- groups_override
  }
  
  ogr_location <- file.path(dir_data_iucn, 'iucn_shp')
  
  for(spp_gp in spp_gp_list) { # spp_gp <- 'LOBSTERS' # spp_gp <- 'CORAL3'   spp_gp <- 'MAMMMARINE'
    maps_in_group <- iucn_range_maps %>%
      filter(spp_group == spp_gp)
    
    ### set file path for output file from this species group.
    if(is.null(fn_tag)) {
      cache_file <- file.path(dir_anx, 'iucn_intersections', sprintf('%s.csv', spp_gp))
    } else {
      cache_file <- file.path(dir_anx, 'iucn_intersections', sprintf('%s_%s.csv', spp_gp, fn_tag))
    }
    
    ### if reload == FALSE, and the file exists, don't waste your friggin' time here, move on to next group.
    if(file.exists(cache_file) & reload == FALSE) {   
      message(sprintf('\nIUCN <-< LOICZID lookup file already exists for species group %s; file location:\n  %s\n', spp_gp, cache_file))
    } else {
      ptm <- proc.time()
      fsize <- round(file.size(file.path(ogr_location, sprintf('%s.shp', spp_gp)))/1e6, 2)
      message(sprintf('\nReading species group shapefile %s, %.2f MB\n  %s/%s\n', spp_gp, fsize, ogr_location, spp_gp))

            ### Because the IUCN metadata states that shapefiles are unprojected lat-long with WGS84, I
      ### will use the faster readShapePoly (rather than readOGR) and manually tell it the projection...
      spp_shp <- readShapePoly(fn = file.path(ogr_location, spp_gp), 
                               proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'),
                               delete_null_obj = TRUE)
      ptm <- proc.time() - ptm
      message(sprintf('Elapsed read time: %.2f seconds\n', ptm[3]))
      
      ### lower-case all the names for convenience...
      names(spp_shp@data) <- tolower(names(spp_shp@data))
      message(paste(names(spp_shp@data), collapse = ', '))
      
      ### Filter shape file to polygons with species ID numbers that match
      ### our list of maps.
      message(sprintf('Filtering features by id_no field in %s.\n', spp_gp))
      spp_shp_filter <- spp_shp@data$id_no %in% maps_in_group$iucn_sid
      spp_shp1 <- spp_shp[spp_shp_filter, ]
      message(sprintf('... selecting %s species out of %s', 
                      length(unique(spp_shp1@data$id_no)), 
                      length(unique(spp_shp@data$id_no))))
      message('In this grouping, breakdown of IUCN categories:')
      print(table(spp_shp1@data$code))
      
      ### Extract the proportions of each species polygon within each LOICZID cell
      ptm <- proc.time()
      spp_shp_prop <- raster::extract(loiczid_raster, spp_shp1, weights = TRUE, normalizeWeights = FALSE, progress = 'text')
      ptm <- proc.time() - ptm
      message(sprintf('Elapsed process time: %.2f minutes\n', ptm[3]/60))
      
      
      ### combine sciname, id_no, presence, and subpop code for a single unique identifier
      sciname_sid_pres <- data.frame('sciname'  = spp_shp1@data$binomial, 
                                     'id_no'    = spp_shp1@data$id_no, 
                                     'presence' = spp_shp1@data$presence,
                                     'subpop'   = spp_shp1@data$subpop)
      
      print(head(sciname_sid_pres))
      sciname_sid_pres <- sciname_sid_pres %>%
        unite(name_id, sciname, id_no, presence, subpop, sep = '_')
      
      ### use unique identifier to name the items in list
      names(spp_shp_prop) <- sciname_sid_pres$name_id
      
      ### convert list to data frame.
      spp_shp_prop_df     <- plyr::ldply(spp_shp_prop, rbind)
      spp_shp_prop_df <- spp_shp_prop_df %>%
        rename(name_id_pres = .id,
               LOICZID = value, 
               prop_area = weight) %>%
        separate(name_id_pres, c('sciname', 'id_no', 'presence', 'subpop'), sep = '_')
      
      ### save .csv for this species group
      message(sprintf('%s: %s species maps, %s total cells in output file', spp_gp, length(unique(spp_shp_prop_df$id_no)), nrow(spp_shp_prop_df)))
      print(head(spp_shp_prop_df))
      message(sprintf('Writing IUCN<->LOICZID intersection file for %s to:\n  %s\n', spp_gp, cache_file))
      write_csv(spp_shp_prop_df, cache_file)
    }
  }
}


##############################################################################=
process_am_summary_per_cell <- function(fn_tag = '', prob_filter = .40, reload = FALSE) {
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
    
    am_cells_spp <- get_am_cells_spp(prob_filter = prob_filter)
    
    # filter species info to just Aquamaps species with category info, and bind to 
    # am_cells_spp to attach category_score and trend_score.
    message('Filtering to just species with non-NA IUCN category scores.\n')
    spp_am_info <- spp_all %>% 
      filter(spatial_source == 'am' | spatial_source == 'am_parent') %>%
      ### NOTE: as of 2015, AM data does not include spatially distinct subpops, so OK to cut 'am_subpops'
      filter(!is.na(category_score) & !(category_score == 'DD')) %>%
      dplyr::select(am_sid, sciname, category_score, trend_score) 
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
    spp_cell_file <- file.path(dir_data_am, 'csv/ohi_hcaf_species_native.csv')
    message(sprintf('Loading AquaMaps cell-species data.  Large file! \n  %s \n', spp_cell_file))
    am_cells_spp <- read_csv(spp_cell_file, col_types = '_ccn__', n_max = n_max) %>%
      rename(am_sid = SpeciesID, csq = CsquareCode, prob = probability)
    
    # filter out below probability threshold; 78 M to 56 M observations.
    am_cells_spp <- am_cells_spp %>%
      filter(prob >= prob_filter) %>%
      dplyr::select(-prob)
    
    # then join to am_cells (from hcaf.csv) to attach loiczid
    cell_file <- file.path(dir_data_am, 'csv/hcaf.csv')
    message(sprintf('Loading AquaMaps cell data.  Less than 1 minute.\n  %s \n', cell_file))
    am_cells <- fread(cell_file, header = TRUE, stringsAsFactors = FALSE) %>%
      dplyr::select(csq = CsquareCode, loiczid = LOICZID)
    
    message('Keyed joining cell spp table to cell lookup table to attach LOICZID (by CSquareCode).\n')
    # Keyed data frame method faster than dplyr... check here: http://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
    acs_keyed <- data.table(am_cells_spp, key = "csq") 
    ac_keyed  <- data.table(am_cells, key = "csq")
    
    am_cells_spp1 <- ac_keyed[acs_keyed] %>%
      dplyr::select(-csq)
    # z <- x[y] is analogous to z <- left_join(y, x, by = 'key'), so the y variable determines which
    # rows to keep (non-matching rows in x will be discarded).  In this case, we only care about cells with 
    # species listed (all species observations will necessarily have a cell ID); and cells with no 
    # match in the species list will be discarded.  So: ac_keyed is x, and acs_keyed is y.
    
    message(sprintf('Writing Aquamaps species per cell file to: \n  %s\n', am_cells_spp_file))
    write_csv(am_cells_spp1, am_cells_spp_file)
  } else {
    message(sprintf('Reading Aquamaps species per cell file from: \n  %s\n', am_cells_spp_file))
    am_cells_spp1 <- read_csv(am_cells_spp_file)
  }
  
  return(am_cells_spp1)
}


##############################################################################=
get_iucn_cells_spp <- function(reload = FALSE) {
  iucn_cells_file <- file.path(dir_data_iucn, sprintf('iucn_cells_%s.csv', scenario))
  if(!file.exists(iucn_cells_file) | reload) {
    message(sprintf('Building IUCN species to cell table.  This might take a few minutes.\n'))
    iucn_map_files      <- file.path(dir_anx, 'iucn_intersections', list.files(file.path(dir_anx, 'iucn_intersections')))
    iucn_cells_spp_list <- lapply(iucn_map_files, read.csv) # read each into dataframe within a list
    iucn_cells_spp      <- bind_rows(iucn_cells_spp_list)   # combine list of dataframes to single dataframe
    # This creates a full data frame of all IUCN species, across all species groups, for all cells.
    # Probably big...
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
process_iucn_summary_per_cell <- function(fn_tag = '', reload = FALSE) {
  # Calculate category and trend scores per cell for IUCN species.
  # * For each IUCN species group:
  #   * load IUCN species <-> cell lookup
  #   * filter to appropriate cells (in regions, meets probability threshold)
  #   * join spatial info: loiczid, region ID, cell area
  #   * join species info: category score and trend score
  #   * filter by cat score != NA
  #   * summarize by loiczid - mean category_score, mean trend_score, count
  # * Each summary data frame should be saved to a list, to be eventually rbind_all'ed
  
  
  iucn_cells_spp_sum_file <- file.path(dir_git, scenario, sprintf('summary/spp_sum_iucn_cells%s.csv', fn_tag))
  
  if(!file.exists(iucn_cells_spp_sum_file) | reload) {
    message('Generating cell-by-cell summary for IUCN range-map species.\n')
    
    ### Load IUCN species per cell tables
    iucn_cells_spp      <- get_iucn_cells_spp()
    
    # filter entire IUCN table to just cells found in appropriate regions
    # (as designated by rgn_cell_lookup file). 
    
    # filter species info to just IUCN species with category info, and bind to 
    # iucn_cells_spp to attach category_score and trend_score.
    message('Filtering to just species with non-NA IUCN category scores.\n')
    spp_iucn_info <- spp_all %>% 
      filter(spatial_source == 'iucn' | spatial_source == 'iucn_parent') %>% 
      ### NOTE: for 2015, no IUCN subpops with spatially explicit shapefiles, so OK to cut all 'iucn_subpop' observations.
      ### Other values in this field: 'iucn_subpop' and 'iucn_alias' (for duped records due to species aliases)
      filter(!is.na(category_score) & !(category_score == 'DD')) %>%
      dplyr::select(iucn_sid, sciname, category_score, trend_score)
    message(sprintf('Length of IUCN species list: %d\n', nrow(spp_iucn_info)))
    spp_iucn_info <- spp_iucn_info %>% unique()
    message(sprintf('Length of IUCN species list, unique: %d\n', nrow(spp_iucn_info)))
    
    
    message('Keyed joining to species master list (filtered for spatial_source == iucn or iucn_parent).\n')
    ics_keyed <- data.table(iucn_cells_spp, key = "sciname") 
    sii_keyed <- data.table(spp_iucn_info,  key = "sciname")
    iucn_cells_spp1 <- ics_keyed[sii_keyed] 
    # z <- x[y] is analogous to z <- left_join(y, x, by = 'key'), so the y variable determines which
    # rows to keep (non-matching rows in x will be discarded).  In this case, all species must be on the spp_iucn_info list
    # (to learn IUCN cat/trend info); and species not on the list will be discarded.  So: ics_keyed is x, and sii_keyed is y.
    # The following species are dropped; they have shapefile info but no IUCN ID number according to this analysis:
    #   Alopex lagopus, Conus eduardi, Conus evansi, Conus hypochlorus, Conus luteus, Conus moncuri, Conus moylani,
    #   Conus sartii, Conus subulatus, Diplodus argenteus, Diplodus sargus, Eptatretus fernholmi, Halichoeres bleekeri,
    #   Holothuria squamifera, Sarda chiliensis chiliensis, Scyphiphora hydrophyllacea, Tetrapturus albidus
    
    message('Eliminating species double-counting due to overlapping polygons in IUCN shapefiles.\n')
    iucn_cells_spp1 <- iucn_cells_spp1 %>%
      as.data.frame() %>%
      # this next part to collapse any duplicated cells (overlapping polygons)
      group_by(sciname, iucn_sid, loiczid, category_score, trend_score) %>%
      summarize(prop_area = max(prop_area))
    
    message('Grouping by cell and summarizing mean category/trend and n_spp for each, for IUCN spatial info.\n')
    iucn_cells_spp_sum <- iucn_cells_spp1 %>%
      group_by(loiczid) %>%
      summarize(mean_cat_score = mean(category_score),      # no na.rm needed; already filtered.
                mean_popn_trend_score = mean(trend_score, na.rm = TRUE), 
                n_cat_species = n(),
                n_trend_species = sum(!is.na(trend_score))) %>% # no na.rm needed; count all with cat_score
      mutate(source = 'iucn')
    
    message(sprintf('Writing cell-by-cell summary for IUCN species to:\n  %s\n', iucn_cells_spp_sum_file))
    write_csv(iucn_cells_spp_sum, iucn_cells_spp_sum_file)
  } else {
    message(sprintf('Cell-by-cell summary for IUCN species already exists.  Reading from:\n  %s\n', iucn_cells_spp_sum_file))
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
  summary_by_loiczid <- bind_rows(am_cell_summary, iucn_cell_summary) %>%
    group_by(loiczid) %>%
    summarize(weighted_mean_cat   = sum(n_cat_species   * mean_cat_score)/sum(n_cat_species),
              weighted_mean_trend = sum(n_trend_species * mean_popn_trend_score, na.rm = TRUE)/sum(n_trend_species)) %>%
    arrange(loiczid)
  
  write_csv(summary_by_loiczid, file.path(dir_git, scenario, sprintf('summary/cell_spp_summary_by_loiczid%s.csv', fn_tag)))
  return(summary_by_loiczid)
}


##############################################################################=
process_means_per_rgn <- function(summary_by_loiczid, rgn_cell_lookup, rgn_note = 'NULL') {  
  ### Joins region-cell info to mean cat & trend per cell.
  ### Groups by region IDs, and calcs area-weighted mean category and trend values
  ### for all cells across entire region.  Cells only partly within a region are
  ### accounted for by multiplying cell area * proportionArea from rgn_cell_lookup.
  
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
                                   sprintf('summary/rgn_summary%s.csv', ifelse(is.null(rgn_note), "", 
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

