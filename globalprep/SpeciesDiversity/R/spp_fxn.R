# spp_fxn.R

##############################################################################=
get_loiczid_raster <- function(dir_anx, reload = FALSE) {
### Generate half-degree raster using am_cells to assign LOICZID to CenterLong x CenterLat.
### * Template and output in <dir_anx>/rgns
### * rasterize takes about 10 seconds...
##############################################################################=
  
  loiczid_raster_file  <- file.path(dir_anx, 'rgns/loiczid_raster.grd')
  raster_template_file <- file.path(dir_anx, 'rgns/am_cells_template.tif')
  
  if(!file.exists(loiczid_raster_file) | reload == TRUE) {
    # load and format AquaMaps half-degree cell authority file
    file_loc <- file.path(dir_anx, 'raw/aquamaps_2014/tables/hcaf.csv')
    cat(sprintf('Loading AquaMaps cell data.  Less than 1 minute.\n  %s \n', file_loc))
    am_cells <- fread(file_loc, header = TRUE, stringsAsFactors = FALSE) %>%
      select(csq = CsquareCode, LOICZID, CenterLat, CenterLong, CellArea)
    
    stopifnot(sum(duplicated(am_cells$LOICZID)) == 0) #key numeric ID for raster generation.... check for dups
    
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


##############################################################################=
extract_cell_id_per_region <- function(dir_anx, reload = FALSE) {
### Determines proportional area of each cell covered by region polygons.  Returns data frame
### of sp_id, loiczid, csq, and proportional area of loiczid cell covered by the sp_id region.
### Should not be year-specific, so leave files in SpeciesDiversity/rgns.
### ??? TO DO: compare loiczid regions <-> CenterLong and CenterLat to last year's table, to make sure consistent from year to year.
### ??? TO DO: update to translate sp_id directly into rgn_id using Melanie's code. Filter out only global regions.
##############################################################################=

  ogr_location  <- file.path(dir_anx, '../../Global/NCEAS-Regions_v2014/data')
  
  rgn_prop_file <- file.path(dir_anx, 'rgns/region_prop_df.csv')
  
  if(!file.exists(rgn_prop_file) | reload == TRUE) {
    
    cat(sprintf('Reading regions shape file - come back in about 4 minutes.\n  %s\n', ogr_location))
    regions <- readOGR(dsn = ogr_location, layer='sp_gcs')
    # slow command... ~ 4 minutes
    regions <- regions[regions@data$rgn_type %in% c('eez', 'fao', 'eez_ccamlr', 'eez-disputed', 'eez-inland'), ]

    raster_file <- file.path(dir_anx, 'rgns/loiczid_raster')
    loiczid_raster <- get_loiczid_raster(dir_anx, reload = reload)
    
    cat('Extracting proportional area of LOICZID cells per region polygon.  Come back in 15-20 minutes.\n')
    region_prop <- raster::extract(loiczid_raster,  regions, weights = TRUE, normalizeWeights = FALSE, progress = 'text') 
    # small = TRUE returns 1 for sp_id 232, not what we want.
    # slow command... ~15 minutes (even with the small = TRUE)
    
    ### assign sp_id identifiers (from `regions`) to region_prop, convert to data.frame
    names(region_prop) <- regions@data$sp_id
    region_prop_df     <- plyr::ldply(region_prop, rbind) # ??? still a plyr function.
    # length(unique(region_prop_df$.id)) 
    #   WAS: less than 254 due to repeats of Canada and one small region (232: Bosnia/Herzegovina) with no rasters identified
    #   IS:  278, including a cell for #232.
    
    # ??? consider converting sp_id into rgn_id code from Melanie
    region_prop_df <- region_prop_df %>%
      rename(sp_id = .id, 
             loiczid = value, 
             proportionArea = weight)
    
    ### ??? add in this region -  Bosnia/Herzegovina (BIH), which appears to be too small to catch using this method (<1% of area)
    ### ??? SKIPPING THIS FOR NOW!!!
    # cells_2013[cells_2013$rgn_id==232, ]
    # cells_2013[cells_2013$csq=='1401:227:4', ]
    # am_cells[am_cells$CsquareCode == '1401:227:4', ]
    # 6.034664/2269.83
    # bih <- data.frame(sp_id=232, LOICZID=68076, proportionArea=0.002658641)
    # region_prop_df <- rbind(region_prop_df, bih)
    
    file_loc <- file.path(dir_anx, 'raw/aquamaps_2014/tables/hcaf.csv')
    cat(sprintf('Loading AquaMaps half-degree cell authority file.  Less than 1 minute.\n  %s \n', file_loc))
    am_cells <- fread(file_loc, header = TRUE, stringsAsFactors = FALSE) %>%
      as.data.frame() %>%
      select(csq = CsquareCode, loiczid = LOICZID, cell_area = CellArea)
    stopifnot(sum(duplicated(am_cells$csq)) == 0)
    
    cat('Joining csq values and cell areas to loiczid values.\n')
    region_prop_df <- region_prop_df %>%
      left_join(am_cells, by = 'loiczid')
    
    
    cat('Filtering out all regions with sp_id > 300, to eliminate high seas and antarctic.\n')
    cat('Really, there should be something in here to turn sp_id into rgn_id proper-like.\n')
    region_prop_df <- region_prop_df %>%
      filter(sp_id <= 300)
    
    cat(sprintf('Writing loiczid/csq/cell proportions/cell areas by region to: \n  %s\n', rgn_prop_file))
    write.csv(region_prop_df, rgn_prop_file, row.names = FALSE)
  } else {
    cat(sprintf('Reading loiczid cell proportions by region from: \n  %s\n', rgn_prop_file))
    cat('Filtered out all regions with sp_id > 300, to eliminate high seas and antarctic.\n')
    cat('Really, there should be something in here to turn sp_id into rgn_id proper-like.\n')
    region_prop_df <- read.csv(rgn_prop_file)
  }
  
  return(invisible(region_prop_df))
}


##############################################################################=
create_spp_master_lookup <- function(dir_anx, scenario = 'v2015', reload = FALSE) {
### Create lookup: species <-> popn_category/popn_trend and spatial_source.
### Output is data frame with these fields:
### * sciname  iucn_sid  am_sid  popn_category  popn_trend info_source  spatial_source
### Aquamaps data is 'ohi_speciesoccursum.csv' with: 
### * SPECIESID   SpecCode   sciname   am_category
### IUCN input is 'spp_iucn_marine_global.csv' with:
### * sid(Red.List.Species.ID)  sciname  iucn_category  popn_trend
### Output details:
### * popn_category: IUCN category. Prioritize iucn_category then am_category; otherwise NA
### * popn_trend: for species with IUCN popn_trend info: 'Increasing', 'Decreasing', 'Stable'
### * info_source: source of IUCN category info (and popn trend if applicable): am, iucn, NA
### * spatial_source: prioritize IUCN range maps, then AquaMaps: iucn, am, NA
### * Filter out anything with spatial_source NA, and anything with category NA? 
###   Or leave in for future filtering?
###   - ??? test how many get filtered out and how big is the file.
##############################################################################=
  spp_all_file <- file.path(dir_anx, scenario, 'intermediate/spp_all.csv')

  if(!file.exists(spp_all_file) | reload == TRUE) {
    spp_am_file <- file.path(dir_anx, 'raw/aquamaps_2014/tables/ohi_speciesoccursum.csv')
    cat(sprintf('Reading AquaMaps species list from: \n  %s\n', spp_am_file))
    
    spp_am <- fread(spp_am_file) %>%
      select(am_sid = SPECIESID, Genus, Species, am_category = iucn_code) %>%
      unite(sciname, Genus, Species, sep = ' ') %>%
      mutate(am_category = ifelse(am_category ==  'N.E.',   NA, am_category), # not evaluated -> NA
             am_category = ifelse(am_category == 'LR/nt', 'NT', am_category), # update 1994 category
             am_category = ifelse(am_category == 'LR/lc', 'LC', am_category), # update 1994 category
             am_category = ifelse(am_category ==   '\\N',   NA, am_category)) # what's this?
    
    # pull the IUCN data from the git-annex file for this year - output from ingest_iucn.R
    iucn_list_file <- file.path(dir_anx, scenario, 'intermediate/spp_iucn_marine_global.csv')
    
    cat(sprintf('Reading IUCN marine species list from: \n  %s\n', iucn_list_file))
    spp_iucn_marine = read.csv(iucn_list_file) %>%
      select(sciname, iucn_sid = sid, iucn_category = category, popn_trend) %>% 
      filter(iucn_category != 'DD')
    
    spp_all <- spp_am %>%
      mutate(sciname = str_trim(sciname)) %>% # ??? this fixed one record - based on spaces, or shifting to caps?
      as.data.frame() %>%
      full_join(spp_iucn_marine, by = 'sciname')
    
    spp_all <- spp_all %>%
      # create single 'category' field, and flag 'info_source' to indicate iucn or am
      mutate(iucn_category = as.character(iucn_category),
             am_category   = as.character(am_category),
             popn_category = ifelse(!is.na(iucn_category), iucn_category, am_category),
             info_source = ifelse(!is.na(iucn_category), 'iucn',
                                  ifelse(!is.na(am_category), 'am',   NA)))
  
    iucn_map_list_file <- file.path(dir_anx, scenario, 'intermediate/spp_iucn_maps_all.csv')
    if(!file.exists(iucn_map_list_file) | reload == TRUE) {
      if(!file.exists(iucn_map_list_file)) cat('No file found for list of available IUCN range maps.  ')
      cat('Generating new list of available IUCN range maps.\n')
  
      dir_iucn_shp <- file.path(dir_anx, 'raw/iucn_shp')
      groups_list <- as.data.frame(list.files(dir_iucn_shp)) %>%
        rename(shp_fn = `list.files(dir_iucn_shp)`) %>%
        filter(tools::file_ext(shp_fn) == 'shp') %>%
        mutate(shp_fn = str_replace(shp_fn, '.shp', ''))
      
      spp_iucn_maps <- data.frame()
      
      for (spp_group in groups_list$shp_fn) { 
        cat(sprintf('Processing species group: %s... \n', tolower(spp_group)))
        spp_dbf <- read.dbf(file.path(dir_iucn_shp, sprintf('%s.dbf', spp_group)))
        cat('file read successfully... ')
        spp_dbf <- as.data.frame(spp_dbf)
        cat('converted to data frame... ')
        spp_dbf <- data.frame(spp_group, spp_dbf)
        if('dbf.ID_NO' %in% names(spp_dbf)) {
          spp_dbf <- spp_dbf %>% 
            rename(dbf.id_no = dbf.ID_NO)
        }
        if('dbf.OBJECTID' %in% names(spp_dbf)) {
          spp_dbf <- spp_dbf %>% rename(dbf.objectid = dbf.OBJECTID)
        }
        if('dbf.BINOMIAL' %in% names(spp_dbf)) {
          spp_dbf <- spp_dbf %>% rename(dbf.binomial = dbf.BINOMIAL)
        }
        if('dbf.id_no' %in% names(spp_dbf)) {
          spp_dbf <- spp_dbf %>% 
            mutate(dbf.id_no = as.integer(dbf.id_no))
        }
        spp_iucn_maps <- bind_rows(spp_iucn_maps, spp_dbf)
        cat('binding to list...\n')
      }
      spp_iucn_maps <- spp_iucn_maps %>%
        select(spp_group, id_no = dbf.id_no, objectid = dbf.objectid, binomial = dbf.binomial) %>% # other fields?
        mutate(spatial_source = as.character('iucn'))%>%
        unique()
      
      cat(sprintf('Writing list of available IUCN range maps to: \n  %s\n', iucn_map_list_file))
      write.csv(spp_iucn_maps, iucn_map_list_file, row.names = FALSE)
    } else {
      cat(sprintf('Reading list of available IUCN range maps from: \n  %s\n', iucn_map_list_file))
      spp_iucn_maps <- read.csv(iucn_map_list_file, stringsAsFactors = FALSE)
    }
      
    
    spp_all <- spp_all %>% 
      left_join(spp_iucn_maps, by = c('sciname' = 'binomial')) %>% 
      mutate(spatial_source = ifelse((is.na(spatial_source) & !is.na(am_sid)), 'am', spatial_source))
    # is.na(spatial_source) means it wasn't in the IUCN maps list;
    # !is.na(am_sid) means it is an aquamaps species.
    # In left_joining, we go from 31429 available species maps to only 21116 in the spp_all.  The omitted
    # maps are not in the spp_iucn_marine_global list then?
    
    # duplicates of sciname need to be dealt with. Some cases:
    # * sciname duped, due to multiple IUCN shapefiles (e.g. REPTILES, SEASNAKES, and non-homolopsids)
    #   - choose one and drop the others.  Which to choose? let R decide.
    dupes <- spp_all %>% 
      select(sciname, iucn_sid, iucn_category, objectid) %>% 
      duplicated()
    spp_all <- spp_all %>%
      filter(!dupes)
    # * sciname duped, due to multiple id_no from IUCN shapefile (e.g. one name with subpops or subspp)
    #   - what if one is "parent" and sub pops are overlapped by parent? this creates double-counting?
    #   - retain all, because spatial data is available for each separately?
    # * sciname duped, due to multiple iucn_sid from IUCN spreadsheet (e.g. one name with subpops or subspp)
    #   - this doesn't help us much if we don't have spatially explicit data.
    #   - if different subspp have different categories, but no spatial info, which category to choose - best case? worst case?
    #   - eventually, decide how to filter out unhelpful lines
    
    
    cat(sprintf('Writing full species lookup table to: \n  %s\n', spp_all_file))
    write.csv(spp_all, spp_all_file, row.names = FALSE)
  } else {
    cat(sprintf('Full species lookup table already exists.  Reading from: \n  %s\n', spp_all_file))
    spp_all <- read.csv(spp_all_file)
  }
  
  return(invisible(spp_all))
}


##############################################################################=
extract_loiczid_per_spp <- function(dir_anx, groups_override = NULL, scenario = 'v2015', reload = FALSE) {
# Determine intersections of IUCN maps with Aquamaps half-degree cells
#
# * from the spp_all.csv, extract the species with IUCN range maps.  Use this file
#   rather than the spp_iucn_maps_all because it is truncated to just species on spp_iucn_marine_global.csv list.
# * for each species group (spp_group), open parent shape file
#   * select by attribute - binomial == sciname in the list
#   * use raster::extract, lay the selected polygons over the LOICZID raster.
#     * NOTE: this takes a long time - 15-20 minutes for the region shapefile, with ~200 fairly simple polygons.
#       We're looking at ~ 4000 complicated polygons.
# * when finished with each spp_group, save a file of sciname | id_no | LOICZID | prop_area for that group.
# * Use groups_override argument to pass a partial list of species groups, for testing/debugging.
  
  # create list of all marine species with IUCN maps.
  spp_all_file <- file.path(dir_anx, scenario, 'intermediate/spp_all.csv')
  cat(sprintf('Reading full species lookup table from: \n  %s\n', spp_all_file))
  spp_all <- read.csv(spp_all_file, stringsAsFactors = FALSE)
  iucn_range_maps <- spp_all %>%
    filter(spatial_source == 'iucn') %>%
    select(sciname, iucn_sid, id_no, spp_group)
  
  # Import LOICZID raster
  raster_file <- file.path(dir_anx, 'rgns/loiczid_raster')
  loiczid_raster <- get_loiczid_raster(dir_anx, reload = FALSE)
  
  # create list of groups (i.e. shape files) to be analyzed
  if(is.null(groups_override)) spp_gp_list <- unique(iucn_range_maps$spp_group)
  else spp_gp_list <- groups_override
  
  ogr_location <- file.path(dir_anx, 'raw/iucn_shp')
  
  for(spp_gp in spp_gp_list) { # spp_gp <- 'LOBSTERS' # spp_gp <- 'CORAL3'
    maps_in_group <- iucn_range_maps %>%
      filter(spp_group == spp_gp)
    
    # set file path for output file from this species group.
    cache_file <- file.path(dir_anx, 'iucn_intersections', sprintf('%s.csv', spp_gp))

    # if reload == FALSE, and the file exists, don't waste your friggin' time here, move on to next group.
    if(file.exists(cache_file) & reload == FALSE) {   
      cat(sprintf('\nIUCN <-< LOICZID lookup file already exists for species group %s; file location:\n  %s\n', spp_gp, cache_file))
    } else {
      ptm <- proc.time()
      fsize <- round(file.size(file.path(ogr_location, sprintf('%s.shp', spp_gp)))/1e6, 2)
      cat(sprintf('\nReading species group shapefile %s, %.2f MB\n  %s/%s\n', spp_gp, fsize, ogr_location, spp_gp))
      spp_shp <- readOGR(dsn = ogr_location, layer= spp_gp)
      ptm <- proc.time() - ptm
      cat(sprintf('Elapsed read time: %.2f seconds\n', ptm[3]))
      
      # Filter shape file to polygons with species names that match our list of
      # marine species with IUCN maps.  Shape files seem to contain a 'binomial' 
      # field but case varies from file to file.
      cat(colnames(spp_shp@data)); cat('\n')
      # find binomial name in here; test in tolower, find the index number, and use that instead?
      binom_index <- which(colnames(spp_shp@data) %in% c('binomial', 'BINOMIAL'))
      if(binom_index > 0) {
        cat(sprintf('Filtering features by %s field in %s.\n', colnames(spp_shp@data)[binom_index], spp_gp))
        spp_shp <- spp_shp[spp_shp@data[ , binom_index] %in% maps_in_group$sciname, ]
      } else {
        cat(sprintf('Couldn\'t find binomial field for species group %s.\n', spp_gp))
      }
      
      # Print out projection of this shapefile.  Should be +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0
      cat(sprintf('Projection for species group %s:   %s \n', spp_gp, projection(spp_shp)))
      
      # Extract the proportions of each species polygon within each LOICZID cell
      ptm <- proc.time()
      spp_shp_prop <- raster::extract(loiczid_raster,  spp_shp, weights = TRUE, normalizeWeights = FALSE, progress = 'text')
      ptm <- proc.time() - ptm
      cat(sprintf('Elapsed process time: %.2f minutes\n', ptm[3]/60))
      
      
      # find id_no in the shapefile, and if it exists, use it to create id_field for dataframe column names.
      # If the field doesn't exist, assign it NAs so at least the column will be created.
      id_no_index <- which(colnames(spp_shp@data) %in% c('id_no', 'ID_NO'))
      if(length(id_no_index) == 0) {
        cat(sprintf('No id_no field found in species group %s.\n', spp_gp))
        id_field <- NA
      } else id_field <- spp_shp@data[ , id_no_index]
      
      # combines sciname and id_no for unique identifier
      sciname_sid <- data.frame(spp_shp@data[ , binom_index], id_field)
      names(sciname_sid) <- c('sciname', 'id_no')
      sciname_sid <- sciname_sid %>%
        unite(name_id, sciname, id_no, sep = '_')
      
      # uses unique identifier to name the list; converts list to data frame.
      names(spp_shp_prop) <- sciname_sid$name_id
      spp_shp_prop_df     <- plyr::ldply(spp_shp_prop, rbind)
      spp_shp_prop_df <- spp_shp_prop_df %>%
        rename(name_id = .id,
               LOICZID = value, 
               prop_area = weight) %>%
        separate(name_id, c('sciname', 'id_no'), sep = '_')
      
      # save .csv for this species group
      cat(sprintf('Writing IUCN<->LOICZID intersection file for %s to:\n  %s\n', spp_gp, cache_file))
      write.csv(spp_shp_prop_df, cache_file, row.names = FALSE)
    }
  }
}


##############################################################################=
process_am_spp_per_cell <- function(dir_anx, rgn_cell_lookup, scenario = 'v2015', reload = FALSE) {
# Calculate category and trend scores per cell for Aquamaps species.
# * load AM species <-> cell lookup
# * filter to appropriate cells (in regions, meets probability threshold)
# * join spatial info: loiczid, region ID, cell area
# * join species info: category score and trend score
# * filter by cat score != NA
# * summarize by loiczid - mean category_score, mean trend_score, count
  
  am_cells_spp_sum_file <- file.path(dir_anx, scenario, 'intermediate/am_cells_spp_sum.csv')
  
  if(!file.exists(am_cells_spp_sum_file) | reload == TRUE) {
    cat('Generating cell-by-cell summary for Aquamaps species.\n')
  
    ### Load Aquamaps species per cell table
    file_loc <- file.path(dir_anx, 'raw/aquamaps_2014/tables/ohi_hcaf_species_native.csv')
    cat(sprintf('Loading AquaMaps cell-species data.  Large file! \n  %s \n', file_loc))
    am_cells_spp <- read_csv(file_loc, col_types = '_ccn__') %>%
      rename(am_sid = SpeciesID, csq = CsquareCode, prob = probability)
    
    # filter entire aquamaps table to just cells found in appropriate regions
    # (as designated by rgn_cell_lookup file). 
    cat('Filtering to just cells within regions, and species with probability greater than 0.4.\n')
    am_cells_spp1 <- am_cells_spp %>% 
      filter(csq %in% rgn_cell_lookup$csq) 
    # filter out below probability threshold; 39 M to 29 M observations.
    am_cells_spp1 <- am_cells_spp1 %>%
      filter(prob >= .40) %>%
      select(-prob)
    # then join to rgn_cell_lookup to attach loiczid, region ID, and cell area
    ptm <- proc.time()
    am_cells_spp2 <- am_cells_spp1 %>%
      inner_join(rgn_cell_lookup, by = 'csq') %>%
      select(-csq)
    proc.time() - ptm
    
    # filter species info to just Aquamaps species with category info, and bind to 
    # am_cells_spp to attach category_score and trend_score.
    cat('Filtering to just species with non-NA IUCN category scores.\n')
    spp_am_info <- spp_all %>% 
      filter(spatial_source == 'am') %>%
      filter(!is.na(category_score)) %>%
      select(am_sid, sciname, category_score, trend_score)
    
    am_cells_spp3 <- am_cells_spp2 %>% 
      inner_join(spp_am_info, by = 'am_sid')
    
    am_cells_spp_sum <- am_cells_spp3 %>%
      group_by(loiczid) %>%
      summarize(am_mean_cat_score = mean(category_score),  # no na.rm needed; already filtered
                am_mean_popn_trend_score = mean(trend_score, na.rm = TRUE), 
                am_n_species = n())                        # no na.rm needed; count all with cat_score
    
    am_cells_spp_sum_file <- file.path(dir_anx, scenario, 'intermediate/am_cells_spp_sum.csv')
    cat(sprintf('Writing cell-by-cell summary for Aquamaps species to:\n  %s\n', am_cells_spp_sum_file))
    write_csv(am_cells_spp_sum, am_cells_spp_sum_file)
  } else {
    cat(sprintf('Cell-by-cell summary for Aquamaps species already exists.  Reading from:\n  %s\n', am_cells_spp_sum_file))
    am_cells_spp_sum <- read.csv(am_cells_spp_sum_file)
  }
  return(invisible(am_cells_spp_sum))
}


##############################################################################=
process_iucn_spp_per_cell <- function(dir_anx, rgn_cell_lookup, scenario = 'v2015', reload = FALSE) {
  # Calculate category and trend scores per cell for IUCN species.
  # * For each IUCN species group:
  #   * load IUCN species <-> cell lookup
  #   * filter to appropriate cells (in regions, meets probability threshold)
  #   * join spatial info: loiczid, region ID, cell area
  #   * join species info: category score and trend score
  #   * filter by cat score != NA
  #   * summarize by loiczid - mean category_score, mean trend_score, count
  # * Each summary data frame should be saved to a list, to be eventually rbind_all'ed
  
  
  iucn_cells_spp_sum_file <- file.path(dir_anx, scenario, 'intermediate/iucn_cells_spp_sum.csv')
  
  if(!file.exists(iucn_cells_spp_sum_file) | reload == TRUE) {
    cat('Generating cell-by-cell summary for IUCN range-map species.\n')
    
    ### Load IUCN species per cell tables
#    iucn_map_files      <- file.path(dir_anx, 'iucn_intersections', list.files(file.path(dir_anx, 'iucn_intersections')))
    iucn_map_files      <- file.path(dir_anx, 'iucn_intersections', c('AMPHANURA.csv', 'ANGELFISH.csv'))
    iucn_cells_spp_list <- lapply(iucn_map_files, read.csv) # read each into dataframe within a list
    iucn_cells_spp      <- rbind_all(iucn_cells_spp_list)   # combine list of dataframes to single dataframe
    # This creates a full data frame of all IUCN species, across all species groups, for all cells.
    # Probably big.
        
    # filter entire IUCN table to just cells found in appropriate regions
    # (as designated by rgn_cell_lookup file). 
    cat('Filtering to cells within regions.\n')

    iucn_cells_spp1 <- iucn_cells_spp %>% 
      rename(loiczid = LOICZID, polygon_prop_area = prop_area) %>%
      filter(loiczid %in% rgn_cell_lookup$loiczid) 
    # then join to rgn_cell_lookup to attach loiczid, region ID, and cell area
    ptm <- proc.time()
    iucn_cells_spp2 <- iucn_cells_spp1 %>%
      inner_join(rgn_cell_lookup, by = 'loiczid') %>%
      select(-csq)
    proc.time() - ptm
    
    # filter species info to just IUCN species with category info, and bind to 
    # iucn_cells_spp to attach category_score and trend_score.
      cat('Filtering to just species with non-NA IUCN category scores.\n')
      spp_iucn_info <- spp_all %>% 
      filter(spatial_source == 'iucn') %>%
      filter(!is.na(category_score)) %>%
      select(iucn_sid, sciname, category_score, trend_score)
    
    iucn_cells_spp3 <- iucn_cells_spp2 %>% 
      inner_join(spp_iucn_info, by = c('sciname'))
    
    iucn_cells_spp_sum <- iucn_cells_spp3 %>%
      group_by(loiczid) %>%
      summarize(iucn_mean_cat_score = mean(category_score),  # no na.rm needed; already filtered.
                iucn_mean_popn_trend_score = mean(trend_score, na.rm = TRUE), 
                iucn_n_species = n())                        # no na.rm needed; count all with cat_score
    
    iucn_cells_spp_sum_file <- file.path(dir_anx, scenario, 'intermediate/iucn_cells_spp_sum.csv')
    cat(sprintf('Writing cell-by-cell summary for IUCN species to:\n  %s\n', iucn_cells_spp_sum_file))
    write_csv(iucn_cells_spp_sum, iucn_cells_spp_sum_file)
  } else {
    cat(sprintf('Cell-by-cell summary for IUCN species already exists.  Reading from:\n  %s\n', iucn_cells_spp_sum_file))
    iucn_cells_spp_sum <- read.csv(iucn_cells_spp_sum_file)
  }
  return(invisible(iucn_cells_spp_sum))
}
