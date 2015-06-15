# ico_fxn.R
# created Jun2015 by Casey O'Hara
# functions to support calculations of the Iconic Species subgoal.  Some 
# functions are also used from the spp_fxn.R script, so that must be loaded as well.


cat('NOTE: ico_fxn.R requires that the following variables be set in the global environment (main script):\n')
cat(sprintf('dir_anx:  currently set to \'%s\'\n', dir_anx))
cat(sprintf('scenario: currently set to \'%s\'\n\n', scenario))


get_ico_list <- function() {
  ico_list_file <- file.path(dir_anx, 'ico/ico_global_list.csv')
  cat(sprintf('Reading raw iconic species list master file from: \n  %s\n', ico_list_file))
  ico_list_raw <- read.csv(ico_list_file, stringsAsFactors = FALSE) %>%
    select(country    = Country, 
           comname    = Specie.Common.Name,  
           sciname    = Specie.Scientific.Name, 
           ico_flag   = Flagship.Species,
           ico_local  = Priority.Species_Regional.and.Local,
           ico_global = Priority.Species_Global,
           ico_rgn    = Nation.Specific.List,
           ico_cat_long   = Current.Red.List.Category..Status.,
           ico_trend  = Population.Increasing..Decreasing..Stable.or.Unknown...over.3.generations.
    )
  # clean up names
  ico_list_raw <- ico_list_raw %>%
    mutate(sciname = str_trim(sciname),
           comname = str_trim(comname)) %>%
    filter(sciname != '')
  
  sum(!is.na(ico_list_raw$ico_global) | !is.na(ico_list_raw$ico_flag))
  # 1673 out of 2592 are globally iconic &/or flagship species
  sum(!is.na(ico_list_raw$ico_local)  &  is.na(ico_list_raw$ico_global) & is.na(ico_list_raw$ico_flag))
  # 63 are locally iconic but not on global or flagship lists; all are minke whales.  The rest of the locally important list is all humpback whales.
  sum((ico_list_raw$ico_rgn != '') & is.na(ico_list_raw$ico_local)  &  is.na(ico_list_raw$ico_global) & is.na(ico_list_raw$ico_flag))
  # 45 are on nation-specific list that aren't included elsewhere
  
  
  # convert global, flagship, and local iconic flags into single global iconic flag
  # note 'local' is just humpbacks and minkes, across 50-60 countries each. Just call it global.
  ico_list <- ico_list_raw %>%
    mutate(ico_gl = (!is.na(ico_global) | !is.na(ico_local) | !is.na(ico_flag))) %>%
    select(-ico_global, -ico_local, -ico_flag) 
  
  
  # convert long text IUCN categories into letter codes.
  cat_lookup  <- data.frame(ico_category = c("LC", "NT", "VU", "EN", "CR", "EX", "DD"), 
                            ico_cat_long = c('least concern', 'near threatened', 'vulnerable', 'endangered', 'critically endangered', 'extinct', 'data deficient'))
  ico_list <- ico_list %>%
    mutate(ico_cat_long = tolower(ico_cat_long)) %>%
    left_join(cat_lookup, by = 'ico_cat_long') %>%
    select(-ico_cat_long)
  
  if(!exists('spp_all')) 
     spp_all <- read.csv(file.path(dir_anx, scenario, 'intermediate/spp_all_cleaned.csv'), 
                         stringsAsFactors = FALSE)

  
  # join to spp_all and update category/trend info if available from IUCN spreadsheet
  ico_list <- ico_list %>%
    left_join(spp_all %>%
                select(iucn_sid, am_sid, sciname, popn_trend, iucn_category, spatial_source, parent_sid, subpop_sid), 
              by = 'sciname') %>%
    mutate(popn_trend    = tolower(popn_trend),
           iucn_category = ifelse(is.na(iucn_category), 
                                  as.character(ico_category), 
                                  as.character(iucn_category)),
           trend         = ifelse(is.na(popn_trend) | popn_trend == 'unknown', 
                                  as.character(ico_trend), 
                                  as.character(popn_trend))) %>%
    select(-ico_category, -ico_trend, -popn_trend)
  
  # convert regional iconic status into a rgn_id, and join to ico_list.
  rgn_name_file <- '~/github/ohi-global/eez2013/layers/rgn_global.csv'
  rgn_names <- read_csv(rgn_name_file)
  ico_list <- ico_list %>%
    left_join(ico_rgn_list <- ico_list %>%
                filter(ico_rgn != '' | is.na(spatial_source)) %>%
                select(country, sciname) %>%
                left_join(rgn_names, by = c('country' = 'label')),
              by = c('country', 'sciname')) %>% 
    select(-country, -ico_rgn, ico_rgn_id = rgn_id) %>% 
    filter(iucn_category != 'DD') %>%
    unique
  return(ico_list)
}

get_ico_rgn_iucn <- function(ico_list, reload = FALSE) {
  ico_rgn_iucn_file <- file.path(dir_anx, scenario, 'intermediate/ico_rgn_iucn.csv')
  if(!file.exists(ico_rgn_iucn_file) | reload) {
    ico_list_iucn <- ico_list %>%
      filter(str_detect(spatial_source, 'iucn'))
    
    ### Load IUCN species per cell tables : takes a while
    iucn_cells_spp <- get_iucn_cells_spp()
    
    iucn_ico_names <- unique(ico_list_iucn$sciname)
    if(!exists('rgn_cell_lookup'))  rgn_cell_lookup <- extract_cell_id_per_region(reload = FALSE)
    
    ### filter is faster than join.  Filter iucn_cells_spp by sciname; then join to LOICZID <-> rgn table
    ico_cells_iucn <- iucn_cells_spp %>%
      filter(sciname %in% iucn_ico_names) %>%
      left_join(rgn_cell_lookup %>%
                  select(rgn_id, rgn_name, loiczid),
                by = c('LOICZID' = 'loiczid'))
    ico_rgn_iucn <- ico_cells_iucn %>%
      group_by(rgn_id, sciname, id_no) %>%
      summarize(ncells = n()) %>%
      inner_join(ico_list_iucn, by = 'sciname')
    ico_rgn_iucn <- ico_rgn_iucn %>%
      filter(ico_gl == TRUE | rgn_id == ico_rgn_id) %>%
      filter(!is.na(rgn_id)) %>%
      select(-id_no, -ncells, -ico_gl, -ico_rgn_id) %>%
      unique()

    cat(sprintf('Writing regional presence of iconic species from IUCN spatial data. \n  %s\n', ico_rgn_iucn_file))
    write_csv(ico_rgn_iucn, ico_rgn_iucn_file)
    
  } else {
    cat(sprintf('Reading regional presence of iconic species from IUCN spatial data from:\n  %s\n', ico_rgn_iucn_file))
    ico_rgn_iucn <- read_csv(ico_rgn_iucn_file)
  }
  
  return(ico_rgn_iucn)
}

get_ico_rgn_am   <- function(ico_list, sp_source = 'am', reload = FALSE) {
  ico_rgn_file <- file.path(dir_anx, scenario, sprintf('intermediate/ico_rgn_%s.csv', sp_source))
  if(!file.exists(ico_rgn_file) | reload) {
    ico_list_sp <- ico_list %>%
      filter(spatial_source == sp_source) %>%
      select(-spatial_source) %>%
      mutate(am_sid = as.character(am_sid))
    
    cells_spp <- get_am_cells_spp() %>%
      select(-proportionArea, -cell_area)
    
    ico_am_sid <- unique(ico_list_sp$am_sid)
    
    
    ### filter is faster than join.  Filter iucn_cells_spp by sciname; then join to LOICZID <-> rgn table
    ico_cells <- cells_spp %>%
      filter(am_sid %in% ico_am_sid)
    ico_rgn <- ico_cells %>%
      group_by(rgn_id, am_sid) %>%
      summarize(ncells = n()) %>%
      inner_join(ico_list, by = 'am_sid')
    ico_rgn <- ico_rgn %>%
      filter(ico_gl == TRUE | rgn_id == ico_rgn_id) %>%
      filter(!is.na(rgn_id)) %>% # ???
      select(-id_no, -ncells, -ico_gl, -ico_rgn_id) %>%
      unique()
    
    cat(sprintf('Writing regional presence of iconic species from %s spatial data. \n  %s\n', sp_source, ico_rgn_file))
    write_csv(ico_rgn, ico_rgn_file)
    
  } else {
    cat(sprintf('Reading regional presence of iconic species from %s spatial data from:\n  %s\n', sp_source, ico_rgn_file))
    ico_rgn <- read_csv(ico_rgn_file)
  }
  return(ico_rgn)
}

process_ico_rgn <- function(ico_rgn_dfs) {
  ### Combine all the various ico_rgn files, convert codes to scores, and summarize for each region.
  ico_rgn <- bind_rows(ico_rgn_dfs)
  write_csv(ico_rgn, file.path(dir_anx, scenario, 'intermediate/ico_rgn_all.csv'))
  
  # to overall lookup table, join scores for population category and trend.
  popn_cat    <- data.frame(iucn_category  = c("LC", "NT", "VU", "EN", "CR", "EX"), 
                            category_score = c(   0,  0.2,  0.4,  0.6,  0.8,   1))
  popn_trend  <- data.frame(trend=c("decreasing", "stable", "increasing"), 
                            trend_score=c(-0.5, 0, 0.5))
  ico_rgn <- ico_rgn %>%
    left_join(popn_cat,   by = 'iucn_category') %>%
    left_join(popn_trend, by = 'trend') %>%
    select(-iucn_category, -trend)
  
  ico_rgn_sum <- ico_rgn %>%
    group_by(rgn_id) %>%
    summarize(mean_cat = mean(category_score), mean_trend = mean(trend_score, na.rm = TRUE)) %>%
    mutate(status = ((1 - mean_cat) - 0.25) / 0.75 * 100)
  
  ico_rgn_sum_file <- file.path(dir_anx, scenario, 'summary/ico_rgn_sum.csv')
  cat(sprintf('Writing file for iconic species summary by region: \n  %s\n', ico_rgn_sum_file))
  write_csv(ico_rgn_sum, ico_rgn_sum_file)
  
  return(invisible(ico_rgn_sum))
}

ico_rgn_source_compare <- function() {
  # check against original list - regions that are missing species? which species are new?
  rgn_name_file <- '~/github/ohi-global/eez2013/layers/rgn_global.csv'
  rgn_names <- read_csv(rgn_name_file)
  
  ico_list_file <- file.path(dir_anx, 'ico/ico_global_list.csv')
  ico_list_raw <- read.csv(ico_list_file, stringsAsFactors = FALSE) %>%
    select(country    = Country, 
           comname    = Specie.Common.Name,  
           sciname    = Specie.Scientific.Name
    )
  # clean up names
  ico_list_raw <- ico_list_raw %>%
    mutate(sciname = str_trim(sciname),
           comname = str_trim(comname)) %>%
    filter(sciname != '')
  
  ico_source_old <- ico_list_raw %>%
    left_join(rgn_names, by = c('country' = 'label')) %>%
    select(rgn_id, sciname, comname)   # 2592 observations according to spreadsheet
  ico_source_new <- read_csv(file.path(dir_anx, scenario, 'intermediate/ico_rgn_all.csv')) %>%
    select(rgn_id, sciname, comname)   # 4121 observations according to new spatial process
  ico_source_compare <- bind_rows(
    list(
      ico_source_new %>% inner_join(ico_source_old) %>%
        mutate(origin = 'both'),       # 1602 matches: agreement between spatial data and spreadsheet
      ico_source_new %>% anti_join(ico_source_old) %>%
        mutate(origin = 'spatial'),    # 2558 matches: not on spreadsheet, but added via spatial data
      ico_source_old %>% anti_join(ico_source_new) %>%
        mutate(origin = 'spreadsheet') # 1216 matches: on spreadsheet, but not found in spatial data
    )
  )
  ico_source_compare_file <- file.path(dir_anx, scenario, 'intermediate/ico_source_compare.csv')
  cat(sprintf('Writing comparison file for ICO to rgn from spreadsheet vs ICO to rgn from spatial data:\n  %s\n', ico_source_compare_file))
  write_csv(ico_source_compare, ico_source_compare_file)
  
  return(invisible(ico_source_compare))
}
