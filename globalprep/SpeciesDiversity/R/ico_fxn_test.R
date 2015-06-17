# ico_fxn.R
# created Jun2015 by Casey O'Hara
# functions to support calculations of the Iconic Species subgoal.  Some 
# functions are also used from the spp_fxn.R script, so that must be loaded as well.


cat('NOTE: ico_fxn.R requires that the following variables be set in the global environment (main script):\n')
cat(sprintf('dir_anx:  currently set to \'%s\'\n', dir_anx))
cat(sprintf('scenario: currently set to \'%s\'\n\n', scenario))


#############################################################################=
get_ico_rgn_iucn <- function(ico_list, reload = FALSE) {
  ico_rgn_iucn_file <- file.path(dir_anx, scenario, 'intermediate/ico_rgn_iucn_test.csv')
  if(!file.exists(ico_rgn_iucn_file) | reload) {
    ico_list_iucn <- ico_list %>%
      filter(str_detect(spatial_source, 'iucn')) %>%
      unique()
    
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
    ico_rgn_iucn <- read.csv(ico_rgn_iucn_file)
  }
  
  return(ico_rgn_iucn)
}


#############################################################################=
get_ico_rgn_am   <- function(ico_list, sp_source = 'am', reload = FALSE) {
   ico_rgn_file <- file.path(dir_anx, scenario, sprintf('intermediate/ico_rgn_%s_test.csv', sp_source))
  if(!file.exists(ico_rgn_file) | reload) {
    ico_list_sp <- ico_list %>%
      filter(str_detect(spatial_source, sp_source)) %>%
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
      select(-ncells, -ico_gl, -ico_rgn_id) %>%
      unique()
    
    cat(sprintf('Writing regional presence of iconic species from %s spatial data. \n  %s\n', sp_source, ico_rgn_file))
    write_csv(ico_rgn, ico_rgn_file)
    
  } else {
    cat(sprintf('Reading regional presence of iconic species from %s spatial data from:\n  %s\n', sp_source, ico_rgn_file))
    ico_rgn <- read.csv(ico_rgn_file, stringsAsFactors = FALSE)
  }
  return(ico_rgn)
}


#############################################################################=
get_countries = function(sid, download_tries = 10) {
  ### function to extract subpopulation information and population trend information
  ### for a species, given the IUCN species ID.
  ### example species Oncorhynchus nerka: sid=135301 # (parent) ## sid=135322 # (child)  # sid=4162
  url <- sprintf('http://api.iucnredlist.org/details/%d/0', sid)
  htm <- file.path(dir_anx, sprintf('cache/iucn_details/%d.htm', sid)) # htm = '135322.htm'
  
  i <- 0
  while (!file.exists(htm) | (file.info(htm)$size == 0 & i < download_tries)){
    download.file(url, htm, method = 'auto', quiet = TRUE, mode = 'wb')
    i <- i + 1
  }
  
  if (file.info(htm)$size == 0) stop(sprintf('Only getting empty file for: %s', url))
  h <- htmlParse(htm)
  countries <- as.character(xpathSApply(h, '//ul[@class="country_distribution"]/li/ul/li', xmlValue))
  # NOTE greater specificity 
  #   for http://www.iucnredlist.org/details/135322/0 -- United States (Alaska) 
  #   vs  http://api.iucnredlist.org/details/135322/0 -- United States
  #countries = xpathSApply(h, '//td[@class="label"][strong="Countries:"]/following-sibling::td/div/text()', xmlValue)    
  
  return(countries)
}


#############################################################################=
get_countries_all <- function(df = ico_list_subpops, reload = FALSE) {
  ### gets a list of countries in which parents and subpops appear, from
  ### the scraped data in iucn_details.
  
  ico_countries_file <- file.path(dir_anx, 'tmp/ico_subpop_countries_test.csv')
  if(!file.exists(ico_countries_file) | reload == TRUE) {
    cat('Creating parent and subpop countries list.\n')
    ico_countries     <- data.frame() # initialize
    
    i  <- 0
    
    for (sid in df$iucn_sid) { # sid = df$iucn_sid[3]
      country_list <-  get_countries(sid)
      # subpop
      if (length(country_list) > 0) {
        cat(sprintf('Found countries list for species %d: %s\n', sid, paste(country_list, collapse = ' ')))
        ico_countries_sid <- data.frame(sid, country_list)
        ico_countries     <- rbind(ico_countries, ico_countries_sid)
      }
      i <- i + 1
    }
    
    ico_countries <- ico_countries %>%
      rename(rgn_name = country_list)
    
    cat(sprintf('Writing parent/subpop countries file to: \n  %s\n', ico_countries_file))
    write_csv(ico_countries, ico_countries_file)
  } else {
    cat(sprintf('Reading parent/subpop countries file from: \n  %s\n', ico_countries_file))
    ico_countries <- read.csv(ico_countries_file, stringsAsFactors = FALSE)
  }
  ico_countries <- ico_rgn_name_to_number(ico_countries)
  return(ico_countries)
}


#############################################################################=
ico_rgn_name_to_number <- function(ico_countries) {
  if(!exists('rgn_cell_lookup'))  
    rgn_cell_lookup <- extract_cell_id_per_region(reload = FALSE)
  rgn_iucn2ohi <- read.csv(file.path(dir_anx, 'rgns/rgns_iucn2ohi.csv'), stringsAsFactors = FALSE)
  ico_countries <- ico_countries %>%
    left_join(rgn_cell_lookup %>%
                select(rgn_id, rgn_name) %>%
                unique(),
              by = 'rgn_name')
  ico_countries <- ico_countries %>%
    left_join(rgn_iucn2ohi, 
              by = c('rgn_name' = 'iucn_rgn_name')) %>%
    mutate(rgn_id = ifelse(is.na(rgn_id), ohi_rgn_id, rgn_id)) %>%
    select(-ohi_rgn_id) #%>%
    #filter(!is.na(rgn_id))
  return(ico_countries)
}


#############################################################################=
process_ico_rgn <- function(ico_rgn_all) {
  ### Summarize category and trend for each region.
  
  # to overall lookup table, join scores for population category and trend.
  popn_cat    <- data.frame(iucn_category  = c("LC", "NT", "VU", "EN", "CR", "EX"), 
                            category_score = c(   0,  0.2,  0.4,  0.6,  0.8,   1))
  popn_trend  <- data.frame(trend=c("decreasing", "stable", "increasing"), 
                            trend_score=c(-0.5, 0, 0.5))
  ico_rgn_all <- ico_rgn_all %>%
    left_join(popn_cat,   by = 'iucn_category') %>%
    left_join(popn_trend, by = 'trend') %>%
    select(-iucn_category, -trend)
  
  ### This section omits parents if a subpopulation is present
#   ico_rgn_all <- ico_rgn_all %>%
#     group_by(rgn_id, sciname) %>%
#     mutate(p_drop_flag = ifelse(n() > 1 & str_detect(spatial_source, 'parent'), TRUE, FALSE)) %>%
#     filter(!p_drop_flag)

  ### This section aggregates category and trend for a single species sciname within a region,
  ### including parent and all subpopulations present in a region.
  ### Species, including parent and all subpops, is weighted same as species w/o parents and subpops.
  ico_rgn_all <- ico_rgn_all %>%
    group_by(rgn_id, sciname) %>%
    summarize(category_score = mean(category_score), trend_score = mean(trend_score, na.rm = TRUE))

  ico_rgn_sum <- ico_rgn_all %>%
    group_by(rgn_id) %>%
    summarize(mean_cat = mean(category_score), mean_trend = mean(trend_score, na.rm = TRUE)) %>%
    mutate(status = ((1 - mean_cat) - 0.25) / 0.75)
  
  ico_rgn_sum_file <- file.path(dir_anx, scenario, 'summary/ico_rgn_sum_test.csv')
  cat(sprintf('Writing file for iconic species summary by region: \n  %s\n', ico_rgn_sum_file))
  write_csv(ico_rgn_sum, ico_rgn_sum_file)
  
  return(invisible(ico_rgn_sum))
}


#############################################################################=
test_get_ico_list <- function() {
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
    mutate(country = str_trim(country),
           sciname = str_trim(sciname),
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
              by = 'sciname') # %>%
#     mutate(popn_trend    = tolower(popn_trend),
#            iucn_category = ifelse(is.na(iucn_category), 
#                                   as.character(ico_category), 
#                                   as.character(iucn_category)),
#            trend         = ifelse(is.na(popn_trend) | popn_trend == 'unknown', 
#                                   as.character(ico_trend), 
#                                   as.character(popn_trend))) %>%
#     select(-ico_category, -ico_trend, -popn_trend)
#   
  # convert regional iconic status into a rgn_id, and join to ico_list.
  rgn_name_file <- '~/github/ohi-global/eez2013/layers/rgn_global.csv'
  rgn_names <- read_csv(rgn_name_file)
  ico_list1 <- ico_list %>%
    left_join(ico_list %>%
                filter(ico_rgn != '' | is.na(spatial_source)) %>%
                select(country, sciname) %>%
                left_join(rgn_names, by = c('country' = 'label')),
              by = c('country', 'sciname')) %>% 
    select(-ico_rgn, ico_rgn_id = rgn_id) %>% 
#    filter(iucn_category != 'DD') %>%
    unique
  ico_list1 <- ico_list1 %>%
    left_join(rgn_names, by = c('country' = 'label'))
  return(ico_list1)
}

