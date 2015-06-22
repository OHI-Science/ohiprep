# ico_fxn.R
# created Jun2015 by Casey O'Hara
# functions to support calculations of the Iconic Species subgoal.  Some 
# functions are also used from the spp_fxn.R script, so that must be loaded as well.


cat('NOTE: ico_fxn.R requires that the following variables be set in the global environment (main script):\n')
cat(sprintf('dir_anx:  currently set to \'%s\'\n', dir_anx))
cat(sprintf('scenario: currently set to \'%s\'\n\n', scenario))


#############################################################################=
get_ico_list <- function() {
### This function loads the ico_global_list.csv to determine which species to
### consider for ICO (as well as global and regional ICO status); then attaches 
### this ICO species list to the IUCN master species list.  

  ico_list_file <- file.path(dir_anx, 'ico/ico_global_list.csv')
  cat(sprintf('Reading raw iconic species list from: \n  %s\n', ico_list_file))
  ico_list_raw <- read.csv(ico_list_file, stringsAsFactors = FALSE) %>%
    select(rgn_name   = Country, 
           comname    = Specie.Common.Name,  
           sciname    = Specie.Scientific.Name, 
           ico_flag   = Flagship.Species,
           ico_local  = Priority.Species_Regional.and.Local,
           ico_global = Priority.Species_Global,
           ico_rgn    = Nation.Specific.List
    )
  # clean up names
  ico_list_raw <- ico_list_raw %>%
    mutate(rgn_name = str_trim(rgn_name),
           sciname  = str_trim(sciname),
           comname  = str_trim(comname)) %>%
    filter(sciname != '')  
  
  # convert global, flagship, and local iconic flags into single global iconic flag
  # ??? NOTE: 'local' is just humpbacks and minkes, across 50-60 countries each. Just call it global?
  ico_list <- ico_list_raw %>%
    mutate(ico_gl = (!is.na(ico_global) | !is.na(ico_local) | !is.na(ico_flag))) %>%
    select(-ico_global, -ico_local, -ico_flag) 

  # load IUCN list - does this contain year? does it matter? if fast, just reload all data
  spp_all <- read.csv(file.path(dir_anx, scenario, 'intermediate/spp_all_cleaned.csv'), 
                     stringsAsFactors = FALSE)

  # join ico_list to spp_all to incorporate category info and parent/subpop info.
  ico_list <- ico_list %>%
    left_join(spp_all %>%
                select(sciname, iucn_category, popn_trend, parent_sid, subpop_sid) %>%
                filter(sciname %in% ico_list$sciname)
              by = 'sciname')
  cat_list <- c('LC', 'NT', 'VU', 'EN', 'CR', 'EX')
  ico_list <- ico_list %>%
    mutate(popn_trend = tolower(popn_trend)
           category = ifelse(category == 'DD', NA, category),
           category = ifelse(category == 'LR/lc', 'LC', category),
           category = ifelse(cateogry == 'LR/nt', 'NT', category)) %>%
    filter(iucn_category %in% cat_list)
  
  # convert regional iconic status into a rgn_id, and join to ico_list.
  # - consider only observations with ico_rgn indicator
  # - for these, attach an 'ico_rgn_id' variable to track which countries to 
  #   count for these species
  rgn_name_file <- '~/github/ohi-global/eez2013/layers/rgn_global.csv' # ??? update this - Mel has a new one
  rgn_names <- read_csv(rgn_name_file)
  ico_list <- ico_list %>%
    left_join(ico_list %>%
                filter(ico_rgn != '' | is.na(spatial_source)) %>%
                select(rgn_name, sciname) %>%
                left_join(rgn_names, by = c('rgn_name' = 'label')),
              by = c('rgn_name', 'sciname')) %>% 
    rename(ico_rgn_id = rgn_id) %>% 
      # note: check that all countries properly translate on this. 
      # Should be easy since I can control the rgn-specific country names in the spreadsheet.
    unique
  return(ico_list)
}


#############################################################################=
get_ico_details = function(sid, download_tries = 10) {
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
  # ??? check for possibly/regionally extinct in here as well
  # NOTE greater specificity 
  #   for http://www.iucnredlist.org/details/135322/0 -- United States (Alaska) 
  #   vs  http://api.iucnredlist.org/details/135322/0 -- United States
  #countries = xpathSApply(h, '//td[@class="label"][strong="Countries:"]/following-sibling::td/div/text()', xmlValue)    
  
  return(countries)
}
# ??? update to scrape possibly/regionally extinct countries
# ??? also include population trend in this function (maybe 'get_details')

#############################################################################=
get_ico_details_all <- function(df = ico_list, reload = FALSE) {
  ### gets a list of countries in which parents and subpops appear, from
  ### the scraped data in iucn_details.
  
  ico_countries_file <- file.path(dir_anx, 'tmp/ico_subpop_countries.csv')
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
# ??? pretty good already - include regionally/possibly extinct functionality and trend

#############################################################################=
ico_rgn_name_to_number <- function(ico_countries) {
  rgn_name_file <- '~/github/ohi-global/eez2013/layers/rgn_global.csv'
  rgn_names <- read_csv(rgn_name_file) %>%
    rename(rgn_name = label)
  
  rgn_iucn2ohi <- read.csv(file.path(dir_anx, 'rgns/rgns_iucn2ohi.csv'), stringsAsFactors = FALSE)
  ico_countries <- ico_countries %>%
    left_join(rgn_names,
              by = 'rgn_name')
  ico_countries <- ico_countries %>%
    left_join(rgn_iucn2ohi, 
              by = c('rgn_name' = 'iucn_rgn_name')) %>%
    mutate(rgn_id = ifelse(is.na(rgn_id), ohi_rgn_id, rgn_id)) %>%
    select(-ohi_rgn_id) %>%
    filter(!is.na(rgn_id))
  return(ico_countries)
}
# ??? good - may need to update with new mismatches
# This function is necessary for translating country lists from IUCN sites, with mismatches
# - so comes after all that process.
# Converts ALL the regions into rgn_id - should start with the basic region list as in 
# the get_ico_list function, and then apply patches afterward?
# OR: bind the patch list to Melanie's rgn name to number table and do it one swoop.

#############################################################################=
process_ico_rgn <- function(ico_rgn_list) {
  ### Summarize category and trend for each region.
  
  # to overall lookup table, join scores for population category and trend.
  popn_cat    <- data.frame(iucn_category  = c('LC', 'NT', 'VU', 'EN', 'CR', 'EX'), 
                            category_score = c(   0,  0.2,  0.4,  0.6,  0.8,   1))
  popn_trend  <- data.frame(trend=c('decreasing', 'stable', 'increasing'), 
                            trend_score=c(-0.5, 0, 0.5))
  ico_rgn_list <- ico_rgn_list %>%
    left_join(popn_cat,   by = 'iucn_category') %>%
    left_join(popn_trend, by = 'trend') %>%
    select(-iucn_category, -trend)
  
  ### This section aggregates category and trend for a single species sciname within a region,
  ### including parent and all subpopulations present in a region.
  ### Species, including parent and all subpops, is weighted same as species w/o parents and subpops.
  ico_rgn_list <- ico_rgn_list %>%
    group_by(rgn_id, sciname) %>%
    summarize(category_score = mean(category_score), trend_score = mean(trend_score, na.rm = TRUE))

  ico_rgn_sum <- ico_rgn_list %>%
    group_by(rgn_id) %>%
    summarize(mean_cat = mean(category_score), mean_trend = mean(trend_score, na.rm = TRUE)) %>%
    mutate(status = ((1 - mean_cat) - 0.25) / 0.75,
           status = ifelse(status < 0, 0, status))
  
  ico_rgn_sum_file <- file.path(dir_anx, scenario, 'summary/ico_rgn_sum.csv')
  cat(sprintf('Writing file for iconic species summary by region: \n  %s\n', ico_rgn_sum_file))
  write_csv(ico_rgn_sum, ico_rgn_sum_file)
  
  return(invisible(ico_rgn_sum))
}

