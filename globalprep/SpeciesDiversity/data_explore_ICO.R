##############################################################################=
### ohiprep/globalprep/SpeciesDiversity/data_prep.R
###
### GOAL: Obtain species diversity data and iconics data for global
### Jun 1, 2015 - CCO.  Combining many different scripts into one data_prep.R
###   that calls functions and sources code within R/spp_fxn.R
##############################################################################=
library(readr)      # for read_csv()
#library(RCurl)
library(XML)


setwd('~/github/ohiprep')
source('src/R/common.R')

goal     <- 'globalprep/SpeciesDiversity'
scenario <- 'v2015'
dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal) 
dir_git  <- file.path('~/github/ohiprep', goal)

source(file.path(dir_git, 'R/spp_fxn.R'))
source(file.path(dir_git, 'R/ico_fxn.R'))
# SPP-specific and ICO-specific functions


##############################################################################=
### ICO -----
##############################################################################=

##############################################################################=
### get master list of Iconic Species -----
##############################################################################=
# this creates ico_list_full.csv
get_ico_list2 <- function() {
  ico_list_file <- file.path(dir_anx, 'ico/ico_global_list.csv')
  cat(sprintf('Reading raw iconic species list master file from: \n  %s\n', ico_list_file))
  ico_list_raw <- read.csv(ico_list_file, stringsAsFactors = FALSE) %>%
    select(rgn_name   = Country, 
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
    mutate(rgn_name = str_trim(rgn_name),
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
              by = 'sciname') 
# This is code to override cat and trend with IUCN.  We will save that for later.
#     %>%
#     mutate(popn_trend    = tolower(popn_trend),
#            iucn_category = ifelse(is.na(iucn_category), 
#                                   as.character(ico_category), 
#                                   as.character(iucn_category)),
#            trend         = ifelse(is.na(popn_trend) | popn_trend == 'unknown', 
#                                   as.character(ico_trend), 
#                                   as.character(popn_trend))) %>%
#     select(-ico_category, -ico_trend, -popn_trend)
  
  # convert regional iconic status into a rgn_id, and join to ico_list.
  # - consider only observations with ico_rgn indicator or without spatial data.
  # - for these, attach an "ico_rgn_id" variable to track which countries to 
  #   count for these species
  # - then from all lines, remove the "country" variable, losing the country
  #   specific info from the spreadsheet for all but locally iconic species and
  #   those species without IUCN or AM spatial data.  The spatial data will
  #   instead be used to fill out the countries for those.
  rgn_name_file <- '~/github/ohi-global/eez2013/layers/rgn_global.csv'
  rgn_names <- read_csv(rgn_name_file)
#   ico_list <- ico_list %>%
#     left_join(ico_list %>%
#                 filter(ico_rgn != '' | is.na(spatial_source)) %>%
#                 select(rgn_name, sciname) %>%
#                 left_join(rgn_names, by = c('rgn_name' = 'label')),
#               by = c('rgn_name', 'sciname')) %>% 
  ico_list <- ico_list %>%
    left_join(ico_list %>%
                filter(ico_rgn != '' | is.na(spatial_source)) %>%
                select(rgn_name, sciname) %>%
                left_join(rgn_names, by = c('rgn_name' = 'label')),
              by = c('rgn_name', 'sciname')) %>% 
    select(-ico_rgn, ico_rgn_id = rgn_id) %>% 
    left_join(rgn_names, by = c('rgn_name' = 'label')) %>%
#  select(-rgn_name, -ico_rgn, ico_rgn_id = rgn_id) %>% 
#    filter(iucn_category != 'DD') %>% let's leave in for now...
    unique
# technically could get rid of rgn_name and comname now if this is all correct...
# fix with patches here, rather than later.
# the rgn_id list is still flaky.
  write_csv(ico_list, file.path(dir_git, 'tmp/ico_list_full.csv'))
  return(ico_list)
}

ico_list <-   read.csv(file.path(dir_git, 'tmp/ico_list_full.csv'), stringsAsFactors = FALSE)

mismatch_rgn <- ico_list %>% 
  filter(is.na(rgn_id)) %>% select(rgn_name) %>% unique()
# 393 mismatches out of 3152 observations, across 41 countries.
# fix country names that don't match the OHI list.
rgn_iucn2ohi <- read.csv(file.path(dir_anx, 'rgns/rgns_iucn2ohi.csv'), stringsAsFactors = FALSE)
ico_list1 <- ico_list %>%
  rename(rgn_name = country) %>%
  left_join(rgn_iucn2ohi, 
            by = c('rgn_name' = 'iucn_rgn_name')) %>%
  mutate(rgn_id = ifelse(is.na(rgn_id), ohi_rgn_id, rgn_id)) %>%
  select(-ohi_rgn_id, -ohi_rgn_name)

mismatch_rgn <- ico_list1 %>% 
  filter(is.na(rgn_id)) %>% select(rgn_name) %>% unique()

ico_list1 <- ico_list1 %>%
  filter(!is.na(rgn_id))

# fix these country names -----
      # C<aa>te d'Ivoire
      # French Southern Territories (the)
      # Libyan Arab Jamahiriya
      # Netherlands Antilles
      # British Indian Ocean Territory (Chagos Archipelago)
      # Disputed Territory (Paracel Is., Spratly Is.)
      # French Polynesia (Tuamotu)
      # Micronesia, Federated States of
      # United States Minor Outlying Islands (Wake Is.)
      # Lao People's Democratic Republic
      # Province of China
      # Dominican Republic Ecuador
      # Ethiopia
      # Sao Tom_ and Principe
# -----
##############################################################################=
### Determine species lists by region based on IUCN and AM spatial data -----
##############################################################################=

get_ico_rgn_iucn2 <- function(ico_list, reload = FALSE) {
  ico_rgn_iucn_file <- file.path(dir_git, 'tmp/ico_rgn_iucn.csv')
  if(!file.exists(ico_rgn_iucn_file) | reload) {
    ico_list_iucn <- ico_list %>%
      filter(str_detect(spatial_source, 'iucn')) %>%
      filter(ico_category != 'EX') %>%
      select(-rgn_name, -rgn_id) %>%
      unique()
    cat(sprintf('ico_list_iucn has %d elements\n', nrow(ico_list_iucn)))
    
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

get_ico_rgn_am2   <- function(ico_list, reload = FALSE) {
  ico_rgn_file <- file.path(dir_git, 'tmp/ico_rgn_am.csv')
  if(!file.exists(ico_rgn_file) | reload) {
    ico_list_am <- ico_list1 %>%
      filter(str_detect(spatial_source, 'am')) %>%
      filter(ico_category != 'EX') %>%
      mutate(am_sid = as.character(am_sid)) %>%
      select(-rgn_name, -rgn_id) %>%
      unique()
    cat(sprintf('ico_list_am has %d elements\n', nrow(ico_list_sp)))
    
    cells_spp <- get_am_cells_spp() %>%
      select(-proportionArea, -cell_area)
    
    ico_am_sid <- unique(ico_list_sp$am_sid)
    
    
    ### filter is faster than join.  Filter iucn_cells_spp by sciname; then join to LOICZID <-> rgn table
    ico_cells <- cells_spp %>%
      filter(am_sid %in% ico_am_sid)
    ico_rgn <- ico_cells %>%
      group_by(rgn_id, am_sid) %>%
      summarize(ncells = n()) %>%
      inner_join(ico_list_am, by = 'am_sid')
    ico_rgn <- ico_rgn %>%
      filter(ico_gl == TRUE | rgn_id == ico_rgn_id) %>%
      filter(!is.na(rgn_id)) %>% # ???
      select(-ncells, -ico_gl, -ico_rgn_id) %>%
      unique()
    
    cat(sprintf('Writing regional presence of iconic species from AM spatial data. \n  %s\n', ico_rgn_file))
    write_csv(ico_rgn, ico_rgn_file)
    
  } else {
    cat(sprintf('Reading regional presence of iconic species from AM spatial data from:\n  %s\n', ico_rgn_file))
    ico_rgn <- read_csv(ico_rgn_file)
  }
  return(ico_rgn)
}

ico_rgn_iucn <- get_ico_rgn_iucn2(ico_list1, reload = FALSE) 
### rgn_id | sciname | comname | iucn_sid | am_sid | iucn_category |
### spatial_source | parent_sid | subpop_sid | trend

ico_rgn_am <- get_ico_rgn_am2(ico_list, reload = FALSE)
### rgn_id | am_sid | comname | sciname | iucn_sid | trend |
### iucn_category | spatial_source | parent_sid | subpop_sid |

### Fill in species lists by region based on original spreadsheet
ico_rgn_other <- ico_list1 %>% 
  filter(is.na(spatial_source)) %>%
  select(-rgn_name, -ico_gl, -ico_rgn_id)

##############################################################################=
### Combine ICO lists from all spatial sources, add parent/subpop regions -----
##############################################################################=

# A few checks:
# mismatches in category
mismatch_cat <- ico_list1 %>% 
  filter(ico_category != iucn_category) %>% select(sciname, ico_category, ico_trend, iucn_category, popn_trend, iucn_sid, spatial_source) %>% unique()
mismatch_cat_no_subpops <- mismatch_cat %>% filter(!str_detect(spatial_source, 'subpop'))

ico_rgn_all <- bind_rows(ico_rgn_iucn, ico_rgn_am, ico_rgn_other)

# Spatial/category from original list (no subpops), compared with 2013 results: 
# - done; close match
# Spatial from original list, category with subpops, compared with 2013: 
# - done; vertical spread
# Category from original list (no subpops), spatial from IUCN 
# - done
# Category from original list (no subpops), spatial from AM 
# - done

# Compare country lists vs presence from IUCN shapefile
# - for each species with IUCN shapefiles, create country list and compare to orig spreadsheet (or IUCN site)
# - do similar for AM species.
##############################################################################=
### compare country list from SS to lists from IUCN and AM -----
##############################################################################=
rgn_list_iucn <- ico_rgn_iucn %>%
  select(sciname, rgn_id, spatial_source)
rgn_list_am <- ico_rgn_am %>%
  select(sciname, rgn_id, spatial_source)
rgn_iucn_in_ss <- ico_list1 %>%
  filter(sciname %in% ico_rgn_iucn$sciname) %>%
  select(sciname, rgn_id, spatial_source)
rgn_am_in_ss <- ico_list1 %>%
  filter(sciname %in% ico_rgn_am$sciname) %>%
  select(sciname, rgn_id, spatial_source)

rgn_name_file <- '~/github/ohi-global/eez2013/layers/rgn_global.csv'
rgn_names <- read_csv(rgn_name_file) %>% rename(rgn_name = label)
  
rgn_iucn_diffs <- setdiff(rgn_list_iucn, rgn_iucn_in_ss) %>%    # in shp, not in ss
  left_join(rgn_names, by = 'rgn_id')
rgn_am_diffs <- setdiff(rgn_list_am, rgn_am_in_ss) %>%          # in map, not in ss
  left_join(rgn_names, by = 'rgn_id')
rgn_iucn_diffs2 <- setdiff(rgn_iucn_in_ss, rgn_list_iucn) %>%   # in ss, not in shp
  left_join(rgn_names, by = 'rgn_id')
rgn_am_diffs2 <- setdiff(rgn_am_in_ss, rgn_list_am) %>%         # in ss, not in map
  left_join(rgn_names, by = 'rgn_id')

            
country_lists_am   <- ico_list1ico_rgn_am

##############################################################################=
### Load 2013 status and convert to scores -----
##############################################################################=
# ico_status and trend not in correct format in ohi-global/eez2013.
dir_global <- ('~/github/ohi-global')
comp_scenario  <- 'eez2013'
cat_conv    <- data.frame(category    = c("LC", "NT", "VU", "EN", "CR", "EX"), 
                          cat_score   = c(   0,  0.2,  0.4,  0.6,  0.8,   1))

ico_status_raw <- read.csv(file.path(dir_global, comp_scenario, 'layers/ico_spp_extinction_status.csv'), stringsAsFactors = FALSE) 
ico_status_iucn_only <- ico_status_raw %>%
  filter(sciname %in% ico_rgn_iucn$sciname)
ico_status_am_only <- ico_status_raw %>%
  filter(sciname %in% ico_rgn_am$sciname)

ico_status_compare <- ico_status_raw %>%
  left_join(cat_conv, by = 'category') %>%
  group_by(rgn_id) %>%
  summarize(mean_cat = mean(cat_score, na.rm = TRUE)) %>%
  mutate(score = ((1 - mean_cat) - 0.25) / 0.75,
         score = ifelse(score < 0, 0, score))
ico_status_compare_iucn_only <- ico_status_iucn_only %>%
  left_join(cat_conv, by = 'category') %>%
  group_by(rgn_id) %>%
  summarize(mean_cat = mean(cat_score, na.rm = TRUE)) %>%
  mutate(score = ((1 - mean_cat) - 0.25) / 0.75,
         score = ifelse(score < 0, 0, score))
write_csv(ico_status_compare_iucn_only, file.path(dir_git, sprintf('tmp/ico_status_%s_iucn.csv', comp_scenario)))
ico_status_compare_am_only <- ico_status_am_only %>%
  left_join(cat_conv, by = 'category') %>%
  group_by(rgn_id) %>%
  summarize(mean_cat = mean(cat_score, na.rm = TRUE)) %>%
  mutate(score = ((1 - mean_cat) - 0.25) / 0.75,
         score = ifelse(score < 0, 0, score))
write_csv(ico_status_compare_am_only, file.path(dir_git, sprintf('tmp/ico_status_%s_am.csv', comp_scenario)))

##############################################################################=
### process new data chunked into AM and IUCN - WITH subpops, for graphing -----
##############################################################################=

process_ico_rgn2 <- function(ico_rgn_all, filename_flag = '') {
  ### Summarize category and trend for each region.
  
  # to overall lookup table, join scores for population category and trend.
  popn_cat    <- data.frame(iucn_category  = c("LC", "NT", "VU", "EN", "CR", "EX"), 
                            category_score = c(   0,  0.2,  0.4,  0.6,  0.8,   1))
  popn_trend  <- data.frame(trend=c("decreasing", "stable", "increasing"), 
                            trend_score=c(-0.5, 0, 0.5))
  ico_rgn_all <- ico_rgn_all %>%
    rename(trend = popn_trend) %>%
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
    mutate(status = ((1 - mean_cat) - 0.25) / 0.75,
           status = ifelse(status < 0, 0, status))
  
  ico_rgn_sum_file <- file.path(dir_git, sprintf('tmp/ico_rgn_sum%s.csv', filename_flag))
  cat(sprintf('Writing file for iconic species summary by region: \n  %s\n', ico_rgn_sum_file))
  write_csv(ico_rgn_sum, ico_rgn_sum_file)
  
  return(invisible(ico_rgn_sum))
}

# create rgn summaries for IUCN, AM separately, with subpops removed.
ico_rgn_iucn1 <- ico_rgn_iucn
ico_rgn_am1   <- ico_rgn_am

ico_rgn_sum_iucn1 <- process_ico_rgn2(ico_rgn_iucn1, filename_flag = '_iucn1')
ico_rgn_sum_am1   <- process_ico_rgn2(ico_rgn_am2,   filename_flag = '_am1')

ico_status_am1 <- ico_rgn_sum_am1 %>%
  select(rgn_id, score = status)
ico_status_iucn1 <- ico_rgn_sum_iucn1 %>%
  select(rgn_id, score = status)
write_csv(ico_status_am2, file.path(dir_git, 'tmp/ico_status_am1.csv'))
write_csv(ico_status_iucn2,  file.path(dir_git, 'tmp/ico_status_iucn1.csv'))

##############################################################################=
### process new data chunked into AM and IUCN without subpops, for graphing -----
##############################################################################=

# create rgn summaries for IUCN, AM separately, with subpops removed.
ico_rgn_iucn2 <- ico_rgn_iucn %>% filter(!str_detect(spatial_source, 'subpop'))
ico_rgn_am2   <- ico_rgn_am %>% filter(!str_detect(spatial_source, 'subpop'))

ico_rgn_sum_iucn2 <- process_ico_rgn2(ico_rgn_iucn2, filename_flag = '_iucn2')
ico_rgn_sum_am2   <- process_ico_rgn2(ico_rgn_am2,   filename_flag = '_am2')

ico_status_am2 <- ico_rgn_sum_am2 %>%
  select(rgn_id, score = status)
ico_status_iucn2 <- ico_rgn_sum_iucn2 %>%
  select(rgn_id, score = status)
write_csv(ico_status_am2, file.path(dir_git, 'tmp/ico_status_am2.csv'))
write_csv(ico_status_iucn2,  file.path(dir_git, 'tmp/ico_status_iucn2.csv'))

##############################################################################=
### process new data chunked into AM and IUCN sans subpops, but with list data for the other side -----
##############################################################################=

# create rgn summaries for IUCN, AM separately, with subpops removed.
### Fill in species lists by region based on original spreadsheet
ico_rgn_non_iucn <- ico_list1 %>% 
  filter(is.na(spatial_source) | str_detect(spatial_source, 'am')) %>%
  select(-rgn_name, -ico_gl, -ico_rgn_id)
ico_rgn_non_am <- ico_list1 %>% 
  filter(is.na(spatial_source) | str_detect(spatial_source, 'iucn')) %>%
  select(-rgn_name, -ico_gl, -ico_rgn_id)

ico_rgn_iucn3 <- ico_rgn_iucn %>% 
  bind_rows(ico_rgn_non_iucn) %>%
  filter(!str_detect(spatial_source, 'subpop'))
ico_rgn_am3   <- ico_rgn_am %>% 
  bind_rows(ico_rgn_non_am) %>%
  filter(!str_detect(spatial_source, 'subpop'))

ico_rgn_sum_iucn3 <- process_ico_rgn2(ico_rgn_iucn3, filename_flag = '_iucn3')
ico_rgn_sum_am3   <- process_ico_rgn2(ico_rgn_am3,   filename_flag = '_am3')

ico_status_am3 <- ico_rgn_sum_am3 %>%
  select(rgn_id, score = status)
ico_status_iucn3 <- ico_rgn_sum_iucn3 %>%
  select(rgn_id, score = status)
write_csv(ico_status_am3, file.path(dir_git, 'tmp/ico_status_am3.csv'))
write_csv(ico_status_iucn3,  file.path(dir_git, 'tmp/ico_status_iucn3.csv'))

### process original spreadsheet data but subpops -----
##############################################################################=

x <- ico_list1 %>% 
  filter(iucn_category != 'DD' & !is.na(iucn_category))


ss1 <- process_ico_rgn2(x, filename_flag = '_iucn_cat_only')
ico_status_ss1 <- ss1 %>%
  select(rgn_id, score = status)
write_csv(ico_status_ss1,  file.path(dir_git, 'tmp/ico_status_ss1.csv'))



# ico_list_subpops <- ico_rgn_all %>% 
#   filter(!is.na(parent_sid) | !is.na(subpop_sid)) %>%
#   select(sciname, iucn_sid) %>%
#   unique()
# 
# ico_subpop_rgn_ids <- get_countries_all(ico_list_subpops, reload = TRUE)
# 
# ico_rgn_all <- ico_rgn_all %>%
#   left_join(ico_subpop_rgn_ids %>%
#               select(sid, rgn_id) %>%
#               mutate(present = TRUE),
#             by = c('iucn_sid' = 'sid', 'rgn_id'))
# 
# ico_rgn_all <- ico_rgn_all %>%
#   filter(!((str_detect(spatial_source, 'parent') | str_detect(spatial_source, 'subpop')) & is.na(present))) %>%
#   select(-present, -parent_sid, -subpop_sid) %>%
#   unique()


# write_csv(ico_rgn_all, file.path(dir_anx, scenario, 'intermediate/ico_rgn_all.csv'))

##############################################################################=
### process new data, using both spatial sources, WITH subpops -----
##############################################################################=
# ico_rgn_all <- read.csv(file.path(dir_anx, scenario, 'intermediate/ico_rgn_all.csv'), stringsAsFactors = FALSE)
ico_rgn_all1 <- ico_rgn_all %>% filter(!str_detect(spatial_source, 'subpop'))
ico_rgn_sum <- process_ico_rgn(ico_rgn_all1)
### rgn_id | mean_cat | mean_trend | status

ico_status <- ico_rgn_sum %>%
  select(rgn_id, score = status)
ico_trend <- ico_rgn_sum %>%
  select(rgn_id, score = mean_trend)
write_csv(ico_status, file.path(dir_git, scenario, 'data/ico_status_no_subpops.csv'))
write_csv(ico_trend,  file.path(dir_git, scenario, 'data/ico_trend_no_subpops.csv'))
# write_csv(ico_status, file.path(dir_git, scenario, 'data/ico_status.csv'))
# write_csv(ico_trend,  file.path(dir_git, scenario, 'data/ico_trend.csv'))


##############################################################################=
### Some check functions -----
##############################################################################=
x <- ico_rgn_source_compare()
# For each region, checks whether the presence of a particular species is
# flagged due to spatially-explicit data (AM or IUCN range maps), the original
# spreadsheet, or both.
# NOTE: doesn't account for parent/subpop designations.




### IUCN species with parent/subpopulations:
# for parents and subpops, get country lists from iucn_details - see BB's script
#   function: get countries
#     str_split, turn into vector
#   for (list of parents/subpops by iucn_sid) get countries for each species ID
#     don't bind to the main table; just leave as lookup of iucn_sid:rgn_id
# convert the country lists into region ID numbers; create a lookup table for
#   parent_sid and subpop_sid vs rgn_id
# create a 'present' flag for the iucn list: each line is rgn_id matched to sciname (and thus all the other fields).
#   for parent/subpops, each will have a distinct iucn_sid (even for AM)
#   for each line, if rgn_id %in% country vector for that iucn_sid, flag present = TRUE
#   * captures both parent and subpop presence...?
# then: for the whole list, filter for (spatial_source == 'iucn' | present == TRUE)
# then: for this list, filter for global == TRUE or region specific == rgn_id

# 25 duplicated rows - species duplicated within a region.  Species: 
# Different IUCN species IDs, but no IUCN ID available for spatial information, 
# so based upon an undifferentiated IUCN shapefile.  Look up each and assign to regions by hand.
# Check in the shapefile itself - MAMMMARINE.shp or whatever.
#   Balaena mysticetus-----
#     - PARENT: http://www.iucnredlist.org/details/2467/0, LC, unknown, Canada; Greenland; United States
#         am_sid iucn_sid            sciname am_category iucn_category popn_trend
#   1 ITS-180533     2468 Balaena mysticetus          LC            NT    Increasing 
#     - http://www.iucnredlist.org/details/2468/0
#     - Bering-Chukchi-Beaufort Seas, Hudson Bay-Foxe Basin and Baffin Bay-Davis Strait
#     - Native: Canada; Russian Federation; United States
#   2 ITS-180533     2469 Balaena mysticetus          LC            EN    Unknown
#     - http://www.iucnredlist.org/details/2469/0
#     - Sea of Okhotsk from Shantarskiye Zaliv east to Zaliv Shelikova, Gizhiginskaya Guba and 
#       Penzhinskaya Guba (Moore and Reeves 1993, Rice 1998). 
#       Native:  Russian Federation
#   3 ITS-180533     2472 Balaena mysticetus          LC            CR    Unknown 
#     - http://www.iucnredlist.org/details/2472/0
#     - Svalbard-Barents Sea (Spitsbergen) subpopulation, This subpopulation ranges from the east 
#       coast of Greenland (Denmark) across the Greenland Sea, the Barents Sea, and the Kara Sea to 
#       Severnaya Zemlya (Russian Federation), and south at least occasionally to northern Iceland 
#       and the coast of Finnmark (Norway) and Jan Mayen (Norway) (Rice 1998).
#     - Native: Greenland; Svalbard and Jan Mayen (Norway)
#   Tursiops truncatus -----
#     - PARENT: http://www.iucnredlist.org/details/22563/0, LC, unknown; worldwide range
#   4 ITS-180426   194300 Tursiops truncatus          LC            CR Decreasing 
#     - Fiordland subpopulation - http://www.iucnredlist.org/details/194300/0
#     - Native: New Zealand 
#   5 ITS-180426 16369383 Tursiops truncatus          LC            VU Decreasing 
#     - Med subpop - http://www.iucnredlist.org/details/16369383/0
#     - Native: Albania; Algeria; Bosnia and Herzegovina; Croatia; Cyprus; France; Gibraltar; 
#       Greece; Israel; Italy; Lebanon; Libya; Malta; Monaco; Montenegro; Morocco; Serbia (Serbia); 
#       Slovenia; Spain; Syrian Arab Republic; Tunisia; Turkey
#   # another, not on our list: Black Sea, ssp ponticus, EN/unknown - http://www.iucnredlist.org/details/133714/0
#
# this might be a case for not removing the parent populations.  Bottlenose dolphins should
# generally be LC.  Check the IUCN shapefiles and see what other info is available.


### Aquamaps ICO species with parents/subpopulations:
# Different IUCN species IDs, but no IUCN spatial information, so based upon
# an undifferentiated Aquamaps map.  Look up each and assign to regions by hand.
# Check on IUCN site by iucn_sid, might have country ranges for each subpop.
# * Dermochelys coriacea: LC, CR -----
#   - PARENT: http://www.iucnredlist.org/details/6494/0, VU, decreasing; large range
#     am_sid  iucn_sid              sciname am_category iucn_category popn_trend
#   Rep-2331  46967807 Dermochelys coriacea          CR            CR Decreasing - East Pacific Ocean subpopulation
#   - http://www.iucnredlist.org/details/46967807/0, CR, dec
#   - Native: Chile; Colombia; Costa Rica; Ecuador; El Salvador; France (Clipperton I.); Guatemala; 
#     Honduras; Mexico; Nicaragua; Panama; Peru; United States (Hawaiian Is.)
#   Rep-2331  46967817 Dermochelys coriacea          CR            CR Decreasing - West Pacific Ocean subpopulation
#   - http://www.iucnredlist.org/details/46967817/0
#   - large range
#   Rep-2331  46967827 Dermochelys coriacea          CR            LC Increasing - Northwest Atlantic Ocean subpopulation
#   - http://www.iucnredlist.org/details/46967827/0, LC, inc, large range
#   Rep-2331  46967838 Dermochelys coriacea          CR            CR Increasing - Southwest Atlantic Ocean subpopulation
#   - http://www.iucnredlist.org/details/46967838/0, CR, inc
#   - Angola (Angola); Argentina; Benin; Brazil; Cameroon; Congo; Congo, The Democratic 
#     Republic of the; Côte d'Ivoire; Equatorial Guinea; Gabon; Gambia; Guinea; Guinea-Bissau; 
#     Liberia; Namibia; Nigeria; Saint Helena, Ascension and Tristan da Cunha; Sao Tomé and Principe; 
#     Senegal; Sierra Leone; South Africa; Togo; Uruguay
#   Rep-2331  46967863 Dermochelys coriacea          CR            CR Decreasing - Southwest Indian Ocean subpopulation
#   - Angola (Angola); Comoros; French Southern Territories (Mozambique Channel Is.); Kenya; Madagascar; 
#     Mauritius; Mayotte; Mozambique; Namibia; Seychelles; South Africa; Tanzania, United Republic of
#   NOTE: couple of data-deficient ones in there too.  See PARENT url.
# * Lamna nasus:          CR, EN -----
#   PARENT: http://www.iucnredlist.org/details/11200/0, VU, dec
#   Fis-22768    39343          Lamna nasus          VU            CR Decreasing - Northeast Atlantic subpopulation
#   - http://www.iucnredlist.org/details/39343/0
#   - Denmark; France; Gibraltar; Iceland; Ireland; Norway; Portugal (Azores, Madeira); 
#     Russian Federation; Spain; Sweden; United Kingdom
#   Fis-22768    39344          Lamna nasus          VU            EN Decreasing - Northwest Atlantic subpopulation
#   - Bermuda; Canada (Newfoundland I, Nova Scotia); Greenland; United States (Maine, Massachusetts, New Jersey, New York, Rhode Island)
#   Fis-22768    61420          Lamna nasus          VU            CR Decreasing - Mediterranean subpopulation
#   - http://www.iucnredlist.org/details/61420/0
#   - Albania; Algeria; Bosnia and Herzegovina; Croatia; Cyprus; Egypt; France; Greece; Israel; Italy; 
#     Lebanon; Libya; Malta; Monaco; Montenegro; Morocco; Slovenia; Spain; Syrian Arab Republic; Tunisia; Turkey
# * Isurus oxyrinchus:    VU, NT -----
# - PARENT: http://www.iucnredlist.org/details/39341/0, VU, dec
#   Fis-58485   161749    Isurus oxyrinchus          VU            VU Decreasing - Atlantic subpopulation
#   - http://www.iucnredlist.org/details/161749/0
#   - wide range!
#   Fis-58485   161750    Isurus oxyrinchus          VU            VU Decreasing - Indo-west Pacific subpopulation
#   - wide range
#   Fis-58485   161751    Isurus oxyrinchus          VU            NT Decreasing - Eastern North Pacific subpopulation
#   - Chile (Antofagasta, Coquimbo, Santiago, Valparaíso); Costa Rica (Costa Rica (mainland)); 
#     Ecuador (Ecuador (mainland)); Guatemala; Honduras (Honduras (mainland)); Mexico; 
#     Nicaragua (Nicaragua (mainland)); Peru; United States (California, Washington)
# -----


