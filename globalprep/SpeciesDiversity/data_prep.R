##############################################################################=
### ohiprep/globalprep/SpeciesDiversity/data_prep.R
###
### GOAL: Obtain species diversity data and iconics data for global
### Jun 1, 2015 - CCO.  Combining many different scripts into one data_prep.R
###   that calls functions and sources code within R/spp_fxn.R
##############################################################################=
library(foreign)
library(data.table) # for fread()
library(sp)
library(rgdal)
library(raster)
library(readr)      # for read_csv()
#library(rPython) # to call Python functions and scripts within R?

setwd('~/github/ohiprep')
source('src/R/common.R')

goal     <- 'globalprep/SpeciesDiversity'
scenario <- 'v2015'
dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal) 
dir_git  <- file.path('~/github/ohiprep', goal)

source(file.path(dir_git, 'R/spp_fxn.R'))
# SPP-specific functions

##############################################################################=
### Ingest Aquamaps data to .csv from .sql ----
##############################################################################=
# get R version of SQL extract code from Melanie?
# ??? updated aquamaps data?  incorporate export.sql into R code?
# ??? This is probably just reading database and assigning variable names,
# ??? there may be better ways of doing this.  
# ??? In future, consider doing this using rodbc.
# ingest_aquamaps.R reads in data from sql files, and converts to .csvs?
# - ohi_speciesoccursum.sql
# - ohi_hcaf_species_native.sql: this is the big one, spatial locations for each species
# - hcaf.sql: hcaf stands for half-degree cells authority file.
# the harder part is probably extracting the data from the sql files.

ingest_aquamaps <- function() {
  library(logging)
  
  # Read in csv outputs from running raw\AquaMaps_OHI_082013\export.sql
  #   See raw\AquaMaps_OHI_082013\README.txt on a running instance of AquaMaps MySQL
  
  # set working directory
  orig_wd <- getwd()
  wd = 'N:/model/GL-NCEAS-SpeciesDiversity_v2013a' # ??? is this the right place to be?
  setwd(file.path(wd, 'raw/AquaMaps_OHI_082013'))  # ??? set up a variable for it rather than moving?
  
  # setup logging
  basicConfig()
  addHandler(writeToFile, logger='', file=file.path(wd,'cache','ingest_aquamaps.log'))
  
  # ??? Jamie's script combining_files.R already combined the headers and data for 2014 data - either use that, or combine it here, figure it out.
  # read in aquamaps header column names (hdr_*.csv) # ??? are these headers separate from the data? weird.
  loginfo('read in aquamaps header column names (hdr_*.csv)')
  cells.hdr     = read.csv('hdr_hcaf.csv'               , stringsAsFactors=F, header=F)[ , 1]
  cells_spp.hdr = read.csv('hdr_hcaf_species_native.csv', stringsAsFactors=F, header=F)[ , 1]
  spp.hdr       = read.csv('hdr_speciesoccursum.csv'    , stringsAsFactors=F, header=F)[ , 1] # use readr::read_csv() or data.table::fread()
  
  # read in aquamaps data (tbl_*.csv)
  loginfo('read in aquamaps data (tbl_*.csv)\n  cells')
  cells     = read.csv('tbl_hcaf.csv'               , header=F, col.names=cells.hdr    , na.strings='\\N')
  loginfo('  cells_spp')
  cells_spp = read.csv('tbl_hcaf_species_native.csv', header=F, col.names=cells_spp.hdr, na.strings='\\N')
  loginfo('  spp')
  spp       = read.csv('tbl_speciesoccursum.csv'    , header=F, col.names=spp.hdr      , na.strings='\\N')
  
  # write out to tmp
  setwd(file.path(wd, 'tmp')) 
  # ??? create loop for csvs
  #       e.g.  for x in c(cells, cells_spp, spp) {
  #               write file and log
  #             }
  loginfo('write out to tmp\n  am_cells_data.csv')
  write.csv(cells    , 'am_cells_data.csv'    , row.names=F, na='')
  loginfo('  am_cells_spp_data.csv')
  write.csv(cells_spp, 'am_cells_spp_data.csv', row.names=F, na='')
  loginfo('  am_spp_data.csv')
  write.csv(spp      , 'am_spp_data.csv'      , row.names=F, na='')
  loginfo('finished!')
  
  setwd(orig_wd)
  
  return(TRUE)
}
ingest_aquamaps()


##############################################################################=
### Ingest IUCN species list ----
##############################################################################=
# Update this and simplify/clarify

# pulls species list from IUCN
# scrapes data from iucn site for each species on the list; saves to cache
# extract habitats from cached files
# filter IUCN list down to just marine species
# fix older IUCN codes
# deal with subspecies - this looks complicated, needs more investigation
# Davis and Baum species?

# ??? convert into functions to be called from this script?
# ??? update for dplyr/tidyr; clean up output files - which are really necessary?
source('ingest_iucn.R')


##############################################################################=
### Generate lookup - species <-> category/trend and spatial_source ----
##############################################################################=

spp_all <- create_spp_master_lookup(reload = FALSE)
### Output is spp_all.csv data frame with these fields:
### | am_sid      | sciname     | am_category | iucn_sid | iucn_category | popn_trend     | 
### | popn_category | info_source | spp_group | id_no    | objectid    | spatial_source |
### Outputs saved to:
### * v201X/intermediate/spp_iucn_maps_all.csv 
###     (list of all species represented in the IUCN shape files)
### * v201X/intermediate/spp_all.csv (complete data frame)


##############################################################################=
### Generate lookup - IUCN species to LOICZID ----
##############################################################################=

extract_loiczid_per_spp(groups_override = tmp, reload = FALSE)
### Extract loiczid cell IDs for each species within each species group.  Save 
### a .csv file for that group, with fields:
###       sciname | iucn_sid | LOICZID | prop_area
### NOTES: this takes a long time - multiple hours for some of the shape files.  
### * reload = FALSE allows it to skip extraction on groups with files already present.
### * use groups_override argument to run function on partial list of species groups.


##############################################################################=
### Generate lookup - region ID to LOICZID/CSQ ----
##############################################################################=

rgn_cell_lookup <- extract_cell_id_per_region(reload = FALSE)
### | sp_id | loiczid | proportionArea | csq | cell_area
#   TO DO:  currently as sp_id; update to translate sp_id to rgn_id
# ??? DOES THIS STILL INCLUDE ALL THE HS and AQ SHIT? get rid of those?
# saves lookup table to git-annex/globalprep/SpeciesDiversity/rgns/region_prop_df.csv


##############################################################################=
### SPP - Generate species per cell tables for Aquamaps and IUCN -----
##############################################################################=

am_cells_spp_sum <- process_am_spp_per_cell(reload = FALSE)
### This returns dataframe with variables:
### loiczid | mean_cat_score | mean_trend_score | n_cat_species | n_trend_species
### AM does not include subspecies: every am_sid corresponds to exactly one sciname.
# > x <- spp_all %>% filter(!is.na(am_sid)) %>% select(am_sid, sciname) %>% unique()
# > sum(duplicated(x$am_sid))
# [1] 0
# > sum(duplicated(x$sciname))
# [1] 0

iucn_cells_spp_sum <- process_iucn_spp_per_cell(reload = FALSE)
### This returns dataframe with variables:
### loiczid | mean_cat_score | mean_trend_score | n_cat_species | n_trend_species
### IUCN includes subspecies - one sciname corresponds to multiple iucn_sid values.
# > x <- spp_all %>% filter(!is.na(iucn_sid)) %>% select(iucn_sid, sciname) %>% unique()
# > sum(duplicated(x$iucn_sid))
# [1] 0
# > sum(duplicated(x$sciname))
# [1] 99


##############################################################################=
### SPP - Summarize mean category and trend per cell and per region -----
##############################################################################=

summary_by_loiczid <- process_means_per_cell(am_cells_spp_sum, iucn_cells_spp_sum)
### This returns dataframe with variables:
### loiczid | weighted_mean_cat | weighted_mean_trend

summary_by_rgn     <- process_means_per_rgn(summary_by_loiczid, rgn_cell_lookup)
### This returns dataframe with variables:
### sp_id | rgn_mean_cat | rgn_mean_trend | status


##############################################################################=
### ICO -----
##############################################################################=

# TO DO: modify for ICO scores - 
#   determine subset of ICO spp per region 
#     (or is it a single global list of iconic species wherever they show up?)
#   not area-weighted - just present/absent within a region.

# TO DO: deal with populations for IUCN species.
#   examine range maps - are there overlaps between parent and subspecies/subpops?
#   - if not - no need to worry.
#   - if so  - worry!  once extracted, overlap info is lost.
#     - do a difference between parent and each subpop, and then use that for extract?
#     - if overlap is consistent (i.e. parent fully covers each subpop), just subtract prop_area?
#       - what if two subpopulations in same cell - might not overlap; so subtract won't work.
#   ??? are parents already taken out of the iucn_spp_marine_global? that's what it seems like.
#       In ingest_iucn script, looks like subpops are subsetted OUT of the main list.
#       But then merged back in, and then parents subsetted out.
#   TO DO: confirm that this is the right way to do things?  Does 'parent' really just consist
#       of sum of 'subpopulations' or could it encompass other areas?
#     * find the full list of parents and subpopulations for marine species, and see how
#       many are IUCN range map species.  Select by sci name, spot check a few in QGIS or ArcGIS.
# subpops <- read_csv(file.path(dir_anx, scenario, 'intermediate/spp_iucn_marine_subpopulations.csv')) %>% 
#   rename(parent = sid)
# spp_all_subpops <- spp_all %>%
#   filter(spatial_source == 'iucn') %>%
#   left_join(subpops,
#             by = c('iucn_sid' = 'subpopulations')) %>%
#   filter(!is.na(parent)) %>%
#   select(iucn_sid, parent, sciname, iucn_category, spp_group, spatial_source)
# TO DO: deal with sp_id --> proper region ID.  
#   Why are there different shape files?
#   Can we just read an updated shape file for global regions?  
#   * maybe just filter out CCAMLR/disputed/inland?
#   If not, can we just fix the region shape files and avoid this in the future?
#   * see github/ohiprep/globalprep/PressuresRegionExtract/CreateRaster.R

# NOTES ON model.R - should be covered in the above code.
# ??? compare BB and MF versions - BB version involves 3nm regions? MF version is for AQ and HS
# Averages IUCN score per cell for all species in that cell; then takes area-weighted average of those scores for each region
# .csv of score and trend output.
# ??? calculate this for both SPP and ICO - SPP is area-weighted (so rare species don't count as much), but ICO is not (rare species count just as much as common)

# For each Aquamaps cell, tally IUCN category scores for all species (AM and IUCN) within that cell.
# area-weighted?
# e.g. in cell 999, three species:
#   lobster, id_no 701, 1.00 area, IUCN cat LC (0.00), popn_trend decreasing (-.5)
#   fish,    id_no 205,  .65 area, IUCN cat NT (0.20), popn_trend stable     (0.0)
#   snail,   id_no 808,  .42 area, IUCN cat CR (0.80), popn_trend increasing (+.5)
# for an endangered species that is only partly indicated within a cell, area weighting reduces the penalty.
# E.g. above, the endangered snail, unweighted, gives the cell a penalty of .8; but the snail is only partly found
#      within the cell, so area-weighting would give that cell a penalty of .8 * .42 = .336.  If
#      only part of this cell fell within a country's EEZ, then it would get further reduced.
#      This seems to go along with proportional weighting of AquaMaps species by probability.  Is that what we want to do?
# count # of species; count # of species with trend.
# mean score across all species; mean trend across all species
# summarize across all cells in region: sum(area of cell * mean score (or trend) for cell) / sum(area of cell)


# some checks: 

# examine species names for mismatches/typos/name variations.  Similar defined as:
# * first five letters of one field (genus or species) match for both AM and IUCN.
# * exact match for other field.
sim_names <- check_sim_names(spp_all)

# check to see how closely IUCN category info matches between IUCN data and AM data
category_check <- spp_all %>% 
  filter(!is.na(iucn_category) & !is.na(am_category)) %>%
  select(sciname, am_category, iucn_category) %>%
  mutate(am_category   = as.character(am_category),
         iucn_category = as.character(iucn_category),
         cat_match = (am_category == iucn_category))
sum(category_check$cat_match)
# Out of 2962 species that include IUCN category from both AM and IUCN,
# 2862 of them are matches.  96.6% match.

# ICO ----
get_ico_list <- function() {
  ico_list_file <- file.path(dir_anx, 'ico/ico_global_list.csv')
  ico_list <- read.csv(ico_list_file, stringsAsFactors = FALSE) %>%
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
  ico_list <- ico_list %>%
    mutate(sciname = str_trim(sciname),
           comname = str_trim(comname)) %>%
    filter(sciname != '')
  
  sum(!is.na(ico_list$ico_global) | !is.na(ico_list$ico_flag))
  # 1673 out of 2592 are globally iconic &/or flagship species
  sum(!is.na(ico_list$ico_local)  &  is.na(ico_list$ico_global) & is.na(ico_list$ico_flag))
  # 63 are locally iconic but not on global or flagship lists; all are minke whales.  The rest of the locally important list is all humpback whales.
  sum((ico_list$ico_rgn != '') & is.na(ico_list$ico_local)  &  is.na(ico_list$ico_global) & is.na(ico_list$ico_flag))
  # 45 are on nation-specific list that aren't included elsewhere
  
  
  # convert global, flagship, and local iconic flags into single global iconic flag
  # note 'local' is just humpbacks and minkes, across 50-60 countries each. Just call it global.
  ico_list <- ico_list %>%
    mutate(ico_gl = (!is.na(ico_global) | !is.na(ico_local) | !is.na(ico_flag))) %>%
    select(-ico_global, -ico_local, -ico_flag) 
  
  
  # convert long text IUCN categories into letter codes.
  cat_lookup  <- data.frame(ico_category = c("LC", "NT", "VU", "EN", "CR", "EX", "DD"), 
                            ico_cat_long = c('least concern', 'near threatened', 'vulnerable', 'endangered', 'critically endangered', 'extinct', 'data deficient'))
  ico_list <- ico_list %>%
    mutate(ico_cat_long = tolower(ico_cat_long)) %>%
    left_join(cat_lookup, by = 'ico_cat_long') %>%
    select(-ico_cat_long)
  
  # join to spp_all and update category/trend info if available from IUCN spreadsheet
  ico_list <- ico_list %>%
    left_join(spp_all %>%
                select(iucn_sid, am_sid, sciname, popn_trend, iucn_category, spatial_source), 
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
      filter(spatial_source == 'iucn') %>%
      select(-spatial_source)
    
    ### Load IUCN species per cell tables : takes a while
    iucn_cells_spp <- get_iucn_cells_spp()
    
    iucn_ico_names <- unique(ico_list_iucn$sciname)
    if(!exists('rgn_cell_lookup'))  rgn_cell_lookup <- extract_cell_id_per_region()
    
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
      select(rgn_id, sciname, comname, iucn_category, trend) %>%
      unique()
    cat(sprintf('Writing regional presence of iconic species from IUCN spatial data. \n  %s\n', ico_rgn_iucn_file))
    write_csv(ico_rgn_iucn, ico_rgn_iucn_file)
    
    # Two problem species appear, with same sciname, two iucn_categories, and no 
    # spatial differentiation by iucn_sid:
    # Tursiops truncatus  CR
    # Tursiops truncatus  VU
    # Balaena mysticetus  NT
    # Balaena mysticetus  EN
    # Balaena mysticetus  CR
  } else {
    cat(sprintf('Reading regional presence of iconic species from IUCN spatial data from:\n  %s\n', ico_rgn_iucn_file))
    ico_rgn_iucn <- read_csv(ico_rgn_iucn_file)
  }
  return(ico_rgn_iucn)
}

ico_list <- get_ico_list()

ico_rgn_iucn <- get_ico_rgn_iucn(ico_list) 

get_ico_rgn_am   <- function(ico_list, reload = FALSE) {
  ico_rgn_file <- file.path(dir_anx, scenario, sprintf('intermediate/ico_rgn_%s.csv', 'am'))
  if(!file.exists(ico_rgn_file) | reload) {
    ico_list_sp <- ico_list %>%
      filter(spatial_source == 'am') %>%
      select(-spatial_source) %>%
      mutate(am_sid = as.character(am_sid))
    
    if(!exists('rgn_cell_lookup'))  rgn_cell_lookup <- extract_cell_id_per_region()
    cells_spp <- get_am_cells_spp() %>%
      select(-proportionArea, -cell_area)
    
    ico_am_sid <- unique(ico_list_sp$am_sid)
    
    
    ### filter is faster than join.  Filter iucn_cells_spp by sciname; then join to LOICZID <-> rgn table
    ico_cells <- cells_spp %>%
      filter(am_sid %in% ico_am_sid)
    ico_rgn <- ico_cells %>%
      group_by(rgn_id, sciname, id_no) %>%
      summarize(ncells = n()) %>%
      inner_join(ico_list, by = 'am_sid')
    ico_rgn <- ico_rgn %>%
      filter(ico_gl == TRUE | rgn_id == ico_rgn_id) %>%
      select(rgn_id, sciname, comname, iucn_category, trend) %>%
      unique()
    cat(sprintf('Writing regional presence of iconic species from %s spatial data. \n  %s\n', 'am', ico_rgn_sp_file))
    write_csv(ico_rgn_iucn, ico_rgn_iucn_file)
    
    # Two problem species appear, with same sciname, two iucn_categories, and no 
    # spatial differentiation by iucn_sid:
    # Tursiops truncatus  CR
    # Tursiops truncatus  VU
    # Balaena mysticetus  NT
    # Balaena mysticetus  EN
    # Balaena mysticetus  CR
  } else {
    cat(sprintf('Reading regional presence of iconic species from IUCN spatial data from:\n  %s\n', ico_rgn_iucn_file))
    ico_rgn_iucn <- read_csv(ico_rgn_iucn_file)
  }
  return(ico_rgn_iucn)
}
