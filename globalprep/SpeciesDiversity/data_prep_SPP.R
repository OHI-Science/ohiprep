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

setwd('~/github/ohiprep')
source('src/R/common.R')

goal     <- 'globalprep/SpeciesDiversity'
scenario <- 'v2015'
dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal) 
dir_data_am    <- file.path(dir_neptune_data, 'git-annex/_raw_data', 'aquamaps/v2014') 
dir_data_iucn  <- file.path(dir_neptune_data, 'git-annex/_raw_data', 'iucn_spp') 
dir_git  <- file.path('~/github/ohiprep', goal)

source(file.path(dir_git, 'R/spp_fxn.R'))
# SPP-specific and ICO-specific functions

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

source(file.path(goal, 'R/ingest_iucn.R'))
### Pulls species list from IUCN
### Scrapes data from iucn site for each species on the list; saves to cache directory.
### Extracts habitats from cached files; filters IUCN list down to just marine species.
### Fixes older IUCN codes, cleans up scinames, gets rid of infraranks (subspecies).
### Determines subpopulations/parents and population trends from scraped files.
### Main outputs, saved to git-annex/globalprep/SpeciesDiversity/v201x/intermediate: 
### * spp_iucn_all.csv      - full list of IUCN species pulled from web, some cleaning.
### * spp_iucn_habitats.csv - list of IUCN species (by iucn_sid) and corresponding habitat.
### * spp_iucn_mar.csv      - prepped list: cleaned marine list with subpops and trends.


##############################################################################=
### Generate lookup - species <-> category/trend and spatial_source ----
##############################################################################=
spp_all <- create_spp_master_lookup(reload = FALSE)
### | am_sid | sciname | am_category | iucn_sid | iucn_category | popn_trend | popn_category | 
### | info_source | spp_group | id_no | objectid | spatial_source | category_score | trend_score |
### Outputs saved to:
### * v201X/intermediate/spp_iucn_maps_all.csv (list of all species represented in the IUCN shape files)
### * v201X/intermediate/spp_all.csv (complete data frame)

##############################################################################=
### Edit subpop/parent entries and synonyms ----
##############################################################################=
# spp_all <- spp_all %>%
#   fix_am_subpops() %>%
#   fix_iucn_subpops()
# 
# spp_all <- spp_all %>% remove_iucn_synonyms()
# 
# write_csv(spp_all, file.path(dir_anx, scenario, 'intermediate/spp_all_cleaned.csv'))

spp_all <- read.csv(file.path(dir_anx, scenario, 'intermediate/spp_all_cleaned.csv'), stringsAsFactors = FALSE)

### Explore duplicate records between AM and IUCN 

# > y <- spp_all %>% filter(!is.na(am_sid)) %>% unique()
# > sum(duplicated(y$am_sid))
# [1] 184
# > sum(duplicated(y$sciname))
# [1] 184
# y_dupes <- show_dupes(y, 'sciname') # all dupes are subpops or iucn_alias flags

# > x <- spp_all %>% filter(!(spatial_source %in% c('iucn_alias', 'iucn_subpop', 'am_subpop')) & !is.na(spatial_source)) %>% unique()
# > sum(duplicated(x$iucn_sid))
# [1] 115
# > sum(duplicated(x$sciname))
# [1] 2 # both 'DD' status.

# NOTE: Still some duplicated IUCN species IDs on the list; treated separately in Aquamaps data,
#       so spatial information is differentiated.  Leave in for 2015...

##############################################################################=
### Generate lookup - IUCN species to LOICZID ----
##############################################################################=
extract_loiczid_per_spp(groups_override = NULL, reload = FALSE)
### Extract loiczid cell IDs for each species within each species group.  Save 
### a .csv file for that group, with fields:
###       sciname | iucn_sid | LOICZID | prop_area
### NOTES: this takes a long time - multiple hours for some of the shape files.  
### * reload = FALSE allows it to skip extraction on groups with files already present.
### * use groups_override argument to run function on partial list of species groups.


##############################################################################=
### SPP - Generate species per cell tables for Aquamaps and IUCN -----
##############################################################################=
am_cells_spp_sum <- process_am_summary_per_cell(reload = TRUE)
### NOTE: the inner_join in here takes a while... 
### loiczid | mean_cat_score | mean_trend_score | n_cat_species | n_trend_species
### AM does not include subspecies: every am_sid corresponds to exactly one sciname.

iucn_cells_spp_sum <- process_iucn_summary_per_cell(reload = TRUE)
### loiczid | mean_cat_score | mean_trend_score | n_cat_species | n_trend_species
### IUCN includes subspecies - one sciname corresponds to multiple iucn_sid values.

##############################################################################=
### SPP - Summarize mean category and trend per cell and per region -----
##############################################################################=
summary_by_loiczid <- process_means_per_cell(am_cells_spp_sum, iucn_cells_spp_sum)
### This returns dataframe with variables:
### loiczid | weighted_mean_cat | weighted_mean_trend

rgn_cell_lookup <- extract_cell_id_per_region(reload = FALSE)
### | sp_id | loiczid | proportionArea | csq | cell_area
### saves lookup table to git-annex/globalprep/SpeciesDiversity/rgns/cellID_region_gcs_global.csv
### To use a different region shapefile, add an argument of: rgn_layer = '<rgn file here, without extension>'
### To use a shape file from a different directory (other than git-annex/globalprep/spatial/v2015/data), add an ogr_location argument.
### To run a different type of analysis, add an ohi_type argument with global (default), HS, or AQ

summary_by_rgn     <- process_means_per_rgn(summary_by_loiczid, rgn_cell_lookup)
### This returns dataframe with variables:
### sp_id | rgn_mean_cat | rgn_mean_trend | status

### Create final outputs:
if(!exists('summary_by_rgn')) 
  summary_by_rgn <- read.csv(file.path(dir_git, scenario, 'summary/rgn_summary.csv'))
spp_status <- summary_by_rgn %>%
  select(rgn_id, score = status)
spp_trend <- summary_by_rgn %>%
  select(rgn_id, score = rgn_mean_trend)
write_csv(spp_status, file.path(dir_git, scenario, 'data/spp_status_global.csv'))
write_csv(spp_trend,  file.path(dir_git, scenario, 'data/spp_trend_global.csv'))


##############################################################################=
### SPP 3nm - Summarize mean category and trend per region within 3 nm of shore -----
##############################################################################=
### create final outputs for 3nm zone:
### This version is for the 3 nm coastal zone cells...
rgn_cell_lookup_3nm <- extract_cell_id_per_region(reload = FALSE, 
                                                  ogr_location = file.path(dir_neptune_data, 'git-annex/Global/NCEAS-Regions_v2014/data'),
                                                  rgn_layer = 'rgn_offshore3nm_gcs')
### | sp_id | loiczid | proportionArea | csq | cell_area
### saves lookup table to git-annex/globalprep/SpeciesDiversity/rgns/cellID_rgn_offshore3nm_gcs_global.csv

summary_by_rgn_3nm <- process_means_per_rgn(summary_by_loiczid, rgn_cell_lookup_3nm, rgn_note = '3nm')

if(!exists('summary_by_rgn_3nm')) 
  summary_by_rgn_3nm <- read.csv(file.path(dir_git, scenario, 'summary/rgn_summary_3nm.csv'))
spp_status_3nm <- summary_by_rgn_3nm %>%
  select(rgn_id, score = status)
spp_trend_3nm <- summary_by_rgn_3nm %>%
  select(rgn_id, score = rgn_mean_trend)
write_csv(spp_status_3nm, file.path(dir_git, scenario, 'data/spp_status_3nm.csv'))
write_csv(spp_trend_3nm,  file.path(dir_git, scenario, 'data/spp_trend_3nm.csv'))

##############################################################################=
### SPP HS and Antarctic - Summarize mean category and trend per region -----
##############################################################################=
### create final outputs for HS zone:
### This version is for the high seas cells...
rgn_cell_lookup_hs <- extract_cell_id_per_region(reload = FALSE, rgn_layer = 'regions_gcs', ohi_type = 'HS')
### | sp_id | loiczid | proportionArea | csq | cell_area
### saves lookup table to git-annex/globalprep/SpeciesDiversity/rgns/cellID_region_gcs_HS.csv

summary_by_rgn_hs <- process_means_per_rgn(summary_by_loiczid, rgn_cell_lookup_hs, rgn_note = 'HS')

if(!exists('summary_by_rgn_hs')) 
  summary_by_rgn_hs <- read.csv(file.path(dir_git, scenario, 'summary/rgn_summary_hs.csv'))
spp_status_hs <- summary_by_rgn_hs %>%
  select(rgn_id, score = status)
spp_trend_hs <- summary_by_rgn_hs %>%
  select(rgn_id, score = rgn_mean_trend)
write_csv(spp_status_hs, file.path(dir_git, scenario, 'data/spp_status_hs.csv'))
write_csv(spp_trend_hs,  file.path(dir_git, scenario, 'data/spp_trend_hs.csv'))


### create final outputs for AQ zone:
### This version is for the Antarctic cells...
rgn_cell_lookup_aq <- extract_cell_id_per_region(reload = FALSE, rgn_layer = 'regions_gcs', ohi_type = 'AQ')
### | sp_id | loiczid | proportionArea | csq | cell_area
### saves lookup table to git-annex/globalprep/SpeciesDiversity/rgns/cellID_region_gcs_AQ.csv

summary_by_rgn_aq <- process_means_per_rgn(summary_by_loiczid, rgn_cell_lookup_aq, rgn_note = 'AQ')

if(!exists('summary_by_rgn_aq')) 
  summary_by_rgn_aq <- read.csv(file.path(dir_git, scenario, 'summary/rgn_summary_aq.csv'))
spp_status_aq <- summary_by_rgn_aq %>%
  select(rgn_id, score = status)
spp_trend_aq <- summary_by_rgn_aq %>%
  select(rgn_id, score = rgn_mean_trend)
write_csv(spp_status_aq, file.path(dir_git, scenario, 'data/spp_status_aq.csv'))
write_csv(spp_trend_aq,  file.path(dir_git, scenario, 'data/spp_trend_aq.csv'))

##############################################################################=


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

# NOTES ON model.R - should be covered in the above code.
# ??? compare BB and MF versions - BB version involves 3nm regions? MF version is for AQ and HS
# Averages IUCN score per cell for all species in that cell; then takes area-weighted average of those scores for each region
# .csv of score and trend output.
# ??? calculate this for both SPP and ICO - SPP is area-weighted (so rare species don't count as much), but ICO is not (rare species count just as much as common)

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
sum(category_check$cat_match)/nrow(category_check)
# Out of 2962 species that include IUCN category from both AM and IUCN,
# 2862 of them are matches.  96.6% match.

