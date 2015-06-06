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
dir_git  <- file.path('~/github/ohiprep', goal)

source(file.path(dir_git, 'R/spp_fxn.R'))
source(file.path(dir_git, 'R/ico_fxn.R'))
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
### | am_sid | sciname | am_category | iucn_sid | iucn_category | popn_trend | popn_category | 
### | info_source | spp_group | id_no | objectid | spatial_source | category_score | trend_score |
### Outputs saved to:
### * v201X/intermediate/spp_iucn_maps_all.csv (list of all species represented in the IUCN shape files)
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
# saves lookup table to git-annex/globalprep/SpeciesDiversity/rgns/region_prop_df.csv


##############################################################################=
### SPP - Generate species per cell tables for Aquamaps and IUCN -----
##############################################################################=
am_cells_spp_sum <- process_am_spp_per_cell(reload = FALSE)
### loiczid | mean_cat_score | mean_trend_score | n_cat_species | n_trend_species
### AM does not include subspecies: every am_sid corresponds to exactly one sciname.
# > x <- spp_all %>% filter(!is.na(am_sid)) %>% select(am_sid, sciname) %>% unique()
# > sum(duplicated(x$am_sid))
# [1] 0
# > sum(duplicated(x$sciname))
# [1] 0

iucn_cells_spp_sum <- process_iucn_spp_per_cell(reload = FALSE)
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
sum(category_check$cat_match)
# Out of 2962 species that include IUCN category from both AM and IUCN,
# 2862 of them are matches.  96.6% match.

##############################################################################=
### ICO -----
##############################################################################=

##############################################################################=
### get master list of Iconic Species -----
##############################################################################=
ico_list <- get_ico_list()
### | comname | sciname | ico_gl (iconic globally?) | iucn_sid | am_sid | spatial_source |
### | iucn_category (code, NA and DD filtered out) | trend (lower case) | ico_rgn_id 
### * ico_rgn_id: rgn_id in which species is iconic by regional/national lists,
###   separately from other lists.

##############################################################################=
### Determine species lists by region based on IUCN and AM spatial data -----
##############################################################################=
ico_rgn_iucn <- get_ico_rgn_iucn(ico_list) 
### rgn_id | sciname | comname | iucn_category | trend
# check:
dupes <- ico_rgn_iucn %>% select(rgn_id, sciname) %>% duplicated()
dupes <- ico_rgn_iucn %>% filter(sciname %in% ico_rgn_iucn$sciname[dupes])
# 25 duplicated rows - species duplicated within a region.  Species: 
# Different IUCN species IDs, but no IUCN ID available for spatial information, 
# so based upon an undifferentiated IUCN shapefile.  Look up each and assign to regions by hand.
# Check in the shapefile itself - MAMMMARINE.shp or whatever.
#         am_sid iucn_sid            sciname am_category iucn_category popn_trend
#   1 ITS-180533     2468 Balaena mysticetus          LC            NT           
#   2 ITS-180533     2469 Balaena mysticetus          LC            EN    Unknown
#   3 ITS-180533     2472 Balaena mysticetus          LC            CR    Unknown
#   4 ITS-180426   194300 Tursiops truncatus          LC            CR Decreasing
#   5 ITS-180426 16369383 Tursiops truncatus          LC            VU Decreasing

ico_rgn_am <- get_ico_rgn_am(ico_list)
### rgn_id | sciname | comname | iucn_category | trend
dupes <- ico_rgn_am %>% select(rgn_id, sciname) %>% duplicated()
dupes <- ico_rgn_am %>% filter(sciname %in% ico_rgn_am$sciname[dupes])
# * Isurus oxyrinchus:    VU, NT
# * Dermochelys coriacea: LC, CR
# * Lamna nasus:          CR, EN
# Different IUCN species IDs, but no IUCN spatial information, so based upon
# an undifferentiated Aquamaps map.  Look up each and assign to regions by hand.
# Check on IUCN site by iucn_sid, might have country ranges for each subpop.
#     am_sid  iucn_sid              sciname am_category iucn_category popn_trend
#   Rep-2331  46967807 Dermochelys coriacea          CR            CR Decreasing
#   Rep-2331  46967817 Dermochelys coriacea          CR            CR Decreasing
#   Rep-2331  46967827 Dermochelys coriacea          CR            LC Increasing
#   Rep-2331  46967838 Dermochelys coriacea          CR            CR Increasing
#   Rep-2331  46967863 Dermochelys coriacea          CR            CR Decreasing
#   Fis-22768    39343          Lamna nasus          VU            CR Decreasing
#   Fis-22768    39344          Lamna nasus          VU            EN Decreasing
#   Fis-22768    61420          Lamna nasus          VU            CR Decreasing
#   Fis-58485   161749    Isurus oxyrinchus          VU            VU Decreasing
#   Fis-58485   161750    Isurus oxyrinchus          VU            VU Decreasing
#   Fis-58485   161751    Isurus oxyrinchus          VU            NT Decreasing

##############################################################################=
### Fill in species lists by region based on original spreadsheet -----
##############################################################################=
ico_rgn_other <- ico_list %>% 
  filter(is.na(spatial_source)) %>%
  select(rgn_id = ico_rgn_id, sciname, comname, iucn_category, trend)
### rgn_id | sciname | comname | iucn_category | trend

##############################################################################=
### Summarize regional iconic species status -----
##############################################################################=
ico_rgn_sum <- process_ico_rgn(list(ico_rgn_iucn, ico_rgn_am, ico_rgn_other))
### rgn_id | mean_cat | mean_trend | status

##############################################################################=
### Some check functions -----
##############################################################################=
x <- ico_rgn_source_compare()
