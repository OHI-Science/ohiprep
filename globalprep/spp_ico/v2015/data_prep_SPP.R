##############################################################################=
### ohiprep/globalprep/SPP_ICO/data_prep.R
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
library(maptools)
library(readr)      # for read_csv()

setwd('~/github/ohiprep')
source('src/R/common.R')

goal     <- 'globalprep/spp_ico'
scenario <- 'v2015'
dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal) 
dir_data_am    <- file.path(dir_neptune_data, 'git-annex/globalprep/_raw_data', 'aquamaps/v2015') 
dir_data_iucn  <- file.path(dir_neptune_data, 'git-annex/globalprep/_raw_data', 'iucn_spp') 
dir_git  <- file.path('~/github/ohiprep', goal)

source(file.path(dir_git, 'v2015/spp_fxn_v2015.R'))
# SPP-specific and ICO-specific functions

##############################################################################=
### Ingest Aquamaps data to .csv from .sql ----
##############################################################################=
source(file.path(dir_git, 'R/ingest_aquamaps.R'))
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
### Main outputs, saved to git-annex/globalprep/SPP_ICO/v201x/intermediate: 
### * spp_iucn_all.csv      - full list of IUCN species pulled from web, some cleaning.
### * spp_iucn_habitats.csv - list of IUCN species (by iucn_sid) and corresponding habitat.
### * spp_iucn_marine.csv   - prepped list: cleaned marine list with subpops and trends.


##############################################################################=
### Generate lookup - species <-> category/trend and spatial_source ----
##############################################################################=
spp_all <- create_spp_master_lookup(source_pref = 'iucn', fn_tag = '', reload = FALSE)
### | am_sid | sciname | am_category | iucn_sid | iucn_category | popn_trend | popn_category | 
### | info_source | spp_group | id_no | objectid | spatial_source | category_score | trend_score |
### NOTE: as currently stands, ignores AquaMaps-mapped species with 
###   subpopulations (fix_am_subpops() is disabled)

spp_all <- spp_all %>%
  filter(!str_detect(spp_group, 'MARINEFISH_Part') | is.na(spp_group))
### For some reason, the MARINEFISH_Part1 and _Part2 shapefiles contain a bunch
### of mammals; and the .shp of _Part1 has no polygons.  These are suspect
### files so I'm ignoring them for now. is.na() will keep the AquaMaps species.

spp_all <- spp_all %>%
  filter(!(str_detect(spatial_source, 'iucn_subpop') & is.na(iucn_subpop))  |    ### subpop tag with no subpop location = FALSE
         is.na(spatial_source)) %>%                                              ### spatial_source is NA = TRUE (keep NAs in for now)
  filter(!(str_detect(spatial_source, 'iucn_parent') & !is.na(iucn_subpop)) |    ### parent tag with no subpop location = TRUE
         is.na(spatial_source))                                                  ### spatial_source is NA = TRUE (keep NAs in for now)
  ### This gets rid of any subpops that don't have identified shapefiles... 
  ### NOTE: this still doesn't uniquely identify which ID number goes with
  ### which subpopulation though!

spp_subpops_parents <- spp_all %>% 
  filter(str_detect(spatial_source, 'subpop') | str_detect(spatial_source, 'parent'))
table(spp_subpops_parents$spp_group)

write_csv(spp_all, file.path(dir_anx, scenario, 'int/spp_all.csv'))

##############################################################################=
### Generate lookup - IUCN species to LOICZID ----
##############################################################################=
# x <- c("DAMSELFISH", "WRASSE", "SURGEONFISH_TANGS_UNICORNFISH", 
#        "COMBTOOTHBLENNIES", "TUNAS_BILLFISHES", "GROUPERS", "PUFFERFISH", "BUTTERFLYFISH", 
#        "LOBSTERS", "CONESNAILS", "CORALS_Part3", 
#        "CORALS_Part1", "SEACUCUMBERS", "MARINE_MAMMALS")
#   
# y <- c("SEABREAMS_PORGIES", "ANGELFISH", "BONEFISH_TARPONS", "CORALS_Part2", 
#        "REPTILES", "SEAGRASSES", "MANGROVES", "TERRESTRIAL_MAMMALS")
# z <- c("TUNAS_BILLFISHES", "MARINE_MAMMALS", "REPTILES", "SEABREAMS_PORGIES")

extract_loiczid_per_spp(spp_all, groups_override = z, fn_tag = NULL, reload = TRUE)
### Extract loiczid cell IDs for each species within each species group.  Save 
### a .csv file for that group, with fields:
###       sciname | iucn_sid | presence | subpop | LOICZID | prop_area
### * presence codes: 1 extant; 2 prob extant (discontinued); 3 Possibly Extant;
###                   4 Possibly Extinct; 5 Extinct (post 1500); 6 Presence Uncertain
### NOTES: this takes a long time - multiple hours for some of the shape files.  
### * reload = FALSE allows it to skip extraction on groups with files already present.
### * use groups_override argument to run function on partial list of species groups.

### Adding back in the missing species - most due to DD designation.
### 'y' here is the 'missing' list from IUCN-AquaMaps, with spp_all left-joined
### to it to capture the other columns as needed...
# extract_loiczid_per_spp(y, groups_override = NULL, fn_tag = 'missing', reload = TRUE)
### NOTES FOR CATCHING DD AND MISSING SPP:
### When processing bonefish/tarpons:
###   Error in vapply(x[[col]], `[`, 0, 1) : values must be type 'double',
###     but FUN(X[[1]]) result is type 'character'
###   (check to make sure 12 species are represented in this file?)

##############################################################################=
### SPP - Generate species per cell tables for Aquamaps and IUCN -----
##############################################################################=
am_cells_spp_sum <- process_am_summary_per_cell(reload = TRUE)
### NOTE: keyed data.table works way faster than the old inner_join or merge.
### loiczid | mean_cat_score | mean_trend_score | n_cat_species | n_trend_species
### AM does not include subspecies or subpops: every am_sid corresponds to exactly one sciname.

iucn_cells_spp_sum <- process_iucn_summary_per_cell(reload = TRUE)
### loiczid | mean_cat_score | mean_trend_score | n_cat_species | n_trend_species
### IUCN includes subpops - one sciname corresponds to multiple iucn_sid values.

##############################################################################=
### SPP Global - Summarize mean category and trend per cell and per region -----
##############################################################################=
summary_by_loiczid <- process_means_per_cell(am_cells_spp_sum, iucn_cells_spp_sum)
### This returns dataframe with variables:
### loiczid | weighted_mean_cat | weighted_mean_trend

rgn_cell_lookup <- extract_cell_id_per_region(reload = FALSE)
### | sp_id | loiczid | proportionArea | csq | cell_area
### saves lookup table to git-annex/globalprep/SPP_ICO/rgns/cellID_region_gcs_global.csv
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
### saves lookup table to git-annex/globalprep/SPP_ICO/rgns/cellID_rgn_offshore3nm_gcs_global.csv

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
### saves lookup table to git-annex/globalprep/SPP_ICO/rgns/cellID_region_gcs_HS.csv

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
### saves lookup table to git-annex/globalprep/SPP_ICO/rgns/cellID_region_gcs_AQ.csv

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
### SPP global, swapping priority on AM and IUCN spatial source -----
##############################################################################=
scenario <- 'vAM_IUCN'

pref_flag <- '_IUCNpref'
prob_filter <- 0.40

spp_all <- create_spp_master_lookup(source_pref = 'iucn', fn_tag = pref_flag, reload = FALSE)
# only affected by am/iucn preference

am_cells_spp_sum <- process_am_summary_per_cell(fn_tag = sprintf('%s_prob%s', pref_flag, prob_filter), prob_filter = prob_filter, reload = TRUE)
# affected by prob filter and preference flag

iucn_cells_spp_sum <- process_iucn_summary_per_cell(fn_tag = sprintf('_pref%s', pref_flag), reload = TRUE)
# affected by preference flag

summary_by_loiczid <- process_means_per_cell(am_cells_spp_sum, iucn_cells_spp_sum, fn_tag = sprintf('%s_%s', pref_flag, prob_filter))
rgn_cell_lookup <- extract_cell_id_per_region(reload = FALSE)
summary_by_rgn     <- process_means_per_rgn(summary_by_loiczid, rgn_cell_lookup, rgn_note = sprintf('%s_%s', pref_flag, prob_filter))
### This returns dataframe with variables:
### sp_id | rgn_mean_cat | rgn_mean_trend | status

### Create final outputs:
spp_status <- summary_by_rgn %>%
  select(rgn_id, score = status)
spp_trend <- summary_by_rgn %>%
  select(rgn_id, score = rgn_mean_trend)
write_csv(spp_status, file.path(dir_git, scenario, sprintf('data/spp_status_global%s_prob%s.csv', pref_flag, prob_filter)))
write_csv(spp_trend,  file.path(dir_git, scenario, sprintf('data/spp_trend_global%s_prob%s.csv', pref_flag, prob_filter)))


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

