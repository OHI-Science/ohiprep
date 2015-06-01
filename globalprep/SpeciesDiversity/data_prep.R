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
# SPP-specific functions

library(rPython) # to call Python functions and scripts within R?


### Set paths for local, git-annex, model(?), and git-annex/tmp
dir_git <- '~/github/ohiprep'
dir_lcl    <- file.path(dir_git, goal)                        ### local:  ~/github/ohiprep/globalprep/SpeciesDiversity
dir_anx    <- file.path(dir_neptune_data, 'git-annex', goal)  ### Neptune: /git-annex/github/ohiprep/globalprep/SpeciesDiversity
dir_mdl    <- file.path(dir_neptune_data, 'model')            ### Neptune: /model


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

# to overall lookup table, join scores for population category and trend.
popn_cat    <- data.frame(popn_category  = c("LC", "NT", "VU", "EN", "CR", "EX"), 
                          category_score = c(   0,  0.2,  0.4,  0.6,  0.8,   1))
popn_trend  <- data.frame(popn_trend=c("Decreasing", "Stable", "Increasing"), 
                          trend_score=c(-0.5, 0, 0.5))

spp_all <- spp_all %>%
  left_join(popn_cat,   by = 'popn_category') %>%
  left_join(popn_trend, by = 'popn_trend') 


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

am_cells_spp_sum <- process_am_spp_per_cell(rgn_cell_lookup, reload = FALSE)
### This returns dataframe with variables:
### loiczid | mean_cat_score | mean_trend_score | n_cat_species | n_trend_species
### AM does not include subspecies: every am_sid corresponds to exactly one sciname.
# > x <- spp_all %>% filter(!is.na(am_sid)) %>% select(am_sid, sciname) %>% unique()
# > sum(duplicated(x$am_sid))
# [1] 0
# > sum(duplicated(x$sciname))
# [1] 0

iucn_cells_spp_sum <- process_iucn_spp_per_cell(rgn_cell_lookup, reload = TRUE)
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

# TO DO: deal with subspecies for IUCN species.
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
