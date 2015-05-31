##############################################################################=
## GOAL: Obtain species diversity data and iconics data for global
## Jun 1, 2015 - CCO

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
popn_cat    <- data.frame(popn_category=c("LC", "NT", "VU", "EN", "CR", "EX"), 
                          category_score =c(0, 0.2, 0.4, 0.6, 0.8, 1))
popn_trend  <- data.frame(popn_trend=c("Decreasing", "Stable", "Increasing"), 
                          trend_score=c(-0.5, 0, 0.5))

spp_all <- spp_all %>%
  left_join(popn_cat, by = 'popn_category') %>%
  left_join(popn_trend,  by = 'popn_trend') 


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
