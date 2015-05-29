##############################################################################=
## GOAL: Obtain species diversity data and iconics data 
## for HS and AQ
## Date: Mar 18 2014
## MRF
## NOTE: This differs from the previous analysis because I
## am testing the results after changing the raster cell values against the previous raster grid values
## Plus: I am creating the following files: 
##      1) raster used to define the locations of the new rgn shapefile
##      2) am_cells_rgn_proportions: describes the proportion of each raster cells within each region
##      3) am_cells_sp: describes the species in each raster cell (modified to use LOICZID rather than cid)  

# Create lookup: species <-> popn_category/popn_trend and spatial_source:
#   * Desired lookup table to include:
#     - sciname  iucn_sid  am_sid  popn_category  popn_trend info_source  spatial_source
#   * Aquamaps species modified to include:
#     - SPECIESID   SpecCode   sciname   am_category
#   * IUCN species modified to include:
#     - sid(Red.List.Species.ID)  sciname  iucn_category  popn_trend
#
# Create lookup: region_id <-> cell_id
# * pretty much already done... include both LOICZID(?) and CsquareCode?
#   - IUCN gets whatever we give it; AM lookup uses csq
#   - so why not just use csq, unless it's less efficient to look up character than integer
# 
# Create lookup: iucn_species <-> cell_id
# * Desired lookup table to include:
#   - IUCN_speciesID  cellID (LOICZID or csq? does it matter?)
# * for each species in iucn_maps list (see the create_spp_master_lookup function),
#   basically do the same process as the region_id <-> cell_id, except this time
#   it's iucn_sid <-> cell_id.
#
# Aquamaps list (already created)
#   AM_speciesID  CsquareCode  prob
# 

# Create Aquamaps master spp spatial chart: all species using AM data
# sciname | am_sid | popn_category | popn_trend | loiczid | prob
# * filter master species lookup by spatial_source == 'am'
# * filter out any popn_category NAs
# * check for subspecies (duplicated scinames)
# * load AM species spatial data
# * filter spatial data for loiczids in region lookup
# * inner_join AquaMaps species spatial data to species lookup 

# Create IUCN master spp spatial chart: all species using IUCN data
# sciname | iucn_sid | popn_category | popn_trend | loiczid | prop_area
# * filter master species lookup by spatial_source == 'iucn'
# * filter out any popn_category NAs
# * check for subspecies (duplicated scinames)
# * filter spatial data for loiczids in region lookup
# * inner_join each IUCN species spatial data to species lookup 

# join full master spp spatial chart:
# select out unneeded columns (prob and prop_area)
# bind_rows by sciname, sid, popn_category, popn_trend, loiczid


##############################################################################=
library(foreign)    # or: library(shapefiles) for read.dbf()
library(data.table) # for fread()
library(sp)
library(rgdal)
library(raster)
library(readr)      # for read_csv()

setwd('~/github/ohiprep') # if not already there!  Should this be a standard practice - always 
# just keep your working directory set to the base github repository location?
source('src/R/common.R')


# Note the following masked package:raster functions
#   intersect, select, union (masked by 'dplyr');  extract (masked by 'tidyr')
# due to calling library(raster) before source('src/R/common.R').

goal <- 'globalprep/SpeciesDiversity'
dir_anx <- file.path(dir_neptune_data, 'git-annex', goal) 
# ??? use global variable for ease of passing file location into functions?
dir_git <- file.path('~/github/ohiprep', goal)

source(file.path(dir_git, 'R/spp_fxn.R'))
# SPP-specific functions



##############################################################################=
### Create lookup: species <-> popn_category/popn_trend and spatial_source.
### * Join Aquamaps species list and the IUCN marine species list by sciname
### * determine IUCN red list popn_category and popn_trend
### * determine which species have IUCN range maps available
##############################################################################=

spp_all <- create_spp_master_lookup(dir_anx, scenario = 'v2015', reload = FALSE)
### Output is spp_all.csv data frame with these fields:
### | am_sid      | sciname     | am_category | iucn_sid | iucn_category | popn_trend     | 
### | popn_category | info_source | spp_group | id_no    | objectid    | spatial_source |
### Outputs saved to:
### * v201X/intermediate/spp_iucn_maps_all.csv 
###     (list of all species represented in the IUCN shape files)
### * v201X/intermediate/spp_all.csv (complete data frame)

# to overall lookup table, add scores for population category and trend.
popn_cat    <- data.frame(popn_category=c("LC", "NT", "VU", "EN", "CR", "EX"), 
                          category_score =c(0, 0.2, 0.4, 0.6, 0.8, 1))
popn_trend  <- data.frame(popn_trend=c("Decreasing", "Stable", "Increasing"), 
                          trend_score=c(-0.5, 0, 0.5))

spp_all <- spp_all %>%
  left_join(popn_cat, by = 'popn_category') %>%
  left_join(popn_trend,  by = 'popn_trend') 


##############################################################################=
### Generate lookup - IUCN species ID to cell ID ----
###   iucn_sid <-> LOICZID
##############################################################################=

extract_loiczid_per_spp(dir_anx, scenario = 'v2015', groups_override = tmp, reload = FALSE)
# Extract loiczid cell IDs for each species within each species group.  Save 
# a .csv file for that group, with fields:
#       sciname | iucn_sid | LOICZID | prop_area
# NOTES: this takes a long time - multiple hours for some of the shape files.  
# * reload = FALSE allows it to skip extraction on groups with files already present.
# * use groups_override argument to run function on partial list of species groups.

##############################################################################=
### load Aquamaps data on cell IDs and species per cell
##############################################################################=
# Create Aquamaps master spp spatial chart: all species using AM data
#  | sciname | am_sid | popn_category | popn_trend | loiczid | prob
# * filter master species lookup by spatial_source == 'am'
# * filter out any popn_category NAs
# * check for subspecies (duplicated scinames)
# * load AM species spatial data
# * filter spatial data for loiczids in region lookup
# * inner_join AquaMaps species spatial data to species lookup 

##############################################################################=
### Generate lookup - region ID to spatial raster cell IDs ----
##############################################################################=

### Load or generate lookup table of region IDs to cell information
rgn_cell_lookup <- extract_cell_id_per_region(dir_anx, reload = FALSE)
### | sp_id | loiczid | proportionArea | csq | cell_area
###   TO DO:  currently as sp_id; update to translate sp_id to rgn_id
# ??? DOES THIS STILL INCLUDE ALL THE HS and AQ SHIT? get rid of those!
# saves lookup table to git-annex/globalprep/SpeciesDiversity/rgns/region_prop_df.csv


# Calculate category and trend scores per cell for Aquamaps species.
# * load AM species <-> cell lookup
# * filter to appropriate cells (in regions, meets probability threshold)
# * join spatial info: loiczid, region ID, cell area
# * join species info: category score and trend score
# * filter by cat score != NA
# * summarize by loiczid - mean category_score, mean trend_score, count


### Generate Aquamaps species per cell table
am_cells_spp_sum1 <- process_am_spp_per_cell(dir_anx, rgn_cell_lookup, scenario = 'v2015', reload = FALSE)

# At this point - all species within each cell, including category and trend scores, for aquamaps
# * summarize by loiczid to mean score, and count, for Aquamaps.
# * for IUCN, for each species group, do same, and join all together into an IUCN dataframe.
# * then, for AM and each IUCN spp group, do count-weighted average score, and summarize for each LOICZID.
# * for each of these, then, 


