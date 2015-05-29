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

# Create lookup: species <-> popn_status/popn_trend and spatial_source:
#   * Desired lookup table to include:
#     - sciname  iucn_sid  am_sid  status  popn_trend info_source  spatial_source
#   * Aquamaps species modified to include:
#     - SPECIESID   SpecCode   sciname   am_status
#   * IUCN species modified to include:
#     - sid(Red.List.Species.ID)  sciname  iucn_status  popn_trend
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
# sciname | am_sid | popn_status | popn_trend | loiczid | prob
# * filter master species lookup by spatial_source == 'am'
# * filter out any popn_status NAs
# * check for subspecies (duplicated scinames)
# * load AM species spatial data
# * filter spatial data for loiczids in region lookup
# * inner_join AquaMaps species spatial data to species lookup 

# Create IUCN master spp spatial chart: all species using IUCN data
# sciname | iucn_sid | popn_status | popn_trend | loiczid | prop_area
# * filter master species lookup by spatial_source == 'iucn'
# * filter out any popn_status NAs
# * check for subspecies (duplicated scinames)
# * filter spatial data for loiczids in region lookup
# * inner_join each IUCN species spatial data to species lookup 

# join full master spp spatial chart:
# select out unneeded columns (prob and prop_area)
# bind_rows by sciname, sid, popn_status, popn_trend, loiczid


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
### Create lookup: species <-> popn_status/popn_trend and spatial_source.
### * Join Aquamaps species list and the IUCN marine species list by sciname
### * determine IUCN red list popn_status and popn_trend
### * determine which species have IUCN range maps available
##############################################################################=

spp_all <- create_spp_master_lookup(dir_anx, scenario = 'v2015', reload = FALSE)
### Output is spp_all.csv data frame with these fields:
### | am_sid      | sciname     | am_status | iucn_sid | iucn_status | popn_trend     | 
### | popn_status | info_source | spp_group | id_no    | objectid    | spatial_source |
### Outputs saved to:
### * v201X/intermediate/spp_iucn_maps_all.csv 
###     (list of all species represented in the IUCN shape files)
### * v201X/intermediate/spp_all.csv (complete data frame)

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
#  | sciname | am_sid | popn_status | popn_trend | loiczid | prob
# * filter master species lookup by spatial_source == 'am'
# * filter out any popn_status NAs
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

### Load Aquamaps species per cell table
file_loc <- file.path(dir_anx, 'raw/aquamaps_2014/tables/ohi_hcaf_species_native.csv')
cat(sprintf('Loading AquaMaps cell-species data.  Come back in 10 minutes. \n  %s \n', file_loc))
am_cells_spp <- read_csv(file_loc, col_types = '_ccn__') %>%
  rename(am_sid = SpeciesID, csq = CsquareCode, prob = probability)

### ??? START HERE ----
# filter entire aquamaps table to just cells found in global regions. 
# 78M <- 39M observations
am_cells_spp1 <- am_cells_spp %>% 
  filter(csq %in% rgn_cell_lookup$csq) 
# filter out below probability threshold; 39 M to 29 M observations.
am_cells_spp1 <- am_cells_spp1 %>%
  filter(prob >= .40) %>%
  select(-prob)
write_csv(am_cells_spp1, file.path(dir_anx, 'v2015/intermediate/am_cells_spp_filtered.csv'))
# then join.
ptm <- proc.time()
am_cells_spp2 <- am_cells_spp1 %>%
  inner_join(rgn_cell_lookup, by = 'csq') %>%
  select(-csq)
proc.time() - ptm

