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
# The process:
# * full_join based on sciname; rename iucn_sid = sid, am_sid = SPECIESID
# * status, and info_source:
#   - for species with iucn_status information, use that; otherwise am_status; otherwise NA
#   - also tag info_source with am, iucn, or na
# * popn_trend: 
#   - for species with IUCN popn_trend info, use that
#   - otherwise NA
# * spatial_source: 
#   - figure out which species have IUCN spatial data, and flag spatial_source with iucn;
#   - for species w/o IUCN spatial data, but with AM data, flag spatial_source with am;
#   - otherwise NA.
# * Filter out anything with spatial_source NA, and with status NA
# 
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
# 

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
### Generate lookup - region ID to spatial raster cell IDs ----
### * generate half-degree LOICZID raster using Aquamaps cell information
### * Get percent of each raster cell in the region polygons
### * create .csv associating raster cell, LOICZID, and CsquareCode
##############################################################################=
if(!exists('am_cells_raw')) {
  file_loc <- file.path(dir_anx, 'raw/aquamaps_2014/tables/hcaf.csv')
  cat(sprintf('Loading AquaMaps cell data.  Less than 1 minute.\n  %s \n', file_loc))
  am_cells_raw <- fread(file.path(dir_anx,    'raw/aquamaps_2014/tables/hcaf.csv'))
  # fread: elapsed time 27 seconds vs  read.csv: elapsed time 100.5 seconds.
}

am_cells <- am_cells_raw %>%
  select(csq = CsquareCode, LOICZID, CenterLat, CenterLong, CellArea, OceanArea)
#rm(am_cells_raw) # remove it if memory is an issue.

### Create raster of .5 degree cells, each identified by LOICZID.  
###   LOICZID and CsquareCode match 1:1?
create_loiczid_raster(am_cells, dir_anx, reload = TRUE)
# saves raster to git-annex/globalprep/SpeciesDiversity/rgns/loiczid_raster.grd and .gri

### Overlay region shapefile on LOICZID raster to create lookup table
### of region IDs to LOICZID
###   TO DO:  currently as sp_id; update to translate sp_id to rgn_id
###   TO DO:  Also include csq, since the AM species lookup uses csq not LOICZID.
ogr_location <- file.path(dir_neptune_data, 'git-annex/Global/NCEAS-Regions_v2014/data')
rgn_cell_lookup <- extract_loiczid_per_region(dir_anx, ogr_location, reload = TRUE)
# saves lookup table to git-annex/globalprep/SpeciesDiversity/rgns/region_prop_df.csv


##############################################################################=
### Create lookup: species <-> popn_status/popn_trend and spatial_source.
### * Join Aquamaps species list and the IUCN marine species list by sciname
### * determine IUCN red list popn_status and popn_trend
### * determine which species have IUCN range maps available
##############################################################################=

spp_all <- create_spp_master_lookup(dir_anx, scenario = 'v2015', reload = FALSE)
### Output is data frame with these fields:
### am_sid | SpecCode | sciname | am_status | iucn_sid | iucn_status | popn_trend | popn_status | info_source | spp_group | id_no | objectid | spatial_source
### ??? do we need SpecCode, objectid? id_no?
### Outputs saved to:
### * v201X/intermediate/spp_iucn_maps_all.csv 
###     (list of all species represented in the IUCN shape files)
### * v201X/intermediate/spp_all.csv (complete data frame)
  


##############################################################################=
### Generate lookup - IUCN species ID to cell ID ----
###   iucn_sid <-> LOICZID
##############################################################################=

# Determine intersections of IUCN maps with Aquamaps half-degree cells
#
# * from the spp_all.csv, extract the species with IUCN range maps.  Use this file
#   rather than the spp_iucn_maps_all because it is truncated to just species on spp_iucn_marine_global.csv list.
# * for each species group (spp_group), open parent shape file
#   * select by attribute - binomial == sciname in the list
#   * use union to join multiple polygons with identical sciname. ??? needed?
#   * use raster::extract, lay the selected polygons over the LOICZID raster.
#     * NOTE: this takes a long time - 15-20 minutes for the region shapefile, with ~200 polygons.
#       We're looking at ~ 4000 polygons, on the order of five hours? probably much more?
# * when finished with each spp_group, save a file of iucn_sid | LOICZID | proportionArea for that group.

extract_loiczid_per_spp(dir_anx, scenario = 'v2015', reload = FALSE)
# use groups_override argument to run function on partial list of species groups.

  

##############################################################################=
### load Aquamaps data on cell IDs and species per cell
##############################################################################=

if(!exists('am_cells_spp_raw')) {
  file_loc <- file.path(dir_anx, 'raw/aquamaps_2014/tables/ohi_hcaf_species_native.csv')
  cat(sprintf('Loading AquaMaps cell-species data.  Come back in 10 minutes. \n  %s \n', file_loc))
  am_cells_spp_raw <- read_csv(file_loc, n_max = 50000)
  # fread: seems to choke in RStudio.
  # read.csv: painfully slow, probably ~20 minutes; but RStudio hangs after 5ish.
  # read_csv: 9 minutes to load 78,168,470 lines.  No problems reported.
}

am_cells_spp <- am_cells_spp_raw %>%
  select(sid = SpeciesID, csq = CsquareCode, probability)

am_cells1 <- am_cells %>%
  select(csq, LOICZID, CellArea) 
### drop lat/long; use CellArea because the weights from extract() will allocate proportional area
stopifnot(sum(duplicated(am_cells$csq)) == 0)

# * Merge the cells_spp and cells, by csq.  This associates LOICZID with
#   each species identifier, instead of csq.  
# * Drop csq.
# * Attach species info (spp) via Aquamaps species ID code.  This brings with it
#   IUCN category, and IUCN species ID (sid), for each species in each cell.
# * This is going to be a massive data.frame.  Consider breaking into small ones
#   * by species group? (maybe family or some such)?
#   * The big file is just SpeciesID and csq.  Maybe divide by chunks of csq, first portion?
#   * or chop off the first bit of speciesID and group that way?
# * for each smaller data frame, process it down to summaries of total score per species.
#   * this implies it would be best to group by family groups, not by cell ID.
am_loiczid_spp <- data.table(am_cells, key = 'csq') %>%
  merge(data.table(am_cells_spp, key = 'csq'))

x <- am_cells_spp_raw %>% select(SpeciesID) %>% mutate(SpeciesID = str_split(SpeciesID, '-', n = 1))
 
# # ??? These are files from HS-AQ 2014 for testing ----
# am_cells2013     <- fread(file.path(dir_anx,    'raw/aquamaps_2013/tables/cells.csv'))
# # ??? compare this to Melanie's am_cellsx <- read.csv(file.path(dir_neptune_data, "/model/GL-NCEAS-SpeciesDiversity_v2013a/tmp/am_cells_data.csv")
# am_cells_spp2013 <- read_csv(file.path(dir_anx, 'raw/aquamaps_2013/tables/cells_spp.csv'))
# am_spp2013       <- fread(file.path(dir_anx,    'raw/aquamaps_2013/tables/spp.csv'))

# am_cells2013: ??? This should be Aquamaps data but possibly modified by one of BB's scripts.  Which one?
#   cid  csq  rgn_id  id_typ  rgn_nam  area_km2  category_linear_cnt  category_linear_avg  
#   popn_trend_linear_cnt  popn_trend_linear_avg  category_linear_score  popn_trend_linear_avg_linear_2012chg_cnt
#   category_linear_2012chg_cnt  category_linear_2012chg_avg  category_linear_2012chg_score  popn_trend_linear_2012chg_cnt  popn_trend_linear_2012chg_avg
# many of these fields created in script below (or similar): 
#   category_linear_cnt2  category_linear_avg2  popn_trend_linear_cnt2  popn_trend_linear_avg2  rgn_spp_score_2013

# am_cells:
#   V1 CsquareCode LOICZID NLimit Slimit WLimit ELimit CenterLat CenterLong CellArea OceanArea CellType PWater FAOAreaM FAOAreaIn CountryMain CountrySecond CountryThird
#   CountrySubMain CountrySubSecond CountrySubThird EEZFirst EEZSecond EEZThird EEZFourth EEZFifth EEZSixth EEZAll EEZRemark LME LME_2010 LMEBorder OceanBasin Longhurst
#   IslandsNo Area0_20 Area20_40 Area40_60 Area60_80 Area80_100 AreaBelow100 ElevationMin ElevationMax ElevationMean ElevationSD DepthMin DepthMax DepthMean DepthSD SSTMnMin
#   SSTMnMax SSTAnMean SSTAnSD SSTMnRange SBTAnMean SalinityMin SalinityMax SalinityMean SalinitySD SalinityBMean PrimProdMean IceConAnn Shelf    Slope IceConSpr IceConSum
#   IceConFal IceConWin LandDist WaveHeight TidalRange  Abyssal Coral Estuary Seamount MPA          SST1950          SBT1950     Salinity1950    SalinityB1950     PrimProd1950
#   IceCon1950          SST1999          SBT1999     Salinity1999    SalinityB1999     PrimProd1999 IceCon1999          SST2050          SBT2050     Salinity2050
#   SalinityB2050     PrimProd2050 IceCon2050          SST2100          SBT2100     Salinity2100    SalinityB2100     PrimProd2100 IceCon2100         SST1950c
#   SBT1950c    Salinity1950c   SalinityB1950c PrimProd1950c IceCon1950c         SST2050c         SBT2050c    Salinity2050c   SalinityB2050c PrimProd2050c IceCon2050c
#   SST2100c        SBT2100c    Salinity2100c   SalinityB2100c IceCon2100c PrimProd2100c

# am_cells_spp2013:
#   cid  sid
# am_cells_spp:
#   [EMPTY]  SpeciesID	CsquareCode	probability	BoundBoxYN	FAOAreaYN

# am_spp2013:
#   sid  sciname  class  order  family  genus  species  authority  modified_year
#   category  criteria  habitat  popn_trend  shp_dbf  shp_grp  shp_cnt  src_distn  
#   am_speciesid  TEXT  category_2012chg  popn_trend_2012chg
# am_spp:
#   V1  SPECIESID  SpecCode  Genus  Species  FBname  Kingdom  Phylum  Class  
#   Order  Family  iucn_code  expert_id


# am_cells:     links  CsquareCode <-> LOICZID.
# am_cells_spp: links  species ID  <-> CsquareCode
# am_spp:       links  IUCN code   <-> species ID; but are we keeping this?


####################################################################################
# The following explores the extent of differences when the IUCN species are
# assigned to one aquamap cell...
# (result: basically produces same results) ----
####################################################################################

# for Aquamaps - ignore IUCN categories and trends!  bring those in from scraped IUCN .htm files
# create lookup table of weights for extinction risk categories and population trends
category   <- data.frame(category   = c('LC', 'NT', 'VU', 'EN', 'CR', 'EX'), 
                         catScore   = c(  0,   0.2,  0.4,  0.6,  0.8,   1 ))
popn_trend <- data.frame(popn_trend = c('Decreasing', 'Stable', 'Increasing'), 
                         trendScore = c(   -0.5,          0,         0.5))

# reformat our am_spp: sid, sciname
am_spp <- am_spp %>%
  unite(sciname, Genus, Species, sep = ' ')

am_spp1 <- am_spp %>%
  # ??? sid is number part of SPECIESID? or SpecCode?
  # ??? This should be done in the extraction of am_spp?
  # ??? category and popn_trend are not in am_spp file.
  select(sid = SPECIESID, category, popn_trend) %>%
  left_join(category) %>%
  left_join(popn_trend) 

# calculate average weight and count for category and trend scores
scoreData <- cells_spp %>%
  group_by(LOICZID) %>%
  inner_join(spp) %>% 
  summarise(category_linear_cnt2 = length(category[!(category %in% 'DD')]),
            category_linear_avg2 = mean(catScore, na.rm=TRUE),
            popn_trend_linear_cnt2 = sum(!is.na(trendScore)),
            popn_trend_linear_avg2 = mean(trendScore, na.rm=TRUE))

cells_2013 <- cells_2013 %>%
  #left_join(am_cells, by='csq') %.%
  left_join(scoreData, by='LOICZID')

# rescale lower end of the biodiversity goal to be 0 when 75% species are extinct, a level comparable to the five documented mass extinctions
cells_2013$category_linear_score2 <- ((1 - cells_2013$category_linear_avg2) - 0.25) / 0.75 * 100


# calculate area-weighted regional scores ----
regionScores <- cells_2013 %.%
  group_by(rgn_id) %.%
  summarise(rgn_spp_score_2013=sum(category_linear_score2*area_km2)/sum(area_km2),
            rgn_spp_trend_2013=sum(popn_trend_linear_avg2*area_km2)/sum(area_km2))


