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
##############################################################################=
library(data.table)
library(sp)
library(rgdal)
library(raster)
library(readr)

setwd('~/github/ohiprep') # if not already there!
source('src/R/common.R')

# Note the following masked package:raster functions
#   intersect, select, union (masked by 'dplyr');  extract (masked by 'tidyr')
# due to calling library(raster) before source('src/R/common.R').

goal <- 'globalprep/SpeciesDiversity'
dir_anx <- file.path(dir_neptune_data, 'git-annex', goal)
dir_git <- file.path('~/github/ohiprep', goal)
setwd(dir_git)


##############################################################################=
# generate raster of LOICZID values ----
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

create_loiczid_raster <- function(am_cells, dir_anx, reload = FALSE) {
### Generate half-degree raster using am_cells to assign LOICZID to CenterLong x CenterLat.
### * Template and output in <dir_anx>/rgns
### * rasterize takes about 10 seconds...
  stopifnot(sum(duplicated(am_cells$LOICZID)) == 0) #key numeric ID for raster generation.... check for dups

  loiczid_raster_file  <- file.path(dir_anx, 'rgns/loiczid_raster.grd')
  raster_template_file <- file.path(dir_anx, 'rgns/am_cells_template.tif')
  
  if(!file.exists(loiczid_raster_file) | reload == TRUE) {
    template_raster <- raster(raster_template_file)
    
    coordinates(am_cells) <- ~ CenterLong + CenterLat
    proj4string(am_cells) <- CRS(projection(template_raster))
    
    cat(sprintf('Writing LOICZID 0.5° raster to: \n  %s', rgn_prop_file))
    rasterize(am_cells, template_raster, field = 'LOICZID', progress = 'text', 
              filename = loiczid_raster_file,
              overwrite = TRUE)
  } else {
    cat(sprintf('Reading LOICZID 0.5° raster from: \n  %s', loiczid_raster_file))
  }
  loiczid_raster <- raster(loiczid_raster_file)
  return(invisible(loiczid_raster))
}

##############################################################################=
### Get percent of each raster cell in the region polygons ----
##############################################################################=

extract_loiczid_per_region <- function(dir_anx, ogr_location = file.path(dir_neptune_data, 'git-annex/Global/NCEAS-Regions_v2014/data'), reload = FALSE) {
### Determines proportional area of each cell covered by region polygons.  Returns data frame
### of sp_id, LOICZID, and proportional area of LOICZID cell covered by the sp_id region.
### Should not be year-specific, so leave files in SpeciesDiversity/rgns.
### ??? TO DO: compare LOICZID regions <-> CenterLong and CenterLat to last year's table, to make sure consistent from year to year.
### TO DO: update to translate sp_id directly into rgn_id using Melanie's code.
  
  rgn_prop_file <- file.path(dir_anx, 'rgns/region_prop_df.csv')
  
  if(!file.exists(rgn_prop_file) | reload == TRUE) {

    cat(sprintf('Reading OGR data source - come back in about 4 minutes.\n  %s\n', ogr_location))
    regions <- readOGR(dsn = ogr_location, layer='sp_gcs')
      # slow command... ~ 4 minutes
    regions <- regions[regions@data$rgn_type %in% c('eez', 'fao', 'eez_ccamlr', 'eez-disputed', 'eez-inland'), ]

    cat('Extracting proportional area of LOICZID cells per region polygon.  Come back in 15-20 minutes.')
    loiczid_raster <- raster(file.path(dir_anx, 'rgns/loiczid_raster'))
    region_prop <- raster::extract(loiczid_raster,  regions, weights = TRUE, normalizeWeights = FALSE, progress = 'text') 
      # small = TRUE returns 1 for sp_id 232, not what we want.
      # slow command... ~15 minutes (even with the small = TRUE)

    ### assign sp_id identifiers (from `regions`) to region_prop, convert to data.frame
    names(region_prop) <- regions@data$sp_id
    region_prop_df     <- plyr::ldply(region_prop, rbind) # ??? still a plyr function.  can we get out of plyr? put into function, does library just stay in function?
      # length(unique(region_prop_df$.id)) 
      #   WAS: less than 254 due to repeats of Canada and one small region (232: Bosnia/Herzegovina) with no rasters identified
      #   IS:  278, including a cell for #232.

    # ??? consider converting sp_id into rgn_id code from Melanie
    region_prop_df <- region_prop_df %>%
      rename(sp_id = .id, 
             LOICZID = value, 
             proportionArea = weight)
      # confusion between dplyr and plyr... this needs dplyr version.

    ### ??? add in this region -  Bosnia/Herzegovina (BIH), which appears to be too small to catch using this method (<1% of area)
    ### ??? SKIPPING THIS FOR NOW!!!
    # cells_2013[cells_2013$rgn_id==232, ]
    # cells_2013[cells_2013$csq=='1401:227:4', ]
    # am_cells[am_cells$CsquareCode == '1401:227:4', ]
    # 6.034664/2269.83
    # bih <- data.frame(sp_id=232, LOICZID=68076, proportionArea=0.002658641)
    # region_prop_df <- rbind(region_prop_df, bih)
    
    cat(sprintf('Writing LOICZID cell proportions by region to: \n  %s', rgn_prop_file))
    write.csv(region_prop_df, rgn_prop_file, row.names = FALSE)
  } else {
    cat(sprintf('Reading LOICZID cell proportions by region from: \n  %s', rgn_prop_file))
    region_prop_df <- read.csv(rgn_prop_file)
  }

  return(invisible(region_prop_df))
}

create_loiczid_raster(am_cells, dir_anx)
ogr_location <- file.path(dir_neptune_data, 'git-annex/Global/NCEAS-Regions_v2014/data')
extract_loiczid_per_region(dir_anx, ogr_location)
# ??? no returns from these; they create files we can read later if needed?
  
##############################################################################=
### Generating master file with raster ID's ----
##############################################################################=


##############################################################################=
### Match AquaMaps species with IUCN sid ----
##############################################################################=
# AquaMaps species list includes AM species ID, AM species code, Genus, Species,
# and AM IUCN code.
# To this list, attach IUCN species ID (sid), IUCN category (overrides the AM version)
# by joining via scientific name.
# This results in: species ID (IUCN), species ID (AM), scientific name, and IUCN category.
am_spp_raw <- fread(file.path(dir_anx, 'raw/aquamaps_2014/tables/ohi_speciesoccursum.csv'))

am_spp <- am_spp_raw %>%
  select(SPECIESID, SpecCode, Genus, Species, iucn_code)

am_spp <- am_spp %>%
  unite(sciname, Genus, Species, sep = ' ')

# pull the IUCN data from the github file for this year, or else from web.
scenario <- 'v2015'
iucn_list_file <- file.path(dir_git, scenario, 'intermediate/spp_iucn_all.csv')
if (!file.exists(iucn_list_file) ){ #| reload == TRUE){
  spp_iucn_all = read.csv('http://api.iucnredlist.org/index/all.csv')         # nrows = 122843
  spp_iucn_all = spp_iucn_all[!duplicated(spp_iucn_all),] # remove duplicates # nrows = 120484
  write.csv(spp_iucn_all, iucn_list_file, row.names = FALSE, na = '')
} else {
  spp_iucn_all = read.csv(iucn_list_file)
}

spp_iucn_tmp <- spp_iucn_all %>%
  select(sciname = Scientific.Name, sid = Red.List.Species.ID, year = Modified.Year, category = Category, criteria = Criteria) %>%
  mutate(sciname = toupper(str_trim(sciname)))
# ??? NOte: this list has category, but not population trend info.  Will have to pull those from the scraped .htms later.

am_spp_tmp <- am_spp %>%
  mutate(sciname = toupper(str_trim(sciname))) %>%
  as.data.frame() %>%
  left_join(spp_iucn_tmp, by = 'sciname')
am_spp_tmp1 <- am_spp %>%
  mutate(sciname = toupper(str_trim(sciname))) %>%
  as.data.frame() %>%
  inner_join(spp_iucn_tmp, by = 'sciname')
x <- unique(am_spp_tmp1$sciname)
z <- am_spp_tmp %>% filter(is.na(sid))

# ??? This join procedure does not match most of the rows, as is.
#     Out of 17348 AM species:
#       4948 match up with IUCN by scientific name
#       4586 of these are unique.
#       12762 do not have a match.
# TO DO: eliminate leading and trailing spaces, coerce to all caps, and try again?
#     This fixed exactly 1 instance.
# ??? Do some of these (AM or IUCN) have subspecies that are messing it up?
#     Probably not, because in IUCN subspecies have a separate column.
# ??? Drop aquamaps data with no IUCN match?  Loses nearly 13000 records of good data.
#     * many of these, we could use the AquaMaps redlist category? (what's N.E.?)
#     * but no popn_trend information will be available.
#     * Some of the matched species will use the IUCN range data instead; but none
#       of the unmatched species.  This makes it worse.

##############################################################################=
### load Aquamaps data on cell IDs and species per cell
##############################################################################=

if(!exists('am_cells_spp_raw')) {
  file_loc <- file.path(dir_anx, 'raw/aquamaps_2014/tables/ohi_hcaf_species_native.csv')
  cat(sprintf('Loading AquaMaps cell-species data.  Come back in 10 minutes. \n  %s \n', file_loc))
  am_cells_spp_raw <- read_csv(file_loc, n_max = 5000000)
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


