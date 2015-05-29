

source('~/github/ohiprep/src/R/common.R') # set pathnames, load common libraries

library(rPython) # to call Python functions and scripts within R

goal       <- 'globalprep/SpeciesDiversity'
prod       <- 'v2015'

### Set paths for local, git-annex, model(?), and git-annex/tmp
dir_git <- '~/github/ohiprep'
dir_lcl    <- file.path(dir_git, goal)                        ### local:  ~/github/ohiprep/globalprep/SpeciesDiversity
dir_anx    <- file.path(dir_neptune_data, 'git-annex', goal)  ### Neptune: /git-annex/github/ohiprep/globalprep/SpeciesDiversity
dir_mdl    <- file.path(dir_neptune_data, 'model')            ### Neptune: /model

setwd(dir_lcl)

# order of operations:



#   ingest_aquamaps.R ----- get script from Jamie -----
# ??? updated aquamaps data?  incorporate export.sql into R code?
# ??? This is probably just reading database and assigning variable names,
# ??? there may be better ways of doing this.  
# ??? In future, consider doing this using rodbc.
# ingest_aquamaps.R reads in data from sql files, and converts to .csvs?
# - ohi_speciesoccursum.sql
# - ohi_hcaf_species_native.sql: this is the big one, spatial locations for each species
# - hcaf.sql: hcaf stands for half-degree cells authority file.
# the harder part is probably extracting the data from the sql files.

# ??? convert into functions to be called from this script?
#     wrap script in function and source/call it from here
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



#   ingest_iucn.R ----- get latest from Jamie -----
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


#   ingest.py -----
# call the ingest.py Python script from within this R script, using rPython: 
#   http://www.r-bloggers.com/calling-python-from-r-with-rpython/
# create lookup tables for:
# IUCN species in Aquamaps cells; and then into regions.
python.load('ingest.py')


#   ingest_intersections.R -----
# is this really needed any more?
ingest_intersections <- function() {
  
  dir_mdl_tmp <- file.path(dir_mdl, 'GL-NCEAS-SpeciesDiversity_v2013a/tmp')
  dir_mdl_cache <- file.path(dir_mdl, 'GL-NCEAS-SpeciesDiversity_v2013a/cache')
  dir_mdl_intsx <- file.path(dir_mdl_cache, 'iucn_intersections')
  
  ### read in spp
  spp <- read.csv(file.path(dir_mdl_tmp, 'spp_iucn_marine_global.csv'), stringsAsFactors = FALSE)
  
  ### get list of dbf files
  dbfs <- list.files(dir_mdl_intsx, glob2rx('range_*_mar_sid*_pts.dbf')) # 2553 before corals
  
  ### extract sid and group from dbf path name, and assign info into spp data.frame
  sids = as.integer(gsub('range_([a-z]+)_mar_sid([0-9]+)_pts.dbf','\\2', dbfs))
  grps =            gsub('range_([a-z]+)_mar_sid([0-9]+)_pts.dbf','\\1', dbfs)
  
  spp <- spp %>% mutate(
    shp_dbf = NA, # path to dbf
    shp_grp = NA, # group like birds or seacucumbers
    shp_cnt = NA) # number of points counted with intersection of rgn_fao_am_cells_pts_gcs
  idx = match(sids, spp$sid)
  # ??? do this with mutate?
  spp$shp_dbf[idx] = dbfs
  spp$shp_grp[idx] = grps
  
  ### compile monster table of species per cell. 
  cells_spp = data.frame(cid=integer(0), sid=integer(0)) 
  ### initialize cells_spp
  
  for (i in 1:length(dbfs)){ # i=1
    ### For loop:
    ### * Read in .dbf info for one species
    ### * rename ORIG_FID to cid
    ### * for all rows where the spp$sid matches this index of sids, set count = number of rows in dbf
    ### * to cells_spp, add the cid and sid for this species
    ### ??? This loop is very slow - file reading is slow. Use as.is = TRUE? use readr?
    
    # use 'ptm <- proc.time()' and 'proc.time() - ptm' around code to time execution.
    
    # read dbf data for one species
    dbf_data <- foreign::read.dbf(file.path(dir_mdl_intsx, dbfs[i]))
    if ('ORIG_FID' %in% names(d)){
      dbf_data <- dbf_data %>% rename(cid = ORIG_FID)
    }
    
    # log dbf path, group, and count of points to species record
    # ??? do this with mutate?
    spp$shp_cnt[spp$sid==sids[i]] = nrow(dbf_data) # spp[spp$sid==sids[i], ]
    
    # merge with cells_spp
    cells_spp <- cells_spp %>%
      bind_rows(dbf_data[ , c('cid','sid')])
    
    
  }
  
  write.csv(cells_spp, file.path(dir_mdl_tmp,'cells_spp_iucn.csv'), row.names=F, na='')
  write.csv(spp,       file.path(dir_mdl_tmp,'spp_iucn.csv'),       row.names=F, na='')
}

ingest_intersections()


#   model.R -----
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

source('model.R')




# TODO: move 'D:/best/tmp/GL-NCEAS-SpeciesDiversity_v2013a/spp.db'
#           r'D:\best\tmp\GL-NCEAS-SpeciesDiversity_v2013a\tmp\geodb.gdb'

# TODO: calculate 2012 SPP with 2013 regions
#       D:\best\docs\data\model\GL-NCEAS-SpeciesDiversity\ohi_spp
