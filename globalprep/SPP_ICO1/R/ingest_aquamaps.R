# ingest_aquamaps.R
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
