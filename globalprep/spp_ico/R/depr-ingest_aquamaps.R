ingest_aquamaps <- function() {
  library(logging)

  # Read in csv outputs from running raw\AquaMaps_OHI_082013\export.sql
  #   See raw\AquaMaps_OHI_082013\README.txt on a running instance of AquaMaps MySQL
  
  # set working directory
  old_wd <- getwd()
  wd = 'N:/model/GL-NCEAS-SpeciesDiversity_v2013a'
  setwd(file.path(wd, 'raw/AquaMaps_OHI_082013'))
  
  # setup logging
  basicConfig()
  addHandler(writeToFile, logger='', file=file.path(wd,'cache','ingest_aquamaps.log'))
  
  # read in aquamaps header column names (hdr_*.csv)
  loginfo('read in aquamaps header column names (hdr_*.csv)')
  cells.hdr     = read.csv('hdr_hcaf.csv'               , stringsAsFactors=F, header=F)[,1]
  cells_spp.hdr = read.csv('hdr_hcaf_species_native.csv', stringsAsFactors=F, header=F)[,1]
  spp.hdr       = read.csv('hdr_speciesoccursum.csv'    , stringsAsFactors=F, header=F)[,1]
  
  # read in aquamaps data (tbl_*.csv)
  loginfo('read in aquamaps data (tbl_*.csv)\n  cells')
  cells     = read.csv('tbl_hcaf.csv'               , header=F, col.names=cells.hdr    , na.strings='\\N')
  loginfo('  cells_spp')
  cells_spp = read.csv('tbl_hcaf_species_native.csv', header=F, col.names=cells_spp.hdr, na.strings='\\N')
  loginfo('  spp')
  spp       = read.csv('tbl_speciesoccursum.csv'    , header=F, col.names=spp.hdr      , na.strings='\\N')
  
  # write out to tmp
  setwd(file.path(wd, 'tmp'))
  loginfo('write out to tmp\n  am_cells_data.csv')
  write.csv(cells    , 'am_cells_data.csv'    , row.names=F, na='')
  loginfo('  am_cells_spp_data.csv')
  write.csv(cells_spp, 'am_cells_spp_data.csv', row.names=F, na='')
  loginfo('  am_spp_data.csv')
  write.csv(spp      , 'am_spp_data.csv'      , row.names=F, na='')
  loginfo('finished!')
  
  setwd(old_wd)
  
  return(TRUE)
}