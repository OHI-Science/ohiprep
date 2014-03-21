# Presssures summary
# March 21 2014

library(rgdal)
library(sp)
library(raster)


wd = '/var/data/ohi/model/GL-NCEAS-SpeciesDiversity_v2013a'
setwd(file.path(wd, 'raw/AquaMaps_OHI_082013'))
spp.hdr       = read.csv('hdr_speciesoccursum.csv'    , stringsAsFactors=F, header=F)[,1]
loginfo('read in aquamaps data (tbl_*.csv)\n  cells')
cells = read.csv('tbl_hcaf.csv', header=F, na.strings='\\N')

setwd("/var/data/ohi/model/GL-NCEAS-SpeciesDiversity_v2013a/tmp")
test <- readOGR(dsn="/var/data/ohi/model/GL-NCEAS-SpeciesDiversity_v2013a/tmp", layer="land_gcs")

data <- read.csv("")
/var/data/ohi
git-annex
ingest
model
raw
stable


