# rename fields and files for ingestion into toolbox

library(plyr)
library(dplyr)

wd = '/Volumes/data_edit/model/GL-NCEAS_FIS_v2013a/RevisingFIS/data'
setwd(wd)

read.csv('cnk_fis_meancatch.csv', na.strings='') %.%
  rename(c('TaxonName_TaxonKey'='taxon_name_key',
           'MeanCatch'         = 'mean_catch')) %.%
  write.csv('snk_fis_meancatch_lyr.csv', row.names=F, na='')

read.csv('fnk_fis_b_bmsy.csv', na.strings='') %.%
  rename(c('TaxonName'   ='taxon_name')) %.%
  write.csv('fnk_fis_b_bmsy_lyr.csv', row.names=F, na='')

read.csv('snk_fis_propArea_saup2rgn.csv', na.strings='') %.%
  rename(c('propArea'   ='prop_area',
           'rgn_id_2013'='rgn_id')) %.%
  write.csv('snk_fis_proparea_saup2rgn_lyr.csv', row.names=F, na='')