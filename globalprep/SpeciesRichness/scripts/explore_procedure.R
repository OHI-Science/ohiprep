## SPECIES RICHNESS OHI 2015 ##

# JAMIE AFFLERBACH


# IUCN data

iucn_2013 = read.csv(file.path(dir_N,'model/GL-NCEAS-SpeciesDiversity_v2013a/tmp/spp_iucn_all.csv'))

iucn_2014 = read.csv('../git-annex/globalprep/SpeciesDiversity/raw/all_iucn_spp.csv')
                     
                     



#load data for species map function


#libraries
library(data.table)
library(raster)
library(dplyr)

#filepaths

dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

dir_aquamaps = file.path(dir_N,"git-annex/Global/NCEAS-SpatialFishCatch_v2014/raw")
dir_cells    = file.path(dir_N,"git-annex/Global/NCEAS-SpatialFishCatch_v2014/raw/ohi_spp/data")

#read in data
#fread comes from data.table package and is much more efficient at reading in the larger .csv files

spp_cells = fread(input=file.path(dir_aquamaps,"aquamaps_2014/tables/ohi_hcaf_species_native.csv"),header=T,
                  colClasses=c('NULL','character',NA,'numeric','NULL','NULL'))%>%
  as.data.frame()
names(spp_cells)<-c("species_id","csquare_code","probability")

spp       = fread(input=file.path(dir_aquamaps,"aquamaps_2014/tables/ohi_speciesoccursum.csv"),header=T)%>%
  mutate(scientific = paste(Genus,Species,sep=" "))

cells     = fread(input=file.path(dir_cells,'cells.csv'),header=T,stringsAsFactors=F)%>%
  dplyr::select(cid,csquare_code = csquarecod, faoaream)%>%
  mutate(cid = as.numeric(cid),
         faoaream=as.numeric(faoaream))%>%
  as.data.frame

#data for raster
r               = raster(file.path(dir_cells, 'cells.tif'))
names(r)        = 'cid'

csq = unique(spp_cells$csquare_code)




             
                     
