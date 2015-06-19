# Create fish pressure layers 


rm(list=ls())

library(raster)
library(dplyr)
library(RColorBrewer)
library(rgdal)
library(ggplot2)

cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme



dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_N,'git-annex/globalprep/Pressures_fishing'))

# File paths

saup_pressures = file.path(dir_N,'git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data')

# Data used for the commercial fishing pressures layers in OHI 2013. 
#These are derivatives and updates to the 2008 data 
ohi_2013  = file.path(dir_N,'model/GL-NCEAS-Pressures_CommercialFisheries_v2013a')

# new SAUP data
saup_2015 = file.path(dir_N,'git-annex/globalprep/SAUP_data_2015')

# Directory where the updates to the 2008 data were done for 2013 ohi
saup_update = file.path(dir_N, 'model/GL-SAUP-FisheriesCatchData_v2013')

#---------------------------------------------------------------------------------

# Get gear catch rasters

area = raster(file.path(saup_pressures,'catch_area_gcs.tif')) # i think this is area!??

dem_d = raster(file.path(saup_pressures,'catch_dem_d_gcs.tif'))

dem_hb = raster(file.path(saup_pressures,'catch_dem_nd_hb_gcs.tif'))

dem_lb = raster(file.path(saup_pressures,'catch_dem_nd_lb_gcs.tif'))

pel_lb = raster(file.path(saup_pressures,'catch_pel_lb_gcs.tif'))

pel_hb = raster(file.path(saup_pressures,'catch_pel_hb_gcs.tif'))

# aggregate high and low bycatch

hb = stack(dem_d,dem_hb,pel_hb)%>%
        calc(.,fun=function(x){sum(x)},progress='text')

lb = stack(pel_lb,dem_lb)%>%
      calc(.,fun=function(x){sum(x)},progress='text')

# aggregate all catch

all_catch = stack(dem_d,dem_hb,dem_lb,pel_lb,pel_hb)%>%
             calc(.,fun=function(x){sum(x)},progress='text')

# create percent rasters


gear_prop_hb = overlay(hb,all_catch,fun=function(x,y){x/y},progress='text')

gear_prop_lb = overlay(lb,all_catch,fun=function(x,y){x/y},progress='text')

# sum these together to see if we have regions with zeros (ideally these are all 1??)

sum = sum(gear_prop_hb,gear_prop_lb)

# for regions where there is no catch in the past, get the average proportion of high and low bycatch

hb_mean = cellStats(gear_prop_hb,'mean')
lb_mean = cellStats(gear_prop_lb,'mean')

# replace NAs with high and low bycatch (then we'll have to mask out land later down the line)

gear_prop_hb[is.na(gear_prop_hb)]<-hb_mean
gear_prop_lb[is.na(gear_prop_lb)]<-lb_mean

# reproject to moll then Resample to 1km then mask

projection(gear_prop_hb)<- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"

moll_crs = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

gear_prop_hb_moll = projectRaster(gear_prop_hb,crs=moll_crs,over=T,progress='text')
gear_prop_lb_moll = projectRaster(gear_prop_lb,crs=moll_crs,over=T,progress='text')

#bring in old SAUP region raster (at 1km with no land)

old_saup_eez = raster(file.path(dir_N,'model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/tmp/saup_fao_mol.tif'))

gear_prop_hb_1km = resample(gear_prop_hb_moll,old_saup_eez,method='ngb',progress='text',filename ='v2015/gear_prop_hb_moll_1km.tif')
gear_prop_lb_1km = resample(gear_prop_lb_moll,old_saup_eez,method='ngb',progress='text',filename ='v2015/gear_prop_lb_moll_1km.tif')

# mask out land

gear_prop_hb_1km_ocean = mask(gear_prop_hb_1km,old_saup_eez,progress='text',filename='v2015/gear_prop_hb_moll_1km_ocean.tif')
gear_prop_hb_1km_ocean = mask(gear_prop_hb_1km,old_saup_eez,progress='text',filename='v2015/gear_prop_hb_moll_1km_ocean.tif')

# TO DO:

# Region 18 should have 0 catch






#--------------------------------------------------------------------------------------

# New SAUP data

saup_all = read.csv(file.path(saup_2015,'tmp/Catch_Value_11062015_summary.csv'))

#-----------------------------------------------------------------------------

# new eezs

saup_2015_eez = read.csv('v2015/ohi_eezs_plus_old_saup_ids.csv')

#-----------------------------------------------------------------------------


# filter out taxonkey not used previously

saup_taxon_exclude = read.csv(file.path(saup_update,'tmp/global_srcdata_ss_saup_excluded_stock.csv'))

# look at what these taxons are

# are these taxon keys the same?

ohi_2015_taxons = read.csv(file.path(saup_2015,'raw/ohi_taxon.csv'))

filter(ohi_2015_taxons,taxonkey%in%saup_taxon_exclude$stock_id)

#   X taxonkey                     scientific.name           common.name
#1  1   100000                      Marine animals        Marine animals
#2  3   100025     Miscellaneous diadromous fishes     Diadromous fishes
#3  8   100039        Marine fishes not identified         Marine fishes
#4 12   100047    Miscellaneous marine crustaceans    Marine crustaceans
#5 14   100058       Miscellaneous marine molluscs       Marine molluscs
#6 15   100077 Miscellaneous aquatic invertebrates Aquatic invertebrates
#7 16   100139        Marine fishes not identified             Finfishes
#8 20   100239        Marine fishes not identified          Groundfishes
#9 23   100339        Marine fishes not identified        Pelagic fishes

# filter our taxons and aggregate catch per year/region - add in the old SAUP IDs
saup_data = saup_all%>% 
  filter(!TaxonKey %in% saup_taxon_exclude$stock_id)%>%
  mutate(id      = ifelse(EEZID==0,FAOAreaID+1000,EEZID),
         id_type = ifelse(id>1000,'fao','eez'))%>% 
  group_by(Year,id_type,id)%>%
  summarize(catch=sum(catch))%>%
  mutate(old_saup_id = as.integer(ifelse(id_type=='eez',saup_2015_eez$old_saup_id[match(id,saup_2015_eez$EEZID)],id)))%>%
  select(new_id = id, Year, id_type,catch,old_saup_id)


# now we have the total catch per year per id.


#-----------------------------------------------------------------------------
