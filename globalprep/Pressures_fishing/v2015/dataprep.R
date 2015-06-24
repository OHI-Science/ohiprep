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

# reproject to moll then Resample to 1km then mask

projection(gear_prop_hb) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"

moll_crs = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

gear_prop_hb_moll = projectRaster(gear_prop_hb,crs=moll_crs,over=T,progress='text')
gear_prop_lb_moll = projectRaster(gear_prop_lb,crs=moll_crs,over=T,progress='text')

#bring in old SAUP region raster (at 1km with no land)

old_saup_eez = raster(file.path(dir_N,'model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/tmp/saup_fao_mol.tif'))

gear_prop_hb_1km = resample(gear_prop_hb_moll,old_saup_eez,method='ngb',progress='text',filename ='v2015/gear_prop_hb_moll_1km.tif',overwrite=T)
gear_prop_lb_1km = resample(gear_prop_lb_moll,old_saup_eez,method='ngb',progress='text',filename ='v2015/gear_prop_lb_moll_1km.tif',overwrite=T)

# mask out land

gear_prop_hb_1km_ocean = mask(gear_prop_hb_1km,old_saup_eez,progress='text',filename='v2015/gear_prop_hb_moll_1km_ocean.tif',overwrite=T)
gear_prop_lb_1km_ocean = mask(gear_prop_lb_1km,old_saup_eez,progress='text',filename='v2015/gear_prop_lb_moll_1km_ocean.tif',overwrite=T)

# TO DO:

# Region 18 should have 0 catch (I think it will after multiplying by catch? Or at least will be NA?)


#-----------------------------------------------------------------------------

# Get catch


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

# Calculate change in catch since the period 1999-2003

# Here I am going to use the data from the old SAUP dataset for the years 1999-2003 to calculate the change
# in catch. This should be more accurate than using the new dataset catches for 1999-2003 since they are likely
# much different.

# bring in old data that has aggregate catch by region from 1999 to 2003

change_old = read.csv(file.path(saup_update,'data/pct_chg_saup_2009to2011_vs_1999to2003.csv'))

# The catch in yrs1999to2003 is what we want to compare to.

# Now aggregate data for periods 

# this is the period of years that was originally used. Though these data are not the same data
# that were used to create the rasters. These have likely changed drastically.
# Look at them here for comparison

# saup_1999to2003 = saup_06_10 = saup_data%>%
#   filter(Year>1998 & Year < 2004)%>%
#   group_by(id_type,old_saup_id)%>%
#   summarize(avg_catch_1999to2003 = mean(catch))%>%
#   mutate(yrs_1999to2003 = change_old$yrs1999to2003[match(old_saup_id,change_old$id)],
#          pct_chg = ((avg_catch_1999to2003-yrs_1999to2003)/yrs_1999to2003)*100,
#          Name = saup_2015_eez$Name[match(old_saup_id,saup_2015_eez$old_saup_id)])%>%
#   as.data.frame()%>%
#   filter(!is.na(old_saup_id))

saup_06_10 = saup_data%>%
  filter(Year>2005 & Year < 2011)%>%
  group_by(new_id)%>%
  summarize(avg_catch_2006to2010 = mean(catch))%>%
  as.data.frame()

#write.csv(saup_06_10,file='v2015/saup_catch_2006_2010.csv')


saup_05_09 = saup_data%>%
  filter(Year>2004 & Year < 2010)%>%
  group_by(new_id)%>%
  summarize(avg_catch_2005to2009 = mean(catch))%>%
  as.data.frame()

#write.csv(saup_05_09,file='v2015/saup_catch_2005_2009.csv')


saup_04_08 = saup_data%>%
  filter(Year>2003 & Year < 2009)%>%
  group_by(new_id)%>%
  summarize(avg_catch_2004to2008 = mean(catch))%>%
  as.data.frame()

#write.csv(saup_04_08,file='v2015/saup_catch_2004_2008.csv')


saup_03_07 = saup_data%>%
  filter(Year>2002 & Year < 2008)%>%
  group_by(new_id)%>%
  summarize(avg_catch_2003to2007 = mean(catch))%>%
  as.data.frame()

#write.csv(saup_03_07,file='v2015/saup_catch_2003_2007.csv')


# merge all catch together

catch_all_yrs = Reduce(function(x,y)merge(x,y,all=TRUE),list(saup_06_10,saup_05_09,saup_04_08,saup_03_07))

#-------------------------------------------------------------------------------------

# Calculate area using the nonprojected original raster

area = raster(file.path(saup_pressures,'catch_area_gcs.tif')) # i think this is area

#remove NAs (where no fishing occurs)

area = mask(area,gear_prop_hb,progress='text')

#bring in new saup shapefile

new_rgns = readOGR(dsn=file.path(saup_2015,'raw/SAU_EEZ_High_Seas'),layer='SAU_EEZ_High_Seas')

new_rgns = spTransform(new_rgns,crs(area))

#need to turn EEZID into the FAO ID (1000+fao area) in order to match catch

new_rgns@data = new_rgns@data%>%
                 mutate(F_AREA=as.numeric(as.character(F_AREA)))%>%
                  mutate(EEZID=ifelse(EEZID>0,EEZID,as.numeric(F_AREA)+1000))

#------------------------------------------------------------------------------------

# Rasterize catch for each period of time

rgns_ras = rasterize(new_rgns,area,field='EEZID',progress='text',fun='min',filename='v2015/new_saup_rgns.tif')


plot(rgns_ras,col=cols)

#rgns_ras = mask(rgns_ras,sum,progress='text') #maybe switch sum to all_catch? Will be same result - JA MAYBE WE DELETE THIS?

#extract total area per polygon of cells that have catch

catch_area = zonal(area,rgns_ras,fun='sum',na.rm=T,progress='text')%>%as.data.frame()


#Add new field to data

new_rgns@data = new_rgns@data%>%
              left_join(catch_all_yrs,by = c('EEZID'='new_id'))%>%
               left_join(catch_area,by= c('EEZID'='zone'))%>%
                mutate(catch_per_km_06_10 = avg_catch_2006to2010/sum,
                       catch_per_km_05_09 = avg_catch_2005to2009/sum,
                       catch_per_km_04_08 = avg_catch_2004to2008/sum,
                       catch_per_km_03_07 = avg_catch_2003to2007/sum)


# rasterize catch per km2  

ras_06_10 = rasterize(new_rgns,rgns_ras,field='catch_per_km_06_10',progress='text',filename='v2015/catch_06_10.tif',overwrite=T)
ras_05_09 = rasterize(new_rgns,rgns_ras,field='catch_per_km_05_09',progress='text',filename='v2015/catch_05_09.tif',overwrite=T)
ras_04_08 = rasterize(new_rgns,rgns_ras,field='catch_per_km_04_08',progress='text',filename='v2015/catch_04_08.tif',overwrite=T)
ras_03_07 = rasterize(new_rgns,rgns_ras,field='catch_per_km_03_07',progress='text',filename='v2015/catch_03_07.tif',overwrite=T)


# catch at 1km resolution

moll_crs = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

rep_res_mask = function(raster){
  
  name=names(raster)
  
  a = projectRaster(raster,crs=moll_crs,progress='text',over=T)
  b = resample(a,old_saup_eez,method='ngb',progress='text',filename= paste0('v2015/',name,'_1km.tif',sep=''),overwrite=T) #using the old saup region raster which is at 1km
  
}

rep_res_mask(ras_05_09)
rep_res_mask(ras_04_08)
rep_res_mask(ras_03_07)

