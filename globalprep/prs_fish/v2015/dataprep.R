# Create fish pressure layers 

rm(list=ls())

options(scipen=999)

#libraries
library(raster)
library(RColorBrewer)
library(rgdal)
library(ggplot2)

# set color pallete for plotting
cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme

# set tmp directory

tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)


dir_M             <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
                       'Darwin'  = '/Volumes/ohi',    ### connect (cmd-K) to smb://mazu/ohi
                       'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_M,'git-annex/globalprep/prs_fish'))


# File paths

    # Original data from 2008 CHI project
    saup_pressures = file.path(dir_M,'git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data')

    # Data used for the commercial fishing pressures layers in OHI 2013. 
    #These are derivatives and updates to the 2008 data 
    ohi_2013  = file.path(dir_M,'model/GL-NCEAS-Pressures_CommercialFisheries_v2013a')

    # new SAUP data
    saup_2015 = file.path(dir_M,'git-annex/globalprep/SAUP_FIS_data/v2015')

    # Directory where the updates to the 2008 data were done for 2013 ohi
    saup_update = file.path(dir_M, 'model/GL-SAUP-FisheriesCatchData_v2013')
    
    
# Spatial information    
    
    #bring in old SAUP region raster (at 1km with no land). This will be used to resample and reproject only
    old_saup_eez = raster(file.path(dir_M,'model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/tmp/saup_fao_mol.tif'))
    
    #define mollweide CRS    
    moll_crs = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
    
#---------------------------------------------------------------------------------

# Get gear catch rasters

    
  #Long/Lat  
    dem_d  = raster(file.path(saup_pressures,'catch_dem_d_gcs.tif'))
    dem_hb = raster(file.path(saup_pressures,'catch_dem_nd_hb_gcs.tif'))
    dem_lb = raster(file.path(saup_pressures,'catch_dem_nd_lb_gcs.tif'))
    pel_lb = raster(file.path(saup_pressures,'catch_pel_lb_gcs.tif'))
    pel_hb = raster(file.path(saup_pressures,'catch_pel_hb_gcs.tif'))
    
    
    # aggregate high and low bycatch
    
    hb_gcs = stack(dem_d,dem_hb,pel_hb)%>%
      calc(.,fun=function(x){sum(x)},progress='text')
    
    lb_gcs = stack(pel_lb,dem_lb)%>%
      calc(.,fun=function(x){sum(x)},progress='text')
    
    # aggregate all catch
    
    all_catch_gcs = stack(dem_d,dem_hb,dem_lb,pel_lb,pel_hb)%>%
      calc(.,fun=function(x){sum(x)},progress='text')
    
    # Create single raster without projection to use to calculate area of fished cells below
    gear_prop_hb_gcs = overlay(hb_gcs,all_catch_gcs,fun=function(x,y){x/y},progress='text')

    
    hb_wgs = stack(dem_d,dem_hb, pel_hb)%>%calc(.,fun=function(x){sum(x,na.rm=T)})
    lb_wgs = stack(pel_lb, dem_lb)%>%calc(.,fun=function(x){sum(x,na.rm=T)})
    
    hb_prop_wgs = overlay(hb_wgs,all_catch_gcs,fun=function(x,y){x/y}, filename = 'v2015/gear_prop_hb_gcs.tif')
    lb_prop_wgs = overlay(lb_wgs,all_catch_gcs,fun=function(x,y){x/y}, filename = 'v2015/gear_prop_lb_gcs.tif')
    
  # MOLLWEIDE  
    # reproject all original raster files
    dem_d_moll = projectRaster(dem_d,crs=moll_crs,over=T,progress='text')
    dem_hb_moll = projectRaster(dem_hb,crs=moll_crs,over=T,progress='text')
    dem_lb_moll = projectRaster(dem_lb,crs=moll_crs,over=T,progress='text')
    pel_lb_moll = projectRaster(pel_lb,crs=moll_crs,over=T,progress='text')
    pel_hb_moll = projectRaster(pel_hb,crs=moll_crs,over=T,progress='text')
    

    
    # aggregate high and low bycatch
    
    hb = stack(dem_d_moll,dem_hb_moll,pel_hb_moll)%>%
      calc(.,fun=function(x){sum(x,na.rm=T)},progress='text')
    
    lb = stack(pel_lb_moll,dem_lb_moll)%>%
      calc(.,fun=function(x){sum(x,na.rm=T)},progress='text')
    
    # aggregate all catch
    
    all_catch = stack(dem_d_moll,dem_hb_moll,dem_lb_moll,pel_lb_moll,pel_hb_moll)%>%
      calc(.,fun=function(x){sum(x)},progress='text')
    
    
    # create percent rasters (what percent of catch in a given cell was caught with high bycatch gear? And what percent caught with low bycatch gear)
    
    gear_prop_hb = overlay(hb,all_catch,fun=function(x,y){x/y},progress='text')%>%
                    resample(.,old_saup_eez,progress='text',method='ngb')%>%
                     mask(.,old_saup_eez,progress='text',filename='gear_prop_hb_moll_1km_ocean.tif',overwrite=T)
    
    gear_prop_lb = overlay(lb,all_catch,fun=function(x,y){x/y},progress='text')%>%
                    resample(.,old_saup_eez,progress='text',method='ngb')%>%
                     mask(.,old_saup_eez,progress='text',filename='gear_prop_lb_moll_1km_ocean.tif',overwrite=T)
    

    # sum these together to see if we have regions with zeros (all cells should be 1 or NA where no catch occurs)

    sum = sum(gear_prop_hb,gear_prop_lb)

#------------------------------------------------------------------------------

# see if any OHI regions have all NA cells

rgn = readOGR(dsn=file.path(dir_M,'git-annex/globalprep/spatial/v2015/data'),layer='regions_mol')


# raster/zonal data
zones    <- raster(file.path(rast_loc, "sp_mol_raster_1km.tif"))  # raster data
rgn_data <- read.csv(file.path(rast_loc, 'regionData.csv'))       # data for sp_id's used in raster

#extract data for each region:
regions_stats <- zonal(rgn_data,  zones, fun="sum", na.rm=TRUE, progress="text")

#bouvet island does have NAs. This should be accounted for when allocating catch

#-----------------------------------------------------------------------------

# Get catch


# New SAUP data

saup_all = read.csv(file.path(saup_2015,'tmp/Catch_v16072015_summary.csv'))

#-----------------------------------------------------------------------------


# filter out taxonkeys not used previously

  saup_taxon_exclude = read.csv(file.path(saup_update,'tmp/global_srcdata_ss_saup_excluded_stock.csv'))

# look at what these taxons are

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

# filter out taxons and aggregate catch per year/region
saup_data = saup_all%>% 
              filter(!TaxonKey %in% saup_taxon_exclude$stock_id)%>%
              mutate(rgn_id      = ifelse(EEZID==0,FAOAreaID+1000,EEZID),
                     id_type     = ifelse(rgn_id>1000,'fao','eez'),
                     eez_fao_id  = as.numeric(paste0(rgn_id,FAOAreaID,sep="")))%>%  #create a unique identifier that is a combination of eezid and fao. This will help allocate catch to EEZs that split FAO regions
              group_by(Year,eez_fao_id,FAOAreaID,EEZID)%>%
              summarize(catch=sum(catch))%>%
              ungroup()

# now we have the total catch per year per eez region per FAO region...


#-----------------------------------------------------------------------------

# Calculate catch for all time periods we are interested in

saup_06_10 = saup_data%>%
              filter(Year>2005 & Year < 2011)%>%
              group_by(eez_fao_id,FAOAreaID,EEZID)%>%
              summarize(avg_catch_2006to2010 = mean(catch))%>%
              ungroup()%>%
              as.data.frame()

write.csv(saup_06_10,file='v2015/saup_catch_2006_2010.csv')


saup_05_09 = saup_data%>%
              filter(Year>2004 & Year < 2010)%>%
              group_by(eez_fao_id,FAOAreaID,EEZID)%>%
              summarize(avg_catch_2005to2009 = mean(catch))%>%
              ungroup()%>%
              as.data.frame()

write.csv(saup_05_09,file='v2015/saup_catch_2005_2009.csv')


saup_04_08 = saup_data%>%
              filter(Year>2003 & Year < 2009)%>%
              group_by(eez_fao_id,FAOAreaID,EEZID)%>%
              summarize(avg_catch_2004to2008 = mean(catch))%>%
              ungroup()%>%
              as.data.frame()

write.csv(saup_04_08,file='v2015/saup_catch_2004_2008.csv')


saup_03_07 = saup_data%>%
              filter(Year>2002 & Year < 2008)%>%
              group_by(eez_fao_id,FAOAreaID,EEZID)%>%
              summarize(avg_catch_2003to2007 = mean(catch))%>%
              ungroup()%>%
              as.data.frame()

write.csv(saup_03_07,file='v2015/saup_catch_2003_2007.csv')


# merge all catch together

catch_all_yrs = Reduce(function(x,y)merge(x,y,all=TRUE),list(saup_06_10,saup_05_09,saup_04_08,saup_03_07))

#-------------------------------------------------------------------------------------
# Calculate catch per km2 for each SAUP region. We don't want to assign catch to cells where no fishing occurrs - according
# to the catch by gear type rasters


# Calculate area using the nonprojected original raster

area_gcs = raster(file.path(saup_pressures,'catch_area_gcs.tif'))%>%
            disaggregate(.,fact=25,progress='text')%>% #then calculate area
            mask(.,new_rgns,progress='text')

hb_gcs_disagg = disaggregate(gear_prop_hb_gcs,fact=25,progress='text')


calc_area = area(area_gcs,na.rm=T,weights=FALSE,progress='text')%>% #calculate area of all cells at 0.02 degrees
             mask(.,hb_gcs_disagg,progress='text',filename='v2015/cell_area_fishing.tif',overwrite=T) #remove all cells where fishing did not occur 


#bring in new saup shapefile

    new_rgns = readOGR(dsn=file.path(saup_2015,'raw/SAU_EEZ_High_Seas'),layer='SAU_EEZ_High_Seas')

    new_rgns = spTransform(new_rgns,crs(calc_area))

#need to turn EEZID into the FAO ID (1000+fao area) in order to match catch

    new_rgns@data = new_rgns@data%>%
                     mutate(F_AREA=as.numeric(as.character(F_AREA)),
                            EEZID=ifelse(EEZID>0,EEZID,as.numeric(F_AREA)+1000),
                            raster_id = as.numeric(paste0(EEZID,F_AREA,sep="")))

# created the column raster_id so that each polygon has a unique id for rasterizing
# the regions shapefile to allow for accurate area calculation and catch allocation


#------------------------------------------------------------------------------------

# Calculate area per cell in each region then total fished area per SAUP region

# rasterizing by objectID because polygons that split FAO regions need to be rasterized as 
# individual polygons. 
    
    rgns_ras = rasterize(new_rgns,calc_area,field='raster_id',progress='text',filename='v2015/new_saup_rgns_rasterid.tif',overwrite=T)
    plot(rgns_ras,col=cols)


#extract total area per polygon of cells that have catch

catch_area = zonal(calc_area,rgns_ras,fun='sum',na.rm=T,progress='text')%>%as.data.frame()


#Add new field (area in km2 for just fished areas) to data

new_rgns@data = new_rgns@data%>%
              left_join(catch_all_yrs,by = c('raster_id'='eez_fao_id'))%>%
               left_join(catch_area,by= c('raster_id'='zone'))%>%
                mutate(catch_per_km_06_10 = (avg_catch_2006to2010/sum)*.872356,#the final cells are 934m*934m so multiply catch by .872356km2 (it's not actually 1km2 cell resolution at the end)
                       catch_per_km_05_09 = (avg_catch_2005to2009/sum)*.872356,
                       catch_per_km_04_08 = (avg_catch_2004to2008/sum)*.872356,
                       catch_per_km_03_07 = (avg_catch_2003to2007/sum)*.872356) #sum is the fished area in km2

#---------------------------------------------------------------------------

# rasterize catch per km2  

# (1) Since we now know the catch per km2 per region, rasterize SAUP rgns, project to mollweide and resample to 1km.
saup_rgns = spTransform(new_rgns,moll_crs,progress='text')
saup_rgns_1km = rasterize(saup_rgns,old_saup_eez,field='raster_id',progress='text',filename='v2015/saup_raster_id_1km.tif')

saup_rgns_1km_area = projectRaster(saup_rgns_1km,crs=crs(area_gcs),progress='text',file='v2015/saup_rgns_1km_gcs.tif')


#(2) Next, substitute values in saup_rgns_1km for the catch per km

catch = as.data.frame(new_rgns@data)%>%
            dplyr::select(raster_id,catch_per_km_06_10,catch_per_km_05_09,catch_per_km_04_08,catch_per_km_03_07)

catch_km_06_10 = subs(saup_rgns_1km,catch,which=2,progress='text')%>%
                  mask(.,gear_prop_hb,progress='text',filename='v2015/catch_km_06_10.tif',overwrite=T)
  
catch_km_05_09 = subs(saup_rgns_1km,catch,which=3,progress='text')%>%
  mask(.,gear_prop_hb,progress='text',filename='v2015/catch_km_05_09.tif',overwrite=T)

catch_km_04_08 = subs(saup_rgns_1km,catch,which=4,progress='text')%>%
  mask(.,gear_prop_hb,progress='text',filename='v2015/catch_km_04_08.tif',overwrite=T)

catch_km_03_07 = subs(saup_rgns_1km,catch,which=5,progress='text')%>%
  mask(.,gear_prop_hb,progress='text',filename='v2015/catch_km_03_07.tif',overwrite=T)


#--------------------------------------------------------------------------------------

# Sum rasterized catch and compare to actual values to see how they match up

catch_zonal_06_10 = zonal(catch_km_06_10,saup_rgns_1km,fun='sum',na.rm=T,progress='text')%>%as.data.frame()%>%
  mutate(catch=saup_06_10$avg_catch_2005to2009[match(zone,saup_06_10$eez_fao_id)],
         diff = catch - sum)

plot(catch_zonal_06_10$sum~catch_zonal_06_10$catch)

catch_zonal_05_09 = zonal(catch_km_05_09,saup_rgns_1km,fun='sum',na.rm=T,progress='text')%>%as.data.frame()%>%
                mutate(catch=saup_05_09$avg_catch_2005to2009[match(zone,saup_05_09$eez_fao_id)],
                       diff = catch - sum)

plot(catch_zonal_05_09$sum~catch_zonal_05_09$catch)

catch_zonal_04_08 = zonal(catch_km_04_08,saup_rgns_1km,fun='sum',na.rm=T,progress='text')%>%as.data.frame()%>%
  mutate(catch=saup_04_08$avg_catch_2004to2008[match(zone,saup_04_08$eez_fao_id)],
         diff = catch - sum)

plot(catch_zonal_04_08$sum~catch_zonal_04_08$catch)

catch_zonal_03_07 = zonal(catch_km_03_07,saup_rgns_1km,fun='sum',na.rm=T,progress='text')%>%as.data.frame()%>%
  mutate(catch=saup_03_07$avg_catch_2003to2007[match(zone,saup_03_07$eez_fao_id)],
         diff = catch - sum)

plot(catch_zonal_03_07$sum~catch_zonal_03_07$catch)



#--------------------------------------------------------------------------------------------------

