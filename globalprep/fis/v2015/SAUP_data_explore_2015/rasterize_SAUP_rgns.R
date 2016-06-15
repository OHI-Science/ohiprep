#Rasterize new SAUP regions to 1km

# This was  created to revisit catch allocation but might prove useful in other OHI instances

#Jamie Afflerbach

#10/9/2015

#-------------------------------------------

#libraries

library(raster)
library(rgdal)

# set tmp directory

tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)

# paths

dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]


#ocean is a raster with all land clipped out - at 1km with value of 1
ocean = raster(file.path(dir_N,'model/GL-NCEAS-Halpern2008/tmp/ocean.tif'))

#SAUP new regions

saup = readOGR(dsn=file.path(dir_N,'git-annex/globalprep/SAUP_FIS_data/v2015/raw/SAU_EEZ_High_Seas'), layer='SAU_EEZ_High_Seas')

#reproject shapefile to mollweide

saup_mol = spTransform(saup,crs(ocean))

#add 1000 to FAO area
saup_mol@data = saup_mol@data%>%mutate(F_AREA_1000 = as.numeric(F_AREA)+1000)%>%
                  mutate(ID = ifelse(EEZID>0,EEZID,F_AREA_1000))

#rasterize to 1km

saup_ras = rasterize(saup_mol,ocean,field='ID',filename=file.path(dir_N,'git-annex/globalprep/SAUP_FIS_data/v2015/saup_rgns_mol_1km.tif'),progress='text')
