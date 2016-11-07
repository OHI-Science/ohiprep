# Preparing the CCAMLR_FAO data for analysis
# 
# subset relevant regions, especially for use with CCAMLR Antarctica data
#
# created: Melanie Frazier Feb 11, 2014
# modified: bbest@nceas.ucsb.edu 2014-02-19

# packages
library(rgdal)

# configuration based on machine name
conf = list(
  'AMPHITRITE'=list(  # BB's Windows 8 on MacBook Pro VMWare
    dir_git = 'G:/ohigit',
    dir_big = 'N:/'       ))[[Sys.info()['nodename']]]

# paths
shp_in  = file.path(conf$dir_big, 'model/GL-FAO-CCAMLR_v2014/orig/FAO_AREAS/FAO_AREAS')
shp_out = file.path(conf$dir_big, 'model/GL-FAO-CCAMLR_v2014/data/fao_ccamlr_gcs')
wd      = file.path(conf$dir_big, 'Global/FAO-CCAMLR_v2014')
setwd(wd)

# read shapefile
fao = readOGR(dsn=dirname(shp_in), layer=basename(shp_in))

# subset relevant regions
fao2 = fao[fao$F_CODE %in% c('41', '31', '51', '71', '81', '67', '77', '57', '47', 
                             '61', '87', '18', '21', '27', '34', 
                             '48.5', '48.2', '48.3', '48.4', '48.6', '58.4.2', '58.4.4.a', 
                             '58.7', '58.6', '58.4.4.b', '58.5.1', '58.5.2', '58.4.3.a', 
                             '58.4.3.b', '58.4.1', '88.1', '88.2', '88.3', '48.1'), ]
plot(fao2, col='red') # looks like I got everything

# add a new column that indicates what type of data
fao2@data$SOURCE = ifelse(fao2@data$F_LEVEL == 'MAJOR', 'FAO', 'CCAMLR')
table(fao2@data$MAJOR_REGION) # this looks correct

# change the CCAMLR names so they are consistent with the CCAMLR website:
fao2@data$F_CODE2 <- gsub('[.]', '', fao2@data$F_CODE)

writeOGR(fao2, dsn=dirname(shp_out), layer=basename(shp_out), driver='ESRI Shapefile')
fao3 = readOGR(dsn=dirname(shp_out), layer=basename(shp_out))
