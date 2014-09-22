############################################################
### Assign RFMOs to High Seas (i.e, FAO) regions
############################################################

require(sp)
library(raster)
library(rgdal)
library(maptools)
library(fields)
library(maps)
library(animation)
library(ncdf)
library(rasterVis)
library(rgeos) # gis analysis
library(geosphere) #find area of polygons for unprojected data


rm(list = ls())


## This changes the defaults to allow polygons that are slightly out of tolerance to be loaded:
sp::set_ll_warn(FALSE)
sp::set_ll_TOL(.02)

source('../ohiprep/src/R/common.R')

data_files <- file.path(dir_neptune_data, "model/GL-HS-Resilience/RFMO")

#-----------------------------------------------------------
# load relevant RFMO regions
# ----------------------------------------------------------

ccsbt <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_CCSBT")
iattc <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_IATTC")
iccat <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_ICCAT")
wcpfc <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_WCPFC")
iotc <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_IOTC")
#ccamlr <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_CCAMLR") #relevant to Antarctica region only
ccbsp <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_CCBSP")
gfcm <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_GFCM")
neafc <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_NEAFC")
nasco <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_NASCO")
nafo <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_NAFO")
seafo <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_SEAFO")
siofa <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_SIOFA")
npafc <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_NPAFC")
sprfmo <- readOGR(dsn = file.path(data_files, "raw/TWAP_Shapefiles - Ben/RFB"), layer="RFB_SPRFMO")

#clip neafc and nasco boundaries ever so slightly to be < 90 latitude:
CP <- as(extent(-85, 75, 30, 90), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(nasco))
neafc <- gIntersection(neafc, CP)
nasco <- gIntersection(nasco, CP)

rfmoList <- c("ccsbt", 'iattc', 'iccat', 'wcpfc', 'iotc', 'ccbsp', 'gfcm',
              'neafc', 'nasco', 'nafo', 'seafo', 'siofa', 'npafc', 'sprfmo')
#-----------------------------------------------------------
# load fao data
# ----------------------------------------------------------
#will want to update spatial file in future to this one: 
#fao <- readOGR(dsn = file.path(dir_neptune_data, "git-annex/Global/NCEAS-Regions_v2014/data"), layer="sp_gcs") 
#used this one in the past, when Ben's file wasn't available (shouldn't be any big differences):
fao <- readOGR(dsn = file.path(data_files, "raw"), layer="eez_ccmlar_fao_gcs")

#plot(fao)
fao <- fao[fao$rgn_type=="fao", ]
fao@data <- subset(fao@data, select=c(rgn_id, rgn_name))

plot(fao)

plot(ccsbt, add=TRUE, col=rgb(1, 0, 0,0.2))
plot(iattc, add=TRUE, col=rgb(1, 0, 0,0.2))
plot(iccat, add=TRUE, col=rgb(1, 0, 0,0.2))
plot(wcpfc, add=TRUE, col=rgb(1, 0, 0,0.2))
plot(iotc, add=TRUE, col=rgb(1, 0, 0,0.2))
#plot(ccamlr, add=TRUE, col=rgb(1, 0, 0,0.1))
plot(ccbsp, add=TRUE, col=rgb(1, 0, 0,0.2))
plot(gfcm, add=TRUE, col=rgb(1, 0, 0,0.2))
plot(neafc, add=TRUE, col=rgb(1, 0, 0,0.2))
plot(nasco, add=TRUE, col=rgb(1, 0, 0,0.2))
plot(nafo, add=TRUE, col=rgb(1, 0, 0,0.2))
plot(seafo, add=TRUE, col=rgb(1, 0, 0,0.2))
plot(siofa, add=TRUE, col=rgb(1, 0, 0,0.2))
plot(npafc, add=TRUE, col=rgb(1, 0, 0,0.2))
plot(sprfmo, add=TRUE, col=rgb(1, 0, 0,0.2))

for(i in 1:length(rfmoList)){
  plot(fao)
  plot(get(rfmoList[i]), add=TRUE, col=rgb(1, 0, 0,0.2))
}

#-----------------------------------------------------------
# overlay data to determine overlap of each RFMO with each FAO region
# ----------------------------------------------------------
## Determine area of the OHI regions that overlap each LME region
overlayData <- over(fao, ccsbt) 

areas <- fao@data
for(j in c(1:length(rfmoList))){
#   j <- 1

rfmoAreas <- ldply(1:length(fao), function(i){
 # i <- 15
  tmp <- gIntersection(get(rfmoList[j]), fao[i, ])
  #tmp
if(class(tmp)[1]=="SpatialLines"){0
} else if(class(tmp)[1]=="SpatialCollections"){ifelse(is.null(tmp@polyobj), 0, areaPolygon(tmp@polyobj))
} else if(class(tmp)[1] != "SpatialCollections"){ifelse(is.null(tmp), 0, areaPolygon(tmp))}
})
names(rfmoAreas) <- rfmoList[j]
areas <- data.frame(areas, rfmoAreas)
}

#-----------------------------------------------------------
## divide by total area of fao regions to get proportion coverage of each rfmo
# ----------------------------------------------------------

area <- areaPolygon(fao) #warnings seem ok...just alerting that there is >90
PropArea <- data.frame(areas[1:2], round(areas[3:16]/area, 2))
PropArea$total <- apply(PropArea[3:16], 1, sum)
write.csv(PropArea, "tmp/RFMOperFAO.csv", row.names=FALSE)

