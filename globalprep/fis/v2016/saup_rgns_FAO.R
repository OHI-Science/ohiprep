############################################
## Determine proportion of each SAUP region
## in each FAO
## Used to assign fisheries in saup region 
## to larger FAO
## MRF 6/9/2016
############################################

library(dplyr)
library(rgdal)
library(raster)
library(sp)
library(rgeos)
library(maptools)
library(seaaroundus)
library(cleangeo)


source('src/R/common.R')

## saup regions

getapibaseurl <- function() {
  return("http://api.seaaroundus.org/api/v1")
}

url <- paste(getapibaseurl(), 'eez', "?geojson=true", sep = "/")

saup_regions <- readOGR(dsn = url, layer = ogrListLayers(url), 
                           verbose = FALSE)

## Some issues with intersections, etc.
saup_regions<- clgeo_Clean(saup_regions, print.log = TRUE)


## FAO region
fao_regions <- readOGR(dsn = file.path(dir_M, "git-annex/Global/NCEAS-Regions_v2014/orig/FAO_AREAS"), layer="FAO_AREAS")  
fao_regions <- fao_regions[fao_regions$F_LEVEL == "MAJOR", ]
fao_regions <- spTransform(fao_regions, CRS(proj4string(saup_regions)))
plot(fao_regions)


# intersect from raster package
pi <- intersect(saup_regions, fao_regions)

# Extract areas from polygon objects then attach as attribute
areas <- data.frame(area=sapply(pi@polygons, FUN=function(x) {slot(x, 'area')}))
row.names(areas) <- sapply(pi@polygons, FUN=function(x) {slot(x, 'ID')})
# Combine attributes info and areas 
attArea <- spCbind(pi, areas)

data <- attArea@data %>%
  group_by(region_id) %>%
  mutate(total_area = sum(area)) %>%
  ungroup() %>%
  mutate(prop_area = area/total_area) %>%
  dplyr::select(saup_id = region_id, saup_name = title, fao_rgn = F_CODE, prop_area) %>%
  data.frame()

write.csv(data, "globalprep/fis/v2016/int/saup_rgn_to_fao.csv", row.names=FALSE)
