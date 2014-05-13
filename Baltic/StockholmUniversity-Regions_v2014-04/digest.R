library(sp)
library(rgdal)
library(maptools)
library(plyr)
library(dplyr)
library(tools)

# paths
shp_in      = 'N:/git-annex/Baltic/StockholmUniversity-Regions_v2014-04/data/rgn_gcs.shp'
geojson_out = 'N:/git-annex/Baltic/StockholmUniversity-Regions_v2014-04/data/rgn_gcs.geojson'
js_out      = 'N:/git-annex/Baltic/StockholmUniversity-Regions_v2014-04/data/rgn_gcs.js'

# read and write shapefile
x = readShapeSpatial(shp_in, proj4string=sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
x = subset(x, rgn_type=='eez')
flds = c('rgn_id'='rgn_id', 'rgn_nam'='rgn_name') #flds = c('rgn_id'='region_id', 'rgn_nam'='region_nam') # too long for shapefile columns
x@data = rename(x@data[,names(flds)], flds) # drop other fields  
writeOGR(x, dsn=json_out, layer=basename(file_path_sans_ext(geojson_out)), driver='GeoJSON')
fw = file(js_out, 'wb')
cat('var regions = ', file=fw)
cat(readLines(geojson_out, n = -1), file=fw)
close(fw)  
