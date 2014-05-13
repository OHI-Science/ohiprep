library(sp)
library(rgdal)
library(maptools)
library(plyr)
library(dplyr)
library(tools)

# paths
shp_in      = 'N:/git-annex/Baltic/StockholmUniversity-Regions_v2014-04/data/rgn_smooth_gcs.shp'
geojson_out = 'N:/git-annex/Baltic/StockholmUniversity-Regions_v2014-04/data/regions_gcs.geojson'
js_out      = 'N:/git-annex/Baltic/StockholmUniversity-Regions_v2014-04/data/regions_gcs.js'

# read and write shapefile
x = readShapeSpatial(shp_in, proj4string=sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
x = subset(x, rgn_type=='eez')
flds = c('rgn_id'='rgn_id', 'rgn_name'='rgn_nam') #flds = c('rgn_id'='region_id', 'rgn_nam'='region_nam') # too long for shapefile columns
x@data = rename(x@data[,names(flds)], flds) # drop other fields  
writeOGR(x, dsn=geojson_out, layer=basename(file_path_sans_ext(geojson_out)), driver='GeoJSON')
fw = file(js_out, 'wb')
cat('var regions = ', file=fw)
cat(readLines(geojson_out, n = -1), file=fw)
close(fw)  
