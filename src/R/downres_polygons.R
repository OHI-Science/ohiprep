# From: http://stackoverflow.com/questions/20976449/r-simplify-shapefile

# There is still a lot of detail in the edges of the main polygons that is unecessary 
# for my requirements. This detail can be simplified using the dp() function in the 
# shapefiles package. Once again, we create a loop that goes through the object 
# simplifying each of the polygons within it. dp() works only on dataframes, so we 
# need to break each @coords matrix into a dataframe, run dp(), then convert it back 
# into a matrix.

# Note: can also cut smaller polygons if necessary... 

setwd('~/github/ohiprep')
source('src/R/common.R')

library(maptools)
library(shapefiles)

dir_rgn  <- file.path(dir_neptune_data, 'git-annex/globalprep/spatial/v2015/data')
poly_layer <- file.path(dir_rgn, 'regions_gcs')
dir_git  <- file.path('~/github/ohiprep/globalprep/spatial/downres')


rgn_poly_orig <- readShapePoly(poly_layer) 
# original .shp: 308 MB

# examine structure of region polygons
rgn_poly <- rgn_poly_orig # create working copy
str(rgn_poly, max.level = 3)

sapply(rgn_poly@polygons, function(x) length(x@Polygons))
# first 100-ish polygons displayed; these are the number of sub-polygons within the main polygon.
#  [1]    22    21     2     1     3     3    55    57   193   196    19    18    23    23     3     2    26    26     2
# [20]     1    33    32   945   944  1898  1897    88    87  2315  2314     8     7   155   155    54    54   233   232
# [39]    55    54    25    24     3     2     3     2    20    19     3     2    10     9     4     3     2     1    59
# [58]    58    38    37    66    65   577   578    39    38    64    63   183   180   234   231   133   133    28    29
# [77]  1524  1523     5     4   195   197   219   219   365   368   155   154    77    76    33    32  1242  1241     2
# [96]     1 18222 18222    16    15  1577  1576    71    70     2     1  3404  3403     8     7    19    18    14    13

# create area list for filtering
area <- lapply(rgn_poly@polygons, function(x) sapply(x@Polygons, function(y) y@area))

quantile(unlist(area))
#     0%          25%          50%          75%         100% 
# 8.307850e-11 5.944036e-06 1.223720e-05 3.958455e-05 2.819291e+03 
# lots of tiny polygons - ditch 'em?

mainPolys <- lapply(area, function(x) which(x > 0.001))

rgn_poly@data <- rgn_poly@data[-c(1:2),]
rgn_poly@polygons <- rgn_poly@polygons[-c(1:2)]
rgn_poly@plotOrder <- 1:length(rgn_poly@polygons)
mainPolys <- mainPolys[-c(1:2)]

for(i in 1:length(mainPolys)){
  if(length(mainPolys[[i]]) >= 1 && mainPolys[[i]][1] >= 1){
    rgn_poly@polygons[[i]]@Polygons <- rgn_poly@polygons[[i]]@Polygons[mainPolys[[i]]]
    rgn_poly@polygons[[i]]@plotOrder <- 1:length(rgn_poly@polygons[[i]]@Polygons)
  }
}
# after this step, should still be the same number of elements (551) but each
# element should have fewer polygons within it (minimum of 1).

# num elements
#  [1]    2    1    1    1    1    1    1    1    3    2    5    4    2    1    1    1    1    1    2
# [20]    1    2    1   39   38    9    8   44   43    4    3    5    5    3    3    8    7    4    3
# [39]    2    1    3    2    1    2    3    2    2    1    2    1    2    1    2    1    3    2    3
# [58]    2    3    2   19   20    9    8    6    5    6    3   25   22    8    7    5    5   45   43
# [77]    2    1   36   35    4    4   29   29    7    6    5    4   16   15    7    6    2    1  542
# [96]  541    2    1   32   31    6    5    1    1  159  158    4    3    3    2    3    2    6    5
# quantile...
#     0%          25%          50%          75%         100% 
# 3.706605e-07 1.449631e-03 2.994446e-03 1.145120e-02 2.819291e+03 



rgn_poly_trunc <- rgn_poly
res_list = c(low = 0.1, med = 0.01, hi = 0.001)
res_list = c(med = 0.01, hi = 0.001)
num_poly <- length(rgn_poly@polygons)
for(k in 1:length(res_list)) { # res = 0.01 
  # res is the resolution for the dp() call
  rgn_poly <- rgn_poly_trunc
  # set working rgn_poly to truncated original rgn_poly
  for(i in 1:num_poly){ # i <- 1
    cat(sprintf('Poly %s out of %s... %s res = %s...\n', i, num_poly, names(res_list[k]), res_list[k]))
    for(j in 1:length(rgn_poly@polygons[[i]]@Polygons)){ # j <- 1
      temp <- as.data.frame(rgn_poly@polygons[[i]]@Polygons[[j]]@coords)
      # data frame of coordinates for subpoly j within poly i
      names(temp) <- c("x", "y")
      temp2 <- dp(temp, res_list[k])
      # this is the function that actually performs the Douglas–Peucker algorithm
      rgn_poly@polygons[[i]]@Polygons[[j]]@coords <- as.matrix(cbind(temp2$x, temp2$y))
      # put the simplified coordinate set values into the place of the original coordinate sets
    }
  }
  output_layer <- sprintf('regions_gcs_trunc_%s_res', names(res_list[k]))
  cat(sprintf('Writing %s to %s  ', output_layer, dir_git))
  writePolyShape(rgn_poly, file.path(dir_git, output_layer))
  #writeOGR(rgn_poly, dsn = dir_git, layer = output_layer, driver = 'ESRI Shapefile', overwrite_layer = TRUE, verbose = TRUE)
  cat(sprintf('Shapefile size: %.3f MB\n', 
              1e-6*(file.size(file.path(dir_git, paste(output_layer, '.shp', sep = ''))))))
}

