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
dir_git  <- file.path('~/github/ohiprep/spatial/downres')


rgn_poly      <- readShapePoly(poly_layer)

rgn_poly_orig <- rgn_poly 
# original .shp: 308 MB

# examine structure of region polygons
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
rgn_poly_trunc <- rgn_poly

num_poly <- length(rgn_poly@polygons)
for(res in c(0.1, 0.01, 0.001)) { # res = 0.01 
  # res is the resolution for the dp() call
  rgn_poly <- rgn_poly_trunc
    # set working rgn_poly to truncated original rgn_poly
  for(i in 1:num_poly){ # i <- 1
    cat(sprintf('Poly %s out of %s... res = %s...\n', i, num_poly, res))
    for(j in 1:length(rgn_poly@polygons[[i]]@Polygons)){ # j <- 1
      temp <- as.data.frame(rgn_poly@polygons[[i]]@Polygons[[j]]@coords)
        # data frame of coordinates for subpoly j within poly i
      names(temp) <- c("x", "y")
      temp2 <- dp(temp, res)
        # this is the function that actually performs the Douglas–Peucker algorithm
      rgn_poly@polygons[[i]]@Polygons[[j]]@coords <- as.matrix(cbind(temp2$x, temp2$y))
        # put the simplified coordinate set values into the place of the original coordinate sets
    }
  }
  output_file <- file.path(dir_git, sprintf('regions_gcs_res%s', res))
  cat(sprintf('Writing %s...  ', output_file))
  write.shapefile(rgn_poly, output_file)
  cat(sprintf('Shapefile size: %.3f\n', 1e-6*(file.size(paste(output_file, '.shp', sep = '')))))
}

