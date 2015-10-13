### downres_polygons.R
### 20150914 - Casey O'Hara

### Simplifies geometry of polygons to create smaller shapefiles; the main
### idea is to use the simplified shapefiles exclusively for plotting
### global maps quickly at a scale where the simplified geometry is
### acceptable.


### From: http://stackoverflow.com/questions/20976449/r-simplify-shapefile

### There is still a lot of detail in the edges of the main polygons that is unecessary 
### for my requirements. This detail can be simplified using the dp() function in the 
### shapefiles package. Once again, we create a loop that goes through the object 
### simplifying each of the polygons within it. dp() works only on dataframes, so we 
### need to break each @coords matrix into a dataframe, run dp(), then convert it back 
### into a matrix.

### Note: can also cut smaller polygons if necessary... 
### Note: the dp() function in the shapefiles package looks similar to the
###   gSimplify() function in the rgeos package... didn't try that one out.

setwd('~/github/ohiprep')
source('src/R/common.R')

library(sp)
library(rgdal)
library(maptools)
library(shapefiles)

#proj_type = 'gcs'
proj_type = 'mol'

dir_rgn  <- file.path(dir_neptune_data, 'git-annex/globalprep/spatial/v2015/data')
poly_layer <- file.path(dir_rgn, sprintf('regions_%s', proj_type)) 
dir_git  <- file.path(getwd(), 'globalprep/spatial/downres')
   # writeOGR dsn needs to be an absolute path? apparently ~ causes issues.


rgn_poly <- readShapePoly(poly_layer) 
# original .shp: 308 MB

### examine structure of region polygons ----
    rgn_poly_orig <- rgn_poly # create safety copy  # rgn_poly <- rgn_poly_orig
    str(rgn_poly, max.level = 3)
    
    sapply(rgn_poly@polygons, function(x) length(x@Polygons))
    # first 100-ish polygons displayed; these are the number of sub-polygons within the main polygon.
    #  [1]    22    21     2     1     3     3    55    57   193   196    19    18    23    23     3     2    26    26     2
    # [20]     1    33    32   945   944  1898  1897    88    87  2315  2314     8     7   155   155    54    54   233   232
    # [39]    55    54    25    24     3     2     3     2    20    19     3     2    10     9     4     3     2     1    59
    # [58]    58    38    37    66    65   577   578    39    38    64    63   183   180   234   231   133   133    28    29
    # [77]  1524  1523     5     4   195   197   219   219   365   368   155   154    77    76    33    32  1242  1241     2
    # [96]     1 18222 18222    16    15  1577  1576    71    70     2     1  3404  3403     8     7    19    18    14    13
    
### Exclude land polygons
rgn_mar <- rgn_poly[!(rgn_poly@data$rgn_typ %in% c('land', 'land-disputed', 'land-noeez')), ]
rgn_mar_orig <- rgn_mar

    sapply(rgn_mar@polygons, function(x) length(x@Polygons))
    #   [1]    22     2     3    55   193    19    23     3    26     2    33   945  1898    88  2315     8   155    54   233    55    25     3     3    20     3    10     4     2    59
    #  [30]    38    66   577    39    64   183   234   133    28  1524     5   195   219   365   155    77    33  1242     2 18222    16  1577    71     2  3404     8    19    14   100
    #  [59]     2   173    56    34     3     5  5880    21   962 11606   357     8    48    40     1    44  1513  1876   394    56 16525   230    95    33    21   671   462  2082   651
    #  [88]   675   108  1266     1    14   748     1     7    71    53   201     1     3   111    56    21    79    85    59    20     2  1874    31     1   175   637    82  1858   589
    # [117]  1693    36  3759   338  2485     1   150   759    98     1  6840    10 28870     7    24     3    25 10011 11216  8042     3   114    81    43     4     2     1     1    12
    # [146]    77     1     1     1     3     4    13     2    36     3     1     2    16    11     2   781     6     2     1     1    13     6     1     1     1     1     1     2     1
    # [175]     2   193     1     2     1     3     3     2     3     3     2    84     2    13     1    17     7     5     3    22    12    26    17    28    17     5   107     3     2
    # [204]     9     3     2    59    63   901    11   146   306   524   123   298   349    20   188    77   147   411   925    27    44    77   232    40    37    31    41     2   157
    # [233]     1   112    22     9    45    13    45    40    15     6  1380   510    21    39  4758     1    20   249   443    16    82     8   115  1621    25    27   141     3     4
    # [262]    16    99   458     9    22     4   365    13   503   115    32     1     2

### filter out tiny sub-polygons -----
### create area list for filtering.  Area will be in square degrees... 
area <- lapply(rgn_mar@polygons, function(x) sapply(x@Polygons, function(y) y@area))
    quantile(unlist(area))
    #       0%          25%          50%          75%         100% 
    # 8.307850e-11 5.946673e-06 1.224704e-05 3.964384e-05 2.662491e+03 

### Determine min-max area - the max area within each region is presumably the main component of the EEZ;
### so find the smallest EEZ.
min(unlist(lapply(area, function(x) (max(x)))))
for (i in 1:length(area)) {
  cat(sprintf('rgn %s: largest polygon %.5f (%s)\n', rgn_mar@data$rgn_id[i], max(area[[i]]), rgn_mar@data$rgn_nam[i]))
} 
    
### ditch the small polys;  smallest EEZ region is Bosnia/Herzegovina #232 at 0.00141 so threshold below that.
### (in Mollweide units (meters), 12791438 m^2)
### Generalizing to do either gcs or mol projections, since different units, need different area threshold.
area_threshold <- switch(proj_type,
                       'gcs' = 0.001,
                       'mol' = 1e5)
mainPolys <- lapply(area, function(x) which(x > area_threshold))

for(i in 1:length(mainPolys)){
  if(length(mainPolys[[i]]) >= 1 && mainPolys[[i]][1] >= 1){
    rgn_mar@polygons[[i]]@Polygons <- rgn_mar@polygons[[i]]@Polygons[mainPolys[[i]]]
    rgn_mar@polygons[[i]]@plotOrder <- 1:length(rgn_mar@polygons[[i]]@Polygons)
  }
}

rgn_mar@plotOrder <- 1:length(rgn_mar@polygons)
# is plotOrder necessary to be continuous?  that would explain this line.

table(rgn_mar@data$rgn_typ)
# eez                eez-disputed    eez-inland           fao          land land-disputed    land-noeez 
# 239                          15             5            15             0             0             0 
table(rgn_mar@data$ant_typ)
# eez    eez-ccamlr  eez-disputed    eez-inland           fao          land   land-ccamlr land-disputed    land-noeez 
# 220            19            15             5            15             0             0             0             0 


### The threshold for resolution in the dp() function:
### Appears to be basically in same units of shapefile - the minimum deviation of a point between two
### other points, to be saved rather than deleted in simplifying.
res_list <- switch(proj_type,
                   'gcs' = c(low = 0.1,   med = .005),
                   'mol' = c(low = 10000, med = 500))


num_poly <- length(rgn_mar@polygons)
for(k in 1:length(res_list)) { # k = 2
  # res is the resolution for the dp() call
  rgn_mar_temp <- rgn_mar
  # set working rgn_mar_temp to original rgn_mar
  for(i in 1:num_poly){ # i <- 1
    cat(sprintf('Poly %s out of %s... %s res = %s...\n', i, num_poly, names(res_list[k]), res_list[k]))
    for(j in 1:length(rgn_mar_temp@polygons[[i]]@Polygons)){ # j <- 1
      temp <- as.data.frame(rgn_mar_temp@polygons[[i]]@Polygons[[j]]@coords)
      # data frame of coordinates for subpoly j within poly i
      names(temp) <- c("x", "y")
      temp2 <- dp(temp, res_list[k])
      # this is the function that actually performs the Douglas–Peucker algorithm
      rgn_mar_temp@polygons[[i]]@Polygons[[j]]@coords <- as.matrix(cbind(temp2$x, temp2$y))
      # put the simplified coordinate set values into the place of the original coordinate sets
    }
  }
  
  area <- lapply(rgn_mar_temp@polygons, function(x) sapply(x@Polygons, function(y) y@area))
  print(quantile(unlist(area)))
  
  rgn_mar_temp@plotOrder <- 1:length(rgn_mar@polygons)

  rgn_eez   <- rgn_mar_temp[str_detect(rgn_mar_temp@data$rgn_typ, 'eez') & !str_detect(rgn_mar_temp@data$ant_typ, 'ccamlr'), ]
  rgn_eez@plotOrder <- 1:length(rgn_eez@polygons)
  # includes eez-disputed and eez-inland, but no FAO or CCAMLR regions

  rgn_ant   <- rgn_mar_temp[rgn_mar_temp@data$ant_typ == 'eez-ccamlr', ]
  rgn_ant@plotOrder <- 1:length(rgn_ant@polygons)
  rgn_fao   <- rgn_mar_temp[rgn_mar_temp@data$rgn_typ == 'fao', ]
  rgn_fao@plotOrder <- 1:length(rgn_fao@polygons)
  
  output_all <- sprintf('rgn_all_%s_%s_res', proj_type, names(res_list[k]))
  cat(sprintf('Writing %s to %s  ', output_all, dir_git))
  writeOGR(rgn_mar_temp, dsn = dir_git, layer = output_all, driver = 'ESRI Shapefile', overwrite_layer = TRUE, morphToESRI = TRUE)
  cat(sprintf('Shapefile size: %.3f MB\n', 1e-6*(file.size(file.path(dir_git, paste(output_all, '.shp', sep = ''))))))

  output_eez <- sprintf('rgn_eez_%s_%s_res', proj_type, names(res_list[k]))
  cat(sprintf('Writing %s to %s  ', output_eez, dir_git))
  writeOGR(rgn_eez, dsn = dir_git, layer = output_eez, driver = 'ESRI Shapefile', overwrite_layer = TRUE, morphToESRI = TRUE)
  cat(sprintf('Shapefile size: %.3f MB\n', 1e-6*(file.size(file.path(dir_git, paste(output_eez, '.shp', sep = ''))))))
  
  output_ant <- sprintf('rgn_ant_%s_%s_res', proj_type, names(res_list[k]))
  cat(sprintf('Writing %s to %s  ', output_ant, dir_git))
  writeOGR(rgn_ant, dsn = dir_git, layer = output_ant, driver = 'ESRI Shapefile', overwrite_layer = TRUE, morphToESRI = TRUE)
  cat(sprintf('Shapefile size: %.3f MB\n', 1e-6*(file.size(file.path(dir_git, paste(output_ant, '.shp', sep = ''))))))
  
  output_fao <- sprintf('rgn_fao_%s_%s_res', proj_type, names(res_list[k]))
  cat(sprintf('Writing %s to %s  ', output_fao, dir_git))
  writeOGR(rgn_fao, dsn = dir_git, layer = output_fao, driver = 'ESRI Shapefile', overwrite_layer = TRUE, morphToESRI = TRUE)
  cat(sprintf('Shapefile size: %.3f MB\n', 1e-6*(file.size(file.path(dir_git, paste(output_fao, '.shp', sep = ''))))))
}

#   rgn_all_gcs_low_res       Shapefile size: 0.700 MB
#   rgn_eez_gcs_low_res       Shapefile size: 0.635 MB
#   rgn_ant_gcs_low_res       Shapefile size: 0.031 MB
#   rgn_fao_gcs_low_res       Shapefile size: 0.034 MB

### Time the reading and plotting speeds
res_list2 <- c('low_res', 'verylow_res', 'ultralow_res')
type_list <- c('all', 'eez', 'fao', 'ant')
for (i in res_list2) { # i = 'low_res'
  for (j in type_list) {
    fn <- sprintf('rgn_%s_%s_%s', j, proj_type, i)
    poly_layer <- file.path(dir_git, fn)
    cat(sprintf('Reading %s file: \n  %s\n', fn, poly_layer))
    ptm <- proc.time()
    rgns <- readShapePoly(poly_layer)
    print((proc.time() - ptm)[3])
    
    cat(sprintf('Plotting %s map\n', fn))
    ptm <- proc.time()
    plot(rgns, border = 'blue', col = 'cyan', add = FALSE)
    map('world', add = TRUE)
    print((proc.time() - ptm)[3])
  }
}

# Reading/Plotting rgn_all_gcs_low_res file: 
# elapsed: 0.416 / 0.975 

### create land layer in Mollweide projection

proj_type  <- 'mol'
poly_layer <- file.path(dir_rgn, sprintf('regions_%s', proj_type))

rgn_poly <- readShapePoly(poly_layer) 

rgn_terr <- rgn_poly[(rgn_poly@data$rgn_typ %in% c('land', 'land-disputed', 'land-noeez')), ]

area_threshold <- switch(proj_type,
                         'gcs' = 0.001,
                         'mol' = 1e5)
mainPolys <- lapply(area, function(x) which(x > area_threshold))

for(i in 1:length(mainPolys)){
  if(length(mainPolys[[i]]) >= 1 && mainPolys[[i]][1] >= 1){
    rgn_terr@polygons[[i]]@Polygons  <- rgn_terr@polygons[[i]]@Polygons[mainPolys[[i]]]
    rgn_terr@polygons[[i]]@plotOrder <- 1:length(rgn_terr@polygons[[i]]@Polygons)
  }
}

rgn_terr@plotOrder <- 1:length(rgn_terr@polygons)
# is plotOrder necessary to be continuous?  that would explain this line.

table(rgn_terr@data$rgn_typ)
# eez                eez-disputed    eez-inland           fao          land land-disputed    land-noeez 
# 239                          15             5            15             0             0             0 
table(rgn_terr@data$ant_typ)
# eez    eez-ccamlr  eez-disputed    eez-inland           fao          land   land-ccamlr land-disputed    land-noeez 
# 220            19            15             5            15             0             0             0             0 


### The threshold for resolution in the dp() function:
### Appears to be basically in same units of shapefile - the minimum deviation of a point between two
### other points, to be saved rather than deleted in simplifying.
res_list <- switch(proj_type,
                   'gcs' = c(low = 0.1),
                   'mol' = c(low = 10000))


num_poly <- length(rgn_terr@polygons)
for(k in 1:length(res_list)) { # k = 1
  # res is the resolution for the dp() call
  rgn_terr_temp <- rgn_terr
  # set working rgn_terr_temp to original rgn_terr
  for(i in 1:num_poly){ # i <- 1
    cat(sprintf('Poly %s out of %s... %s res = %s...\n', i, num_poly, names(res_list[k]), res_list[k]))
    for(j in 1:length(rgn_terr_temp@polygons[[i]]@Polygons)){ # j <- 1
      temp <- as.data.frame(rgn_terr_temp@polygons[[i]]@Polygons[[j]]@coords)
      # data frame of coordinates for subpoly j within poly i
      names(temp) <- c("x", "y")
      temp2 <- dp(temp, res_list[k])
      # this is the function that actually performs the Douglas–Peucker algorithm
      rgn_terr_temp@polygons[[i]]@Polygons[[j]]@coords <- as.matrix(cbind(temp2$x, temp2$y))
      # put the simplified coordinate set values into the place of the original coordinate sets
    }
  }
  
  area <- lapply(rgn_terr_temp@polygons, function(x) sapply(x@Polygons, function(y) y@area))
  print(quantile(unlist(area)))
  
  rgn_terr_temp@plotOrder <- 1:length(rgn_mar@polygons)
  
  output_all <- sprintf('rgn_land_%s_%s_res', proj_type, names(res_list[k]))
  cat(sprintf('Writing %s to %s  ', output_all, dir_git))
  writeOGR(rgn_terr_temp, dsn = dir_git, layer = output_all, driver = 'ESRI Shapefile', overwrite_layer = TRUE, morphToESRI = TRUE)
  cat(sprintf('Shapefile size: %.3f MB\n', 1e-6*(file.size(file.path(dir_git, paste(output_all, '.shp', sep = ''))))))
}
