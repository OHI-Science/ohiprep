
##############################################################################=
### setup -----
##############################################################################=

library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
library(rgdal)
library(raster) 

setwd('~/github/ohiprep')
source('src/R/common.R')

goal     <- 'globalprep/coastal_population'
scenario <- 'v2015'
dir_git  <- file.path('~/github/ohiprep', goal)
dir_int  <- file.path(dir_git, scenario, 'int')
dir_data <- file.path(dir_git, scenario, 'data')
dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal)


##############################################################################=
### extract population raster info within region polygons -----
##############################################################################=

### Locations of ASCII raster files for 2005/2010/2015 populations at 2.5 arc-minute resolution
pop_raster_files <- c(pop_2005 = file.path(dir_anx, 'pop_count_data', 'glp05ag.asc'),
                      pop_2010 = file.path(dir_anx, 'pop_count_data', 'glp10ag.asc'),
                      pop_2015 = file.path(dir_anx, 'pop_count_data', 'glp15ag.asc'))
### location of region vector data - the folder, not the file.
OGR_location    <- file.path(dir_neptune_data, 'git-annex/Global/NCEAS-Regions_v2014/data/')



### readOGR looks in OGR_location for the set of files indicated by layer (without extension)
cat(sprintf('Reading regions shape file - come back in about 4 minutes.\n  %s\n', OGR_location))
regions        <- readOGR(dsn = OGR_location, layer = 'rgn_inland25mi_gcs')

rgn_types   <- 'land' #  for ocean analyses, use: c('eez', 'eez-disputed', 'eez-inland')
regions     <- regions[regions@data$rgn_type %in% rgn_types, ]

for (i in 1:length(pop_raster_files)) {
  ### create a raster object from the ascii raster file
  cat(sprintf('\nCreating raster object from %s:\n  %s\n', names(pop_raster_files)[i], pop_raster_files[i]))
  pop_raster  <- raster(pop_r_file, crs = '+proj=longlat +datum=WGS84')
  
  ### Extracting region info for population raster
  cat(sprintf('Extracting region info for %s:\n  %s\n', names(pop_raster_files)[i], pop_raster_files[i]))
  rgn_pop     <- raster::extract(pop_raster,  regions, weights = TRUE, normalizeWeights = FALSE, progress = 'text') 
  
  f_name <- sprintf('%s.csv', names(pop_raster_files)[i])
  write.csv(rgn_pop, file.path(dir_int, sprintf('%s.csv', names(pop_raster_files)[i])), row.names = FALSE)
}
