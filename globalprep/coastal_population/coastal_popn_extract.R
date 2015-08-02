### coastal_popn_extract.R
##############################################################################=
### Jul2015: Casey O'Hara
###
### Extract coastal population count (25 miles inland) for regions, from 2.5 arc-minute
### resolution GPW Population Count Grid Future Estimates (v3) raster data set.
### Input files:     Neptune: git-annex/globalprep/coastal_population/pop_count_data
### Output files: 
###   Cell-by-cell:  Neptune: git-annex/globalprep/coastal_population/v2015/int
###   Region sums:   github:    ohiprep/globalprep/coastal_population/v2015/int
###
### From OHI 2013 supplement:
###   Description: Coastal population, defined as the total population inland of 25 miles, was based on
###   the Gridded Population of the World (GPW) Population Density Grid Future Estimates, v3. These
###   data were accessed from the Center for International Earth Science Information Network
###   (CIESIN)/Columbia University (CIESIN & CIAT 2005). Rasters at 2.5 arc-minute resolution
###   globally available for 2005, 2010, and to 2015 in units of population density (# people per square
###   kilometer). ....
###
### NOTE: For 2015, changing the methodology.
### Using the CIESEN & CIAT Gridded Population of the World (GPW) Population Count Grid Future 
### Estimates, v3 (rather than Population Density).  These data were accessed from:
###    http://sedac.ciesin.columbia.edu/data/set/gpw-v3-population-count-future-estimates
### Rasters at 2.5 arc-minute resolution globally available for 2005, 2010, and to 2015 
### in units of population count (# people per 2.5 arc-minute pixel). 
### * Using raster::extract() to overlay the 25 mile inland region (rgn_inland25mi_gcs) over
###   the population count grid, we can extract the population count for all grid cells within
###   each global region.  
### * Summing these provides a count of coastal population per region.
### * raster::extract() provides information on cell proportion within buffer boundary.
###   But the entire population of a coastal cell can be attributed to that cell, while
###   the population of a cell on a border between two regions should be divided between
###   the two (likewise for cells split by the inland boundary).  These are not
###   currently accounted for with a simple sum.

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

### Locations of ASCII raster files for 2005/2010/2015 populations at 15 arc-minute resolution
# pop_raster_files <- c(pop_2005 = file.path(dir_anx, 'pop_count_data', 'glp05ag15.asc'),
#                       pop_2010 = file.path(dir_anx, 'pop_count_data', 'glp10ag15.asc'),
#                       pop_2015 = file.path(dir_anx, 'pop_count_data', 'glp15ag15.asc'))

### Locations of ASCII raster files for 2005/2010/2015 populations at 2.5 arc-minute resolution
pop_raster_files <- c(pop_2005_25 = file.path(dir_anx, 'pop_count_data', 'glp05ag.asc'),
                      pop_2010_25 = file.path(dir_anx, 'pop_count_data', 'glp10ag.asc'),
                      pop_2015_25 = file.path(dir_anx, 'pop_count_data', 'glp15ag.asc'))

# ### Locations of ASCII raster files for 2005/2010/2015 population density at 2.5 arc-minute resolution
# pop_dens_raster_files <- c(pop_dens_2005_25 = file.path(dir_anx, 'pop_density_data', 'glfedens05/glds05ag/w001001.adf'),
#                            pop_dens_2010_25 = file.path(dir_anx, 'pop_density_data', 'glfedens10/glds10ag/w001001.adf'),
#                            pop_dens_2015_25 = file.path(dir_anx, 'pop_density_data', 'glfedens15/glds15ag/w001001.adf'))

### location of region vector data - the folder, not the file.
OGR_location    <- file.path(dir_neptune_data, 'git-annex/Global/NCEAS-Regions_v2014/data/')

buffer <- '25mi'

### readOGR looks in OGR_location for the set of files indicated by layer (without extension)
cat(sprintf('Reading regions shape file - come back in about 4 minutes.\n  %s\n', OGR_location))
regions        <- readOGR(dsn = OGR_location, layer = sprintf('rgn_inland%s_gcs', buffer))

rgn_types   <- 'land' #  for ocean analyses, use: c('eez', 'eez-disputed', 'eez-inland')
regions     <- regions[regions@data$rgn_type %in% rgn_types, ]

for (i in 1:length(pop_raster_files)) { # i = 3
  ### create a raster object from the ascii raster file
  pop_sum_fname <- file.path(dir_int, sprintf('%s_sum_%s.csv', names(pop_raster_files)[i], buffer))
  if(!file.exists(pop_sum_fname)) {
    cat(sprintf('\nCreating raster object from %s data:\n  %s\n', names(pop_raster_files)[i], pop_raster_files[i]))
    pop_raster  <- raster(pop_raster_files[i], crs = '+proj=longlat +datum=WGS84')
    
    ### Extracting region info for population raster
    cat(sprintf('Extracting region info for %s data:\n  %s\n', names(pop_raster_files)[i], pop_raster_files[i]))
    rgn_pop_ex <- raster::extract(pop_raster,  regions, weights = TRUE, normalizeWeights = FALSE, progress = 'text') 
    
    
    ### Create region name & id dataframe based on regions polygon. Combine rgn_name and rgn_id
    ### into one identifier... to be split later.
    rgn_id_name <- data.frame(regions@data$rgn_id, regions@data$rgn_name) %>%
      unite(combo, regions.data.rgn_id, regions.data.rgn_name, sep = '_')
    names(rgn_pop_ex) <- rgn_id_name$combo
    
    ### turn list elements into dataframe rows
    rgn_pop <- plyr::ldply(rgn_pop_ex, rbind) # ??? still a plyr function.
    
    ### fix dataframe variable names - split the rgn identifier
    rgn_pop <- rgn_pop %>%
      separate(.id, c('rgn_id', 'rgn_name'), sep = '_') %>%
      rename(pop = value, 
             proportionArea = weight) %>%
      mutate(rgn_id = as.integer(rgn_id))
    rgn_pop_sum <- rgn_pop %>%
      group_by(rgn_id, rgn_name) %>%
      summarize(pop_total = round(sum(pop, na.rm = TRUE)))
    
    ### write output files: summary to github, cell-by-cell to git-annex for future possible reference?
    cat(sprintf('Writing summarized region/pop info for %s data:\n  %s\n', names(pop_raster_files)[i], pop_sum_fname))
    write.csv(rgn_pop_sum, pop_sum_fname, row.names = FALSE)
    
    pop_cell_fname <- file.path(dir_anx, scenario, sprintf('int/%s_cells_%s.csv', names(pop_raster_files)[i], buffer))
    cat(sprintf('Writing cell-by-cell region/pop info for %s data:\n  %s\n', names(pop_raster_files)[i], pop_cell_fname))
    write.csv(rgn_pop, pop_cell_fname, row.names = FALSE)
  } else {
    cat(sprintf('File already exists: region/pop info for %s data:\n  %s\n', names(pop_raster_files)[i], pop_sum_fname))
  }
}


