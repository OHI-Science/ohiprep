#### Direct human impact

source('../ohiprep/src/R/common.R')

library(raster)
library(rgdal)

library(dplyr)
library(stringr)

library(parallel)
library(foreach)
library(doParallel)

### sample raster for projecting
ocean = raster(file.path(dir_M,'git-annex/globalprep/spatial/ocean.tif'))

### reproject and resample density data
raw <- list.files(file.path(dir_M, "git-annex/globalprep/_raw_data/CIESEandCIAT_population/d2017"), 
                  full.names = TRUE, pattern = "\\.tif$",
                  recursive = TRUE)
raw <- raw[grep("density", raw)]


for(rast in raw){ #rast = raw[1]
  
  yr <- as.numeric(as.character(str_sub(rast, -8, -5)))
  
  raster(rast)%>%
    projectRaster(crs = crs(ocean), over=TRUE) %>%
    resample(ocean, method = 'ngb',
    filename = file.path(dir_M, sprintf("git-annex/globalprep/mar_prs_population/v2017/int/human_density_%s_mol.tif", yr)),
    overwrite = TRUE)

  }

# check 
tmp <- raster(file.path(dir_M, 
              "git-annex/globalprep/mar_prs_population/v2017/int/human_density_2000_mol.tif"))
tmp2 <- raster(file.path(dir_M, 
                        "git-annex/globalprep/mar_prs_population/v2017/int/human_density_2000_mol.tif"))

#interpolate missing years

## function to calculate yearly change
files <- list.files(file.path(dir_M, 
                              "git-annex/globalprep/mar_prs_population/v2017/int/"), pattern = "_mol.tif", full = TRUE)

yearly_diff <- function(year_min, year_max, density_files = files){
  rast_diff <- stack(density_files[grep(year_max, density_files)], density_files[grep(year_min, density_files)]) %>%
  overlay(fun = function(x, y){(x - y)/5},
  filename = file.path(dir_M,
            sprintf('git-annex/globalprep/mar_prs_population/v2017/yearly_change_%s_to_%s.tif', year_min, year_max)), 
            overwrite = TRUE)
}

yearly_diff(year_min = 2000, year_max = 2005)
#check
tmp <- raster(file.path(dir_M,
              'git-annex/globalprep/mar_prs_population/v2017/yearly_change_2000_to_2005.tif'))

yearly_diff(year_min = 2005, year_max = 2010)
yearly_diff(year_min = 2010, year_max = 2015)
yearly_diff(year_min = 2015, year_max = 2020)

##########################################
## apply yearly change to inbetween years
##########################################

# function to interpolate between years

yearly_interpolate <- function(year, start_raster, yearly_change){
  
stack(start_raster, yearly_change) %>%
  overlay(fun = function(x, y){(x + y)},
          filename = file.path(dir_M,
                               sprintf('git-annex/globalprep/mar_prs_population/v2017/int/human_density_%s_mol.tif', 
                                       (year + 1))), 
          overwrite = TRUE)

stack(start_raster, yearly_change) %>%
  overlay(fun = function(x, y){(x + 2*y)},
          filename = file.path(dir_M,
                               sprintf('git-annex/globalprep/mar_prs_population/v2017/int/human_density_%s_mol.tif', 
                                       (year + 2))), 
          overwrite = TRUE)

stack(start_raster, yearly_change) %>%
  overlay(fun = function(x, y){(x + 3*y)},
          filename = file.path(dir_M,
                               sprintf('git-annex/globalprep/mar_prs_population/v2017/int/human_density_%s_mol.tif', 
                                       (year + 3))), 
          overwrite = TRUE)

stack(start_raster, yearly_change) %>%
  overlay(fun = function(x, y){(x + 4*y)},
          filename = file.path(dir_M,
                               sprintf('git-annex/globalprep/mar_prs_population/v2017/int/human_density_%s_mol.tif', 
                                       (year + 4))), 
          overwrite = TRUE)
}

# apply function to interpolate between years
files <- list.files(file.path(dir_M, 
                              "git-annex/globalprep/mar_prs_population/v2017/int/"), pattern = ".tif", full = TRUE)

yearly_interpolate(2000, start_raster = files[grep("2000_mol", files)], yearly_change = files[grep("2000_to_2005", files)])
yearly_interpolate(2005, start_raster = files[grep("2005_mol", files)], yearly_change = files[grep("2005_to_2010", files)])
yearly_interpolate(2010, start_raster = files[grep("2010_mol", files)], yearly_change = files[grep("2010_to_2015", files)])
yearly_interpolate(2015, start_raster = files[grep("2015_mol", files)], yearly_change = files[grep("2015_to_2020", files)])

