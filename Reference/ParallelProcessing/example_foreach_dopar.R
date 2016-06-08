# This script shows how to parallelize a for loop using as limited modifications as possible
# Compared to the original code.
# Comment: the functions were built for  comparison purpose. In general, I would recommend
# to build the loop so it calls the function (not adding the loop in the function),ie:
# foreach(m = mths) %dopar%{
# my_function(m,l)
# }
#
# Julien Brun
#### LIBRARIES ####
source('~/github/ohiprep/src/R/common.R')
library(raster)
library(ncdf4)
library(dplyr)
library(ggplot2)

# Multiprocessing
library(doParallel)
library(foreach)

#### CONSTANTS ####
## Multiprocessing cores
# best to leave empty arguments; by default, the number of cores used for parallel
# execution is 1/2 the number of detected cores (if number is unspecified)
# You can also detect the number of Cores: ?detectCores

registerDoParallel(12)


## Paths
raw_data_dir = file.path(dir_M,'git-annex/globalprep/_raw_data/NASA_OMI_AURA_UV/d2016')

#years of data we are using for this data layer

yrs <- c("2011", "2012")
mths <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

#list all .nc files from raw data folder
list = list.files(file.path(raw_data_dir),pattern='*.nc',full.names=T)

#subset list to get those files in our years
l <- list[substr(list[],91,94) %in% yrs]

#### FUNCTIONS ####

# Original loop (simplified)
the_stack_averager_sder <- function(my_filelist){
  #Function to create monthly mean and standard deviation raster for each month
  for (m in mths){
      s <- stack()
      k <- my_filelist[substr(my_filelist[],96,97)==m]
      
      for (j in 1:length(k)){
     
        #skip the .nc files that are corrupt from the data source
        r <- try(raster(k[j]))
        # Add to stack
        s <- stack(s,r)
      }
      
      month_mean<- calc(s,fun=function(x){mean(x,na.rm=T)},filename=paste0('~/github/ohiprep/Reference/ParallelProcessing/test/monthly_mean_',m,'_',j,'.tif'),overwrite=T)
      }
  }
# Same with foreach dopar

the_stack_averager_sder_par <- function(my_filelist){
  
    #Function to create monthly mean and standard deviation raster for each month
    foreach(m = mths) %dopar%{
      
      s <- stack()
      k <- my_filelist[substr(my_filelist[],96,97)==m]
      
          for (j in 1:length(k)){

          #skip the .nc files that are corrupt from the data source
          r <- try(raster(k[j]))
          # Add to stack
          s <- stack(s,r)
          }
      
    month_mean <- calc(s,fun=function(x){mean(x,na.rm=T)},filename=paste0(file.path(dir_M,'git-annex/globalprep/fis/test/monthly_mean_'),m,'_',j,'.tif'),overwrite=T)
    }
}
# For loop
#system.time(the_stack_averager_sder(l))
system.time(the_stack_averager_sder_par(l))
