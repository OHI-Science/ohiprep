#### Example using code to generate b/bmsy values for fisheries catch data
#### Code is for a uniform distribution and no "res" values
#### This was the original model used to calculate the OHI values

rm(list = ls())
library(dplyr)
library(ggplot2)
library(grid)
library(parallel)

source("http://nceas.ucsb.edu/~frazier/myTheme.txt")
source("~/ohiprep/src/R/common.R")

############################################################
## Running CMSY script on handful of species to compare data (July 16 2014)----
## NOTE: if doing this again, the code for the next set of species is
## probably more organized. 
############################################################

cdat <- read.csv("testData.csv", stringsAsFactors=FALSE)

## Run bbmsy script
source('cmsy_uniform.R')

### run CMSY function:
#start the clock
Sys.time()
ptm <- proc.time()

b_bmsy <- data.frame() # create empty dataframe to add values

for(i in 1:length(unique(cdat$stock_id))){  
  #for(i in 1:1){    ##troubleshooting    
  test <- runCMSY(cdat, stockNumber=i)
  new <- data.frame(taxon_name=test[[1]],
                    b_bmsy=test[[2]],
                    year=test[[7]])
  b_bmsy <- rbind(b_bmsy, new)
}
# Stop the clock
proc.time() - ptm
Sys.time()

write.csv(b_bmsy, "my_bbmsy_uniform_Aug6_2014_test.csv", row.names=FALSE)
