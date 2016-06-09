###############################################
## Download the fisheries data associated with each 
## ocean-based SAUP cell
## MRF: June 6 2016
###############################################

## without cores 31,000 in ~12 hours, 
## total of ~180,000 cells to get data for
## Would take about 6 days per year
## that will not work

source('src/R/common.R')

install.packages("devtools")
devtools::install_github("SeaAroundUs/rseaaroundus")

library(dplyr)
library(seaaroundus)
library(foreach)
library(doParallel)
library(parallel)


data <- read.csv(file.path(dir_M, 
                           "git-annex/globalprep/fis/v2015/raw/saup_rasters_to_ohi_rgns_noLand.csv"))
unique_saup_cells <- unique(data$saup_cell_id)


numCores <- detectCores()
cl <- makeCluster(numCores)
registerDoParallel(cl)

inputs <- unique_saup_cells[10001:20000]

year <- 2010


processInput <- function(i) { # i = 1000
  data <- seaaroundus::getcelldata(year=year, i)
  write.csv(data, file.path(dir_M, "git-annex/globalprep/fis/v2015/raw/saup_cell_data", 
                            sprintf("cell_%s_year_%s.csv", i, year)), row.names=FALSE)
}

## 100 took 175 seconds
## 180000*1.75/60/60/24   # estimated days it will take 

ptm <- proc.time()
results <- foreach(i=inputs) %dopar% {
  processInput(i)
}
proc.time() - ptm


### some cells are being skipped...evaluate at end and get these data
tmp <- list.files(file.path(dir_M, "git-annex/globalprep/fis/v2015/raw/saup_cell_data"))
tmp <- data.frame(filename = tmp)
tmp <- separate(tmp, filename, c("cell", "cell_id", "year", "year_id"), sep="_")
setdiff(inputs, tmp$cell_id)

getcelldata(year=2010, cell=204129)
###################################
## Exploring!
###################################
# without parallelizing
ptm <- proc.time()
for(cell in inputs){ # cell = 142395 
  data <- getcelldata(year=year, cell)
  write.csv(data, file.path(dir_M, "git-annex/globalprep/fis/v2015/raw/saup_cell_data", 
                            sprintf("cell_%s_year_%s.csv", cell, year)), row.names=FALSE)
}
proc.time() - ptm
# elapsed = 762 seconds
180000*7.6/60/60/24


# without parallelizing, and getting 100 cells all at once
# oops! error in that function only returns the first year!
ptm <- proc.time()
  data <- getcelldata(year=year, as.list(inputs))
  write.csv(data, file.path(dir_M, "git-annex/globalprep/fis/v2015/raw/saup_cell_data/allData.csv"), row.names=FALSE)
proc.time() - ptm
