
source('~/github/ohiprep/src/R/common.R')

library(seaaroundus)
library(raster)
library(tidyverse)


#get all unique cell ids 
saup_cells <- getcells("POLYGON ((-180 90, 180 90, 180 -90, -180 -90, -180 90))")

#put cell ids into batches of length 200
batches <- seq(1, length(saup_cells), 200)

#for every year, grab the data for each cell and save it to an RDS file on the Mazu server
for(yr in 1950:2013){ #yr=2014
  
  out <- list()
  
  pb <- utils::txtProgressBar(min = 0, max = length(batches), initial = 0, style = 3)
  for (i in seq_along(batches)){
    utils::setTxtProgressBar(pb, i)
    print(i)
    start <- batches[i]
    end <- start + 199
    
    batchn <- saup_cells[start:end]
    
    out[[i]] <- getcelldata(year=yr, batchn)
    Sys.sleep(1)
  }
  close(pb)
  
  df <- bind_rows(out)
  
  saveRDS(df, paste0(file.path(dir_M),"/git-annex/globalprep/_raw_data/SAUP/d2017/annual_data/saup_data_",yr,".rds"))
  
}