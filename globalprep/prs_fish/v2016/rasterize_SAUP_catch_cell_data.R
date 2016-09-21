library(readr)
library(data.table)
library(seaaroundus)
library(raster)
library(rgdal)

source('~/github/ohiprep/src/R/common.R')

# Path to data
path_data               = file.path(dir_M,"git-annex/globalprep/_raw_data/SAUP/d2016/Data")
file_allocation_data    = "SeaAroundUs/AllocationData.dat"
file_allocation_results = "SeaAroundUs/AllocationResult.dat"
file_taxon              = "SeaAroundUs/taxon.dat"
file_entity             = "FishingEntity.dat"

# load the allocation info
dt_data           <- fread(file.path(path_data,file_allocation_data), sep=";", header = FALSE)
colnames(dt_data) <- c("UniversalDataID","DataLayerID","FishingEntityID", "Year", "TaxonKey",
                       "InputTypeID", "sector_type_name", "catch_type_name", 
                       "reporting_status_name")


#load the Results data
dt_results           <- fread(file.path(path_data,file_allocation_results), sep=";", header = FALSE)
colnames(dt_results) <- c("UniversalDataID","CellID","AllocatedCatch")

yearlist = unique(dt_data$Year);
#yearlist = c(1979);

#create total catch rasters per year

for(year in yearlist){
  
  print(paste("working on year ", year));
  yearly_data <- dt_data[Year==year,]
  
  #subset the data for 10 unique UniversalDataID
  udi <- unique(yearly_data$UniversalDataID)
  
  #subset the allocation, results
  alloc_sub   <- yearly_data[UniversalDataID %in% udi]
  results_sub <- dt_results[UniversalDataID %in% udi]
  setkey(alloc_sub,UniversalDataID)
  setkey(results_sub,UniversalDataID)
  
  
  j <- results_sub[alloc_sub]
  
  #get template raster for SAUP data with cell values equal to CellID
  
  saup_cells <- getcells("POLYGON ((-180 90,-180 -90, 180 -90, 180 90, -180 90))")
  saup_rast <- raster(ncol=720, nrow=360)
  saup_rast[] <- saup_cells
  
  #groupby/summarize
  grouped_data <- group_by(j, CellID);
  summed_data <- summarise(grouped_data, val = sum(AllocatedCatch));
  out_rast <- raster::subs(saup_rast, summed_data, subsWithNA=TRUE);
  out_dir = "yearly_saup_rasters";
  if(!dir.exists(out_dir)){
    dir.create(out_dir);
  }
  out_name = file.path(out_dir,paste(year,"catch.tif",sep="_"));
  writeRaster(out_rast, out_name,format="GTiff", overwrite=TRUE);
  plot(out_rast);
}

#create artisanal catch only raster layers

for(year in yearlist){
  
  print(paste("working on year ", year));
  yearly_data <- dt_data[Year==year & sector_type_name != "Industrial",]
  
  #subset the data for 10 unique UniversalDataID
  udi <- unique(yearly_data$UniversalDataID)
  
  #subset the allocation, results
  alloc_sub   <- yearly_data[UniversalDataID %in% udi]
  results_sub <- dt_results[UniversalDataID %in% udi]
  setkey(alloc_sub,UniversalDataID)
  setkey(results_sub,UniversalDataID)
  
  j <- results_sub[alloc_sub]
  
  #get template raster for SAUP data with cell values equal to CellID
  
  saup_cells <- getcells("POLYGON ((-180 90,-180 -90, 180 -90, 180 90, -180 90))")
  saup_rast  <- raster(ncol=720, nrow=360)
  saup_rast[] <- saup_cells
  
  #groupby/summarize
  grouped_data <- group_by(j, CellID);
  summed_data  <- summarise(grouped_data, val = sum(AllocatedCatch));
  out_rast     <- raster::subs(saup_rast, summed_data, subsWithNA=TRUE);
  out_dir = "int/annual_artisanal_rasters";
  if(!dir.exists(out_dir)){
    dir.create(out_dir);
  }
  out_name = file.path(out_dir,paste(year,"catch.tif",sep="_"));
  writeRaster(out_rast, out_name,format="GTiff", overwrite=TRUE);
  plot(out_rast);
}

#create industrial catch only layers

for(year in yearlist){
  
  print(paste("working on year ", year));
  yearly_data <- dt_data[Year==year & sector_type_name == 'Industrial',]
  
  #subset the data for 10 unique UniversalDataID
  udi <- unique(yearly_data$UniversalDataID)
  
  #subset the allocation, results
  alloc_sub   <- yearly_data[UniversalDataID %in% udi]
  results_sub <- dt_results[UniversalDataID %in% udi]
  setkey(alloc_sub,UniversalDataID)
  setkey(results_sub,UniversalDataID)
  
  
  j <- results_sub[alloc_sub]
  
  #get template raster for SAUP data with cell values equal to CellID
  
  saup_cells <- getcells("POLYGON ((-180 90,-180 -90, 180 -90, 180 90, -180 90))")
  saup_rast <- raster(ncol=720, nrow=360)
  saup_rast[] <- saup_cells
  
  #groupby/summarize
  grouped_data <- group_by(j, CellID);
  summed_data <- summarise(grouped_data, val = sum(AllocatedCatch));
  out_rast <- raster::subs(saup_rast, summed_data, subsWithNA=TRUE);
  out_dir = "int/annual_industrial_rasters";
  if(!dir.exists(out_dir)){
    dir.create(out_dir);
  }
  out_name = file.path(out_dir,paste(year,"catch.tif",sep="_"));
  writeRaster(out_rast, out_name,format="GTiff", overwrite=TRUE);
  plot(out_rast);
}

