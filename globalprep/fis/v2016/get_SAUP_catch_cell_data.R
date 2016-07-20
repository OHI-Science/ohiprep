library(readr)
library(data.table)
library(dplyr)


# Path to data
path_data = "/home/shares/ohi/git-annex/globalprep/_raw_data/SAUP/d2016/Data"
file_allocation_data = "SeaAroundUs/AllocationData.dat"
file_allocation_results = "SeaAroundUs/AllocationResult.dat"
file_taxon = "SeaAroundUs/taxon.dat"
file_entity = "FishingEntity.dat"

# load the allocation info
dt_data <- fread(file.path(path_data,file_allocation_data), sep=";", header = FALSE)
colnames(dt_data) <- c("UniversalDataID","DataLayerID","FishingEntityID", "Year", "TaxonKey",
                       "InputTypeID", "sector_type_name", "catch_type_name", 
                       "reporting_status_name")

## slice per year
my_year <- 2006
system.time(data_yearly <- dt_data[Year==my_year])
# user  system elapsed 
# 0.452   0.220   0.713 

## with key
setkey(dt_data,Year)
system.time(data_yearlyk <- dt_data[Year==my_year,])
# user  system elapsed 
# 0.032   0.004   0.036 

#load the Results data
dt_results <- fread(file.path(path_data,file_allocation_results), sep=";", header = FALSE)
colnames(dt_results) <- c("UniversalDataID","CellID","AllocatedCatch")
# setkey(dt_results,UniversalDataID) # not necessary the data seems to be already ordered with the keys (univ and Cell IDs)

#load the Taxon data
dt_taxon <- fread(file.path(path_data,file_taxon), sep=";", header = FALSE)
colnames(dt_taxon) <- c("TaxonKey","Scientific_Name","Common_Name","FunctionalGroupDescription")
setkey(dt_taxon,TaxonKey)

#load the fishing entity data
dt_entity <- fread(file.path(path_data,file_entity), sep = ";", header=FALSE)
colnames(dt_entity) <- c("FishingEntityID","Name")
setkey(dt_entity,FishingEntityID)

head(dt_results)
head(data_yearlyk)

#subset the data for 10 unique UniversalDataID
udi <- unique(data_yearly$UniversalDataID)[1:10]

#subset the allocation, results
alloc_sub <- data_yearlyk[UniversalDataID %in% udi]
results_sub <- dt_results[UniversalDataID %in% udi]
setkey(alloc_sub,UniversalDataID)
setkey(results_sub,UniversalDataID)

# try a join

j <- results_sub[alloc_sub]

t <- j%>%
      left_join(dt_taxon,by='TaxonKey')%>%
      left_join(dt_entity,by='FishingEntityID')


#------------------------------------------------------------

# Try a larger join

#1. Subset the allocation data for single year

my_year <- 2010
data_yearly <- dt_data[Year==my_year,]

#2. Now we want it ordered by UniversalDataID
setkey(data_yearly,UniversalDataID)

#3. Get unique UniversalDataID

udi <- unique(data_yearly$UniversalDataID)

#4. Subset results

system.time(results_sub <- dt_results[UniversalDataID %in% udi])
# user  system elapsed 
# 9.856  16.232  26.086 
setkey(results_sub,UniversalDataID) #sort by UDI

#5. read in csv that matches cells to Ohi regions

cells <- read.csv(file.path(dir_M, 
                            "git-annex/globalprep/fis/v2015/raw/saup_rasters_to_ohi_fao_rgns_noLand.csv"))%>%
          rename(CellID=saup_cell_id)

#5b. Check cells from raw data with cells we matched with

uni_c <- unique(dt_results$CellID)
length(uni_c) #153183
uni_ohi_c <- unique(cells$CellID)
length(uni_ohi_c) #180186

diff <- setdiff(uni_ohi_c,uni_c)
length(diff) #27475

#this difference may be because the cells-to-ohi regions lookup table includes all global cells, but the SAUP data may only include cells where catch != 0.

#6. Join allocation, taxon and entity data to results

all_data <- results_sub%>%
                        left_join(data_yearly)%>%
                        left_join(dt_taxon)%>%
                        left_join(dt_entity)%>%
                        left_join(cells)%>%
                  mutate(catch_prop = AllocatedCatch * proportionArea,
                         year = my_year)

catch_fao <- all_data%>%
              group_by(fao_id,Scientific_Name,Common_Name, TaxonKey)%>%
              summarise(catch = sum(catch_prop))













