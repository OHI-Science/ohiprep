############################################
## Determine proportion of each SAUP region
## in each OHI region
## MRF 6/9/2016
############################################

library(dplyr)
library(rgdal)
library(raster)
library(sp)

source('src/R/common.R')

### NOTE to self...should have used the 
## OHI region raster
ohi_rgns <- raster(file.path(dir_M, "git-annex/globalprep/spatial/v2015/data/sp_mol_raster_1km/sp_mol_raster_1km.tif"))


## saup regions
getapibaseurl <- function() {
  return("http://api.seaaroundus.org/api/v1")
}

url <- paste(getapibaseurl(), 'eez', "?geojson=true", sep = "/")

saup_regions <- readOGR(dsn = url, layer = ogrListLayers(url), 
                        verbose = FALSE)

writeOGR(saup_regions, file.path(dir_M, "git-annex/globalprep/_raw_data/SAUP/d2016/SAUP_rgns"), "SAUP_rgns", driver="ESRI Shapefile")

saup_regions@data %>%
  arrange(title)

saup_regions <- spTransform(saup_regions, CRS(proj4string(ohi_rgns)))


### extract data
#data <- raster::extract(ohi_rgns, saup_regions[1,], weights=TRUE, normalizeWeights = FALSE, progress = 'text')

regions_summary <- data.frame()
for(i in 1:dim(saup_regions)[1]){ # i=2
  data <- raster::extract(ohi_rgns, saup_regions[i,], progress = 'text')
  table_data <- table(data)
  new <- data.frame(table_data) %>%
    mutate(saup_id = saup_regions@data$region_id[i]) %>%
    dplyr::select(saup_id, ohi_rgn=data, Freq)
  regions_summary <- rbind(regions_summary, new)
}

saup_to_ohi <- regions_summary %>%
  group_by(saup_id) %>%
  mutate(percent = Freq/sum(Freq)) %>%
  ungroup() %>%
  filter(percent>.01) %>%
  arrange(saup_id, ohi_rgn) %>%
  data.frame()

write.csv(saup_to_ohi, 'globalprep/fis/v2016/int/saup_to_ohi_rough_draft.csv', row.names=FALSE)


#### I took the saup_to_ohi_rough_draft.csv and hand corrected the weirdnesses due to 
### mismatches in the maps, etc.  

### After the hand correcting, I checked the data and did some final formatting
### (code is below, and saved the final key as: saup_to_ohi_key.csv)

convert <- read.csv('C:/Users/Melanie/Desktop/saup_to_ohi/saup_to_ohi_rough_draft.csv')
regions <- read.csv("C:/Users/Melanie/Desktop/saup_to_ohi/regionData.csv") %>%
  filter(sp_type == "eez")

setdiff(regions$rgn_id, convert$ohi_rgn)
setdiff(convert$ohi_rgn, regions$rgn_id)

dup_saups <- convert$saup_id[duplicated(convert$saup_id)]
convert %>%
  filter(saup_id %in% dup_saups) %>%
  arrange(saup_id)

dup_ohi <- convert$ohi_rgn[duplicated(convert$ohi_rgn)]
convert %>%
  filter(ohi_rgn %in% dup_ohi) %>%
  arrange(ohi_rgn)

convert <- convert %>%
  mutate(complication = 
           ifelse(saup_id %in% dup_saups, "split SAUP catch among OHI regions", NA)) %>%
  mutate(complication =
           ifelse(ohi_rgn %in% dup_ohi & !is.na(complication), "split and then combine SAUP catch per OHI region", complication))%>%
  mutate(complication =
           ifelse(ohi_rgn %in% dup_ohi & is.na(complication), "combine SAUP catch per OHI region", complication)) %>%
  select(saup_rgn=saup_id, ohi_rgn, percent_saup, complication) %>%
  arrange(saup_rgn, ohi_rgn)

write.csv(convert, 'C:/Users/Melanie/Desktop/saup_to_ohi/saup_to_ohi_key.csv', row.names=FALSE)

