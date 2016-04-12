#################################################
## Antarctica pathogens for CW and pressures
## STEP 2: overlay points to determine ccamlr region
#################################################


library(sp)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(colorspace)
library(rgeos)

# Load in data ----
sites <- read.csv("Antarctica/AQ_CW_pressure_pathogen/intermediate/Antarctic_Facilities_OHIscorecalc_final_sites.csv")

map <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_gcs")
map <- map[map@data$sp_type %in% c("eez-ccamlr", "land-ccamlr"), ]
plot(map)

# convert site data to spatial dataframe
sites_sp <- sites
coordinates(sites_sp) <- ~Lon+Lat
proj4string(sites_sp) <- CRS(proj4string(map))

plot(sites_sp, add=TRUE, col="red", pch=as.character(sites_sp@data$base))


# overlay points ----
ccamlr_rgn <- over(sites_sp, map) %>%
  select(sp_id) 

data <- cbind(sites_sp@data, ccamlr_rgn)%>%
  select(sp_id, year, population, treatment) %>%
  data.frame()

write.csv(data, "Antarctica/AQ_CW_pressure_pathogen/intermediate/pathogen_data.csv", row.names=FALSE)
