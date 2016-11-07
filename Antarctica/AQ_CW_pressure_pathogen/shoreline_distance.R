#################################################
## Antarctica pathogens for CW and pressures
## STEP 1: Determine distance from shoreline
#################################################

library(sp)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(colorspace)
library(rgeos)

# Load in data ----
sites <- read.csv("Antarctica/AQ_CW_pressure_pathogen/raw/Antarctic_Facilities_OHIscorecalc.csv")

sites_map <- sites %>%  # get rid of this site, messing crs conversion up and is not coastal
  filter(Lat != -90)

map <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_gcs")
map <- map[map@data$sp_type %in% c("eez-ccamlr", "land-ccamlr"), ]
eez_ccamlr <-  map[map@data$sp_type %in% c("eez-ccamlr"), ]
eez_ccamlr <- spTransform(eez_ccamlr, CRS("+init=epsg:3857"))
eez_ccamlr@data$merge <- 1
eez_ccamlr <- gUnaryUnion(eez_ccamlr, id = eez_ccamlr@data$merge)
plot(eez_ccamlr)

# convert site data to spatial dataframe
coordinates(sites_map) <- ~Lon+Lat
proj4string(sites_map) <- CRS(proj4string(map))
sites_mer <- spTransform(sites_map, CRS("+init=epsg:3857"))

plot(sites_mer, add=TRUE, col="red", pch=as.character(sites_mer@data$base))

for(i in 1:dim(sites_mer)[1]){
sites_mer@data$dist[i] <- gDistance(sites_mer[i, ], eez_ccamlr)
}

sites_100km <- sites_mer@data     
sites_100km <- sites_100km %>%
  filter(dist < 100000) %>%      # get rid of sites that are >100 km from coast
  filter(population > 0)         # get rid of sites with population of zero

sites_final <- sites %>%
  filter(base %in% sites_100km$base) %>%
  mutate(gapfill = ifelse(is.na(treatment), 1, 0)) %>%
  select(base, population, treatment=treatment_gf, Lat, Lon, ccamlr, gapfill, year)

write.csv(sites_final, "Antarctica/AQ_CW_pressure_pathogen/intermediate/Antarctic_Facilities_OHIscorecalc_final_sites.csv", row.names=FALSE)

