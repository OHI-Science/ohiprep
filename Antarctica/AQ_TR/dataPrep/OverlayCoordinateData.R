#################################################
## Antarctica Tourism & Recreation Goal
## STEP 2: Overlaying the data and obtaining global reference points
#################################################

# these got cut for not being in the CCAMLR region:
# South Bay, Trinity Island-Mikkelsen Hrbr, Campbell Island

# Sites with no coordinates: Chutney Cove and Stony Point

library(sp)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(colorspace)

# Load in data ----
sites <- read.csv("Antarctica/AQ-Tourism_v2014/tmp/sites_dd.csv")
sites <- sites %>%
  filter(!is.na(latitude_dd))

map <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_gcs")
map <- map[map@data$sp_type %in% c("eez-ccamlr", "land-ccamlr"), ]
plot(map)

# convert site data to spatial dataframe
coordinates(sites) <- ~longitude_dd+latitude_dd
proj4string(sites) <- CRS(proj4string(map))

plot(sites, add=TRUE, col="red", pch=as.character(sites@data$Site_name))
identify(sites)


# Check by plotting ----
png("Antarctica/AQ-Tourism_v2014/dataPrep/AntarcticaMap_points.png", units="cm", width=15, height=15, res=300)
plot(map)
plot(sites, add=TRUE, cex=.5, col="red", pch=16)
dev.off()


# overlay points ----
sites_CCAMLR <- over(sites, map)
sites_CCAMLR <- data.frame(data.frame(sites), sites_CCAMLR) # add sites data


# check on NA sites:
sites_NA <- sites[is.na(sites_CCAMLR$sp_id), ]

png("AntarcticaMap_NApoints.png", units="cm", width=15, height=15, res=300)
plot(map)
plot(sites_NA, add=TRUE, cex=1.3, col="red", pch=16)
dev.off()

# cut sites outside of CCAMLR:
sites_CCAMLR <- sites_CCAMLR[!is.na(sites_CCAMLR$sp_id), ]

sites_CCAMLR <- subset(sites_CCAMLR, select=c(Site_name, sp_id, latitude_dd, longitude_dd, rgn_type))

write.csv(sites_CCAMLR, "Antarctica/AQ-Tourism_v2014/tmp/Sites_CCAMLR.csv", row.names=FALSE)

