#################################################
## Antarctica Tourism & Recreation Goal
## STEP 2: Overlaying the data
#################################################

# these got cut for not being in the CCAMLR region:
# South Bay, Trinity Island-Mikkelsen Hrbr, Campbell Island

library(sp)
library(rgdal)
library(RColorBrewer)
library(colorspace)

rm(list = ls())

setwd("C:\\Users\\Melanie\\Desktop\\GL-AQ-Tourism_v2013")


# Load in data ----
sites <- read.csv("tmp\\sites_dd.csv")
map <- readOGR(dsn="N:\\git-annex\\Global\\NCEAS-Regions_v2014\\data", layer="sp_gcs")


# convert site data to spatial dataframe
coordinates(sites) <- ~longitude_dd+latitude_dd
proj4string(sites) <- CRS(proj4string(map))


# Check by plotting ----
png("AntarcticaMap_points.png", units="cm", width=15, height=15, res=300)
plot(map)
plot(sites, add=TRUE, cex=.5, col="red", pch=16)
dev.off()

map_AQ <- map[map$rgn_name %in% "Antarctica",]
map_AQ@data$rgn_id <- map_AQ@data$rgn_id[drop=TRUE]
png("AntarcticaMap.png", units="cm", width=15, height=15, res=300)
spplot(map_AQ, c("rgn_id"), col.regions=diverge_hsv(19))
dev.off()

# overlay points ----
sites_CCAMLR <- over(sites, map)

sites_CCAMLR <- data.frame(data.frame(sites), sites_CCAMLR) 
sites_CCAMLR <- subset(sites_CCAMLR, select=c(Site_name, rgn_id, latitude_dd, longitude_dd, F_CODE, rgn_type))


# check on NA sites:
sites_NA <- sites[is.na(sites_CCAMLR$F_CODE), ]

png("AntarcticaMap_NApoints.png", units="cm", width=15, height=15, res=300)
plot(map)
plot(sites_NA, add=TRUE, cex=1.3, col="red", pch=16)
dev.off()

# cut sites outside of CCAMLR:
sites_CCAMLR <- sites_CCAMLR[!is.na(sites_CCAMLR$F_CODE), ]
write.csv(sites_CCAMLR, "Sites_CCAMLR.csv", row.names=FALSE)
