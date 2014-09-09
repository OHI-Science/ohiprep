#################################################
## Antarctica Tourism & Recreation Goal
## STEP 1: Preparing the data
## These sites will be merged with the sites in Tourism & Recreation
## The data was expanded and there were some additional sitest that 
## we needed to obtain the CCAMLR locations for.
#################################################

rm(list = ls())

setwd("/var/data/ohi/model/GL-AQ-Livelihoods_v2013")

sites <- read.csv("raw/sites no rgn_id.csv", stringsAsFactors=FALSE, fileEncoding="latin1", strip.white=TRUE)

# latitude
sites$deg_latitude <- sapply(strsplit(sites$lat, "°"), function(x) x[1])
sites$deg_latitude_extra <- sapply(strsplit(sites$lat, "°"), function(x) x[2])
sites$min_latitude <- sapply(strsplit(sites$deg_latitude_extra, "'"), function(x) x[1])
sites$region_latitude <- sapply(strsplit(sites$deg_latitude_extra, "'"), function(x) x[2])
sites$region_latitude <- toupper(sites$region_latitude)


sites$deg_latitude <- as.numeric(sites$deg_latitude)
sites$min_latitude <- as.numeric(sites$min_latitude)
sites[is.na(sites$min_latitude),]
table(as.factor(sites$region_latitude))

sites$latitude_dd <- (sites$deg_latitude + sites$min_latitude/60) * ifelse(sites$region_latitude=="S", -1, 1)


# longtitude
sites$deg_longitude <- sapply(strsplit(sites$lon, "°"), function(x) x[1])
sites$deg_longitude_extra <- sapply(strsplit(sites$lon, "°"), function(x) x[2])
sites$min_longitude <- sapply(strsplit(sites$deg_longitude_extra, "'"), function(x) x[1])
sites$region_longitude <- sapply(strsplit(sites$deg_longitude_extra, "'"), function(x) x[2])

sites$deg_longitude <- as.numeric(sites$deg_longitude)
sites$min_longitude <- as.numeric(sites$min_longitude)
sites[is.na(sites$min_longitude),]
sites$region_longitude <- toupper(sites$region_longitude)

summary(sites)
table(as.factor(sites$region_longitude))


sites$longitude_dd <- (sites$deg_longitude + sites$min_longitude/60) * ifelse(sites$region_longitude=="W", -1, 1)

## add in location of B15 iceberg...calved from Ross Ice Shelf (will assume this is its general location, although it drifted, it probably stayed in the Ross sea)
sites$latitude_dd[sites$Site.names %in% "B15A Iceberg"] <- -81.5
sites$longitude_dd[sites$Site.names %in% "B15A Iceberg"] <- -175


## save data
sites <- rename(sites, c(Site.names = "Site_name"))
sites <- subset(sites, select=c(Site_name, latitude_dd, longitude_dd))
#write.csv(sites, "tmp/sites_dd_v2.csv", row.names=FALSE)

