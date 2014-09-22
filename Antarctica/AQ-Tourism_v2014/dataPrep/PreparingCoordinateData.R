#################################################
## Antarctica Tourism & Recreation Goal
## STEP 1: Preparing the data
#################################################
#Peter Oy Island  68?48?14?S, 90?35?35?W
#Peter 1 Island 68?47S, 90?35W

# McMurdo Ice Channel
# 77?30'S, 162WE 
# Changed to 166?06'E based on word document (ATCM35_att068_e.doc)

rm(list = ls())

setwd("/var/data/ohi/model/GL-AQ-Tourism_v2013")

sites <- read.csv("raw/Tourist site coordinates.csv", stringsAsFactors=FALSE, fileEncoding="latin1", strip.white=TRUE)
sites[is.na(sites$Coordinates), ]
sites <- sites[!(sites$Site_name %in% "Unknown Name"), ]

sites$Coordinates2 <- gsub(" ", "", sites$Coordinates)
sites$latitude <- sapply(strsplit(sites$Coordinates2, ","), function(x) x[1])
sites$longitude <- sapply(strsplit(sites$Coordinates2, ","), function(x) x[2])

# latitude
sites$deg_latitude <- sapply(strsplit(sites$latitude, "°"), function(x) x[1])
sites$deg_latitude_extra <- sapply(strsplit(sites$latitude, "°"), function(x) x[2])
sites$min_latitude <- sapply(strsplit(sites$deg_latitude_extra, "'"), function(x) x[1])
sites$region_latitude <- sapply(strsplit(sites$deg_latitude_extra, "'"), function(x) x[2])

sites$deg_latitude <- as.numeric(sites$deg_latitude)
sites$min_latitude <- as.numeric(sites$min_latitude)
sites[is.na(sites$min_latitude),]
table(as.factor(sites$region_latitude))

sites$latitude_dd <- (sites$deg_latitude + sites$min_latitude/60) * ifelse(sites$region_latitude=="S", -1, 1)

# longtitude
sites$deg_longitude <- sapply(strsplit(sites$longitude, "°"), function(x) x[1])
sites$deg_longitude_extra <- sapply(strsplit(sites$longitude, "°"), function(x) x[2])
sites$min_longitude <- sapply(strsplit(sites$deg_longitude_extra, "'"), function(x) x[1])
sites$region_longitude <- sapply(strsplit(sites$deg_longitude_extra, "'"), function(x) x[2])

sites$deg_longitude <- as.numeric(sites$deg_longitude)
sites$min_longitude <- as.numeric(sites$min_longitude)
summary(sites)
table(as.factor(sites$region_longitude))


sites$longitude_dd <- (sites$deg_longitude + sites$min_longitude/60) * ifelse(sites$region_longitude=="W", -1, 1)

## save data
sites <- subset(sites, select=c(Site_name, latitude_dd, longitude_dd))
#write.csv(sites, "tmp/sites_dd.csv", row.names=FALSE)

