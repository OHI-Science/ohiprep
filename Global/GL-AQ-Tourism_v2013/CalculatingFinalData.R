#################################################
## Antarctica Tourism & Recreation Goal
## STEP 3: Calculating the tourists per region per year
#################################################

# question: need to add zeros?

library(dplyr)

rm(list = ls())

setwd("/var/data/ohi/model/GL-AQ-Tourism_v2013")


# Load in data ----
sites <- read.csv("tmp/Sites_CCAMLR.csv")
tourists <- read.csv("raw/TR_data.csv")
area <- read.csv("/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data/sp_inland1km_ccamlr_gcs_data.csv")
labels <- read.csv("/var/data/ohi/git-annex/Global/NCEAS-Antarctica-Other_v2014/rgn_labels_ccamlr.csv")


# Check for consistency among names:
setdiff(tourists$Site_Name, sites$Site_name) # Ok:  these three sites were cut because they were outside the CCAMLR regions
setdiff(sites$Site_name, tourists$Site_Name)

sites <- sites %.%
  select("Site_Name"=Site_name, sp_id)


tourists_summary <- tourists %.%
  inner_join(sites, by="Site_Name") %.%
  group_by(sp_id, year)%.%
  summarise(TotalTourists=sum(Total))

# merge with areas to get density of tourists for each region
area <- subset(area, select=c(sp_id, area_km2))

tourists_summary <- merge(tourists_summary, area, by="sp_id")
tourists_summary$Tourists_per_km2 <- tourists_summary$TotalTourists/tourists_summary$area_km2 

write.csv(tourists_summary, "rgn_year_TR.csv", row.names=FALSE)
