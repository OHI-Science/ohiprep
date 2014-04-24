#################################################
## Antarctica Tourism & Recreation Goal
## STEP 3: Calculating the tourists per region per year
#################################################

# question: need to add zeros?

library(dplyr)

rm(list = ls())

setwd("C:\\Users\\Melanie\\Desktop\\AQ-TourismData_v2013")


# Load in data ----
sites <- read.csv("tmp/Sites_CCAMLR.csv")
tourists <- read.csv("raw/TR_data.csv")

# Check for consistency among names:
setdiff(tourists$Site_Name, sites$Site_name) # Ok:  these three sites were cut because they were outside the CCAMLR regions
setdiff(sites$Site_name, tourists$Site_Name)

sites <- sites %.%
  select("Site_Name"=Site_name, rgn_id, F_CODE)


tourists_summary <- tourists %.%
  inner_join(sites) %.%
  group_by(F_CODE, rgn_id, year)%.%
  summarise(Total=sum(Total))

write.csv(tourists_summary, "tmp/rgn_year_TR.csv", row.names=FALSE)
