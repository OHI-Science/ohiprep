#################################################
## Antarctica Tourism & Recreation Goal
## STEP 3: Calculating the tourists per region per year
## Update 10/1/2015: using within region values as reference point 
#################################################
## previously we used tourist density and used the global value as our reference point.
## if we go back to this approach, the previous version of this script should have that
## information

library(dplyr)
library(tidyr)

status.year <- 2015
trend.years <- (status.year-4):status.year

# Load in data ----
sites <- read.csv("Antarctica/AQ-Tourism_v2014/tmp/Sites_CCAMLR.csv")
tourists <- read.csv("Antarctica/AQ-Tourism_v2014/tmp/TR_data.csv")

sites <- sites %>%
  select("Site_Name"=Site_name, sp_id)

## check that no new sites have been added
setdiff(sites$Site_Name, tourists$Site_Name)
tmp <- setdiff(tourists$Site_Name, sites$Site_Name)
tmp
## these got cut for not being in the CCAMLR region:
# South Bay, Trinity Island-Mikkelsen Hrbr, Campbell Island
# Sites with no coordinates we could find: Chutney Cove and Stony Point


# determine number of tourist days per region
tourist_days <- tourists %>%
  inner_join(sites, by="Site_Name") %>%
  group_by(sp_id, year) %>%
  summarize(days = sum(Total)) %>%
  ungroup() 

# filling in missing data with zero values:
tourist_days <- spread(tourist_days, year, days) %>%
  data.frame()
write.csv(tourist_days, "Antarctica/AQ-Tourism_v2014/tmp/tourists_spread.csv", row.names=FALSE)
# generate gap_filled data (in this case, missing values are assumed to be zero)
gap_fill <- ifelse(is.na(tourist_days[, -1]), "missing data - presumed zero", 'NA')
gap_fill <- cbind(sp_id = tourist_days$sp_id, gap_fill)
gap_fill <- data.frame(gap_fill)
gap_fill <- gather(gap_fill, "year", "gap_fill", 2:(dim(gap_fill)[2]))
gap_fill$year <- as.numeric(gsub("X", "", gap_fill$year))

write.csv(gap_fill, 'Antarctica/AQ-Tourism_v2014/data/gap_fill.csv', row.names=FALSE)


## final toolbox data:
tourist_days <- tourist_days %>%
  gather('year', 'days', -1) %>%
  mutate(days = ifelse(is.na(days), 0, days)) %>%
  mutate(year = as.numeric(gsub("X", "", year)))

write.csv(tourist_days, 'Antarctica/AQ-Tourism_v2014/data/tr_days.csv', row.names=FALSE)



