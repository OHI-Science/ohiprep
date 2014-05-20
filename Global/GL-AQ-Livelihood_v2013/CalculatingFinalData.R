#################################################
## Antarctica Tourism & Recreation Goal
## STEP 3: Calculating the tourists per region per year
#################################################

library(dplyr)

rm(list = ls())

setwd("/var/data/ohi/model/GL-AQ-Tourism_v2013")

status.year <- 2013
trend.years <- (status.year-4):status.year

# Load in data ----
sites <- read.csv("tmp/Sites_CCAMLR.csv")
tourists <- read.csv("raw/TR_data.csv")
area_1km <- read.csv("/var/data/ohi/git-annex/Global/NCEAS-Antarctica-Other_v2014/rgn_area_ccamlr_inland_1km.csv")
area_eez <- read.csv("/var/data/ohi/git-annex/Global/NCEAS-Antarctica-Other_v2014/rgn_area_ccamlr_eez.csv")

# following obtained using (raw/global_arrivals.csv) in "OverlayCoordinateData.R"
gl_median <- 0.06419833
gl_90ile <- 7.271842


# sum inland and eez areas
totalArea <- area_1km %.%
  left_join(area_eez, by=c("ccamlr_id2", "ccamlr_id", "sp_id", "sp_name")) %.%
  mutate(area_tot = inland_1km2 + eez_area_km2) %.%
  select(sp_id, inland_1km2, eez_area_km2, area_tot)


# Check for consistency among names:
#setdiff(tourists$Site_Name, sites$Site_name) # Ok:  these three sites were cut because they were outside the CCAMLR regions
#setdiff(sites$Site_name, tourists$Site_Name)

sites <- sites %.%
  select("Site_Name"=Site_name, sp_id)

# Calculate density of tourists
tourists_summary <- tourists %.%
  inner_join(sites, by="Site_Name") %.%
  group_by(sp_id, year)%.%
  summarise(Tourist_days_eq=sum(Total)) 

# fill in missing values
all.values <- expand.grid(sp_id = unique(tourists_summary$sp_id),
                          year = unique(tourists_summary$year))
tourists_summary <- merge(all.values, tourists_summary, all=TRUE, by=c("sp_id", "year"))
tourists_summary$Tourist_days_eq[is.na(tourists_summary$Tourist_days_eq)] <- 0 

tourists_summary  <- tourists_summary %.%
  left_join(totalArea, by="sp_id") %.%
  mutate(tr_days_area_AQ = Tourist_days_eq/area_tot)
    
# Calculate score based on density of tourists and global reference points
score_range  <- 1 # 1 - 0 (scores corresponding to: median - 90%ile)
value_range <- gl_median - gl_90ile# median - 90%ile
max_value <- gl_90ile # the value beyond which tourist density is too high and TR gets a score of 0
intercept <- 0.5 # the smallest score possible for scarecely frequented tourist destinations
score_range2 <- 1 # 1 - 0 
value_range2 <- gl_median-0  # median - 0 (scores corresponding to: median - 0)

tourists_summary <- tourists_summary %.%
  mutate(score = ifelse(tr_days_area_AQ < gl_median,  
                               ifelse(tr_days_area_AQ/value_range2 > intercept, 1,
                                      intercept + tr_days_area_AQ/value_range2), 
                               ifelse(tr_days_area_AQ > gl_median, 
                                      ifelse(tr_days_area_AQ < max_value, 
                                             1+tr_days_area_AQ *score_range/value_range,0 ),
                                      1)))

## Status ----
library(plyr)
regions <- data.frame(sp_id=area_eez$sp_id)

Status <- tourists_summary %.%
  filter(year==status.year) %.%
  select(sp_id, status = score) %.%
  mutate(status = round(status*100, 2))

Status <- merge(regions, Status, by="sp_id", all.x=TRUE)

write.csv(Status, "data/status.csv", row.names=F, na="")


#Trend ----

tourists_summary <- tourists_summary[tourists_summary$year %in% trend.years,]

lm = dlply(
  tourists_summary, .(sp_id),
  function(x) lm(score ~ year, x))

Trend <- ldply(lm, coef)

Trend <- Trend %.%
  select(sp_id, trend=year) %.%
  mutate(trend = round(trend*5, 4))

Trend <- merge(regions, Trend, by="sp_id", all.x=TRUE)

write.csv(Trend, "data/trend.csv", row.names=F, na='')

#lm(score ~ year, data=subset(tourists_summary, sp_id==248200)) #data check....


