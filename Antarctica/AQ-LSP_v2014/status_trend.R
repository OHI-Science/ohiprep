##################################################
## Antarctica: LSP
## May 1 2014
## MRF
##################################################

library(dplyr)


rm(list = ls())

setwd("N://model/GL-AQ-LSP_v2013")

status_year = 2012
trend_years = status_year:(status_year-4)
ref_pct_cmpa=30

## read data
mpa <- read.csv("raw/Antarctica_MPAs.csv")
pa <- read.csv("tmp/Antarctica_PA.csv")  # all scores are 1

area_ccamlr <- read.csv("N://git-annex/Global/NCEAS-Antarctica-Other_v2014/rgn_area_ccamlr_eez.csv")


## summarize mpa areas by year/region:
mpa2  <- mpa %.%
  select(ccamlr_id2 = CCAMLR_AREA, Area_Km2, year=year_established) %.%
  group_by(ccamlr_id2, year) %.%
  summarize(mpa_area_km2 = sum(Area_Km2)) %.%
  arrange(ccamlr_id2, year, mpa_area_km2)

## add zeros for years/regions without data
gaps <- expand.grid(ccamlr_id2=area_ccamlr$ccamlr_id2, year=seq(1975,2012))
mpa2 <- merge(mpa2, gaps, by=c("ccamlr_id2", "year"), all=TRUE)
mpa2$mpa_area_km2 <- ifelse(is.na(mpa2$mpa_area_km2), 0, mpa2$mpa_area_km2) 

## calculate cumulative sum per region over time:
mpa2  <- mpa2 %.%
  group_by(ccamlr_id2) %.%
  mutate(cumSumMPA_km2 = cumsum(mpa_area_km2))


#merge with region data
mpa2 <- merge(mpa2, area_ccamlr, all=TRUE)

# calculate percent of MPA in CCAMLR region
mpa2 <-  mpa2 %.%
  mutate(score = round(cumSumMPA_km2/(eez_area_km2*ref_pct_cmpa), 4)) %.% #goal is for 30% of CCAMLR region to be protected area
  mutate(type="cmpa") %.%
  select(type, sp_id, year, score, ccamlr_area=eez_area_km2)

## save this data to resilience:
#resilience <- mpa2 %.%
#  filter(year==2012) %.%
#  select(sp_id, resilience.score=score)
#write.csv(resilience, "N:/model/GL-AQ-Resilience/MPAs.csv", row.names=FALSE)

# merge with PA data
scores <- rbind(mpa2, pa)
scores <- scores %.%
  group_by(sp_id, year) %.%
  summarize(status=weighted.mean(score, ccamlr_area))

## status
status <- scores %.%
  filter(year==status_year) %.%
  mutate(status=round(status*100, 2)) %.%
  select(sp_id, status)

write.csv(status, "data\\status.csv", row.names=F, na="")


#Trend ----
trend_data <- subset(scores, year %in% trend_years)

library(plyr)
lm = dlply(
  trend_data, .(sp_id),
  function(x) lm(status ~ year, x))

 Trend <- ldply(lm, coef)

Trend <- Trend %.%
  select(sp_id, trend=year) %.%
  mutate(trend = round(trend*5, 4))


write.csv(Trend, "data\\trend.csv", row.names=F, na='')

#lm(score ~ year, data=subset(mpa2, sp_id==248200)) #data check....
#plot(score ~ year, data=subset(mpa2, sp_id==248200))
