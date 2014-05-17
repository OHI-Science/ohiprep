 ########################################################################################
## GOAL: Obtain species diversity data and iconics data 
## for HS and AQ
## Date: Mar 18 2014
## MRF
########################################################################################
library(sp)
library(rgdal)
library(raster)
library(plyr)
library(dplyr)
rm(list = ls())

setwd("/var/data/ohi/model/GL-HS-AQ-SpeciesRichness_v2013")
cells <- read.csv("tmp\\am_cells_rgn_proportions.csv")
cells_spp <- read.csv("tmp\\am_cells_spp.csv")
spp <- read.csv("raw\\spp.csv")
am_cells <- read.csv("N:\\model\\GL-NCEAS-SpeciesDiversity_v2013a\\tmp\\am_cells_data.csv")

### Master file of cells structured: 
am_cells <- am_cells %.%
  select(LOICZID, CellArea)

setdiff(cells$LOICZID, am_cells$LOICZID) # should be 0
setdiff(am_cells$LOICZID, cells$LOICZID) # These are cells that fell outside a rgn polygon
am_cells$LOICZID <- as.numeric(am_cells$LOICZID)

cells <- cells %.%
  left_join(am_cells, by="LOICZID")
cells$rgn_area <- cells$CellArea*cells$proportionArea

# create lookup table of weights for extinction risk categories and population trends
category <- data.frame(category=c("LC", "NT", "VU", "EN", "CR", "EX"), 
                       catScore =c(0, 0.2, 0.4, 0.6, 0.8, 1))
popn_trend <- data.frame(popn_trend=c("Decreasing", "Stable", "Increasing"), 
                         trendScore=c(-0.5, 0, 0.5))

spp <- spp %.%
  select(sid, category, popn_trend)%.%
  left_join(category) %.%
  left_join(popn_trend) 


# calculate average weight and count for category and trend scores
scoreData <- cells_spp %.%
  group_by(LOICZID) %.%
  inner_join(spp) %.% 
  summarise(category_linear_cnt = length(category[!(category %in% "DD")]),
            category_linear_avg = mean(catScore, na.rm=TRUE),
            popn_trend_linear_cnt = sum(!is.na(trendScore)),
            popn_trend_linear_avg = mean(trendScore, na.rm=TRUE))
# rescale lower end of the biodiversity goal to be 0 when 75% species are extinct, a level comparable to the five documented mass extinctions
scoreData$category_linear_score <- ((1 - scoreData$category_linear_avg) - 0.25) / 0.75 * 100
scoreData$LOICZID <- as.numeric(scoreData$LOICZID)

cells <- cells %.%
  left_join(scoreData, by="LOICZID")

# calculate area-weighted regional scores ----
regionScores <- cells %.%
  group_by(rgn_id) %.%
  summarise(rgn_spp_score_2013=sum(category_linear_score*rgn_area, na.rm=TRUE)/sum(rgn_area, na.rm=TRUE),
            rgn_spp_trend_2013=sum(popn_trend_linear_avg*rgn_area, na.rm=TRUE)/sum(rgn_area, na.rm=TRUE))
regionScores$rgn_id <- as.numeric(regionScores$rgn_id)


# merge with region data ----
#tmp <- readOGR(dsn="C:\\Users\\Melanie\\Desktop\\GL-NCEAS-Regions_v2014\\data", layer="eez_ccmlar_fao_gcs")
regions <- tmp@data[tmp@data$rgn_type %in% c("CCAMLR", "fao"), ]
regions <- select(regions, c(rgn_type, rgn_id, rgn_name, F_CODE))
regions$rgn_id <- as.numeric(as.character(regions$rgn_id))


# organize and write to csv:
rgn_spp_score_2013_HS_AQ <- regionScores %.%
  mutate(score=rgn_spp_score_2013/100) %.%
  select(rgn_id, score) %.%
  inner_join(regions) %.%
  arrange(rgn_type, rgn_id)

write.csv(rgn_spp_score_2013_HS_AQ, 
          "data\\rgn_spp_score_2013_HS_AQ.csv", 
          row.names=FALSE)
write.csv(subset(rgn_spp_score_2013_HS_AQ, rgn_type=="CCAMLR", select=c(rgn_id, score)), 
          "data\\rgn_spp_score_2013_AQ.csv", 
          row.names=FALSE)
write.csv(subset(rgn_spp_score_2013_HS_AQ, rgn_type=="fao", select=c(rgn_id, score)), 
          "data\\rgn_spp_score_2013_HS.csv", 
          row.names=FALSE)



rgn_spp_trend_2013_HS_AQ <- regionScores %.%
  mutate(score=rgn_spp_trend_2013) %.%
  select(rgn_id, score) %.%
  inner_join(regions, by="rgn_id") %.%
  arrange(rgn_type, rgn_id)
write.csv(rgn_spp_trend_2013_HS_AQ, 
          "data\\rgn_spp_trend_2013_HS_AQ.csv", 
          row.names=FALSE)
write.csv(subset(rgn_spp_trend_2013_HS_AQ, rgn_type=="CCAMLR", select=c(rgn_id, score)), 
          "data\\rgn_spp_trend_2013_AQ.csv", 
          row.names=FALSE)
write.csv(subset(rgn_spp_trend_2013_HS_AQ, rgn_type=="fao", select=c(rgn_id, score)), 
          "data\\rgn_spp_trend_2013_HS.csv", 
          row.names=FALSE)


## checking against old data:
oldScore <- read.csv("N:\\model\\GL-NCEAS-SpeciesDiversity_v2013a\\data\\rgn_spp_score_2013.csv")
oldScore$rgn_id <- as.numeric(oldScore$rgn_id)
oldTrend <- read.csv("N:\\model\\GL-NCEAS-SpeciesDiversity_v2013a\\data\\rgn_spp_trend_2013.csv")
oldTrend$rgn_id <- as.numeric(oldTrend$rgn_id)
regionScores <- left_join(regionScores, oldScore)
plot(regionScores$rgn_spp_score_2013, regionScores$score*100)
abline(0,1, col="red")
sum(regionScores$rgn_spp_score_2013 -regionScores$score*100, na.rm=TRUE)
hist(regionScores$rgn_spp_score_2013 -regionScores$score*100)
regionScores[(regionScores$rgn_spp_score_2013 -regionScores$score*100>.2),]

oldTrend <- rename(oldTrend, c(score="trend"))
regionScores <- left_join(regionScores, oldTrend)
plot(regionScores$rgn_spp_trend_2013, regionScores$trend)
sum(regionScores$rgn_spp_trend_2013 -regionScores$trend, na.rm=TRUE)
hist(regionScores$rgn_spp_trend_2013 -regionScores$trend)
regionScores[(regionScores$rgn_spp_trend_2013 -regionScores$trend>.001),]




