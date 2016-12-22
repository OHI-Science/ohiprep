###########################################################
## GOAL: Obtain species diversity data and iconics data 
## for HS and AQ
## Date: Mar 18 2014
## MRF
## NOTE: This differs from the previous analysis because I
## am testing the results after changing the raster cell values against the previous raster grid values
## Plus: I am creating the following files: 
##      1) raster used to define the locations of the new rgn shapefile
##      2) am_cells_rgn_proportions: describes the proportion of each raster cells within each region
##      3) am_cells_sp: describes the species in each raster cell (modified to use LOICZID rather than cid)  
###########################################################
library(sp)
library(rgdal)
library(raster)
library(plyr)
library(dplyr)
rm(list = ls())

# Extracted and saved the following data from the spp.db file (2013 data)
# This will be the baseline data used in the analyses:
### data prepped for new analysis:
# library(foreign)
# library(DBI)
# library(RSQLite)

# m = dbDriver("SQLite")
# database <- "/var/data/ohi/model/GL-NCEAS-SpeciesDiversity_v2013a/tmp/spp.db"
# db <- dbConnect(m, dbname=database)
# dbListTables(db)
# 
# dbReadTable(db, "cells")
#write.csv(dbReadTable(db, "cells"), "C:\\Users\\Melanie\\Desktop\\Species and Iconics\\TestingAgainstOldData\\raw\\cells.csv", row.names=FALSE)
#write.csv(dbReadTable(db, "cells_spp"), "C:\\Users\\Melanie\\Desktop\\Species and Iconics\\TestingAgainstOldData\\raw\\cells_spp.csv", row.names=FALSE)
#write.csv(dbReadTable(db, "spp"), "C:\\Users\\Melanie\\Desktop\\Species and Iconics\\TestingAgainstOldData\\raw\\spp.csv", row.names=FALSE)
setwd("/var/data/ohi/model/GL-HS-AQ-SpeciesRichness_v2013")
cells_2013 <- read.csv("raw/cells.csv")
cells_spp <- read.csv("raw/cells_spp.csv")
spp <- read.csv("raw/spp.csv")

###################################
# new grid data ----
###################################
# Preparing the new cells and cells_spp data with new IDs to match the .tif file I want to use for extracting the new regions

#original cell data from aqua maps:  Based on this it is the most complete data.  I need to work from this.
am_cells <- read.csv("/var/data/ohi/model/GL-NCEAS-SpeciesDiversity_v2013a/tmp/am_cells_data.csv")
sum(duplicated(am_cells$LOICZID)) #key numeric ID for raster generation....

### Generate raster ----
#spCells <- raster("raw/cells_template.tif") #old raster file for template, from the GL-NCEAS-SpeciesDiversityData data file
#coordinates(am_cells) <- ~CenterLong+CenterLat
#proj4string(am_cells) <- CRS(projection(spCells))
#rasterize(am_cells, spCells, field="LOICZID", progress="text", filename="tmp/am_cells_data_raster")

### Get percent of each raster cell in the region polygons ----
am_cells_raster <- raster("tmp/am_cells_data_raster")
regions <- readOGR(dsn="/var/data/ohi/git-annex/Global/NCEAS-Regions_v2014/data", layer="sp_gcs")
regions <- regions[regions@data$rgn_type %in% c("eez", "fao", "eez_ccamlr", "eez-disputed", "eez-inland"), ]
#plot(regions, add=TRUE)
regionProp <- extract(am_cells_raster,  regions, weights=TRUE, progress="text")
names(regionProp) <- regions@data$sp_id
regionProp_df <- ldply(regionProp, rbind) 
length(unique(regionProp_df$.id)) #less than 254 due to repeats of Canada and one small region (232) with no rasters identified
tmp <- ldply(regionProp, length)
setdiff(regionProp_df$.id, tmp$.id)
setdiff(tmp$.id, regionProp_df$.id)
regionProp_df <- rename(regionProp_df, c(.id="sp_id", value="LOICZID", weight="proportionArea"))
#### add in this region - which appears to be too small to catch using this method (<1% of area)
cells_2013[cells_2013$rgn_id==232, ]
cells_2013[cells_2013$csq=="1401:227:4", ]
am_cells[am_cells$CsquareCode == "1401:227:4", ]
6.034664/2269.83
BosniaHerzegovina <- data.frame(sp_id=232, LOICZID=68076, proportionArea=0.002658641)
regionProp_df <- rbind(regionProp_df, BosniaHerzegovina)  
## Add in cell areas:
#write.csv(regionProp_df, "tmp/am_cells_rgn_proportions_May162014.csv", row.names=FALSE)
rgn_proportions <- read.csv("tmp/am_cells_rgn_proportions_May162014.csv")

##################################################
### Generating master file with raster ID's ----
##################################################
am_cells <- read.csv("/var/data/ohi/model/GL-NCEAS-SpeciesDiversity_v2013a/tmp/am_cells_data.csv")
am_cells <- am_cells %.%
  select("csq"=CsquareCode, LOICZID, CellArea)
sum(duplicated(am_cells$csq))

###First figure out differences between LOICZID/csq and cid
setdiff(am_cells$csq, cells_2013$csq) #I don't think the cells file contains land ID values
setdiff(cells_2013$csq, am_cells$csq)
cells_2013 <- inner_join(cells_2013, am_cells)

setdiff(cells_2013$cid, cells_spp$cid)
setdiff(cells_spp$cid, cells_2013$cid)
summary(cells_spp)

##### Exploring the difference between cid and other IDs (csq and LOICZID)
#these have the same csq id: 5207:364:1
tmp1 <- cells_spp[cells_spp$cid==4121,]
tmp2 <- cells_spp[cells_spp$cid==80020,]
setdiff(tmp1$sid, tmp2$sid)
setdiff(tmp2$sid, tmp1$sid)
cells_2013[cells_2013$cid==4121,]
cells_2013[cells_2013$cid==80020,]
 #these have the same csq id: 1401:227:4:
tmp1 <- cells_spp[cells_spp$cid==43639,]
tmp2 <- cells_spp[cells_spp$cid==43640,]
setdiff(tmp1$sid, tmp2$sid)
setdiff(tmp2$sid, tmp1$sid)
cells_2013[cells_2013$cid==43639,]
cells_2013[cells_2013$cid==43640,]

#summary: when a regional  polygon runs through a cell, the cell ends up with two CID values.
# it appears that IUCN species were extracted according to polygons (or smaller raster size) rather than assigning
# IUCN data to the aqua maps rasters. Then, CID's are assigned by the aqua map Cell ID and region ID. 
# Consequently, a species may be in one CID and not another
# even though they share the same csq ID from Aquamaps.  This only makes a difference for border
# cells that are intersected by a region polygon and only a handful of IUCN species.  Across the entire
# region, this makes no real difference in the final results.  The only exception is for very
# small polygons.
# For this analysis, I am going to collapse the cid's into a single csq/LOICZID
# 
cells_spp <- cells_spp %.%
  left_join(cells_2013, by="cid") %.%
#  left_join(am_cells, by="csq") 
   select(LOICZID, sid)
cells_spp <- unique(cells_spp)
summary(cells_spp)  
#write.csv(cells_spp, "tmp\\am_cells_spp.csv", row.names=FALSE)


####################################################################################
# The following explores the extent of differences when the IUCN species are
# assigned to one aquamap cell...
# (result: basically produces same results) ----
####################################################################################


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
  summarise(category_linear_cnt2 = length(category[!(category %in% "DD")]),
            category_linear_avg2 = mean(catScore, na.rm=TRUE),
            popn_trend_linear_cnt2 = sum(!is.na(trendScore)),
            popn_trend_linear_avg2 = mean(trendScore, na.rm=TRUE))

cells_2013 <- cells_2013 %.%
  #left_join(am_cells, by="csq") %.%
  left_join(scoreData, by="LOICZID")

# rescale lower end of the biodiversity goal to be 0 when 75% species are extinct, a level comparable to the five documented mass extinctions
cells_2013$category_linear_score2 <- ((1 - cells_2013$category_linear_avg2) - 0.25) / 0.75 * 100

## Checking against old data
plot(cells_2013$category_linear_avg2, cells_2013$category_linear_avg)
plot(cells_2013$category_linear_cnt2, cells_2013$category_linear_cnt)
head(cells_2013[cells_2013$category_linear_cnt2 != cells_2013$category_linear_cnt, ])
plot(cells_2013$category_linear_score2, cells_2013$category_linear_score)
plot(cells_2013$popn_trend_linear_cnt2, cells_2013$popn_trend_linear_cnt)
plot(cells_2013$popn_trend_linear_avg2, cells_2013$popn_trend_linear_avg)
head(cells_2013[cells_2013$category_linear_cnt2 != cells_2013$category_linear_cnt, ])

# calculate area-weighted regional scores ----
regionScores <- cells_2013 %.%
  group_by(rgn_id) %.%
  summarise(rgn_spp_score_2013=sum(category_linear_score2*area_km2)/sum(area_km2),
            rgn_spp_trend_2013=sum(popn_trend_linear_avg2*area_km2)/sum(area_km2))


## checking against old data:
oldScore <- read.csv("/var/data/ohi/model/GL-NCEAS-SpeciesDiversity_v2013a/data/rgn_spp_score_2013.csv")
oldTrend <- read.csv("N:\\model\\GL-NCEAS-SpeciesDiversity_v2013a\\data\\rgn_spp_trend_2013.csv")
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




