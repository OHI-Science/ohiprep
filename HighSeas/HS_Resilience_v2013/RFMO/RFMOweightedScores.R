############################################################
### Assign scores to RFMO proportional areas.
############################################################

library(dplyr)
library(reshape2)

rm(list = ls())

setwd("C:\\Users\\Melanie\\Desktop\\GL-HS-Resilience")


#-----------------------------------------------------------
# load/format data
# ----------------------------------------------------------

weights <- read.csv("data\\RFMOperFAO.csv")
scores <- read.csv("data\\RFMOscores.csv")

#-----------------------------------------------------------
# calculate regional score based on weighted RFMOs
# ----------------------------------------------------------

weights <- melt(weights, id=c("rgn_name", "rgn_id"))
FAO_RFMO_scores <- weights %.%
  rename(c(variable="RFMO", value="PropArea", rgn_name="rgn_nam", rgn_id="rgn_id_2013")) %.%
  filter( !(RFMO %in% "total")) %.%
  mutate(RFMO = as.factor(toupper(RFMO))) %.%
  left_join(scores, by="RFMO") %.%
  mutate(weightedScore=PropArea*Score) %.%
  group_by(rgn_nam, rgn_id_2013) %.%
  summarise(RegionScore=weighted.mean(weightedScore, PropArea))

FAO_RFMO_scores$RegionScore[is.na(FAO_RFMO_scores$RegionScore)] <- 0 

#-----------------------------------------------------------
# save data
# ----------------------------------------------------------
# merge with full ID
ids <- read.csv("C:\\Users\\Melanie\\Desktop\\GL-NCEAS-Regions_v2014\\FAOregions.csv")
FAO_RFMO_scores <- merge(ids, FAO_RFMO_scores, by=c("rgn_id_2013", "rgn_nam"))

write.csv(FAO_RFMO_scores, "data\\FAO_scores.csv", row.names=FALSE)
