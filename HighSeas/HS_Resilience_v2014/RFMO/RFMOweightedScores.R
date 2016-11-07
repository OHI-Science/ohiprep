############################################################
### Assign scores to RFMO proportional areas.
############################################################

library(dplyr)
library(reshape2)

rm(list = ls())

#-----------------------------------------------------------
# load/format data
# ----------------------------------------------------------

weights <- read.csv("HighSeas/HS_Resilience_v2014/RFMO/tmp/RFMOperFAO.csv")
scores <- read.csv("HighSeas/HS_Resilience_v2014/RFMO/tmp/RFMOscores.csv")

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
ids <- read.csv("HighSeas/HS_other_v2014/FAOregions.csv")
FAO_RFMO_scores <- merge(ids, FAO_RFMO_scores, by=c("rgn_id_2013", "rgn_nam"))

# for toolbox ingestion
data <- FAO_RFMO_scores %>%
  select(rgn_id=rgn_id_2013, resilience.score=RegionScore)

write.csv(data, "HighSeas/HS_Resilience_v2014/RFMO/data/rfmo_2014.csv", row.names=FALSE)
