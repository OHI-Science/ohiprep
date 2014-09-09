######  Looking at raw data for Fisheries   ##################
rm(list=ls())
library(dplyr)


regions <- read.csv("C:\\Users\\Melanie\\Desktop\\GL-NCEAS-Regions_v2014\\FAOregions.csv")

catch1 <- read.csv("N:\\model\\GL-HS-AQ-Fisheries_v2013\\HighSeas\\qa_qc\\AssessedCatches.csv")
catch2 <- read.csv("N:\\model\\GL-HS-AQ-Fisheries_v2013\\HighSeas\\qa_qc\\UnassessedCatches.csv")

catch1 <- catch1 %.%
  select(fao_id, year, taxon_name, TaxonKey, b_bmsy, mean_catch)%.%
  left_join(regions) %.%
  select(fao_id, rgn_nam, rgn_id_2013, year, taxon_name, TaxonKey, b_bmsy, mean_catch)

catch2 <- catch2 %.%
  mutate(b_bmsy=ifelse(TaxonPenaltyCode == 6, Medianb_bmsy, Minb_bmsy)) %.%
  left_join(regions) %.%
  select(fao_id, rgn_nam, rgn_id_2013, year, taxon_name, TaxonKey, b_bmsy, mean_catch)

allCatch <- rbind(catch1, catch2)
allCatch <- allCatch[!(is.na(allCatch$b_bmsy)), ]

allCatch_summary <- allCatch %.%
  group_by(fao_id, year) %.%
  summarize(weighted_b_bmsy=weighted.mean(b_bmsy, mean_catch)) %.%
  filter(year %in% "2011") %.%
  left_join(regions) %.%
  select(rgn_id_2013, fao_id, rgn_nam, year, weighted_b_bmsy)

write.csv(allCatch_summary, "N:\\model\\GL-HS-AQ-Fisheries_v2013\\HighSeas\\qa_qc\\b_bmsy_allcatch.csv", row.names=FALSE)

catch1_summary <- catch1 %.%
  group_by(fao_id, year) %.%
  summarise(weighted_b_bmsy=weighted.mean(b_bmsy, mean_catch)) %.%
  filter(year==2011) %.%
  left_join(regions) %.%
  select(rgn_id_2013, fao_id, rgn_nam, year, weighted_b_bmsy)

write.csv(catch1_summary, "N:\\model\\GL-HS-AQ-Fisheries_v2013\\HighSeas\\qa_qc\\b_bmsy_evaluatedCatch.csv", row.names=FALSE)
