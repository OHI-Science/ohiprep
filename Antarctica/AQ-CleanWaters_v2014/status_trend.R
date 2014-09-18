####################################################################
## CleanWaters (CW) calculation
## May 6 2014
## MRF
####################################################################
library(dplyr)


rm(list=ls())
setwd("N:\\model\\GL-AQ-CleanWaters_v2013")

### This goal is calculated using the pressures data for chemicals (shipping is proxy in AQ) and trash
chem <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\data\\po_chemicals_2013_AQ.csv")
trash <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\data\\po_trash_2013_AQ.csv")

chem <- chem %.%
  mutate(score_chem = 1 - pressure_score) %.%
  select(sp_id, score_chem)

trash  <- trash %.%
  mutate(score_trash = 1 - as.numeric(pressure_score)) %.%
  select(sp_id, score_trash)
 
scores <- trash %.%
  left_join(chem, by="sp_id") %.%
  mutate(status = round(sqrt(score_trash*score_chem) * 100, 2)) %.%
  select(sp_id, status)

write.csv(scores, "data\\CWstatus.csv", row.names=FALSE)

trend <- scores %.%
  mutate(trend = ifelse(round(status, 0)==100, 0, NA)) %.%
  select(sp_id, trend)

write.csv(trend, "data\\CWtrend.csv", row.names=FALSE)
