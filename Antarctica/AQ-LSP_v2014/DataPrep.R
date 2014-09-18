##################################################
## Antarctica: LSP
## Creating data for the inland protected area 
## (all get a score of 1)
## May 1 2014
## MRF
##################################################

library(dplyr)


rm(list = ls())

setwd("N://model/GL-AQ-LSP_v2013")


## 
all_ccamlr <- read.csv("N://git-annex/Global/NCEAS-Antarctica-Other_v2014/rgn_labels_ccamlr.csv")

land_ccamlr <- read.csv("N:/git-annex/Global/NCEAS-Regions_v2014/data/sp_inland1km_ccamlr_gcs_data.csv")
land_ccamlr <- land_ccamlr %.%
  select(sp_id, ccamlr_area = area_km2)


## add zeros for years/regions without data
gaps <- expand.grid(type="cp", sp_id=all_ccamlr$sp_id, year=seq(1975,2012), score=1)
pa <- merge(gaps, land_ccamlr, by=c("sp_id"), all=TRUE)
pa <- pa %.%
  select(type, sp_id, year, score, ccamlr_area) 

pa$ccamlr_area <- ifelse(is.na(pa$ccamlr_area), 0, pa$ccamlr_area)
write.csv(pa, "tmp\\Antarctica_PA.csv", row.names=F)

