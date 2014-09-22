###################################################
## Calculating pressures (standardizing zonal data)
###################################################

require(dplyr)

### This all needs to be cleaned, but that will require me running the data again!
### For now:
# Replace sp_id so that it equals:
#   rgn_id for eez's and fao's
#   new ccamlr sp_id values   

# new ccamlr ID
ccamlr_id <- read.csv("N:/git-annex/Global/NCEAS-Antarctica-Other_v2014/rgn_labels_ccamlr.csv")
ccamlr_id <- ccamlr_id %.%
  select(new_sp_id = sp_id, sp_name)


## SST ----
new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\SST_ZonalMean.csv") 
new <- merge(new, ccamlr_id, all.x=TRUE, by="sp_name")
new$sp_id <- ifelse(new$sp_type %in% "ccamlr", new$new_sp_id, new$rgn_id)

new <- new %.% 
  group_by(sp_id, sp_type) %.%
  summarize(mean = weighted.mean(mean, area_km2)) %.%
  mutate(standardizedMean = (abs(mean) / (313 * 1.10)))

new_HS <- new %.%
  filter(sp_type == "fao") %.%
  select(rgn_id=sp_id, pressure_score=standardizedMean)
write.csv(new_HS, "N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\data\\cc_sst_2013_HS.csv", row.names=FALSE)


new_AQ <- new %.%
  filter(sp_type == "ccamlr") %.%
  select(sp_id, pressure_score=standardizedMean)
write.csv(new_AQ, "N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\data\\cc_sst_2013_AQ.csv", row.names=FALSE)



## pH ----

new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\acid_ZonalMean.csv") 
new <- merge(new, ccamlr_id, all.x=TRUE, by="sp_name")
new$sp_id <- ifelse(new$sp_type %in% "ccamlr", new$new_sp_id, new$rgn_id)

new <- new %.% 
  group_by(sp_id, sp_type) %.%
  summarize(mean = weighted.mean(mean, area_km2))

new_HS <- new %.%
  filter(sp_type == "fao") %.%
  select(rgn_id=sp_id, pressure_score=mean)
write.csv(new_HS, "N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\data\\cc_acid_2013_HS.csv", row.names=FALSE)

new_AQ <- new %.%
  filter(sp_type == "ccamlr") %.%
  select(sp_id, pressure_score=mean)
write.csv(new_AQ, "N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\data\\cc_acid_2013_AQ.csv", row.names=FALSE)


## UV ----
## No UV for Antarctica

new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\uv_ZonalMean.csv") 

new <- merge(new, ccamlr_id, all.x=TRUE, by="sp_name")
new$sp_id <- ifelse(new$sp_type %in% "ccamlr", new$new_sp_id, new$rgn_id)

new <- new %.% 
  group_by(sp_id, sp_type) %.%
  summarize(mean = weighted.mean(mean, area_km2)) %.%
  mutate(standardizedMean = mean / (0.945994 * 1.10))

new_HS <- new %.%
  filter(sp_type == "fao") %.%
  select(rgn_id=sp_id, pressure_score=standardizedMean)
write.csv(new_HS, "N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\data\\cc_uv_2013_HS.csv", row.names=FALSE)



## Commercial low bycatch ----
new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\CommLBC_ZonalMean.csv") 
new <- merge(new, ccamlr_id, all.x=TRUE, by="sp_name")
new$sp_id <- ifelse(new$sp_type %in% "ccamlr", new$new_sp_id, new$rgn_id)

new <- new %.% 
  group_by(sp_id, sp_type) %.%
  summarize(mean_fp_com_lb_dem_nd_2013_rescaled = weighted.mean(fp_com_lb_dem_nd_2013_rescaled, area_km2),
            mean_fp_com_lb_pel_2013_rescaled = weighted.mean(fp_com_lb_pel_2013_rescaled, area_km2)) %.%
  mutate(stMean_com_lb_dem_nd_2013_rescaled = mean_fp_com_lb_dem_nd_2013_rescaled / (0.214706 * 1.10),
         stMean_fp_com_lb_pel_2013_rescaled = mean_fp_com_lb_pel_2013_rescaled / (0.214997 * 1.10)) %.%
  mutate(pressure_score= (stMean_com_lb_dem_nd_2013_rescaled + stMean_fp_com_lb_pel_2013_rescaled)/2)
  
new_HS <- new %.%
  filter(sp_type == "fao") %.%
  select(rgn_id=sp_id, pressure_score)
write.csv(new_HS, "N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\data\\fp_com_lb_2013_HS.csv", row.names=FALSE)

new_AQ <- new %.%
  filter(sp_type == "ccamlr") %.%
  select(sp_id, pressure_score)
write.csv(new_AQ, "N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\data\\fp_com_lb_2013_AQ.csv", row.names=FALSE)



## Commercial high bycatch ----
new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\CommHBC_ZonalMean.csv") 
new <- merge(new, ccamlr_id, all.x=TRUE, by="sp_name")
new$sp_id <- ifelse(new$sp_type %in% "ccamlr", new$new_sp_id, new$rgn_id)

new <- new %.% 
  group_by(sp_id, sp_type) %.%
  summarize(mean_fp_com_hb_dem_2013_rescaled = weighted.mean(fp_com_hb_dem_2013_rescaled, area_km2),
            mean_fp_com_hb_dem_nd_2013_rescaled = weighted.mean(fp_com_hb_dem_nd_2013_rescaled, area_km2),
            mean_fp_com_hb_pel_2013_rescaled = weighted.mean(fp_com_hb_pel_2013_rescaled, area_km2)) %.%
  mutate(stMean_fp_com_hb_dem_2013_rescaled = mean_fp_com_hb_dem_2013_rescaled / (0.244261 * 1.10),
         stMean_fp_com_hb_dem_nd_2013_rescaled = mean_fp_com_hb_dem_nd_2013_rescaled / (0.315616 * 1.10),
         stMean_fp_com_hb_pel_2013_rescaled = mean_fp_com_hb_pel_2013_rescaled / (0.388111 * 1.10)) %.%
  mutate(pressure_score= (stMean_fp_com_hb_dem_2013_rescaled + stMean_fp_com_hb_dem_nd_2013_rescaled + stMean_fp_com_hb_pel_2013_rescaled)/3)

new_HS <- new %.%
  filter(sp_type == "fao") %.%
  select(rgn_id=sp_id, pressure_score)
write.csv(new_HS, "N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\data\\fp_com_hb_2013_HS.csv", row.names=FALSE)

new_AQ <- new %.%
  filter(sp_type == "ccamlr") %.%
  select(sp_id, pressure_score)
write.csv(new_AQ, "N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\data\\fp_com_hb_2013_AQ.csv", row.names=FALSE)


## Shipping ----
#not included as a pressure in HS analysis
# This is actually a proxy for chemicals.

new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\shipping_ZonalMean.csv") 
new <- merge(new, ccamlr_id, all.x=TRUE, by="sp_name")
new$sp_id <- ifelse(new$sp_type %in% "eez-ccamlr", new$new_sp_id, new$rgn_id)

new <- new %.% 
  group_by(sp_id, sp_type) %.%
  summarize(mean = weighted.mean(mean, area_km2)) 

scaler <- max(new$mean, na.rm=TRUE) # 0.9301

new <- new %.%
  mutate(standardizedMean = mean / (scaler * 1.10))

new_AQ <- new %.%
  filter(sp_type == "eez-ccamlr") %.%
  select(sp_id, pressure_score=standardizedMean)
#write.csv(new_AQ, "N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\data\\po_chemicals_2013_AQ.csv", row.names=FALSE)


## SLR ----
#not included as a pressure in HS analysis
# should it be NA for ccamlr subregions without coastline?
coastline <- c(248100, 248200, 248500, 248600, 258410, 258420, 288100, 288200, 288300)

new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\slr_ZonalMean.csv") 
new <- merge(new, ccamlr_id, all.x=TRUE, by="sp_name")
new$sp_id <- ifelse(new$sp_type %in% "ccamlr", new$new_sp_id, new$rgn_id)

new <- new %.% 
  group_by(sp_id, sp_type) %.%
  summarize(mean = weighted.mean(mean, area_km2)) %.% 
  mutate(standardizedMean = ifelse(mean<0, 0, mean) / (0.284222 * 1.10))

new_AQ <- new %.%
  filter(sp_type == "ccamlr") %.%
  select(sp_id, pressure_score=standardizedMean)

new_AQ$pressure_score <- ifelse(new_AQ$sp_id %in% coastline, new_AQ$pressure_score, NA)

#write.csv(new_AQ, "N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\data\\cc_slr_2013_AQ.csv", row.names=FALSE)

