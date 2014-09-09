###################################################
## comparing zonal data for pressures data
## Ben's data vs. mine to make sure I have the correct data
###################################################

library(foreign)
library(dplyr)

## SST ----

new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\SST_ZonalMean.csv") 
new <- new %.% 
  filter(sp_type %in% c("eez", "fao")) %.%
  group_by(rgn_id) %.%
  summarize(mean = weighted.mean(mean, area_km2))
new <- data.frame(new)

old <- read.dbf("N:\\model\\GL-NCEAS-Pressures_v2013a\\tmp\\rgn_fao_mol_sst_05_10-82_86i_mol.dbf")
old <- old %.%
  select(rgn_id=VALUE, oldMean=MEAN) %.%
  left_join(new, by="rgn_id") %.%
  filter(rgn_id != 213)
plot(old$oldMean, old$mean)


## pH ----

new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\acid_ZonalMean.csv") 
new <- new %.% 
  filter(sp_type %in% c("eez", "fao")) %.%
  group_by(rgn_id) %.%
  summarize(mean = weighted.mean(mean, area_km2))
new <- data.frame(new)

old <- read.dbf("N:\\model\\GL-NCEAS-Pressures_v2013a\\tmp\\rgn_fao_mol_masked_impacts_acid.dbf")
old <- old %.%
  select(rgn_id=VALUE, oldMean=MEAN) %.%
  left_join(new, by="rgn_id") %.%
  filter(rgn_id != 213)
plot(old$oldMean, old$mean)


## UV ----

new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\uv_ZonalMean.csv") 
new <- new %.% 
  filter(sp_type %in% c("eez", "fao")) %.%
  group_by(rgn_id) %.%
  summarize(mean = weighted.mean(mean, area_km2))
new <- data.frame(new)

old <- read.dbf("N:\\model\\GL-NCEAS-Pressures_v2013a\\tmp\\rgn_fao_mol_omi_aura_uv_anomaly_2008m01-2012m12_trans.dbf")
old <- old %.%
  select(rgn_id=VALUE, oldMean=MEAN) %.%
  left_join(new, by="rgn_id") %.%
  filter(rgn_id != 213)
plot(old$oldMean, old$mean)


## Commercial low bycatch ----

new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\CommLBC_ZonalMean.csv") 
new <- new %.% 
  filter(sp_type %in% c("eez", "fao")) %.%
  group_by(rgn_id) %.%
  summarize(mean = weighted.mean(fp_com_lb_dem_nd_2013_rescaled, area_km2))
new <- data.frame(new)

old <- read.dbf("N:\\model\\GL-NCEAS-Pressures_v2013a\\tmp\\rgn_fao_mol_fp_com_lb_dem_nd_2013_rescaled.dbf")
old <- old %.%
  select(rgn_id=VALUE, oldMean=MEAN) %.%
  left_join(new, by="rgn_id") %.%
  filter(rgn_id != 213)
plot(old$oldMean, old$mean)


new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\CommLBC_ZonalMean.csv") 
new <- new %.% 
  filter(sp_type %in% c("eez", "fao")) %.%
  group_by(rgn_id) %.%
  summarize(mean = weighted.mean(fp_com_lb_pel_2013_rescaled, area_km2))
new <- data.frame(new)

old <- read.dbf("N:\\model\\GL-NCEAS-Pressures_v2013a\\tmp\\rgn_fao_mol_fp_com_lb_pel_2013_rescaled.dbf")
old <- old %.%
  select(rgn_id=VALUE, oldMean=MEAN) %.%
  left_join(new, by="rgn_id") %.%
  filter(rgn_id != 213)
plot(old$oldMean, old$mean)

## Commercial high bycatch ----

new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\CommHBC_ZonalMean.csv") 
new <- new %.% 
  filter(sp_type %in% c("eez", "fao")) %.%
  group_by(rgn_id) %.%
  summarize(mean = weighted.mean(fp_com_hb_dem_2013_rescaled, area_km2))
new <- data.frame(new)

old <- read.dbf("N:\\model\\GL-NCEAS-Pressures_v2013a\\tmp\\rgn_fao_mol_fp_com_hb_dem_2013_rescaled.dbf")
old <- old %.%
  select(rgn_id=VALUE, oldMean=MEAN) %.%
  left_join(new, by="rgn_id") %.%
  filter(rgn_id != 213)
plot(old$oldMean, old$mean)

###

new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\CommHBC_ZonalMean.csv") 
new <- new %.% 
  filter(sp_type %in% c("eez", "fao")) %.%
  group_by(rgn_id) %.%
  summarize(mean = weighted.mean(fp_com_hb_dem_nd_2013_rescaled, area_km2))
new <- data.frame(new)

old <- read.dbf("N:\\model\\GL-NCEAS-Pressures_v2013a\\tmp\\rgn_fao_mol_fp_com_hb_dem_nd_2013_rescaled.dbf")
old <- old %.%
  select(rgn_id=VALUE, oldMean=MEAN) %.%
  left_join(new, by="rgn_id") %.%
  filter(rgn_id != 213)
plot(old$oldMean, old$mean)

###

new <- read.csv("N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\tmp\\CommHBC_ZonalMean.csv") 
new <- new %.% 
  filter(sp_type %in% c("eez", "fao")) %.%
  group_by(rgn_id) %.%
  summarize(mean = weighted.mean(fp_com_hb_pel_2013_rescaled, area_km2))
new <- data.frame(new)

old <- read.dbf("N:\\model\\GL-NCEAS-Pressures_v2013a\\tmp\\rgn_fao_mol_fp_com_hb_pel_2013_rescaled.dbf")
old <- old %.%
  select(rgn_id=VALUE, oldMean=MEAN) %.%
  left_join(new, by="rgn_id") %.%
  filter(rgn_id != 213)
plot(old$oldMean, old$mean)
