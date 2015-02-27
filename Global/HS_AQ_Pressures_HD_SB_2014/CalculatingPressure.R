######################################################
## Calculating HD_subtidal_SB_v2013 for High Seas
######################################################

library(dplyr)
#library(reshape)
library(stringr)

rm(list=ls())
#zonal data calculations in SoftBottomZonal.R (ohiprep/Global/HS_AQ_Pressures_2013_HD_subtidal_SB2013)

######################################
## Area of soft bottom habitat ----
###################################### 

## checked estimated aree of SB against total area of FAOs.  Rnged from 78%-100% soft-bottom.  
## With this metric, the arctic has slightly more SB area than the total area
## Could be due to small rounding error.  (going with this because the relative measure is improtant here.)
area <- read.csv("N:\\model\\GL-HS-pressure_HD_subtidal_SB_v2013\\tmp\\SoftBottom_ZonalSum.csv")
area2 <- area %.%
  #filter(sp_type %in% c("ccamlr", "fao")) %.% # decided to weight on all regions
  mutate(rgn_id=ifelse(rgn_id==213, as.character(sp_name), rgn_id)) %.%
  mutate(rgn_id = gsub("CCAMLR ", "", rgn_id)) %.%
  mutate(rgn_id = gsub("\\.", "", rgn_id)) %.%
  mutate(rgn_id = gsub("a", "1", rgn_id)) %.%
  mutate(rgn_id = as.numeric(gsub("b", "2", rgn_id))) %.%
  mutate(sp_type = as.character(sp_type)) %.%
  select(sp_type, sp_name, rgn_id, subtidal=masked_ecosystems_s_t_s_bottom, 
         shelf=masked_ecosystems_soft_shelf,
         slope=masked_ecosystems_soft_slope,
         deep=masked_ecosystems_d_s_bottom) %.%
  mutate(totalCells=subtidal + shelf + slope + deep) %.%
  mutate(areaSB_km2 = totalCells*934.4789^2/1000000) %.%
  select(rgn_id, areaSB_km2) %.%  
  group_by(rgn_id) %.%
  summarize(areaSB_km2=sum(areaSB_km2))
         


###################################################
## Proportion of destructive demersal fisheries ----
###################################################

gear_dem_d <- read.csv("N:\\model\\GL-HS-pressure_HD_subtidal_SB_v2013\\tmp\\Gear_ZonalSum_catch_dem_d_gcs.csv")
gear_dem_nd_hb <- read.csv("N:\\model\\GL-HS-pressure_HD_subtidal_SB_v2013\\tmp\\Gear_ZonalSum_catch_dem_nd_hb_gcs.csv")
gear_dem_nd_lb <- read.csv("N:\\model\\GL-HS-pressure_HD_subtidal_SB_v2013\\tmp\\Gear_ZonalSum_catch_dem_nd_lb_gcs.csv")
gear_pel_hb <- read.csv("N:\\model\\GL-HS-pressure_HD_subtidal_SB_v2013\\tmp\\Gear_ZonalSum_catch_pel_hb_gcs.csv")
gear_pel_lb <- read.csv("N:\\model\\GL-HS-pressure_HD_subtidal_SB_v2013\\tmp\\Gear_ZonalSum_catch_pel_lb_gcs.csv")

gear <- merge(gear_dem_d, gear_dem_nd_hb)
gear <- merge(gear, gear_dem_nd_lb)
gear <- merge(gear, gear_pel_hb)
gear <- merge(gear, gear_pel_lb)

gear2 <- gear %.%
  filter(sp_type %in% c("eez", "fao", "eez-ccamlr")) %.%
  mutate(rgn_id=ifelse(rgn_id==213, as.character(sp_name), rgn_id)) %.%
  mutate(rgn_id = gsub("CCAMLR ", "", rgn_id)) %.%
  mutate(rgn_id = gsub("\\.", "", rgn_id)) %.%
  mutate(rgn_id = gsub("a", "1", rgn_id)) %.%
  mutate(rgn_id = as.numeric(gsub("b", "2", rgn_id))) %.%
  mutate(sp_type = as.character(sp_type)) %.%
  select(sp_type, sp_name, rgn_id, area_km2, catch_pel_lb_gcs, 
         catch_pel_hb_gcs, catch_dem_d_gcs, 
         catch_dem_nd_lb_gcs, catch_dem_nd_hb_gcs) %.%
  mutate(propDest = catch_dem_d_gcs / (catch_pel_lb_gcs + catch_dem_d_gcs +
         catch_pel_hb_gcs + catch_dem_nd_lb_gcs + catch_dem_nd_hb_gcs)) %.%
  group_by(sp_type, rgn_id) %.%
  summarize(propDest=weighted.mean(propDest, area_km2))


gear2$propDest[gear2$propDest == "NaN"]  <- 0


###################################################
## Total Catch (2011) ----
###################################################

# catch for fao regions
catch_fao <- read.csv("N:\\model\\GL-HS-AQ-Fisheries_v2013\\HighSeas\\raw\\Extension_redo_withFlag.csv")
regions_fao <- read.csv("N:\\model\\GL-HS-AQ-Fisheries_v2013\\HighSeas\\data\\FAOregions.csv")
regions_fao <- regions_fao %.%
  select(rgn_id=rgn_id_2013, rgn_typ, rgn_nam, FAO=fao_id) %.%
  mutate(FAO=as.character(FAO))

catch_fao2 <- catch_fao %.%
  filter(EEZ==0,
         IYear==2011) %.%
  group_by(FAO) %.%
  summarize(TotalCatch=sum(Catch, na.rm=TRUE)) %.%
  mutate(FAO=as.character(FAO)) %.%
  left_join(regions_fao, by="FAO") %.%
  select(rgn_id, TotalCatch) %.%
  filter(!(rgn_id %in% c(268, 271, 278))) #cut the Antarctica regions that are covered below in the CCAMLR regions
  
  
# catch for CCAMLR regions
catch_ccamlr <- read.csv("N:\\model\\GL-HS-AQ-Fisheries_v2013\\Antarctica\\raw\\CCAMLR_w_update.csv")

catch_ccamlr2 <- catch_ccamlr %.%
  filter(season.year==2011) %.%
  select(rgn_id=ASD, Catch=corr.Catch) %.%
  mutate(rgn_id=str_trim(rgn_id)) %.%
  mutate(rgn_id = gsub("a", "1", rgn_id)) %.%
  mutate(rgn_id = as.numeric(gsub("b", "2", rgn_id))) %.%
  group_by(rgn_id) %.%
  summarize(TotalCatch=sum(Catch, na.rm=TRUE))


# catch for EEZ regions
saup2region  <- read.csv("N:\\model\\GL-NCEAS_FIS_v2013a\\RevisingFIS\\data\\snk_fis_proparea_saup2rgn_lyr.csv") #weight catch by area of saup in eez
catch_eez <- read.csv("N:\\model\\GL-NCEAS_FIS_v2013a\\RevisingFIS\\raw\\Extension_redo_withFlag.csv")

catch_eez2 <- catch_eez %.%
  filter(EEZ!=0,
         IYear==2011) %.%
  group_by(EEZ) %.%
  summarize(TotalCatch=sum(Catch, na.rm=TRUE)) %.%
  select(saup_id=EEZ, TotalCatch) %.%
  left_join(saup2region, by="saup_id") %.%
  filter(!is.na(rgn_id)) %.%
  group_by(rgn_id) %.%
  summarize(TotalCatch = sum(TotalCatch*prop_area))
  
catch2 <- rbind(catch_ccamlr2, catch_eez2, catch_fao2)

## add in some regions with zero catch:
catchZero <- expand.grid(rgn_id=c(485, 58441, 30, 33, 34, 35, 148, 157, 221, 227, 228, 237, 244, 245, 248, 249, 260), TotalCatch=0)
catch2 <- rbind(catch2, catchZero)

###################################################
# calculate Destructive Catch
###################################################
setdiff(catch2$rgn_id, area2$rgn_id) # 71,72,74, 75 were excluded from the regions data used to extract fis data...this has been corrected and these data should probably be re-run so that these regions are included.
setdiff(catch2$rgn_id, gear2$rgn_id) #Bosnia (232) is so small, it was not captured in gear
setdiff(area2$rgn_id, catch2$rgn_id)
setdiff(gear2$rgn_id, catch2$rgn_id)
setdiff(area2$rgn_id, gear2$rgn_id) #Bosnia (232) not captured in gear 2
setdiff(gear2$rgn_id, area2$rgn_id)

HD_SB <- catch2 %.%
  left_join(gear2, by=c("rgn_id"))%.%
  mutate(DestCatch=TotalCatch*propDest) %.%
  left_join(area2, by=c("rgn_id"))%.%  
  mutate(DestCatch_SB = ifelse(areaSB_km2==0, 0, DestCatch/areaSB_km2)) %.%
  mutate(pressure_score=DestCatch_SB/(max(DestCatch_SB, na.rm=TRUE)*1.10))

#write.csv(HD_SB, "tmp//rawData.csv", row.names=FALSE)

HD_SB <- HD_SB %.% 
  filter(sp_type=="fao") %.%
  select(rgn_id, pressure_score)
#write.csv(HD_SB, "data//po_rgn_hd_subtidal_sb_2013a_HS.csv", row.names=FALSE)
