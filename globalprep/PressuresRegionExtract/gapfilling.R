################################################
## Creating gapfilling layers for the pressures
## data based on raster layers
################################################

### Fisheries pressures
## There is a file describing gapfilling for this pressure: fisheries_gap_filling.csv
 
fish <- read.csv("globalprep/PressuresRegionExtract/data/fisheries_gap_filling.csv")

#high bycatch
hb <- fish %>%
  filter(layer == "catch_06_10_npp_hb") %>%
  filter(sp_type == "eez") %>%
  mutate(pressure_score = ifelse(gap_filled=="gap-filled", 1, 0)) %>%
  select(rgn_id, pressure_score)

write.csv(hb, "globalprep/PressuresRegionExtract/data/catch_06_10_npp_hb_eez_gf.csv", row.names=FALSE)

#low bycatch
lb <- fish %>%
  filter(layer == "catch_06_10_npp_lb") %>%
  filter(sp_type == "eez") %>%
  mutate(pressure_score = ifelse(gap_filled=="gap-filled", 1, 0)) %>%
  select(rgn_id, pressure_score)

write.csv(lb, "globalprep/PressuresRegionExtract/data/catch_06_10_npp_lb_eez_gf.csv", row.names=FALSE)


#### SST: No gapfilling
sst <- read.csv("globalprep/PressuresRegionExtract/data/sst_eez_2015.csv")
sst <- mutate(sst, pressure_score=0)
write.csv(sst, "globalprep/PressuresRegionExtract/data/sst_eez_2015_gf.csv", row.names=FALSE)

#### UV: No gapfilling
uv <- read.csv("globalprep/PressuresRegionExtract/data/uv_eez_2015.csv")
uv <- mutate(uv, pressure_score=0)
write.csv(uv, "globalprep/PressuresRegionExtract/data/uv_eez_2015_gf.csv", row.names=FALSE)


#### Trash: No gapfilling
trash <- read.csv("globalprep/PressuresRegionExtract/data/trash_eez_2015.csv")
trash <- mutate(trash, pressure_score=0)
write.csv(trash, "globalprep/PressuresRegionExtract/data/trash_eez_2015_gf.csv", row.names=FALSE)

#### SLR: spatial interpolation of raster data
slr <- read.csv("globalprep/PressuresRegionExtract/data/slr_gap_fill_attr.csv")%>%
  select(rgn_id, pressure_score = gap_filled)
write.csv(slr, "globalprep/PressuresRegionExtract/data/slr_eez_2015_gf.csv", row.names=FALSE)

#### acid: spatial interpolation of raster data
acid <- read.csv("globalprep/PressuresRegionExtract/data/acid_gap_fill_attr.csv")%>%
  select(rgn_id, pressure_score = gap_filled)
write.csv(acid, "globalprep/PressuresRegionExtract/data/acid_eez_2014_gf.csv", row.names=FALSE)

