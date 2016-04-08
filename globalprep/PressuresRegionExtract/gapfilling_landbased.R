library(ohicore)
library(dplyr)

fert <- data.frame()

for(year in 2012:(2012-4)){ #year=2012
tmp <- read.csv(sprintf('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/step1/output/%s/fao_fert.csv', year))
tmp$year <- year
fert <- rbind(fert, tmp)
}

pest <- data.frame()
for(year in 2012:(2012-4)){ #year=2012
tmp <- read.csv(sprintf('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/step1/output/%s/fao_pest.csv', year))
tmp$year <- year
pest <- rbind(pest, tmp)
}

all <- data.frame()
for(year in 2012:(2012-4)){
tmp <- read.csv(sprintf('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/step1/output/%s/fert_pest.csv', year)) %>%
  dplyr::select(country.code.fao, fert_all = tfc.mean, pest_all = tpc.mean)
tmp$year <- year
all <- rbind(all, tmp)
}


codes <- read.csv('/var/cache/halpern-et-al/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/step1/input/country_codes_fao.csv') %>%
  select(country.code.fao=country.codes, Area)

regions <- read.csv('../ohi-global/eez2015/layers/rgn_global.csv')
regions <- expand.grid(year = (2012-4):2012, rgn_id = regions$rgn_id)


data <- full_join(fert, pest) %>%
  full_join(all) %>%
  left_join(codes) %>%
  filter(!is.na(fert_all)) %>%
  filter(Area != "Sudan (former)")


#### figure out the fertilizer data:

fert <- data %>%
  mutate(gapfill = ifelse(is.na(tfc.mean), 1, 0)) %>%
  mutate(original_name = Area) %>%
  select(Area, original_name, gapfill, year)


cc_names <- name_to_rgn(filter(fert, year==2012), fld_name = 'Area', fld_value = c('gapfill', "original_name"), dir_lookup="src/LookupTables") %>%
  select(rgn_id, original_name)


fert <- fert %>%
  select(original_name = Area, gapfill, year) %>% 
  left_join(cc_names) %>%
  filter(!is.na(rgn_id)) %>%
  full_join(regions) %>%
  mutate(gapfill = ifelse(is.na(gapfill), 0, gapfill)) %>%  ## assuming that no reporting of any of the variables indicates true zero (not gapfilling)
  filter(rgn_id <= 250) %>%  # disputed areas
  filter(rgn_id != 213) %>%  # antarctica  
  select(rgn_id, year, gapfill)

fert_status <- fert %>%
  filter(year==2012) %>%
  select(rgn_id, gapfill) %>%
  arrange(rgn_id)

write.csv(fert_status, 
          "globalprep/PressuresRegionExtract/data/cw_fertilizer_score_2015_gf.csv",
          row.names=FALSE)


fert_trend <- fert %>%
  group_by(rgn_id) %>%
  summarize(gapfill = mean(gapfill, na.rm=TRUE)) %>%
  select(rgn_id, gapfill) %>%
  arrange(rgn_id)

write.csv(fert_trend, 
          "globalprep/PressuresRegionExtract/data/cw_fertilizer_trend_2015_gf.csv",
          row.names=FALSE)

#### figure out the pesticide data:
pest <- data %>%
  mutate(gapfill = ifelse(is.na(tpc.mean), 1, 0)) %>%
  mutate(original_name = Area) %>%
  select(Area, original_name, gapfill, year)

cc_names <- name_to_rgn(filter(pest, year==2012), fld_name = 'Area', fld_value = c('gapfill', "original_name"), dir_lookup="src/LookupTables") %>%
  select(rgn_id, original_name)


pest <- pest %>%
  select(original_name = Area, gapfill, year) %>% 
  left_join(cc_names) %>%
  filter(!is.na(rgn_id)) %>%
  full_join(regions) %>%
  mutate(gapfill = ifelse(is.na(gapfill), 0, gapfill)) %>%
  filter(rgn_id <= 250) %>%  # disputed areas
  filter(rgn_id != 213) %>%  # antarctica  
  select(rgn_id, year, gapfill)

pest_status <- pest %>%
  filter(year==2012) %>%
  select(rgn_id, gapfill) %>%
  arrange(rgn_id)

write.csv(pest_status, 
          "globalprep/PressuresRegionExtract/data/cw_pesticide_score_2015_gf.csv",
          row.names=FALSE)


pest_trend <- pest %>%
  group_by(rgn_id) %>%
  summarize(gapfill = mean(gapfill, na.rm=TRUE)) %>%
  select(rgn_id, gapfill) %>%
  arrange(rgn_id)

write.csv(pest_trend, 
          "globalprep/PressuresRegionExtract/data/cw_pesticide_trend_2015_gf.csv",
          row.names=FALSE)

### chemical: combination of pesticide, shipping, land-based inorganic - only pesticide is gapfilled
chem_status <- pest_status %>%
  mutate(gapfill = gapfill/3)
write.csv(chem_status, 
          "globalprep/PressuresRegionExtract/data/cw_chemical_score_2015_gf.csv",
          row.names=FALSE)
