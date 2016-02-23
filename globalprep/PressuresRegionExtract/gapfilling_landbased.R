library(ohicore)

fert <- read.csv('Y:/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/step1/output/2012/fao_fert.csv')
pest <- read.csv('Y:/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/step1/output/2012/fao_pest.csv')
all <- read.csv('Y:/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/step1/output/2012/fert_pest.csv') %>%
  dplyr::select(country.code.fao, fert_all = tfc.mean, pest_all = tpc.mean)

codes <- read.csv('Y:/mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/work/land_based/before_2007/step1/input/country_codes_fao.csv') %>%
  select(country.code.fao=country.codes, Area)

regions <- read.csv('C:/Users/Melanie/github/ohi-global/eez2015/layers/rgn_global.csv')

data <- full_join(fert, pest) %>%
  full_join(all) %>%
  left_join(codes) %>%
  filter(!is.na(fert_all))

fert <- data %>%
  mutate(gapfill = ifelse(is.na(tfc.mean), 1, 0)) %>%
  select(Area, gapfill)

fert <- name_to_rgn(fert, fld_name = 'Area', fld_value = 'gapfill', dir_lookup="C:/Users/Melanie/github/ohiprep/src/LookupTables") %>%
  full_join(regions) %>%
  mutate(is.na(gapfill), 1, gapfill) %>%
  select(rgn_id, gapfill)

write.csv(fert, 
          "C:/Users/Melanie/github/ohiprep/globalprep/PressuresRegionExtract/data/cw_ferilizer_score_2015_gf.csv",
          row.names=FALSE)

pest <- data %>%
  mutate(gapfill = ifelse(is.na(tpc.mean), 1, 0)) %>%
  select(Area, gapfill)

pest <- name_to_rgn(pest, fld_name = 'Area', fld_value = 'gapfill', dir_lookup="C:/Users/Melanie/github/ohiprep/src/LookupTables") %>%
  full_join(regions) %>%
  mutate(is.na(gapfill), 1, gapfill) %>%
  select(rgn_id, gapfill)

write.csv(pest, 
          "C:/Users/Melanie/github/ohiprep/globalprep/PressuresRegionExtract/data/cw_chemical_score_2015_gf.csv",
          row.names=FALSE)