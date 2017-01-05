
source('~/github/ohiprep/src/R/common.R')

##################################################

### Species counts 2015

dir_goal_anx_15 <- '/home/shares/ohi/git-annex/globalprep/spp_ico/v2015'
spp_2015 <- read_csv(file.path(dir_goal_anx_15, 'int/spp_all.csv')) %>%
  select(am_sid, iucn_sid, sciname,
         popn_category, spp_group, spatial_source, category_score, trend_score) %>%
  filter(!is.na(spatial_source)) %>%
  distinct()

### IUCN species 2015
iucn_spp_list_2015 <- read_csv(file.path(dir_goal_anx_15, 'int/spp_iucn_marine.csv')) %>%
  select(class, order, family, iucn_sid)
spp_iucn_2015 <- spp_2015 %>%
  filter(str_detect(spatial_source, 'iucn')) %>%
  select(iucn_sid) %>%
  left_join(iucn_spp_list_2015, by = 'iucn_sid') %>%
  distinct()

### AM species 2015
am_spp_list_2015 <- read_csv(file.path(dir_M, 'git-annex/globalprep',
                                       '_raw_data/aquamaps/d2014', 
                                       'tables/ohi_speciesoccursum.csv'))
names(am_spp_list_2015) <- tolower(names(am_spp_list_2015))
am_spp_list_2015 <- am_spp_list_2015 %>%
  select(am_sid = speciesid, 
         kingdom, phylum, class, order, family)

spp_am_2015 <- spp_2015 %>%
  filter(str_detect(spatial_source, 'am')) %>%
  select(am_sid) %>%
  left_join(am_spp_list_2015, by = 'am_sid') %>%
  distinct()

### 2015: AM: 2002; IUCN: 3469
### 5471 species in 2015

##################################################

### Species counts 2016:

dir_goal_anx_16 <- '/home/shares/ohi/git-annex/globalprep/spp_ico/v2016'
spp_2016 <- read_csv(file.path(dir_goal_anx_16, 'int/spp_all_cleaned.csv')) %>%
  select(am_sid, iucn_sid, sciname, iucn_subpop,
         pop_cat, spp_group, spatial_source, cat_score, trend_score) %>%
  filter(!is.na(spatial_source)) %>%
  filter(!is.na(cat_score))
spp_2016_subpops <- spp_2016 %>%
  filter(sciname %in% (spp_2016 %>% filter(!is.na(iucn_subpop)) %>% .$sciname)) %>%
  group_by(sciname) %>%
  summarize(iucn_sid = first(iucn_sid))
spp_2016a <- spp_2016 %>%
  filter(!(sciname %in% spp_2016_subpops$sciname & !iucn_sid %in% spp_2016_subpops$iucn_sid))

### IUCN species 2016
iucn_spp_list_2016 <- read_csv(file.path(dir_goal_anx_16, 'int/spp_iucn_marine.csv')) %>%
  select(class, order, family, iucn_sid)
spp_iucn_2016 <- spp_2016a %>%
  filter(str_detect(spatial_source, 'iucn')) %>%
  select(iucn_sid, spatial_source) %>%
  left_join(iucn_spp_list_2016, by = 'iucn_sid') %>%
  distinct()

### AM species 2016
am_spp_list_2016 <- read_csv(file.path(dir_M, 'git-annex/globalprep',
                                       '_raw_data/aquamaps/d2015', 
                                       'csv/speciesoccursum.csv')) %>%
  select(am_sid = speciesid, 
         kingdom, phylum, class, order, family)

spp_am_2016 <- spp_2016a %>%
  filter(str_detect(spatial_source, 'am')) %>%
  select(am_sid) %>%
  left_join(am_spp_list_2016, by = 'am_sid') %>%
  distinct()

n_spp_2016 <- nrow(spp_am_2016) + 
              nrow(spp_iucn_2016 %>% filter(spatial_source == 'iucn')) +
              nrow(spp_iucn_2016 %>% filter(spatial_source == 'iucn-bli'))
### 2016: 3732 AM + 3209 IUCN + 856 BLI
### 7797 species in 2016

##################################################

### Species counts 2017:

dir_goal_17 <- 'globalprep/spp_ico/v2017'
dir_goal_anx_17 <- '/home/shares/ohi/git-annex/globalprep/spp_ico/v2017'

spp_2017 <- read_csv(file.path(dir_goal_17, 'int', 'spp_list_scored.csv')) %>%
  filter(year == max(year))

### IUCN species 2017
iucn_spp_list_2017 <- read_csv(file.path(dir_goal_anx_17, 'int/spp_info_from_api.csv')) %>%
  select(kingdom, phylum, class, order, family, iucn_sid)
spp_iucn_2017 <- spp_2017 %>%
  filter(str_detect(spatial_source, 'iucn')) %>%
  select(map_iucn_sid, spp_group) %>% ### use map_iucn_sid to ditch subpops
  left_join(iucn_spp_list_2017, by = c('map_iucn_sid' = 'iucn_sid')) %>%
  distinct()

### AM species 2017
am_spp_list_2017 <- read_csv(file.path(dir_M, 'git-annex/globalprep',
                                       '_raw_data/aquamaps/d2015', 
                                       'csv/speciesoccursum.csv')) %>%
  select(am_sid = speciesid, 
         kingdom, phylum, class, order, family)

spp_am_2017 <- spp_2017 %>%
  filter(str_detect(spatial_source, 'am')) %>%
  select(am_sid) %>%
  left_join(am_spp_list_2017, by = 'am_sid') %>%
  distinct()

n_spp_2017 <- nrow(spp_am_2017) + 
  nrow(spp_iucn_2017 %>% filter(!str_detect(spp_group, 'BOTW'))) +
  nrow(spp_iucn_2017 %>% filter(str_detect(spp_group, 'BOTW')))
### 2017: 3497 AM + 4483 IUCN + 857 BLI

### 8837 species in 2017, using map_iucn_sid to count just parents

