library(data.table) # for fread()
library(readr)      # for read_csv()

setwd('~/github/ohiprep')
source('src/R/common.R')

goal     <- 'globalprep/SPP_ICO'
scenario <- 'v2015'
dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal) 
dir_data_am    <- file.path(dir_neptune_data, 'git-annex/globalprep/_raw_data', 'aquamaps/v2014') 
dir_data_iucn  <- file.path(dir_neptune_data, 'git-annex/globalprep/_raw_data', 'iucn_spp') 
dir_git  <- file.path('~/github/ohiprep', goal)

source(file.path(dir_git, 'R/spp_fxn.R'))

spp_all <- create_spp_master_lookup(reload = FALSE)

spp_am_iucn <- spp_all %>%
  filter(str_detect(spatial_source, 'iucn') & !is.na(am_sid))
### These will have an IUCN shapefile (thus the spatial source) as
### well as an Aquamaps map (thus the AM species ID).
### 1958 species in this list

spp_am_iucn <- spp_am_iucn %>%
  filter(!str_detect(spatial_source, 'subpop')) %>%
  filter(!str_detect(spatial_source, 'alias'))
### filter out the subpopulations and aliases
### 1923 species in this list

length(unique(spp_am_iucn$sciname))
### 1923 unique species names

length(unique(spp_am_iucn$am_sid))
### 1923 unique Aquamaps species IDs

length(unique(spp_am_iucn$iucn_sid))
### 1919 unique IUCN species IDs

dupes <- show_dupes(spp_am_iucn, 'iucn_sid')
### IUCN spp ID numbers that cover two different AM species...?

dupes %>% 
  select(-parent_sid, -subpop_sid, -info_source, -am_category, -iucn_category, -id_no) %>% 
  arrange(iucn_sid)
#    am_sid                   sciname iucn_sid popn_trend popn_category                     spp_group spatial_source category_score trend_score
# Fis-25402     Epinephelus goreensis   132817 Decreasing            LC                      GROUPERS           iucn              0        -0.5
# Fis-25570     Epinephelus fasciatus   132817 Decreasing            LC                      GROUPERS           iucn              0        -0.5
# Fis-26116    Cephalopholis aurantia   132825    Unknown            LC                      GROUPERS           iucn              0          NA
# Fis-26127 Cephalopholis spiloparaea   132825    Unknown            LC                      GROUPERS           iucn              0          NA
# Fis-26145     Epinephelus undulosus   132830     Stable            LC                      GROUPERS           iucn              0         0.0
# Fis-27382 Mycteroperca acutirostris   132830     Stable            LC                      GROUPERS           iucn              0         0.0
# Fis-25907          Zebrasoma scopas   178005     Stable            LC SURGEONFISH_TANGS_UNICORNFISH           iucn              0         0.0
# Fis-26618        Zebrasoma gemmatum   178005     Stable            LC SURGEONFISH_TANGS_UNICORNFISH           iucn              0         0.0

write_csv(spp_am_iucn, file.path(dir_git, 'vAM_IUCN/data/spp_am_v_iucn.csv'))
