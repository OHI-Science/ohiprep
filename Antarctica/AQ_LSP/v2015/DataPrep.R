##################################################
## Antarctica: LSP
## May 1 2014
## Updated Feb 4 2016
## MRF
##################################################

## Creating data for the inland protected area 
## (all get a score of 1)

library(dplyr)

#### inland (1km) data prep
#### all inland is considered protected
#### Start data in 1974 (earliest MPA for Antarctica)

inland <- read.csv('Antarctica/Other/rgn_area_ccamlr_inland_1km_lyr.csv')
tmp <- expand.grid(sp_id = inland$sp_id, year = 1975:2015) %>%
  left_join(inland, by="sp_id") %>%
  mutate(area_km2 = ifelse(area_km2==0, NA, area_km2))
write.csv(tmp, "Antarctica/AQ_LSP/v2015/data/lsp_prot_area_inland_1km.csv", row.names=FALSE)

### offshore ccamlr protected areas

# convert region names:
area_ccamlr <- read.csv("Antarctica/Other_v2014/rgn_area_ccamlr_eez.csv") %>%
  select(CCAMLR_AREA = ccamlr_id2, sp_id) %>%
  mutate(CCAMLR_AREA = as.character(CCAMLR_AREA)) 

offshore <- read.csv('Antarctica/AQ-LSP_v2014/raw/Antarctica_MPAs.csv') %>% 
  mutate(CCAMLR_AREA = as.character(CCAMLR_AREA)) %>%
  left_join(area_ccamlr, by='CCAMLR_AREA') %>%
  select(sp_id, year=year_established, area_km2 = Area_Km2)
  
write.csv(offshore, "Antarctica/AQ-LSP_v2014/data/lsp_prot_area_offshore.csv", row.names=FALSE)

