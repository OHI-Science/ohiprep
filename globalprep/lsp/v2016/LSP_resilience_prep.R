######################################################
## Calculating MPA for resilience for relevant years
## Also final formatting of area files to be consistent 
## with previous years
## MRF June 22 2016
#######################################################

library(dplyr)

### toolbox can only process one year of data for pressures/resilience, 
### so the scores need a bit of formatting

status <- read.csv("globalprep/lsp/v2016/output/lsp_status.csv")
summary(status)

for(year in 2011:2015){ #year=2011
  tmp <- status[status$year == year, ] %>%
    select(rgn_id, resilience.score = lsp_status) %>%
    mutate(resilience.score = resilience.score/100) %>%
    filter(rgn_id <= 250)
  write.csv(tmp, sprintf("globalprep/lsp/v2016/output/mpa_3nm_%s.csv", year), row.names=FALSE)
}


area_inland <- read.csv("globalprep/lsp/v2016/output/lsp_a_total_inland1km.csv") %>%
  select(rgn_id, area = a_tot_1km) %>%
  filter(rgn_id <= 250) %>%
  unique()

write.csv(area_inland, "globalprep/lsp/v2016/output/rgn_area_inland1km.csv", row.names=FALSE)

area_offshore <- read.csv("globalprep/lsp/v2016/output/lsp_a_total_offshore3nm.csv") %>%
  select(rgn_id, area = a_tot_3nm) %>%
  filter(rgn_id <= 250) %>%
  unique()

write.csv(area_offshore, "globalprep/lsp/v2016/output/rgn_area_offshore3nm.csv", row.names=FALSE)
  

### eez resilience 

## first need to calculate status

# select data ----
r = read.csv("globalprep/lsp/v2016/output/lsp_a_total_eez.csv") %>% #total eez area
  dplyr::select(rgn_id, a_tot_eez) %>%
  unique() %>%
  filter(rgn_id <= 250)


ry = read.csv("globalprep/lsp/v2016/output/lsp_protected_eez.csv") %>% #total cumulative protected areas                      
 dplyr::select(rgn_id, year, mpa=a_prot_eez) %>%
  filter(rgn_id <= 250)

r.yrs <- expand.grid(rgn_id = unique(ry$rgn_id),
                     year = unique(ry$year)) %>%
  left_join(ry, by=c('rgn_id', 'year')) %>%
  arrange(rgn_id, year) %>%
  mutate(mpa = ifelse(is.na(mpa), 0, mpa)) 

# get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
# and calculate status score
r.yrs = r.yrs %>%
  full_join(r, by="rgn_id") %>%
  mutate(pct_mpa    = pmin(mpa   / a_tot_eez   * 100, 100),
         prop_protected    = ( pmin(pct_mpa / 30, 1))) %>%
  filter(!is.na(prop_protected))

for(year in 2011:2015){ #year=2011
  tmp <- r.yrs[r.yrs$year == year, ] %>%
    dplyr::select(rgn_id, resilience.score = prop_protected)   
  write.csv(tmp, sprintf("globalprep/lsp/v2016/output/mpa_eez_%s_resilience.csv", year), row.names=FALSE)
}
