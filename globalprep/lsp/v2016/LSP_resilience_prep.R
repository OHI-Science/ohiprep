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
  
