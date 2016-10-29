############################################
## GCI gapfilling
############################################
library(dplyr)

d_regions <- read.csv('../ohi-global/global2015/gapFilling/dissaggregated_gap_fill.csv') %>%
  select(rgn_id = rgn_id_2013, rgn_name = rgn_nam_2013, sov_id, description)

data <- read.csv("globalprep/WEF-Economics/v2015/data/rgn_wef_gci_2015.csv") %>%
  left_join(d_regions) %>%
  arrange(sov_id)

gf <- read.csv("globalprep/WEF-Economics/v2015/data/rgn_wef_gci_2015_attr.csv") %>%
  select(rgn_id=id, gapfill_type = z_level) %>%
  arrange(rgn_id) %>%
  mutate(score = ifelse(gapfill_type=="v", 0, 1)) %>%
  select(rgn_id, score, gapfill_type)

write.csv(gf, "globalprep/WEF-Economics/v2015/data/rgn_wef_gci_2015_gf.csv", row.names=FALSE)
  