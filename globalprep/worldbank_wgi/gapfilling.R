#######################################
## Gapfilling for wgi
#######################################

# I believe this is the first gapfilling step:

gap1 <- read.csv('globalprep/worldbank_wgi/data/wgi_gap_fill_1_attr.csv') %>%
  filter(year == 2013) %>%
  arrange(rgn_id) %>%
  select(rgn_id, gap_fill)

# The second gapfilling step averages UN georegions:

data <- read.csv("globalprep/worldbank_wgi/data/rgn_wb_wgi_2015a.csv") %>%
  left_join(gap1) %>%
  mutate(score = ifelse(is.na(gap_fill) | gap_fill!=0, 1, 0)) %>%
  select(rgn_id, score)

write.csv(data, "globalprep/worldbank_wgi/data/rgn_wb_wgi_2015a_gf.csv", row.names=FALSE)
