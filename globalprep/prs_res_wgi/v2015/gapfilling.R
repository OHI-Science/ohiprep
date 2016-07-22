#######################################
## Gapfilling for wgi
#######################################

# I believe this is the first gapfilling step (no gapfilling at this stage):

gap1 <- read.csv('globalprep/worldbank_wgi/data/wgi_gap_fill_1_attr.csv') %>%
  filter(year == 2013) %>%
  arrange(rgn_id) %>%
  select(rgn_id, gap_fill)

# The second gapfilling step averages UN georegions:
# (note data that has a value but no original data as indicated above is considered gapfilled)
data <- read.csv("globalprep/worldbank_wgi/data/rgn_wb_wgi_2015a.csv") %>%
  left_join(gap1) %>%
  mutate(score = ifelse(is.na(gap_fill) | gap_fill!=0, 1, 0)) %>%
  select(rgn_id, score)

write.csv(data, "globalprep/worldbank_wgi/data/rgn_wb_wgi_2015a_gf.csv", row.names=FALSE)

### checking against official gapfilling file
## The one discrepancy is when we downscaled the data (fine to think of that as gapfilling)
tmp <- read.csv('globalprep/worldbank_wgi/data/rgn_wb_wgi_2014a_attr.csv') %>%
  filter(z_level == 'r2') %>%
  select(rgn_id = id) %>%
  mutate(gapfill=TRUE) %>%
  unique()

gf <- read.csv("globalprep/worldbank_wgi/data/rgn_wb_wgi_2015a_gf.csv") %>%
  left_join(tmp)