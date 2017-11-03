## gapfilling file creation

## There was no gapfilling for these pressures (at least at our end)

library(dplyr)

art <- read.csv("globalprep/prs_fish/v2017/output/art.csv") %>%
  mutate(pressure_score = 0)
write.csv(art, "globalprep/prs_fish/v2017/output/art_gf.csv", row.names=FALSE)

lowbc <- read.csv("globalprep/prs_fish/v2017/output/lb.csv") %>%
  mutate(pressure_score = 0)
write.csv(lowbc, "globalprep/prs_fish/v2017/output/lb_gf.csv", row.names=FALSE)

highbc <- read.csv("globalprep/prs_fish/v2017/output/hb.csv") %>%
  mutate(pressure_score = 0)
write.csv(highbc, "globalprep/prs_fish/v2017/output/hb_gf.csv", row.names=FALSE)
