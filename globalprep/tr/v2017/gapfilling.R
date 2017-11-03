#### gapfilling for travel warnings (no gapfilling of this layer)

warn <- read.csv("globalprep/tr/v2017/output/tr_travelwarnings.csv") %>%
  mutate(multiplier = 0)

write.csv(warn, "globalprep/tr/v2017/output/tr_travelwarnings_gf.csv", row.names=FALSE)
