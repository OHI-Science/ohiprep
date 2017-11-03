# gapfilling species status
# no gapfilling for this goal

spp_s <- read.csv("globalprep/spp_ico/v2017/output/spp_status_3nm.csv") %>%
  mutate(score = 0)

write.csv(spp_s, "globalprep/spp_ico/v2017/output/spp_status_3nm_gf.csv", row.names=FALSE)


spp_s_eez <- read.csv("globalprep/spp_ico/v2017/output/spp_status_global.csv") %>%
  mutate(score = 0)

write.csv(spp_s_eez, "globalprep/spp_ico/v2017/output/spp_status_global_gf.csv", row.names=FALSE)


spp_t_eez <- read.csv("globalprep/spp_ico/v2017/output/spp_trend_global.csv") %>%
  mutate(score = 0)

write.csv(spp_t_eez, "globalprep/spp_ico/v2017/output/spp_trend_global_gf.csv", row.names=FALSE)
