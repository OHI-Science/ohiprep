# gapfilling habitat destruction soft-bottom

# no gapfilling for this data layer

prs <- read.csv("globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/output/hd_sb_subtidal.csv") %>%
  mutate(pressure_score = 0)

write.csv(prs, "globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/output/hd_sb_subtidal_gf.csv", row.names =FALSE)

health <- read.csv("globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/output/habitat_health_softbottom.csv") %>%
  mutate(health = 0)

write.csv(health, "globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/output/habitat_health_softbottom_gf.csv", row.names=FALSE)

trend <- read.csv("globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/output/habitat_trend_softbottom.csv") %>%
  mutate(health = 0)

write.csv(trend, "globalprep/hab_prs_hd_subtidal_soft_bottom/v2017/output/habitat_trend_softbottom_gf.csv", row.names=FALSE)