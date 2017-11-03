########## gapfilling population data
#### No gapfilling for these data

prs <- read.csv("globalprep/mar_prs_population/v2017/output/prs_pop_density.csv") %>%
  mutate(pressure_score = 0)

write.csv(prs, "globalprep/mar_prs_population/v2017/output/prs_pop_density_gf.csv", row.names = FALSE)


mar <- read.csv("globalprep/mar_prs_population/v2017/output/mar_pop_25mi.csv") %>%
  mutate(popsum = 0)

write.csv(mar, "globalprep/mar_prs_population/v2017/output/mar_pop_25mi_gf.csv", row.names = FALSE)
