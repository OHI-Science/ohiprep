### update for 2017 assessment

# resilience
data <- read.csv("globalprep/prs_res_spi/v2016/output/spi_res.csv") %>%
  mutate(year = 2016) %>%
  select(rgn_id, year, resilience_score)

write.csv(data, "globalprep/prs_res_spi/v2016/output/spi_res_updated.csv", row.names=FALSE)


# pressure
data <- read.csv("globalprep/prs_res_spi/v2016/output/spi_prs.csv") %>%
  mutate(year = 2016) %>%
  select(rgn_id, year, pressure_score)

write.csv(data, "globalprep/prs_res_spi/v2016/output/spi_prs_updated.csv", row.names=FALSE)
