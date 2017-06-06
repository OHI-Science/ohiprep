#### 2017 update for new ohi-global framework

library(dplyr)

data <- data.frame()

for (year in 2012:2016){ # year = 2012
  prs <- read.csv(sprintf("globalprep/prs_sst/v2016/output/sst_%s.csv", year))
  
  prs <- prs %>%
    mutate(year = year) %>%
    select(rgn_id, year, pressure_score)
  
  data <- rbind(data, prs)
  
}


write.csv(data, "globalprep/prs_sst/v2016/output/sst_updated.csv", row.names=FALSE)
