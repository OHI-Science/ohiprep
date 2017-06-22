#### 2017 update for new ohi-global framework

library(dplyr)


## Genetic escapes
data <- data.frame()

for (year in 2012:2016){ # year = 2012
  prs <- read.csv(sprintf("globalprep/mar/v2016/output/GenEsc_v%s.csv", year))
  
  prs <- prs %>%
    mutate(year = year) %>%
    select(rgn_id, year, pressure_score)
  
  data <- rbind(data, prs)
  
}

write.csv(data, "globalprep/mar/v2016/output/GenEsc_updated.csv", row.names=FALSE)

# mariculture sustainability

data <- read.csv("globalprep/mar/v2016/output/mar_sustainability.csv") %>%
  mutate(year = 2012) %>%
  select(rgn_id, year, taxa_code, sust_coeff)

write.csv(data, "globalprep/mar/v2016/output/mar_sustainability_updated.csv", row.names=FALSE)
