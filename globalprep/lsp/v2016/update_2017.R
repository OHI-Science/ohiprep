#### 2017 update for new ohi-global framework

library(dplyr)

## 3 nm
data <- data.frame()

for (year in 2011:2015){ # year = 2012
  data_new <- read.csv(sprintf("globalprep/lsp/v2016/output/mpa_3nm_%s.csv", year))
  
  data_new <- data_new %>%
    mutate(year = year) %>%
    select(rgn_id, year, resilience.score)
  
  data <- rbind(data, data_new)
  
}

write.csv(data, "globalprep/lsp/v2016/output/mpa_3nm_updated.csv", row.names=FALSE)


# eez
data <- data.frame()

for (year in 2011:2015){ # year = 2012
  data_new <- read.csv(sprintf("globalprep/lsp/v2016/output/mpa_eez_%s_resilience.csv", year))
  
  data_new <- data_new %>%
    mutate(year = year) %>%
    select(rgn_id, year, resilience.score)
  
  data <- rbind(data, data_new)
  
}

write.csv(data, "globalprep/lsp/v2016/output/mpa_eez_updated_resilience.csv", row.names=FALSE)

