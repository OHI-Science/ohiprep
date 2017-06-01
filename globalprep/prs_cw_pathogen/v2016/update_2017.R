#### 2017 update for new ohi-global framework

library(dplyr)


# trend data
data <- data.frame()

for (year in 2012:2015){ # year = 2012
  trend <- read.csv(sprintf("globalprep/prs_cw_pathogen/v2016/output/pathogens_popdensity25mi_trend_%sa.csv", year))
  
  trend <- trend %>%
    mutate(year = year) %>%
    select(rgn_id, year, trend)
  
  data <- rbind(data, trend)
  
}

write.csv(data, "globalprep/prs_cw_pathogen/v2016/output/pathogens_popdensity25mi_trend_updated.csv",
          row.names=FALSE)


# pressure data
data <- data.frame()

for (year in 2012:2015){ # year = 2012
  path_data <- read.csv(sprintf("globalprep/prs_cw_pathogen/v2015/data/po_pathogens_popdensity25mi_%sa.csv", year))
  
  path_data <- path_data %>%
    mutate(year = year) %>%
    select(rgn_id, year, pressure_score)
  
  data <- rbind(data, path_data)
  
}

write.csv(data, "globalprep/prs_cw_pathogen/v2016/output/pathogens_popdensity25mi_updated.csv",
          row.names=FALSE)

