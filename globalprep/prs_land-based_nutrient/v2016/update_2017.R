#### 2017 update for new ohi-global framework

library(dplyr)

# trend data
data <- data.frame()

for (year in 2012:2016){ # year = 2012
  trend <- read.csv(sprintf("globalprep/prs_land-based_nutrient/v2016/output/cw_fertilizers_trend_%s_new.csv", year))
  
  trend <- trend %>%
    mutate(year = year) %>%
    select(rgn_id, year, trend)
  
  data <- rbind(data, trend)
  
}


write.csv(data, "globalprep/prs_land-based_nutrient/v2016/output/cw_fertilizers_trend_updated.csv",
          row.names=FALSE)


#pressure data 3nm
data <- data.frame()

for (year in 2012:2015){ # year = 2012
  prs <- read.csv(sprintf("globalprep/prs_land-based_nutrient/v2015/output/cw_fertilizers_score_3nm_%s.csv", year))
  
  prs <- prs %>%
    mutate(year = year) %>%
    select(rgn_id, year, pressure_score)
  
  data <- rbind(data, prs)
  
}

prs_2016 <- read.csv("globalprep/prs_land-based_nutrient/v2016/output/cw_fertilizers_score_3nm_2016.csv") %>%
  mutate(year = 2016) %>%
  select(rgn_id, year, pressure_score)

data <- rbind(data, prs_2016)

write.csv(data, "globalprep/prs_land-based_nutrient/v2016/output/cw_fertilizers_score_3nm_updated.csv",
          row.names=FALSE)


#pressure data eez
data <- data.frame()

for (year in 2012:2015){ # year = 2012
  prs <- read.csv(sprintf("globalprep/prs_land-based_nutrient/v2015/output/cw_fertilizers_score_%s.csv", year))
  
  prs <- prs %>%
    mutate(year = year) %>%
    select(rgn_id, year, pressure_score)
  
  data <- rbind(data, prs)
  
}

prs_2016 <- read.csv("globalprep/prs_land-based_nutrient/v2016/output/cw_fertilizers_score_2016.csv") %>%
  mutate(year = 2016) %>%
  select(rgn_id, year, pressure_score)

data <- rbind(data, prs_2016)

write.csv(data, "globalprep/prs_land-based_nutrient/v2016/output/cw_fertilizers_score_updated.csv",
          row.names=FALSE)
