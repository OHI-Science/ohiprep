##### Checking out the LE data

library(dplyr)
library(tidyr)

#################################
# le_jobs_cur_adj_value
# jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_jobs_cur_adj_value.csv") %>%
#   rename(value2012 = value)
# jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_jobs_cur_adj_value.csv") %>%
#   rename(value2013 = value)
# 
# tmp <- full_join(jobs_curr_adj_2012, jobs_curr_adj_2013, by=c("cntry_key", "sector")) %>%
#   mutate(diff = value2012 - value2013)
# 
# summary(tmp)
# 
# tmp[tmp$diff != 0, ]
# tmp[is.na(tmp$value2012),]
# tmp[is.na(tmp$value2013),]

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_jobs_cur_adj_value.csv") %>%
  mutate(year = 2012) %>%
  select(cntry_key, sector, year, value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_jobs_cur_adj_value.csv") %>%
  mutate(year = 2013) %>%
  select(cntry_key, sector, year, value)
updated <- rbind(jobs_curr_adj_2012, jobs_curr_adj_2013)

write.csv(updated, "globalprep/le/v2013/data/le_status_model_curref_jobs_cur_adj_value_updated.csv", 
          row.names=FALSE)

#########################
# le_jobs_cur_base_value
# jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_jobs_cur_base_value.csv") %>%
#   rename(value2012 = value)
# jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_jobs_cur_base_value.csv") %>%
#   rename(value2013 = value)
# 
# tmp <- full_join(jobs_curr_adj_2012, jobs_curr_adj_2013, by=c("cntry_key", "sector")) %>%
#   mutate(diff = value2012 - value2013)
# 
# summary(tmp)
# 
# 
# tmp[tmp$diff != 0, ]
# tmp[is.na(tmp$value2012),]
# tmp[is.na(tmp$value2013),]

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_jobs_cur_base_value.csv") %>%
  mutate(year = 2012) %>%
  select(cntry_key, sector, year, value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_jobs_cur_base_value.csv") %>%
  mutate(year = 2013) %>%
  select(cntry_key, sector, year, value)
updated <- rbind(jobs_curr_adj_2012, jobs_curr_adj_2013)

write.csv(updated, "globalprep/le/v2013/data/le_status_model_curref_jobs_cur_base_value_updated.csv", 
          row.names=FALSE)
          


#########################
# le_jobs_ref_adj_value

# jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_jobs_ref_adj_value.csv") %>%
#   rename(value2012 = value)
# jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_jobs_ref_adj_value.csv") %>%
#   rename(value2013 = value)
# 
# tmp <- full_join(jobs_curr_adj_2012, jobs_curr_adj_2013, by=c("cntry_key", "sector")) %>%
#   mutate(diff = value2012 - value2013)
# 
# summary(tmp)
# 
# 
# tmp[tmp$diff != 0, ]
# tmp[is.na(tmp$value2012),]
# tmp[is.na(tmp$value2013),]

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_jobs_ref_adj_value.csv") %>%
  mutate(year = 2012) %>%
  select(cntry_key, sector, year, value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_jobs_ref_adj_value.csv") %>%
  mutate(year = 2013) %>%
  select(cntry_key, sector, year, value)
updated <- rbind(jobs_curr_adj_2012, jobs_curr_adj_2013)

write.csv(updated, "globalprep/le/v2013/data/le_status_model_curref_jobs_ref_adj_value_updated.csv", 
          row.names=FALSE)


#########################
# le_jobs_ref_base_value

# jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_jobs_ref_base_value.csv") %>%
#   rename(value2012 = value)
# jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_jobs_ref_base_value.csv") %>%
#   rename(value2013 = value)
# 
# tmp <- full_join(jobs_curr_adj_2012, jobs_curr_adj_2013, by=c("cntry_key", "sector")) %>%
#   mutate(diff = value2012 - value2013)
# 
# summary(tmp)
# 
# 
# tmp[tmp$diff != 0, ]
# tmp[is.na(tmp$value2012),]
# tmp[is.na(tmp$value2013),]

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_jobs_ref_base_value.csv") %>%
  mutate(year = 2012) %>%
  select(cntry_key, sector, year, value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_jobs_ref_base_value.csv") %>%
  mutate(year = 2013) %>%
  select(cntry_key, sector, year, value)
updated <- rbind(jobs_curr_adj_2012, jobs_curr_adj_2013)

write.csv(updated, "globalprep/le/v2013/data/le_status_model_curref_jobs_ref_base_value_updated.csv", 
          row.names=FALSE)



#########################
# le_rev_cur_adj_value

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_rev_cur_adj_value.csv") %>%
  mutate(year = 2012) %>%
  select(cntry_key, sector, year, value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_rev_cur_adj_value.csv") %>%
  mutate(year = 2013) %>%
  select(cntry_key, sector, year, value)
updated <- rbind(jobs_curr_adj_2012, jobs_curr_adj_2013)

write.csv(updated, "globalprep/le/v2013/data/le_status_model_curref_rev_cur_adj_value_updated.csv", 
          row.names=FALSE)


#########################
# le_rev_cur_base_value

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_rev_cur_base_value.csv") %>%
  mutate(year = 2012) %>%
  select(cntry_key, sector, year, value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_rev_cur_base_value.csv") %>%
  mutate(year = 2013) %>%
  select(cntry_key, sector, year, value)
updated <- rbind(jobs_curr_adj_2012, jobs_curr_adj_2013)

write.csv(updated, "globalprep/le/v2013/data/le_status_model_curref_rev_cur_base_value_updated.csv", 
          row.names=FALSE)


#########################
# le_rev_ref_adj_value

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_rev_ref_adj_value.csv") %>%
  mutate(year = 2012) %>%
  select(cntry_key, sector, year, value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_rev_ref_adj_value.csv") %>%
  mutate(year = 2013) %>%
  select(cntry_key, sector, year, value)
updated <- rbind(jobs_curr_adj_2012, jobs_curr_adj_2013)

write.csv(updated, "globalprep/le/v2013/data/le_status_model_curref_rev_ref_adj_value_updated.csv", 
          row.names=FALSE)


#########################
# le_rev_ref_base_value

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_rev_ref_base_value.csv") %>%
  mutate(year = 2012) %>%
  select(cntry_key, sector, year, value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_rev_ref_base_value.csv") %>%
  mutate(year = 2013) %>%
  select(cntry_key, sector, year, value)
updated <- rbind(jobs_curr_adj_2012, jobs_curr_adj_2013)

write.csv(updated, "globalprep/le/v2013/data/le_status_model_curref_rev_ref_base_value_updated.csv", 
          row.names=FALSE)


#########################
# le_wage_cur_adj_value

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_wage_cur_adj_value.csv") %>%
  mutate(year = 2012) %>%
  select(cntry_key, sector, year, value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_wage_cur_adj_value.csv") %>%
  mutate(year = 2013) %>%
  select(cntry_key, sector, year, value)
updated <- rbind(jobs_curr_adj_2012, jobs_curr_adj_2013)

write.csv(updated, "globalprep/le/v2013/data/le_status_model_curref_wage_cur_adj_value_updated.csv", 
          row.names=FALSE)


#########################
# le_wage_cur_base_value

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_wage_cur_base_value.csv") %>%
  mutate(year = 2012) %>%
  select(cntry_key, sector, year, value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_wage_cur_base_value.csv") %>%
  mutate(year = 2013) %>%
  select(cntry_key, sector, year, value)
updated <- rbind(jobs_curr_adj_2012, jobs_curr_adj_2013)

write.csv(updated, "globalprep/le/v2013/data/le_status_model_curref_wage_cur_base_value_updated.csv", 
          row.names=FALSE)


#########################
# le_wage_ref_adj_value

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_wage_ref_adj_value.csv") %>%
  mutate(year = 2012) %>%
  select(cntry_key, sector, year, value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_wage_ref_adj_value.csv") %>%
  mutate(year = 2013) %>%
  select(cntry_key, sector, year, value)
updated <- rbind(jobs_curr_adj_2012, jobs_curr_adj_2013)

write.csv(updated, "globalprep/le/v2013/data/le_status_model_curref_wage_ref_adj_value_updated.csv", 
          row.names=FALSE)


#########################
# le_wage_ref_base_value

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_2012_status_model_curref_wage_ref_base_value.csv") %>%
  mutate(year = 2012) %>%
  select(cntry_key, sector, year, value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_2013_status_model_curref_wage_ref_base_value.csv") %>%
  mutate(year = 2013) %>%
  select(cntry_key, sector, year, value)
updated <- rbind(jobs_curr_adj_2012, jobs_curr_adj_2013)

write.csv(updated, "globalprep/le/v2013/data/le_status_model_curref_wage_ref_base_value_updated.csv", 
          row.names=FALSE)


###################################
# le_jobs_sector_year

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_eez2012_trend_jobs_sector-year.csv") %>%
  rename(value_2012 = value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_eez2013_trend_jobs_sector-year.csv") %>%
  rename(value_2013 = value)

data <- full_join(jobs_curr_adj_2012, jobs_curr_adj_2013, by=c("cntry_key", "sector", "year")) %>%
  mutate(diff = value_2012-value_2013)
tmp <- filter(data, diff != 0)

plot(data$value_2012, data$value_2013)
abline(0,1, col="red")
summary(data)

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_eez2012_trend_jobs_sector-year.csv") %>%
  mutate(scenario_year = 2012)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_eez2013_trend_jobs_sector-year.csv") %>%
  mutate(scenario_year = 2013)

updated <- rbind(jobs_curr_adj_2012, jobs_curr_adj_2013)

write.csv(updated, "globalprep/le/v2013/data/le_trend_jobs_sector-year_updated.csv", 
          row.names=FALSE)


##############################
###################################
# le_wage_sector_year

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_eez2012_trend_wage_sector-year.csv") %>%
  rename(value_2012 = value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_eez2013_trend_wage_sector-year.csv") %>%
  rename(value_2013 = value)

data <- full_join(jobs_curr_adj_2012, jobs_curr_adj_2013, by=c("cntry_key", "sector", "year")) %>%
  mutate(diff = value_2012-value_2013)
tmp <- filter(data, diff != 0)

plot(data$value_2012, data$value_2013)
abline(0,1, col="red")
summary(data)


##############################
###################################
# le_rev_sector_year

jobs_curr_adj_2012 <- read.csv("globalprep/le/v2013/data/le_eez2012_trend_rev_sector-year.csv") %>%
  rename(value_2012 = value)
jobs_curr_adj_2013 <- read.csv("globalprep/le/v2013/data/le_eez2013_trend_rev_sector-year.csv") %>%
  rename(value_2013 = value)

data <- full_join(jobs_curr_adj_2012, jobs_curr_adj_2013, by=c("cntry_key", "sector", "year")) %>%
  mutate(diff = value_2012-value_2013)
tmp <- filter(data, diff != 0)

plot(data$value_2012, data$value_2013)
abline(0,1, col="red")
summary(data)

