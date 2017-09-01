#################################################
## UN population was to check that the 
## population data processing was done correctly
#################################################

library(ohicore)
library(dplyr)
library(tidyr)
library(stringr)

source("src/R/common.R")

pop <- read.csv(file.path(dir_M,
        "git-annex/globalprep/_raw_data/UnitedNations_population/v2017/UN_pop_clean.csv"), 
        strip.white = TRUE, stringsAsFactors = FALSE)

pop_gather <- pop %>%
  gather("year", "population", starts_with("X")) %>%
  mutate(population = gsub(" ", "", population)) %>%
  mutate(year = gsub("X", "", year)) %>%
  mutate(population = as.numeric(as.character(population)) * 1000)

# Channel Islands: Jersey and Guernsey (ignore for now)

pop_gather_rename <- pop_gather %>%
  mutate(country = ifelse(country=="Côte d'Ivoire", "Cote d'Ivoire", country)) %>%  # this one is hard to 
  mutate(country = ifelse(country=="Réunion", "Reunion", country)) %>%
  mutate(country = ifelse(country=="Curaçao", "Curacao", country)) %>%
  mutate(country = ifelse(country=="China, Taiwan Province of China", "Taiwan", country)) %>%
  mutate(country = ifelse(country=="Dem. People's Republic of Korea", "North Korea", country))
  
pop_rgn <- name_2_rgn(df_in = pop_gather_rename, 
                      fld_name='country', 
                      flds_unique=c('year'))
pop_rgn <- pop_rgn %>%
  group_by(rgn_id, year) %>%
  summarize(population = sum(population)) %>%
  data.frame()
  
write.csv(pop_rgn, "globalprep/mar_prs_population/v2017/output/UN_population.csv", row.names=FALSE)
