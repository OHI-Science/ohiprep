# Data prep CITES

# downloaded on: Oct 19 2017
# from: https://www.cites.org/eng/disc/parties/chronolo.php

library(ohicore)
library(dplyr)
library(tidyr)
library(stringr)

setwd("globalprep/res_cites_signatories/v2017")

## territory data (territories get same score as administrative country)

territory <- read.csv("../../../../ohi-global/eez/spatial/rgns_list.csv") %>%
  select(rgn_id, admin_rgn_id)


## get signatories
cites <- read.csv("raw/CITES_Oct_19_2017.csv") %>%
  select(Country, date_of_joining) 


## convert to OHI regions
cites_rgn <- name_2_rgn(df_in = cites, 
                      fld_name='Country', 
                      flds_unique=c('Country'))

cites_rgn <- cites_rgn %>%
  mutate(resilience_score_tmp = 1)

## assign territories same value as administrative region
cites_terr <- territory %>%
  left_join(cites_rgn, by = "rgn_id") %>%
  group_by(admin_rgn_id) %>%
  mutate(resilience_score = mean(resilience_score_tmp, na.rm = TRUE)) %>%
  mutate(date_of_joining = date_of_joining[!is.na(date_of_joining)][1]) %>%
  ungroup()

## check that everything went ok
summary(cites_terr)  # resilience_score should all be 1
filter(cites_terr, is.na(resilience_score)) # regions that have not signed
# Micronesia, Nauru, Marshall Islands, Taiwan, Tuvalu, North Korea, 
# Kiribati (and territories Line Group, Phoenix Group), 
# East Timor (and territory Oecussi Ambeno)

## add all year information based on date of joining
resil <- cites_terr %>%
  mutate(year = sub(".*(.*?)/", "\\1", date_of_joining)) %>%
  mutate(year = as.numeric(as.character(year))) %>%
  select(rgn_id, year, resilience_score) %>%
  mutate(resilience_score = ifelse(is.na(resilience_score), 0, 1))

all_years <- min(resil$year, na.rm=TRUE):2017
all_rgns <- min(resil$rgn_id):max(resil$rgn_id)
all_combos <- expand.grid(year = c(all_years, NA), rgn_id = all_rgns)

resil_full <- all_combos %>%
  left_join(resil, by = c("year", "rgn_id")) %>%
  filter(!is.na(year)) %>%
  arrange(rgn_id, year) %>%
  group_by(rgn_id) %>%
  fill(resilience_score) 


# check by looking at data
tmp_check <- spread(resil_full, "year", "resilience_score")  

resil_final <- resil_full %>%
  mutate(resilience_score = ifelse(is.na(resilience_score), 0, resilience_score)) %>%
  filter(year >= 2010) %>%
  select(rgn_id, year, resilience_score)

write.csv(resil_final, "output/cites.csv", row.names=FALSE)
