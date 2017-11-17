### Checking fisheries stuff

library(dplyr)
library(tidyr)

source('src/R/common.R')

## Paths for data
catch_raw <- read.csv(file.path(dir_M,'git-annex/globalprep/fis/v2017/int/stock_catch_by_rgn.csv'))
catch_mean <- read.csv("globalprep/fis/v2017/data/mean_catch.csv")

ram_processed <- read.csv("globalprep/fis/v2017/int/ram_bmsy.csv")

load(file.path(dir_M, "git-annex/globalprep/_raw_data/RAM/d2017/RAM v3.80/DB Files With Assessment Data/DBdata.RData"))
ram_raw <- data.frame(timeseries) %>%
  dplyr::filter(tsid == "BdivBmsytouse-dimensionless") %>%
  dplyr::filter(!is.na(tsvalue)) %>%
  dplyr::mutate(tsyear = as.numeric(as.character(tsyear))) %>%
  dplyr::filter(tsyear > 1979) %>%
  dplyr::select(assessid, year=tsyear, ram_bmsy = tsvalue)

bbmsys <- read.csv("data/fis_bbmsy.csv")
alpha <- 0.5
beta <- 0.25
lowerBuffer <- 0.95
upperBuffer <- 1.05

bbmsy$score = ifelse(bbmsy$bbmsy < lowerBuffer, bbmsy$bbmsy,
                 ifelse (bbmsy$bbmsy >= lowerBuffer & bbmsy$bbmsy <= upperBuffer, 1, NA))
bbmsy$score = ifelse(!is.na(bbmsy$score), bbmsy$score,  
                 ifelse(1 - alpha*(bbmsy$bbmsy - upperBuffer) > beta,
                        1 - alpha*(bbmsy$bbmsy - upperBuffer), 
                        beta))

bbmsy$score2 = ifelse(bbmsy$bbmsy < lowerBuffer, bbmsy$bbmsy, 1)

## NZ check (162)

tmp <- filter(catch_mean, rgn_id==162 & year == 2014) %>%
  arrange(mean_catch)
tmp

filter(catch_mean, rgn_id==162 & stock_id_taxonkey == 'Macruronus_novaezelandiae-81_601825') # these should all be the same

filter(bbmsy, rgn_id ==162 & year == 2014)
9.370719e+04/(sum(tmp$mean_catch))
1.739980e+05/(sum(tmp$mean_catch))
5.480903e+04/(sum(tmp$mean_catch))
(5.329995e+04 + 4.207865e+04)/(sum(tmp$mean_catch))

tmp <- filter(catch_raw, rgn_id == 162)
no_fao <- filter(tmp, is.na(fao_rgn))
head(no_fao)
sum(no_fao$tons)
sum(tmp$tons)

tmp <- filter(bbmsy, rgn_id==162 & year==2014)
summary(tmp)
