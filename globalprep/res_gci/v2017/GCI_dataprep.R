# setwd('globalprep/res_gci/v2017') #comment out when knitting

# library(devtools)
# devtools::install_github("ohi-science/ohicore@dev") 
library(ohicore)

source('../../../src/R/common.R')
library(readr)
library(stringr)
library(dplyr)
library(tidyr)


## Code to get Global competiveness index data

## describe location of raw data:
dir_wef  <- file.path(dir_M, 'git-annex/globalprep/_raw_data/WEF-Economics/d2017/GCI_Dataset_2006-2016.csv')

# read in files
wef_raw <- read.csv(dir_wef, 
                     skip = 3, check.names = FALSE, stringsAsFactors = FALSE)
### NOTE: check.names = FALSE because of Cote d'Ivoire has an accent circonflex over the 'o' (probably other issues in there too)

wef <- wef_raw[ , names(wef_raw) != '']
### Eliminate columns without names

wef <- wef %>%
  filter(Series == "Global Competitiveness Index") %>%
  filter(Attribute == "Value") %>%
  select(-(1:2), -(4:8), year = Edition) %>%
  gather(country, value, -year) %>%
  mutate(score = as.numeric(value)) %>%
  mutate(year = as.numeric(as.character(substring(year, 1, 4)))) %>%
  select(year, country, score) %>%
  filter(year >= 2010)

### Gapfilling

# 1) if a region has only one value from 2010 onward, use that one value to gapfill other years
# 2) use a lm to gapfill years if there is > 1 missing year for a region

wef_gf <- wef %>%
  group_by(country) %>%
  mutate(N = sum(!is.na(score))) %>%
  mutate(gf_mean = mean(score, na.rm=TRUE)) %>%
  ungroup()

table(wef_gf$N)
filter(wef_gf, N==1)

wef_gf <- wef_gf %>%
  group_by(country) %>%
  do({
    mod <- lm(score ~ year, data = .)
    gf_lm <- predict(mod, newdata = .[c('year')])
    data.frame(., gf_lm)
  }) %>%
  ungroup()

filter(wef_gf, N==4) # check the data

wef_gf <- wef_gf %>%
  mutate(gapfill = ifelse(N < 7, 1, 0)) %>%
  mutate(method = ifelse(N == 1, "within region: one value all years", NA)) %>%
  mutate(method = ifelse(N < 7 & N > 1, "within region: lm", method)) %>%
  mutate(score = ifelse(N == 1, gf_mean, score)) %>%
  mutate(score = ifelse(N < 7 & N > 1, gf_lm, score)) %>%
  select(year, country, score, gapfill, method)
  
#table(wef_gf$method, wef_gf$N)  #intermediate check for above

wef <- wef_gf %>%
  mutate(country = as.character(country)) %>%
  mutate(country = ifelse(country == "Congo, Democratic Rep.", "Democratic Republic of the Congo", country)) %>%
  mutate(country = ifelse(country == "Côte d'Ivoire", "Ivory Coast", country))


wef_rgn <- name_2_rgn(df_in = wef, 
                       fld_name='country', 
                       flds_unique=c('country','year'))

table(wef_rgn$rgn_id)
filter(wef_rgn, rgn_id == 209)

weight_data <- data.frame(country = c("China", "Hong Kong SAR"),
                          population = c(1379000000, 7347000))

wef_rgn <- wef_rgn %>%
  arrange(country, year) %>%
  left_join(weight_data, by = "country") %>%
  mutate(population = ifelse(is.na(population), 1, population)) %>%
  group_by(rgn_id, rgn_name, gapfill, method, year) %>%
  summarize(score = weighted.mean(score, population)) %>%
  select(rgn_id, rgn_name, year, gapfill, method, score)

head(wef_rgn, 10)

#### gapfill using UN georegions

years <- data.frame(year = min(wef_rgn$year):max(wef_rgn$year))

wef_rgn_gf_un <-  georegions %>%
  merge(years) %>%
  left_join(wef_rgn)


## Compare models to select a gapfilling method
mod1 <- lm(score ~ as.factor(r2), data = wef_rgn_gf_un)
mod2 <- lm(score ~ as.factor(r2) + year, data = wef_rgn_gf_un)
mod3 <- lm(score ~ as.factor(r1), data = wef_rgn_gf_un)
## model 1 is better
summary(mod1)
summary(mod2)
summary(mod3)

## need to add this because some R2 regions have no data
r2_regions <- unique(wef_rgn_gf_un$r2[!is.na(wef_rgn_gf_un$score)])
wef_rgn_gf_un$r2 <- ifelse(wef_rgn_gf_un$r2 %in% r2_regions, wef_rgn_gf_un$r2, NA)

r1_regions <- unique(wef_rgn_gf_un$r1[!is.na(wef_rgn_gf_un$score)])
wef_rgn_gf_un$r1 <- ifelse(wef_rgn_gf_un$r1 %in% r1_regions, wef_rgn_gf_un$r1, NA)

## Estimate missing data and gapfill
mod_gf_r2 <- lm(score ~ as.factor(r2), data = wef_rgn_gf_un, na.action = na.exclude)
wef_rgn_gf_un$score_pred_r2 <- predict(mod_gf_r2, newdata = data.frame(r2 = wef_rgn_gf_un$r2))

mod_gf_r1 <- lm(score ~ as.factor(r1), data = wef_rgn_gf_un, na.action = na.exclude)
wef_rgn_gf_un$score_pred_r1 <- predict(mod_gf_r1, newdata = data.frame(r1 = wef_rgn_gf_un$r1))

## organize data

wef <- wef_rgn_gf_un %>%
  mutate(gapfill = ifelse((is.na(score) & !(is.na(score_pred_r1))), 1, gapfill)) %>%
  mutate(method = ifelse((is.na(score) & !(is.na(score_pred_r2))), "UN r2 georegion", method)) %>%
  mutate(method = ifelse((is.na(score) & is.na(score_pred_r2) & !is.na(score_pred_r1)), "UN r1 georegion", method)) %>%
  mutate(score = ifelse(is.na(score), score_pred_r2, score)) %>%
  mutate(score = ifelse(is.na(score), score_pred_r1, score)) %>%
  select(rgn_id, year, gapfill, method, score)
  

summary(wef)
length(unique(wef$rgn_id)) # should be 220 regions


# Uninhabited regions

#These regions will receive an NA for their score (when established population is < 100 people). 

uninhab <- read.csv('../../../src/LookupTables/rgn_uninhabited_islands.csv') %>%
  filter(is.na(est_population) | est_population < 100)

wef <- wef %>%
  mutate(score = ifelse(rgn_id %in% uninhab$rgn_id, NA, score)) %>%
  mutate(gapfill = ifelse(rgn_id %in% uninhab$rgn_id, NA, gapfill)) %>%
  mutate(method = ifelse(rgn_id %in% uninhab$rgn_id, NA, method))

## gapfill North Korea
## This appears to have been calculated at one time with real data
## I am going to use the old data

wef <- wef[-which(wef$rgn_id==21),]

nk <- data.frame(year = min(wef$year):max(wef$year), rgn_id = 21, gapfill = 1, method = "previous score", score = 2.8)
wef <- rbind(wef, nk)

#################################
# Save final data


gf_data <- wef %>%
  select(rgn_id, year, gapfill, method)
write.csv(gf_data, "output/gci_gf.csv", row.names=FALSE)


res_data <- wef %>%
  select(rgn_id, year, resilience_score=score) %>%
  mutate(resilience_score = resilience_score/7)   #score ranges from 0-7
write.csv(res_data, "output/gci_res.csv", row.names=FALSE)


### compare to previous years
old <- read.csv("../v2015/data/rgn_wef_gci_updated.csv") %>%
  rename(old_score = score) %>%
  full_join(res_data, by = c("rgn_id", "year"))
plot(old$old_score, old$resilience_score)
abline(0,1, col="red")
## looks reasonable to me
