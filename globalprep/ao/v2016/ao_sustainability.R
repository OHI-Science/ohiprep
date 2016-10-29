###########################################
## Exploring a sustainability layer
## for the AO goal
## MRF: Oct 6 2016
############################################

## We will use B/Bmsy scores (from the fisheries goal) of artisanally fished taxa to 
## estimate sustainability of AO

library(ohicore)
setwd("globalprep/ao/v2016")
source("../../../src/R/common.R")

catch <- read.csv(file.path(dir_M, "git-annex/globalprep/ao/v2016/int/ao_catch_saup.csv")) %>%
  filter(rgn_id <= 250) %>%
  filter(year > 2000)

bmsy <- read.csv("../../fis/v2016/data/fis_bbmsy.csv")

data <- catch %>%
  left_join(bmsy, by=c("rgn_id", "stock_id", "year"))

# about 28% of the catch records have a b/bmsy value:
# > 213636-153805
# [1] 59831
# > 59831/213636
# [1] 0.2800605

## gapfill the b/bmsy data using the mean of known b/bmsy values in same region/year:
data2 <- data %>%
  mutate(bbmsy = ifelse(bbmsy>1, 1, bbmsy)) %>%
  group_by(rgn_id, year) %>%
  mutate(median_bbmsy = median(bbmsy, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(median_bbmsy2 = median(bbmsy, na.rm=TRUE))

hist(data2$bbmsy[data2$year==2010], main="B/Bmsy values of stocks in 2010")

data2 <- data2 %>%
  mutate(bbmsy_gf = ifelse(is.na(bbmsy), median_bbmsy, bbmsy)) %>%
  mutate(bbmsy_gf = ifelse(is.na(bbmsy_gf), median_bbmsy2, bbmsy_gf))

### There are no underharvest penalties for this goal

### apply a taxonomic penalty:
penaltyTable <- data.frame(TaxonPenaltyCode=1:6, 
                           penalty=c(0.1, 0.25, 0.5, 0.8, 0.9, 1))

data2_penalty <- data2 %>%
  mutate(TaxonPenaltyCode = as.numeric(substring(TaxonKey, 1, 1))) %>%
  left_join(penaltyTable, by='TaxonPenaltyCode') %>%
  mutate(score = bbmsy_gf * penalty)

### sustainability score: average scores weighted by catch

sus <- data2_penalty %>%
  group_by(rgn_id, year) %>%
  summarize(sustainability = weighted.mean(score, tons))

hist(sus$sustainability[sus$year==2010], main="sustainability scores in 2010")

write.csv(sus, "output/ao_sustainability.csv", row.names=FALSE)

### Gapfilling missing sustainability scores:
sus_gf <- georegions %>%
  left_join(sus, by="rgn_id")

no_sus <- filter(sus_gf, is.na(sustainability))

## select out the uninhabitated islands
unin <- read.csv("../../../src/LookupTables/rgn_uninhabited_islands.csv")

sus_gf <- sus_gf %>%
  filter(!(rgn_id %in% unin$rgn_id))

no_sus <- filter(sus_gf, is.na(sustainability))

## All the countries without a sustainability score are uninhabited islands, so 
## no need to gapfill this variable.


sus <- read.csv("output/ao_sustainability.csv")

rgn_names <- read.csv("../../../../ohi-global/eez2016/layers/rgn_labels.csv") %>%
  select(rgn_id, label)

sus <- sus %>%
  left_join(rgn_names) %>%
  select(year, sustainability, rgn_name=label)

library(googleVis)
Motion = gvisMotionChart(sus,
                         idvar="rgn_name",
                         timevar="year")
plot(Motion)
print(Motion, file="sustainability.html")
