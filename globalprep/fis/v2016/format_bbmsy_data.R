##############################################
## This script is used to do the final B/Bmsy
## calculations after
## generating the initial B/Bmsy values
## using the functions in the R datalimited
## package
##############################################

library(zoo)
library(tidyr)
library(dplyr)
library(ggplot2)

source('src/R/common.R')
# -------------------------------------------------------------------
## Taking the 5 year running average of b/bmsy values to smooth data
# -------------------------------------------------------------------

cmsy <- read.csv('globalprep/fis/v2016/int/cmsy_bbmsy.csv') %>%
  mutate(prior = 'constrained') %>%
  filter(!is.na(bbmsy_mean))
comsir <- read.csv('globalprep/fis/v2016/int/comsir_bbmsy.csv') %>%
  mutate(prior = NA) %>%
  filter(!is.na(bbmsy_mean))
sscom <- read.csv('globalprep/fis/v2016/int/sscom_bbmsy.csv') %>%
  mutate(prior=NA) %>%
  filter(!is.na(bbmsy_mean))

new_b_bmsy <- function(b_bmsy=constrained, method = "comsir"){
  b_bmsy <- b_bmsy %>%
    dplyr::select(stock_id, year, bbmsy_mean, prior, model) %>%
    arrange(stock_id, year) %>%
    group_by(stock_id) %>%
    mutate(mean_5year = rollmean(bbmsy_mean, 5, align="right", fill=NA))
  write.csv(b_bmsy, sprintf('globalprep/fis/v2016/int/%s_b_bmsy_%s_mean5yrs.csv', method, unique(b_bmsy$prior)), row.names=FALSE)
} 

new_b_bmsy(cmsy, method="cmsy")
new_b_bmsy(comsir, method="comsir")
new_b_bmsy(sscom, method="sscom")

#--------------------------------------------------------------------
## Previously, we tried to select CMSY priors based on 
## stock resilience scores, but ultimately, it looked like 
## the constrained prior worked best regardless of method
### NOTE: 5 year running average of cmsy derived b/bmsy values do seem to perform 
### slightly better.
## cor.coefficients
##                     no_avg     5_year_avg
## resilience based    0.04       0.007
## uniform             0.01       -0.03
## constrained         0.14       0.12
# -------------------------------------------------------------------

b_bmsy_cmsy <- read.csv('globalprep/fis/v2016/int/cmsy_b_bmsy_constrained_mean5yrs.csv') %>%
  select(stock_id, year, b_bmsy_cmsy = mean_5year) 

b_bmsy_comsir <- read.csv('globalprep/fis/v2016/int/comsir_b_bmsy_NA_mean5yrs.csv') %>%
  select(stock_id, year, b_bmsy_comsir=mean_5year) 

b_bmsy_sscom <- read.csv('globalprep/fis/v2016/int/sscom_b_bmsy_NA_mean5yrs.csv') %>%
  select(stock_id, year, b_bmsy_sscom=mean_5year) 


bmsy <- b_bmsy_cmsy %>%
  left_join(b_bmsy_comsir, by=c("stock_id", "year")) %>%
  left_join(b_bmsy_sscom, by=c("stock_id", "year"))

bmsy <- bmsy %>%
  rowwise() %>%
  mutate(mean_all = mean(c(b_bmsy_cmsy, b_bmsy_comsir, b_bmsy_sscom), na.rm=TRUE))

#--------------------------------------------------------------------
#### getting b/bmsy data to the correct spatial scale
#### need a value for each fao/ohi-region combination 
#### (not just FAO)
#-----------------------------------------------------------------------
catch <- catch <- read.csv(file.path(dir_M,'git-annex/globalprep/fis/v2016/int/spatial_catch_saup.csv')) %>%
  rename(common = Common_Name, fao_id = fao_rgn, species=Scientific_Name)
summary(catch)
hist(log(catch$tons))

## a lot of catch data with an ohi-region but no fao region (constrained to some regions)
tmp <- filter(catch, is.na(fao_id) & !is.na(rgn_id))
#table(tmp$rgn_id, tmp$year)
table(tmp$rgn_id[tmp$year==2010])
sum(table(tmp$rgn_id[tmp$year==2010]))
## and seems to be a relatively small amount of the catch
hist(log(tmp$tons)) 

## filter out catch records without region identifiersand bind to b/bmsy data
bmsy_fao_rgn <- catch %>%
  filter(!is.na(rgn_id)) %>%
  filter(!is.na(fao_id)) %>%
  filter(rgn_id <= 250) %>%
  filter(rgn_id != 213) %>%
  left_join(bmsy, by=c('stock_id', 'year'))
head(bmsy_fao_rgn)
summary(bmsy_fao_rgn)


# -------------------------------------------------------------------
### Read in RAM data and format
# -----------------------------------------------------------------

#### In this section, I am just getting a list of the species that are in the RAM data, but 
#### not in the SAUP catch data.  I am hand checking these to find potential synonyms
#### and creating a synonym master file that can be used to align the datasets.
ram_sp <- read.csv('globalprep/fis/v2016/ram/ram_extended.csv', stringsAsFactors = FALSE) %>%
  dplyr::select(scientificname) %>%
  unique()

# get a list of the ram species that are not in the catch data 
# some of these were hand matched based on synonyms...these are saved in:
# RAM_species_to_SAUP.csv
# tmp <- sort(setdiff(ram_sp$scientificname, bmsy_fao_rgn$Scientific_Name))
# write.csv(tmp, "globalprep/fis/v2016/int/unmatched_RAM_species.csv", row.names=FALSE)
ram_name_corr <- read.csv("globalprep/fis/v2016/int/RAM_species_to_SAUP.csv", stringsAsFactors = FALSE) %>%
  filter(!is.na(SAUP_species))  # SAUP to RAM name conversion


# the species that are in the catch and ram data N=102
sort(intersect(bmsy_fao_rgn$species, ram_sp$scientificname))


###########################################
## read in the ram data and substitute names
###########################################

## This file associates the RAM stocks with the FAO and OHI regions:
ram <- read.csv('globalprep/fis/v2016/ram/ram_extended.csv', , stringsAsFactors = FALSE) %>%
  dplyr::select(stockid, RAM_species=scientificname, rgn_id=OHI_rgn, fao_id=FAO) %>%
  left_join(ram_name_corr, by="RAM_species") %>%
  mutate(species = ifelse(!is.na(SAUP_species), SAUP_species, RAM_species)) %>%
  select(stockid_ram = stockid, rgn_id, fao_id, species)
length(unique(ram$stockid_ram))  #237 stocks with RAM B/Bmsy data

## filter out the species that have no match in the SAUP data
ram <- filter(ram, species %in% bmsy_fao_rgn$species)
length(unique(ram$stockid_ram))  #203 stocks that match our species

## filter out the regions that are not in an eez
ram <- filter(ram, rgn_id<250)
length(unique(ram$stockid_ram))  #202 stocks that match our species and in the eez regions

### This dataset has the actual B/Bmsy values:
ram_bmsy <- read.csv('globalprep/fis/v2016/ram/ram_timeseries.csv') %>%
  dplyr::filter(tsid == "BdivBmsytouse-dimensionless") %>%
  dplyr::filter(!is.na(tsvalue)) %>%
  select(stockid_ram = stockid, year=tsyear, ram_bmsy = tsvalue) 

## gapfill ram_bmsy
ram_gf_check <- ram_bmsy %>%
  filter(year >= 2005) %>%
  spread(year, ram_bmsy) 
## based on this it seams reasonable to gap-fill missing 2010 values with lm

ram_bmsy_gf <- ram_bmsy %>%
  filter(year >= 2005 & year <= 2010) %>%
  group_by(stockid_ram) %>%
  mutate(years_data = length(ram_bmsy)) %>%
  ungroup() %>%
  filter(years_data >= 3) %>%
  spread(year, ram_bmsy) %>%
  gather("year", "ram_bmsy", -stockid_ram, -years_data) %>%
  mutate(year = as.numeric(year)) %>%  ### stocks down to 195
  mutate(gapfilled = NA) %>%
  mutate(gapfilled = ifelse(years_data == 3, 3, gapfilled)) %>%
  mutate(gapfilled = ifelse(years_data == 4, 2, gapfilled)) %>%
  mutate(gapfilled = ifelse(years_data == 5, 1, gapfilled))
  
tmp <- ram_bmsy_gf %>%
  select(stockid_ram, gapfilled) %>%
  unique()
length(tmp$gapfilled)  
table(tmp$gapfilled)
34/195
18/195
40/195

# regression model for prediction for each stock
ram_bmsy_gf <- ram_bmsy_gf %>%
  group_by(stockid_ram) %>%
  do({
    mod <- lm(ram_bmsy ~ year, data=.)  
    ram_bmsy_predict <- predict(mod, newdata=.[c('year')])
    data.frame(., ram_bmsy_predict)
  })



##############################################
## This estimates error due to 
## gapfilling RAM B/Bmsy data
###############################################

## checking error and model fit for stocks
ram_bmsy_gf_check1 <- ram_bmsy_gf %>%
  group_by(stockid_ram) %>%
  do({
    mod <- lm(ram_bmsy ~ year, data= filter(., year < 2010))  ### change year to test different accuracy levels based on # years of data used in lm
    ram_bmsy_predict_error <- predict(mod, newdata=.[c('year')])
    data.frame(., ram_bmsy_predict_error)
  })

ggplot(data = filter(ram_bmsy_gf_check1, year==2010), aes(y=ram_bmsy_predict_error, x=ram_bmsy)) +
         geom_point() +
         theme_bw() +
         geom_abline(slope=1, intercept=0, color="red")
summary(lm(ram_bmsy_predict_error ~ ram_bmsy, data=filter(ram_bmsy_gf_check1, year==2010)))

ram_bmsy_gf_check2 <- ram_bmsy_gf %>%
  group_by(stockid_ram) %>%
  do({
    mod <- lm(ram_bmsy ~ year, data= filter(., year < 2009))  ### change year to test different accuracy levels based on # years of data used in lm
    ram_bmsy_predict_error <- predict(mod, newdata=.[c('year')])
    data.frame(., ram_bmsy_predict_error)
  })

ggplot(data = filter(ram_bmsy_gf_check2, year==2010), aes(y=ram_bmsy_predict_error, x=ram_bmsy)) +
  geom_point() +
  theme_bw() +
  geom_abline(slope=1, intercept=0, color="red")
summary(lm(ram_bmsy_predict_error ~ ram_bmsy, data=filter(ram_bmsy_gf_check2, year==2010)))

ram_bmsy_gf_check3 <- ram_bmsy_gf %>%
  group_by(stockid_ram) %>%
  do({
    mod <- lm(ram_bmsy ~ year, data= filter(., year < 2008))  ### change year to test different accuracy levels based on # years of data used in lm
    ram_bmsy_predict_error <- predict(mod, newdata=.[c('year')])
    data.frame(., ram_bmsy_predict_error)
  })

ggplot(data = filter(ram_bmsy_gf_check3, year==2010), aes(y=ram_bmsy_predict_error, x=ram_bmsy)) +
  geom_point() +
  theme_bw() +
  geom_abline(slope=1, intercept=0, color="red")
summary(lm(ram_bmsy_predict_error ~ ram_bmsy, data=filter(ram_bmsy_gf_check3, year==2010)))

##############################################
## End of error estimating
###############################################


ram_bmsy_gf <- ram_bmsy_gf %>%
  mutate(ram_bmsy = ifelse(is.na(ram_bmsy), ram_bmsy_predict, ram_bmsy)) %>%
  select(stockid_ram, year, ram_bmsy, gapfilled)

## 202 RAM species
setdiff(ram_bmsy_gf$stockid_ram, ram$stockid_ram) # these have B/Bmsy data, but no species match in the SAUP data...ok
setdiff(ram$stockid_ram, ram_bmsy_gf$stockid_ram) # these got cut due to not having at least 3 years of data from 2005-2010
## 202 - 36 = 166 stocks with RAM data

ram_data <- ram %>% 
  left_join(ram_bmsy_gf, by="stockid_ram") %>%
  group_by(rgn_id, fao_id, species, year) %>%  # sometimes a region will have multiple stocks of the same species, average these!
  summarize(ram_bmsy = mean(ram_bmsy, na.rm=TRUE),
            gapfilled = ifelse(all(is.na(gapfilled)), NA, max(gapfilled, na.rm=TRUE)),  
            stockid_ram = paste(stockid_ram, collapse=", ")) %>%  ## slightly overestimating gapfilling for a few species....oh-well
  filter(!is.na(year)) %>%   ## these are the ones that didn't have enough ram data to make an accurate guess
  ungroup()

# -------------------------------------------------------------------
### Now join ram data with catch and bmsy data
# -----------------------------------------------------------------

bmsy_dl_ram <- bmsy_fao_rgn %>%
  left_join(ram_data, by=c('rgn_id', 'fao_id', "species", "year"))
head(bmsy_dl_ram) 
summary(bmsy_dl_ram)


# -------------------------------------------------------------------
### final formatting of data
# -----------------------------------------------------------------

### this one describes RAM gapfilling:
bbmsy_data_gf <- bmsy_dl_ram %>%
  mutate(bbmsy = ifelse(is.na(ram_bmsy), b_bmsy_cmsy, ram_bmsy)) %>%
  mutate(bbmsy_data_source = ifelse(!is.na(ram_bmsy), "RAM", "cmsy")) %>%
  filter(!is.na(bbmsy)) %>%
  select(rgn_id, stock_id, year, bbmsy, gapfilled, bbmsy_data_source) %>%
  filter(year >= 2005)
write.csv(bbmsy_data_gf, 'globalprep/fis/v2016/data/fis_bbmsy_gf.csv', row.names = FALSE)

### This is the actual B/Bmsy data:
### (ultimately decided it was best to go with cmsy data, rather than other b/bmsy values or ensembles, see below...)
bbmsy_data <- bmsy_dl_ram %>%
  mutate(bbmsy = ifelse(is.na(ram_bmsy), b_bmsy_cmsy, ram_bmsy)) %>%
  filter(!is.na(bbmsy)) %>%
  select(rgn_id, stock_id, year, bbmsy) %>%
  filter(year >= 2005)
write.csv(bbmsy_data, 'globalprep/fis/v2016/data/fis_bbmsy.csv', row.names = FALSE)



# -------------------------------------------------------------------
### compare b/bmsy values
# -----------------------------------------------------------------

data <- filter(bmsy_dl_ram, ram_bmsy < 2.5) %>%  # ignore the very high values, these are impossible to fit
  filter(!is.na(stockid_ram)) %>%
  filter(year == 2010) %>%
  select( b_bmsy_cmsy, b_bmsy_comsir, b_bmsy_sscom, mean_all, ram_bmsy, stockid_ram) %>%
  unique()

###
ggplot(data, aes(x=ram_bmsy, y=b_bmsy_cmsy)) +
  annotate("rect", xmin=0, xmax=0.5, ymin=0, ymax=0.5, fill = "green", alpha=0.5) + 
  annotate("rect", xmin=0, xmax=1, ymin=0, ymax=1, fill = "yellow", alpha=0.5) + 
  annotate("rect", xmin=1, xmax=2.5, ymin=1, ymax=2.5, fill = "green", alpha=0.5) + 
  geom_point() +
  labs(x='RAM B/Bmsy', y='CMSY B/Bmsy, constrained') +
  theme_bw()

hist(bmsy_dl_ram$b_bmsy_cmsy)

mod <- lm(ram_bmsy ~ b_bmsy_cmsy, data=data)
summary(mod)
var(data$b_bmsy_cmsy, data$ram_bmsy, na.rm=TRUE)

###

ggplot(data, aes(x=ram_bmsy, y=b_bmsy_comsir)) +
  annotate("rect", xmin=0, xmax=0.5, ymin=0, ymax=0.5, fill = "green", alpha=0.5) + 
  annotate("rect", xmin=0, xmax=1, ymin=0, ymax=1, fill = "yellow", alpha=0.5) + 
  annotate("rect", xmin=1, xmax=2.5, ymin=1, ymax=2.5, fill = "green", alpha=0.5) + 
  geom_point() +
  labs(x='RAM B/Bmsy', y='COMSIR B/Bmsy') +
  theme_bw()

mod1 <- lm(ram_bmsy ~ b_bmsy_comsir, data=data)
summary(mod1)
var(data$b_bmsy_comsir, data$ram_bmsy, na.rm=TRUE)

###

ggplot(data, aes(x=ram_bmsy, y=b_bmsy_sscom)) +
  annotate("rect", xmin=0, xmax=0.5, ymin=0, ymax=0.5, fill = "green", alpha=0.5) + 
  annotate("rect", xmin=0, xmax=1, ymin=0, ymax=1, fill = "yellow", alpha=0.5) + 
  annotate("rect", xmin=1, xmax=2.5, ymin=1, ymax=2.5, fill = "green", alpha=0.5) + 
  geom_point() +
  labs(x='RAM B/Bmsy', y='SSCOM B/Bmsy') +
  theme_bw()

mod1 <- lm(ram_bmsy ~ b_bmsy_sscom, data=data)
summary(mod1)
var(data$b_bmsy_sscom, data$ram_bmsy, na.rm=TRUE)

### 

ggplot(data, aes(x=ram_bmsy, y=mean_all)) +
  annotate("rect", xmin=0, xmax=0.5, ymin=0, ymax=0.5, fill = "green", alpha=0.5) + 
  annotate("rect", xmin=0, xmax=1, ymin=0, ymax=1, fill = "yellow", alpha=0.5) + 
  annotate("rect", xmin=1, xmax=2.5, ymin=1, ymax=2.5, fill = "green", alpha=0.5) + 
  geom_point() +
  labs(x='RAM B/Bmsy', y='mean cmsy, comsir, sscom B/Bmsy') +
  theme_bw()

mod1 <- lm(ram_bmsy ~ mean_all, data=data)
summary(mod1)
var(data$mean_all, data$ram_bmsy, na.rm=TRUE)


### 
## Exporing ensemble approaches:

## 1. additive and interaction linear models that include all three variables
## (this is an approximation because values are not based on cross-validation):
mod5 <- lm(ram_bmsy ~ b_bmsy_cmsy + b_bmsy_comsir + b_bmsy_sscom , data=data)
summary(mod5)

mod6 <- lm(ram_bmsy ~ b_bmsy_cmsy*b_bmsy_comsir*b_bmsy_sscom , data=data)
summary(mod6)

## 2. Generalized Boosted Regression Models
library(caret)
library(gbm)

set.seed(227)
samples <- sample(1:length(data$stockid_ram), length(data$stockid_ram)*0.75, replace = FALSE)
training <- data[samples, ]
testing <- data[-samples, ]  # 25% of the data

### GBM: weight the low values higher
m_gbm <- gbm::gbm(ram_bmsy ~ b_bmsy_cmsy + b_bmsy_comsir + b_bmsy_sscom,
                  data = training, n.trees = 2000L, interaction.depth = 3, shrinkage = 0.1,
                  distribution = "gaussian", weights = 1/training$ram_bmsy)
m_gbm

gbm_predict <- gbm::predict.gbm(m_gbm,
                 n.trees = m_gbm$n.trees, newdata = training[,1:3], type = "response")

plot(training$ram_bmsy, gbm_predict)

gbm_predict <- gbm::predict.gbm(m_gbm,
                                n.trees = m_gbm$n.trees, newdata = testing[,1:3], type = "response")

plot(testing$ram_bmsy, gbm_predict, xlab="RAM B/Bmsy", ylab="Predicted B/Bmsy, boosted regression")

### GBM: no weights
m_gbm <- gbm::gbm(ram_bmsy ~ b_bmsy_cmsy + b_bmsy_comsir + b_bmsy_sscom,
                  data = training, n.trees = 2000L, interaction.depth = 3, shrinkage = 0.1,
                  distribution = "gaussian")
m_gbm

gbm_predict <- gbm::predict.gbm(m_gbm,
                                n.trees = m_gbm$n.trees, newdata = training[,1:3], type = "response")
plot(training$ram_bmsy, gbm_predict)

gbm_predict <- gbm::predict.gbm(m_gbm,
                                n.trees = m_gbm$n.trees, newdata = testing[,1:3], type = "response")
plot(testing$ram_bmsy, gbm_predict)


### Random forest
library(randomForest)
training_rf <- na.omit(training)
testing_rf <- na.omit(testing)
m_rf <- randomForest::randomForest(ram_bmsy ~ b_bmsy_cmsy + b_bmsy_comsir + b_bmsy_sscom, data = training_rf,
                           ntree = 1000L)
rf_predict <- predict(m_rf, newdata = training_rf)
plot(training_rf$ram_bmsy, rf_predict)
rf_predict <- predict(m_rf, newdata = testing_rf)
plot(testing_rf$ram_bmsy, rf_predict, xlab="RAM B/Bmsy", ylab="Predicted B/Bmsy, random forest")

## linear model
lm_mod <- lm(ram_bmsy ~ b_bmsy_cmsy + b_bmsy_comsir + b_bmsy_sscom, data = training, na.action=na.exclude)
lm_predict <- predict(lm_mod, newdata = training)
plot(training$ram_bmsy, lm_predict, xlab="RAM B/Bmsy", ylab="Predicted B/Bmsy, lm")
lm_predict <- predict(lm_mod, newdata = testing)
plot(testing$ram_bmsy, lm_predict)


# -------------------------------------------------------------------
### summary of data
# -----------------------------------------------------------------

tmp <- bmsy_dl_ram %>%
  mutate(ram_data = ifelse(is.na(ram_bmsy), "no", "yes")) %>%
  mutate(bbmsy = ifelse(is.na(ram_bmsy), b_bmsy_cmsy, ram_bmsy)) %>%
  mutate(bbmsy_data = ifelse(!is.na(bbmsy), "yes", "no"))


## Proportion of catch with a B/Bmsy values that is based on ram data
tmp %>%
  filter(!is.na(bbmsy)) %>%
  group_by(ram_data, year) %>%
  summarize(sum_tons = sum(tons)) %>%
  group_by(year) %>%
  summarize(prop_catch_ram = sum_tons[ram_data=="yes"]/(sum(sum_tons))) %>%
  data.frame()
# ~30% of catch with ram b/bmsy estimates

## Proportion of catch with b/bmsy estimates
tmp %>%
  group_by(bbmsy_data, year) %>%
  summarize(sum_tons = sum(tons)) %>%
  group_by(year) %>%
  summarize(prop_catch_bmsy = sum_tons[bbmsy_data=="yes"]/(sum(sum_tons))) %>%
  data.frame()
# ~47% of catch with ram b/bmsy estimates

