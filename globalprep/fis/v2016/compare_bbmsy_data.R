##############################################
## This script is used to do the final B/Bmsy
## calculations after
## generating the initial B/Bmsy values
## using the functions in the R datalimited
## package
##############################################


# For the model testing, I compared the stockid_ram value to the 
# CMSY values we calculated at the FAO scale.  So, if a RAM stock 
# covers 2 FAO regions there is one RAM value, but 2 CMSY values.  
# And, if there is RAM data for multiple stocks of the same species 
# that are located in the same FAO region, the RAM B/Bmsy values 
# were averaged

library(tidyr)
library(dplyr)
library(ggplot2)

source('src/R/common.R')
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
#### getting b/bmsy data to the correct spatial scale to test model
#### need a value for each fao/ohi-region combination 
#### (not just FAO)
#### NOTE: This is just for testing the models, the actual B/Bmsy data
#### ultimately needs to be joined to the mean_catch data!
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

## filter out catch records without region identifiers and bind to b/bmsy data
bmsy_fao_rgn <- catch %>%
  filter(!is.na(rgn_id)) %>%
  filter(!is.na(fao_id)) %>%
  filter(rgn_id <= 250) %>%
  filter(rgn_id != 213) %>%
  left_join(bmsy, by=c('stock_id', 'year'))
head(bmsy_fao_rgn)
summary(bmsy_fao_rgn)


# -------------------------------------------------------------------
### Now join ram data with catch and bmsy data
# -----------------------------------------------------------------

# formatted RAM data:

## These data were gapfilled using 5 years of RAM data
## ultimately, we might want to use 10 years of data to fit model (see format_RAM_data.R)
ram_data <- read.csv('globalprep/fis/v2016/int/ram_data.csv')

bmsy_dl_ram <- bmsy_fao_rgn %>%
  left_join(ram_data, by=c('rgn_id', 'fao_id', "species", "year"))
head(bmsy_dl_ram) 
summary(bmsy_dl_ram)
#Thunnus alalunga


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
# ~47% of catch with b/bmsy estimates

tmp %>%
  filter(!is.na(bbmsy)) %>%
  filter(year >= 2005) %>%
  group_by(ram_data) %>%
  summarize(sum_tons = sum(tons)) %>%
  summarize(prop_catch_ram = sum_tons[ram_data=="yes"]/(sum(sum_tons))) %>%
  data.frame()

tmp %>%
  group_by(ram_data, year) %>%
  summarize(sum_tons = sum(tons)) %>%
  group_by(year) %>%
  summarize(prop_catch_ram = sum_tons[ram_data=="yes"]/(sum(sum_tons))) %>%
  data.frame()

#############################################################################################
## Comparing RAM and SAUP catch data to help validate whether or not these comparisons are 
## appropriate to determine our model choice. The theory being that if the catch streams are not similar
## between the same stock in SAUP and RAM, then we shouldn't expect the model outputs to be similar.
##
##
## JCA - 1.9.2017
############################################################################################

# Get RAM catch data. The ram_data above only has info on bbmsy from RAM, not catch time series

ram_ts <- read.csv('~/github/ohiprep/globalprep/fis/v2016/ram/ram_timeseries.csv')%>%
          filter(tsid %in% c('BdivBmgttouse-dimensionless', "TC-MT"))%>%  ## TC-MT is total catch in metric tons
          spread(tsid, tsvalue)%>%
          rename(ram_bbmsy = `BdivBmgttouse-dimensionless`,
                 tons      = `TC-MT`)

# Now we need to aggregate the ram stocks to FAO areas.

ram_ext <- read.csv('~/github/ohiprep/globalprep/fis/v2016/ram/ram_extended.csv')%>%
           select(stockid,region,FAO,commonname,scientificname)%>%
           unique()

ram <- ram_ts%>%
       left_join(ram_ext,by = c('stockid'))%>%
       group_by(stockid,tsyear,FAO,commonname,scientificname)%>%   #we need to calculate total, and mean (maybe not?) catch per FAO region for all RAM stocks to match up with SAUP  
       summarize(ram_catch = sum(tons,na.rm=T))%>%
       rename(year = tsyear,
              common = commonname,
              species = scientificname,
              fao_id = FAO)

# now we need to join this with SAUP data that has total catch per species per FAO region

# need to first aggregate catch to FAO level for SAUP 
catch_fao <- catch%>%
             group_by(fao_id, species, year, common)%>%
             summarize(saup_catch = sum(tons,na.rm=T))

both_ts <- ram%>%
            inner_join(catch_fao,by = c('fao_id','species','common','year'))%>%
            group_by(stockid,year,common,species)%>%
            summarize(ram_catch = mean(ram_catch,na.rm=T), #RAM catch is doubled after the join, so take mean
                      saup_catch = sum(saup_catch,na.rm=T))%>%
            mutate(species_code = paste(stockid,common,sep = ':'))%>%
            mutate(species_code = str_replace_all(species_code," ","_"))


write.csv(both_ts,file = '~/github/ohiprep/globalprep/fis/v2016/saup_ram_catch_compare/saup_ram_catch_ts.csv')


### SOme of hte catch series definitely do line up after looking at the app.
## next get a measurement of correlation between the two datasets, and maybe we should rerun our model
## comparisons only on those with a high correlation? still don't expect perfect match due to
## the models not being great to begin with but it's a better comparison

## Group by each stock and get a correlation coefficent (using Pearsons for now, I'm sure there might be better ways to do this)

#it seems like the Pearsons correlation gives an estiamte of trend more than similarity of magnitude of catches

library(TSdist) #TSDistances() comes from this package

catch_corrs <- both_ts%>%
               group_by(stockid)%>%
               summarize(corr = cor(ram_catch,saup_catch),
                         ccorr = TSDistances(ram_catch,saup_catch,distance='ccor'))%>% #using the cross-correlation based distance between two numeric time series
               filter(!is.na(corr)) #remove NAs

nrow(catch_corrs) #we have 71 stocks to compare.

## Filtering out stocks that have a Pearson's rank correlation >0.5 and cross-corr distance <0.5 (arbitrarily picked)

df <- catch_corrs%>%
      filter(corr>0.8)#,ccorr<0.5)

nrow(df) #now we have 32 stocks to compare.

## Use the unique stock ids in the df to filter out the bbmsy comparisons above and repeat the plots mel made

#****BELOW IS THE PLOT COMPARISON CODE COPIED FROM ABOVE******

data = data%>%
       filter(stockid_ram %in% df$stockid)%>% #this leads to 23 stocks to compare
       group_by(stockid_ram)%>%
       summarize(b_bmsy_cmsy = mean(b_bmsy_cmsy,na.rm=T),
                 b_bmsy_comsir = mean(b_bmsy_comsir, na.rm=T),
                 b_bmsy_sscom = mean(b_bmsy_sscom,na.rm=T),
                 mean_all = mean(mean_all,na.rm=T),
                 ram_bmsy = mean(ram_bmsy,na.rm=T))
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



>>>>>>> 196feca22eeb6908a7654e4e3f53e4a7e65cea1d
