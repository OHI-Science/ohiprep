############################################################
## Exploring methods of gap-filling for NP commodities
############################################################

#####################################################################
### setup ---- libraries, pathnames, etc
#####################################################################

### load libraries. Note dplyr, tidyr, stringr are loaded later in common.R
library(zoo)  
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)


####### Create mod and test data sets -----


### set dir_neptune_data and load common libraries (tidyr, dplyr, stringr) 


dir_d <- 'globalprep/FAO_Commodities'
### NOTE: Set output paths to here, but do not use setwd().
###       This way, any scripts and code in ohiprep will still work, b/c always in root ohiprep dir.

### Load NP-specific user-defined functions
source(sprintf('%s/R/np_fxn.R', dir_d))


#####################################################################
### read in and process files -- loops across value and quant data
#####################################################################

   f=list.files(file.path(dir_d, 'v2015/raw'), pattern=glob2rx('*.csv'), full.names=T)[1]
  
  ### data read in
  cat(sprintf('\n\n\n====\nfile: %s\n', basename(f)))
  d <- read.csv(f, check.names=F, strip.white=TRUE) # , stringsAsFactors=T
  units <- c('tonnes','usd')[str_detect(f, c('quant','value'))] # using American English, lowercase
  
  
  #####################################################################
  ### gather into long format and clean up FAO-specific data foibles
  #####################################################################
  
    ### warning: attributes are not identical across measure variables; they will be dropped
    m <- d %>% 
      rename(country   = `Country (Country)`,
             commodity = `Commodity (Commodity)`,
             trade     = `Trade flow (Trade flow)`) %>%
      gather(year, value, -country, -commodity, -trade)
  
  
  m <- m %>%
    filter(!country %in% c('Totals', 'Yugoslavia SFR')) %>%
    mutate(  
      value = str_replace(value, fixed( ' F'),    ''),  # FAO denotes with F when they have estimated the value using best available data
      value = str_replace(value, fixed('0 0'), '0.1'),  # FAO denotes something as '0 0' when it is > 0 but < 1/2 of a unit. 
      value = str_replace(value, fixed(  '-'),   '0'),  # FAO's 0
      value = str_replace(value, fixed('...'),    NA),  # FAO's NA
      value = str_replace(value, fixed('.'),      NA),
      value = ifelse(value =='', NA, value),  
      value = as.numeric(as.character(value)),
      year  = as.integer(as.character(year))) %>% 
  # search in R_inferno.pdf for "shame on you"
    select(country, commodity, year, tonnes=value) %>%                                  # eliminate 'trade' column
    arrange(country, commodity, is.na(tonnes), year)


### dollar value
f=list.files(file.path(dir_d, 'v2015/raw'), pattern=glob2rx('*.csv'), full.names=T)[2]

### data read in
cat(sprintf('\n\n\n====\nfile: %s\n', basename(f)))
d <- read.csv(f, check.names=F, strip.white=TRUE) # , stringsAsFactors=T
units <- c('tonnes','usd')[str_detect(f, c('quant','value'))] # using American English, lowercase


#####################################################################
### gather into long format and clean up FAO-specific data foibles
#####################################################################

### warning: attributes are not identical across measure variables; they will be dropped
m_usd <- d %>% 
  rename(country   = `Country (Country)`,
         commodity = `Commodity (Commodity)`,
         trade     = `Trade flow (Trade flow)`) %>%
  gather(year, value, -country, -commodity, -trade)


m_usd <- m_usd %>%
  filter(!country %in% c('Totals', 'Yugoslavia SFR')) %>%
  mutate(  
    value = str_replace(value, fixed( ' F'),    ''),  # FAO denotes with F when they have estimated the value using best available data
    value = str_replace(value, fixed('0 0'), '0.1'),  # FAO denotes something as '0 0' when it is > 0 but < 1/2 of a unit. 
    value = str_replace(value, fixed(  '-'),   '0'),  # FAO's 0
    value = str_replace(value, fixed('...'),    NA),  # FAO's NA
    value = str_replace(value, fixed('.'),      NA),
    value = ifelse(value =='', NA, value),  
    value = as.numeric(as.character(value)),
    year  = as.integer(as.character(year))) %>% 
  # search in R_inferno.pdf for "shame on you"
  select(country, commodity, year, usd=value) %>%                                  # eliminate 'trade' column
  arrange(country, commodity, is.na(usd), year)


data <- m %>%
  left_join(m_usd) %>%
  rowwise() %>%
  mutate(NAnum= sum(c(is.na(tonnes), is.na(usd)))) %>%
  group_by(country, commodity) %>%
  mutate(N_reg = sum(NAnum==0)) %>%
  mutate(N_gap = sum(NAnum==1)) %>%
  na.omit()

set.seed(227)
randSamp <- sample(1:length(data$usd), length(data$usd)*.05)
length(randSamp)

data_test <- data[randSamp,]
write.csv(data_test, 'globalprep/FAO_commodities/gap_fill_explore/data_test.csv', row.names=FALSE)
data_train <- data[-randSamp,]
write.csv(data_train, 'globalprep/FAO_commodities/gap_fill_explore/data_train.csv', row.names=FALSE)

############# Read in test/mod data ----

data_test <- read.csv('globalprep/FAO_commodities/gap_fill_explore/data_test.csv')
data_train <- read.csv('globalprep/FAO_commodities/gap_fill_explore/data_train.csv')



####################################################3
### Reading in new data - realized that I included a lot of 
#### zeros that probably aren't legit
#######################################################
data <- read.csv(file.path(dir_d, "gap_fill_explore/cv_data.csv"))

data <- data %>%
  rowwise() %>%
  mutate(NAnum= sum(c(is.na(tonnes), is.na(usd)))) %>%
  group_by(rgn_name, commodity) %>%
  mutate(N_reg = sum(NAnum==0)) %>%
  mutate(N_gap = sum(NAnum==1)) %>%
  na.omit()



data <- na.omit(data)
set.seed(227)
randSamp <- sample(1:length(data$usd), length(data$usd)*.05)
length(randSamp)

data_test <- data[randSamp,]
write.csv(data_test, 'globalprep/FAO_commodities/gap_fill_explore/cv_data_test.csv', row.names=FALSE)
data_train <- data[-randSamp,]
write.csv(data_train, 'globalprep/FAO_commodities/gap_fill_explore/cv_data_train.csv', row.names=FALSE)

############# Read in test/mod data ----

data_test <- read.csv('globalprep/FAO_commodities/gap_fill_explore/cv_data_test.csv', stringsAsFactors=FALSE) %>%
  select(rgn_id, country=rgn_name, commodity, year, tonnes, usd, NAnum, N_reg, N_gap)
data_train <- read.csv('globalprep/FAO_commodities/gap_fill_explore/cv_data_train.csv', stringsAsFactors=FALSE) %>%
  select(rgn_id, country=rgn_name, commodity, year, tonnes, usd, NAnum, N_reg, N_gap)


############# Model 1: lm with year/country

coms <- unique(data_train$commodity)
predict_tonnes <- data.frame()

for(i in 1:length(coms)){ 
  #i=1
data_train_sub <- data_train %>%
  filter(commodity==coms[i]) 
predict_tonnes_sub <- data_test %>%
  filter(commodity==coms[i]) %>%
  filter(country %in% unique(data_train_sub$country)) %>% # only include if country is in training dataset, otherwise function breaks
  select(country, year, commodity, usd)

if(length(predict_tonnes_sub$usd)==0 |sum(data_train_sub$usd)==0 | length(unique(data_train_sub$country))<2){ #move to next if there are if resulting testing data is N=0
  predict_tonnes <- predict_tonnes
  cat(i)
} else {
mod <- lm(tonnes~usd + year + country, data=data_train_sub)

predict_tonnes_sub$full_model_predict <- predict(mod, newdata=predict_tonnes_sub)
predict_tonnes_sub$full_model_predict <- round(predict_tonnes_sub$full_model_predict, 0)
predict_tonnes_sub$full_model_predict <- ifelse(predict_tonnes_sub$usd==0, 
                                                0,
                                                predict_tonnes_sub$full_model_predict)
predict_tonnes_sub$full_model_predict <- ifelse(predict_tonnes_sub$full_model_predict<0, 
                                                1,
                                                predict_tonnes_sub$full_model_predict)
predict_tonnes <- rbind(predict_tonnes, predict_tonnes_sub)
cat(i)
}
}


data_test_pred <- data_test %>%
  left_join(predict_tonnes)

ggplot(data_test_pred, aes(y=log(full_model_predict), x=log(tonnes))) +
  geom_point(shape=19, alpha=0.2) +
  labs(x="tonnes_observed, ln", y="tonnes_predicted, ln") + 
  theme_bw()

mod <- lm(full_model_predict ~ tonnes, data=data_test_pred)
summary(mod)



############# Model 2: lm with year (no country)

coms <- unique(data_train$commodity)
predict_tonnes <- data.frame()

for(i in 1:length(coms)){ 
  #i=241
  data_train_sub <- data_train %>%
    filter(commodity==coms[i]) 
  predict_tonnes_sub <- data_test %>%
    filter(commodity==coms[i]) %>%
    select(country, year, commodity, usd)
  
  if(length(predict_tonnes_sub$usd)==0 |sum(data_train_sub$usd)==0 ){ #move to next if there are if resulting testing data is N=0
    predict_tonnes <- predict_tonnes
    cat(i)
  } else {
    mod <- lm(tonnes~ usd + year, data=data_train_sub)
    
    predict_tonnes_sub$full_model_predict <- predict(mod, newdata=predict_tonnes_sub)
    predict_tonnes_sub$full_model_predict <- round(predict_tonnes_sub$full_model_predict, 0)
    predict_tonnes_sub$full_model_predict <- ifelse(predict_tonnes_sub$usd==0, 
                                                    0,
                                                    predict_tonnes_sub$full_model_predict)
    predict_tonnes_sub$full_model_predict <- ifelse(predict_tonnes_sub$full_model_predict<0, 
                                                    1,
                                                    predict_tonnes_sub$full_model_predict)
    predict_tonnes <- rbind(predict_tonnes, predict_tonnes_sub)
    cat(i)
  }
}


data_test_pred <- data_test %>%
  left_join(predict_tonnes)

ggplot(data_test_pred, aes(y=log(full_model_predict), x=log(tonnes))) +
  labs(x="tonnes_observed, ln", y="tonnes_predicted, ln") + 
  geom_point(shape=19, alpha=0.2) +
  theme_bw()

mod <- lm(full_model_predict ~ tonnes, data=data_test_pred)
summary(mod)

############# Model 3: country (no year)

coms <- unique(data_train$commodity)
predict_tonnes <- data.frame()

for(i in 1:length(coms)){ 
  #i=241
  data_train_sub <- data_train %>%
    filter(commodity==coms[i]) 
  predict_tonnes_sub <- data_test %>%
    filter(commodity==coms[i]) %>%
    filter(country %in% unique(data_train_sub$country)) %>% # only include if country is in training dataset, otherwise function breaks
    select(country, commodity, year, usd)
  
  if(length(predict_tonnes_sub$usd)==0 |sum(data_train_sub$usd)==0 | length(unique(data_train_sub$country))<2){ #move to next if there are if resulting testing data is N=0
    predict_tonnes <- predict_tonnes
    cat(i)
  } else {
    mod <- lm(tonnes~usd + country, data=data_train_sub)
    
    predict_tonnes_sub$full_model_predict <- predict(mod, newdata=predict_tonnes_sub)
    predict_tonnes_sub$full_model_predict <- round(predict_tonnes_sub$full_model_predict, 0)
    predict_tonnes_sub$full_model_predict <- ifelse(predict_tonnes_sub$usd==0, 
                                                    0,
                                                    predict_tonnes_sub$full_model_predict)
    predict_tonnes_sub$full_model_predict <- ifelse(predict_tonnes_sub$full_model_predict<0, 
                                                    1,
                                                    predict_tonnes_sub$full_model_predict)
    predict_tonnes <- rbind(predict_tonnes, predict_tonnes_sub)
    cat(i)
  }
}


data_test_pred <- data_test %>%
  left_join(predict_tonnes)

ggplot(data_test_pred, aes(y=log(full_model_predict), x=log(tonnes))) +
  geom_point(shape=19, alpha=0.2) +
  labs(x="tonnes_observed, ln", y="tonnes_predicted, ln") + 
  theme_bw()

mod <- lm(full_model_predict ~ tonnes, data=data_test_pred)
summary(mod)



############# Model 4: linear interpolation based on surrounding values
data_train$cat <- "train"
data_test$cat <- "test"

data_zoo <- rbind(data_train, data_test)

## trying another method (I think I was skipping a few years, which might make the weighting different)
data_zoo_lm <- data_zoo %>%
#   filter(country=="Philippines", commodity=="Oyster meat, prepared or preserved, nei") %>%
  arrange(country, commodity, year) %>%
  group_by(country, commodity) %>%
 filter(N_reg>3) %>% #N_reg is sample size, need surrounding values for na.fill
  mutate(tonnes_test = ifelse(cat=="test", NA, tonnes)) %>%
  do({
    tonnes_zoo <- zoo(.[["tonnes_test"]], .[["year"]])
    tonnes_pred <- na.fill(tonnes_zoo, "extend")
    data.frame(., tonnes_pred)
  })


data_zoo_lm_test  <- data_zoo_lm %>%
  filter(cat=="test") %>%
  mutate(tonnes_pred = round(tonnes_pred, 0)) %>%
  mutate(tonnes_pred = ifelse(usd==0, 0, tonnes_pred)) %>%
  mutate(tonnes_pred = ifelse(tonnes_pred<0, 1, tonnes_pred))         


ggplot(data_zoo_lm_test, aes(y=log(tonnes_pred), x=log(tonnes))) +
  geom_point(shape=19, alpha=0.2) +
  labs(x="tonnes_observed, ln", y="tonnes_predicted, ln") + 
  theme_bw()


mod <- lm(tonnes_pred ~ tonnes, data=data_zoo_lm_test)
summary(mod)


############# Model 5: splinal interpolation based on surrounding values
data_train$cat <- "train"
data_test$cat <- "test"

data_zoo <- rbind(data_train, data_test)

data_zoo_sp <- data_zoo %>%
  #   filter(country=="Philippines", commodity=="Oyster meat, prepared or preserved, nei") %>%
  arrange(country, commodity, year) %>%
  group_by(country, commodity) %>%
  filter(N_reg>3) %>% #N_reg is sample size, need surrounding values for na.fill
  mutate(tonnes_test = ifelse(cat=="test", NA, tonnes)) %>%
  do({
    tonnes_zoo <- zoo(.[["tonnes_test"]], .[["year"]])
    tonnes_pred <- na.spline(tonnes_zoo)
    data.frame(., tonnes_pred)
  })


# data_zoo_sp  <- data_zoo %>%
#   filter(N_reg>3) %>%
#   mutate(tonnes_test = ifelse(cat=="test", NA, tonnes)) %>%
#   arrange(country, commodity, year) %>%
#   group_by(country, commodity) %>%
#   mutate(tonnes_pred = zoo::na.spline(tonnes_test)) 


data_zoo_sp_test  <- data_zoo_sp %>%
  filter(cat=="test") %>%
  mutate(tonnes_pred = round(tonnes_pred, 0)) %>%
  mutate(tonnes_pred = ifelse(usd==0, 0, tonnes_pred)) %>%
  mutate(tonnes_pred = ifelse(tonnes_pred<0, 1, tonnes_pred))         


ggplot(data_zoo_sp_test, aes(y=log(tonnes_pred), x=log(tonnes))) +
  geom_point(shape=19, alpha=0.2) +
  labs(x="tonnes_observed, ln", y="tonnes_predicted, ln") + 
  theme_bw()


mod <- lm(tonnes_pred ~ tonnes, data=data_zoo_sp_test)
summary(mod)




############# Model 6: within country/commodity linear model
data_train$cat <- "train"
data_test$cat <- "test"

data_zoo <- rbind(data_train, data_test)

data_mod_country_lm <- data_zoo %>%
#   filter(country=="Philippines",
#          commodity=="Oyster meat, prepared or preserved, nei") %>%
  filter(N_reg>=4) %>%
  group_by(country, commodity) %>%
  mutate(sum_usd=sum(usd)) %>%
  filter(sum_usd>0) %>%
  do({
    mod <- lm(tonnes ~ usd, data=.)
    tonnes_pred <- predict(mod, newdata=.['usd'])
    data.frame(., tonnes_pred)
  })


data_mod_country_lm  <- data_mod_country_lm %>%
  filter(cat=="test") %>%
  mutate(tonnes_pred = round(tonnes_pred, 0)) %>%
  mutate(tonnes_pred = ifelse(usd==0, 0, tonnes_pred)) %>%
  mutate(tonnes_pred = ifelse(tonnes_pred<0, 1, tonnes_pred))         


ggplot(data_mod_country_lm, aes(y=log(tonnes_pred), x=log(tonnes))) +
  geom_point(shape=19, alpha=0.2) +
  labs(x="tonnes_observed, ln", y="tonnes_predicted, ln") + 
  theme_bw()


mod <- lm(tonnes_pred ~ tonnes, data=data_mod_country_lm)
summary(mod)



############# Model 7: within georegion/commodity linear model including time
data_train$cat <- "train"
data_test$cat <- "test"

data_zoo <- rbind(data_train, data_test)

key <- read.csv("C:/Users/Melanie/github/ohi-global/eez2014/layers/cntry_rgn.csv")
dups <- key$rgn_id[duplicated(key$rgn_id)]
key[key$rgn_id %in% dups, ]

key  <- key %>%
  filter(!(cntry_key %in% c('Galapagos Islands', 'Alaska',
                          'Hawaii', 'Trindade', 'Easter Island',
                          'PRI', 'GLP', 'MNP')))  #PRI (Puerto Rico) and VIR (Virgin Islands) in the same r2 zone (just selected one), 
                                     #GLP (Guadalupe) and MTQ (Marinique) in the same r2 zone (just selected one),  
                                #MNP (Northern Mariana Islands) and GUM (Guam)



georegion <- read.csv("C:/Users/Melanie/github/ohi-global/eez2014/layers/cntry_georegions.csv")
unique(georegion$georgn_id[georegion$level=="r0"])  # 1 level
unique(georegion$georgn_id[georegion$level=="r1"])  # 7 levels
unique(georegion$georgn_id[georegion$level=="r2"])  # 22 levels

georegion <- georegion %>%
  filter(level == "r2")

data_zoo2  <- data_zoo %>%
  left_join(key) %>%
  left_join(georegion) %>%
  rowwise() %>%
  mutate(non_zero_num = sum(c(tonnes>0, usd>0))) %>%
  group_by(commodity, georgn_id) %>%
  mutate(reg_num_geo=sum(non_zero_num>=2))

data_mod_country_lm <- data_zoo %>%
  #   filter(country=="Philippines",
  #          commodity=="Oyster meat, prepared or preserved, nei") %>%
  group_by(georgn_id, commodity) %>%
  mutate(sum_usd=sum(usd)) %>%  # do not include when all usd data is equal to 0
  filter(sum_usd>0) %>%
  do({
    mod <- lm(tonnes ~ usd + year, data=.)
    tonnes_pred <- predict(mod, newdata=.[c('usd', 'year')])
    data.frame(., tonnes_pred)
  })


data_mod_country_lm  <- data_mod_country_lm %>%
  filter(cat=="test") %>%
  mutate(tonnes_pred = round(tonnes_pred, 0)) %>%
  mutate(tonnes_pred = ifelse(usd==0, 0, tonnes_pred)) %>%
  mutate(tonnes_pred = ifelse(tonnes_pred<0, 1, tonnes_pred))         


ggplot(data_mod_country_lm, aes(y=log(tonnes_pred), x=log(tonnes))) +
  geom_point(shape=19, alpha=0.2) +
  labs(x="tonnes_observed, ln", y="tonnes_predicted, ln") + 
  theme_bw()


mod <- lm(tonnes_pred ~ tonnes, data=data_mod_country_lm)
summary(mod)

############# Model 8: within georegion/commodity linear model no time
data_mod_country_lm <- data_zoo %>%
  #   filter(country=="Philippines",
  #          commodity=="Oyster meat, prepared or preserved, nei") %>%
  group_by(georgn_id, commodity) %>%
  mutate(sum_usd=sum(usd)) %>%
  filter(sum_usd>0) %>%
  do({
    mod <- lm(tonnes ~ usd, data=.)
    tonnes_pred <- predict(mod, newdata=.[c('usd')])
    data.frame(., tonnes_pred)
  })


data_mod_country_lm  <- data_mod_country_lm %>%
  filter(cat=="test") %>%
  mutate(tonnes_pred = round(tonnes_pred, 0)) %>%
  mutate(tonnes_pred = ifelse(usd==0, 0, tonnes_pred)) %>%
  mutate(tonnes_pred = ifelse(tonnes_pred<0, 1, tonnes_pred))         


ggplot(data_mod_country_lm, aes(y=log(tonnes_pred), x=log(tonnes))) +
  geom_point(shape=19, alpha=0.2) +
  labs(x="tonnes_observed, ln", y="tonnes_predicted, ln") + 
  theme_bw()


mod <- lm(tonnes_pred ~ tonnes, data=data_mod_country_lm)
summary(mod)





