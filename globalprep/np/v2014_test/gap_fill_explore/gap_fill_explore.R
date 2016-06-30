############################################################
## Exploring methods of gap-filling for NP commodities
## mrf 4/2/2015
############################################################

# This method is based on explorations from: gap_fill_explore.R and Issue #397 and this document:
# https://www.lucidchart.com/documents/edit/20f29b5a-4a15-4128-ac40-9f14a7f1ccdc?shared=true

#####################################################################
### setup ---- libraries, pathnames, etc
#####################################################################

### load libraries. Note dplyr, tidyr, stringr are loaded later in common.R
library(zoo)  
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)


dir_d <- 'globalprep/FAO_commodities'
source(sprintf('%s/R/np_fxn.R', dir_d))

h_tonnes <- read.csv(file.path(dir_d, 'v2014_test/intermediate/tonnes.csv'))
h_usd    <- read.csv(file.path(dir_d, 'v2014_test/intermediate/usd.csv'))


h <- np_harvest_cat(h_tonnes, h_usd)
### concatenates h_tonnes and h_usd data
### h includes rgn_name, rgn_id, commodity, product, year, tonnes, usd.


h <- h %>% np_harvest_preclip
### clips out years prior to first reporting year, for each commodity per region

h <- h %>% np_harvest_gapflag  
### Adds flag for required gap-filling, based upon NAs in data. 
### NOTE: Does not perform any gap-filling.
### At this point, h includes: 
###   rgn_name   rgn_id   commodity   product   year   tonnes   usd   gapfill
### 'gapfill' will be in (zerofill, endfill, tbd, none)

data_check <- h %>% np_datacheck()
### for each commodity within each region, creates (but doesn't save...) summary info:
###   num_years:        the length of the data series for this commodity in this region
###   usd_unique_nz:    (or 'tns') number of unique non-zero values for usd or tonnes 
###   usd_na & tns_na:  number of NA occurrences
###   paired_obs:       number of non-zero paired observations
###   usd_unique_pairs: (or 'tns') within set of paired observations, count of unique usd and tonnes
###   unique_pairs:     lesser of usd_unique_pairs and tns_unique_pairs
###   count_no_data:    number of paired NAs - years with no value reported

h <- h %>% np_zerofill
### for post-reporting years with NA for both tonnes and USD, fill zero - 
### assumes that non-reporting indicates zero harvest to report.
### Also cross-fills zeros where one side is 0, other is NA (not flagged as gapfill)

h <- h %>% np_lowdata_filter()
### Exclude commodities (within a region) that have few non-zero data points.
### Optional parameter with default: nonzero_h_yr_min = 4
### NOTE: This filter has consequences for the regression, but also has meaning in terms of 
###       not inflicting a penalty on regions trying, and then stopping, an experimental harvest.

h <- h %>% add_georegion_id()
### Melanie's script to add a georegional ID tag based on country keys and IDs.


h <- h %>%
  filter(usd>0)
# eliminates samples where both values are 0


#####################################################
## Calculate error for within country relationships
#####################################################

## within country relationship:
years_back <- 10 
lower_bound_year <- max(h$year) - years_back

## min number of paired data:
min_pairs <- 4

rgn_data <- h %>%
  filter(year > lower_bound_year) %>%
  filter(!(is.na(tonnes))) %>%
  group_by(rgn_name, commodity) %>%
  mutate(pairs = length(!is.na(tonnes))) %>%
  ungroup() %>%
  filter(pairs >= min_pairs) %>%
  mutate(rgn_com = paste(rgn_id, commodity, sep="_")) %>%
  mutate(predict_rgn_loo = NA) %>%
  mutate(predict_rgn = NA) %>%
  mutate(predict_rgn_year_loo = NA) %>%
  mutate(predict_rgn_year = NA)

units <- unique(rgn_data$rgn_com)

for(unit in units){
#unit <- '5_Miscellaneous corals and shells'

tmp <- rgn_data[rgn_data$rgn_com == unit, ]
years_tmp <- unique(tmp$year)

for(year_loo in years_tmp){
#year_loo <- 2003
pred <- subset(tmp, year == year_loo) 

## leave one out model, rgn only:
mod_loo <- lm(tonnes ~ usd, data = subset(tmp, year != year_loo))
rgn_data$predict_rgn_loo[rgn_data$rgn_com == unit & rgn_data$year == year_loo] <- predict(mod_loo, newdata=pred)

## standard model, rgn only:
mod <- lm(tonnes ~ usd, data = tmp)
rgn_data$predict_rgn[rgn_data$rgn_com == unit & rgn_data$year == year_loo] <- predict(mod, newdata=pred)

## leave one out model, rgn and year:
mod_loo_yr <- lm(tonnes ~ usd + year, data = subset(tmp, year != year_loo))
rgn_data$predict_rgn_year_loo[rgn_data$rgn_com == unit & rgn_data$year == year_loo] <- predict(mod_loo_yr, newdata=pred)

## standard model, rgn only:
mod_yr <- lm(tonnes ~ usd + year, data = tmp)
rgn_data$predict_rgn_year[rgn_data$rgn_com == unit & rgn_data$year == year_loo] <- predict(mod_yr, newdata=pred)
}
}

plot(tonnes ~ predict_rgn_loo, data=rgn_data)
plot(tonnes ~ predict_rgn, data=rgn_data)
plot(tonnes ~ predict_rgn_year_loo, data=rgn_data)
plot(tonnes ~ predict_rgn_year, data=rgn_data)

### RMSE calculations
sqrt(mean((rgn_data$predict_rgn_loo - rgn_data$tonnes)^2))
sqrt(mean((rgn_data$predict_rgn - rgn_data$tonnes)^2))
sqrt(mean((rgn_data$predict_rgn_year_loo - rgn_data$tonnes)^2))
sqrt(mean((rgn_data$predict_rgn_year - rgn_data$tonnes)^2))


#####################################################
## Calculate error for within georegion relationships
#####################################################

## within country relationship:
years_back <- 10 
lower_bound_year <- max(h$year) - years_back

## min number of paired data:
min_pairs <- 4

georgn_data <- h %>%
  filter(year > lower_bound_year) %>%
  filter(!(is.na(tonnes))) %>%
  group_by(georgn_id, commodity) %>%
  mutate(pairs = length(!is.na(tonnes))) %>%
  ungroup() %>%
  filter(pairs >= min_pairs) %>%
  mutate(georgn_com = paste(georgn_id, commodity, sep="_")) %>%
  mutate(predict_id = paste(rgn_id, year, sep="_")) %>%
  mutate(predict_rgn_loo = NA) %>%
  mutate(predict_rgn = NA) %>%
  mutate(predict_rgn_year_loo = NA) %>%
  mutate(predict_rgn_year = NA)

units <- unique(georgn_data$georgn_com)

for(unit in units){
  # unit <- "54_Miscellaneous corals and shells"
  
  tmp <- georgn_data[georgn_data$georgn_com == unit, ]
  id_tmp <- unique(tmp$predict_id)
  
  for(id in id_tmp){
  #  id <- '5_2002'
    pred <- subset(tmp, predict_id == id) 
    
    ## leave one out model, rgn only:
    mod_loo <- lm(tonnes ~ usd, data = subset(tmp, predict_id != id))
    georgn_data$predict_rgn_loo[georgn_data$georgn_com == unit & georgn_data$predict_id == id] <- predict(mod_loo, newdata=pred)
    
    ## standard model, rgn only:
    mod <- lm(tonnes ~ usd, data = tmp)
    georgn_data$predict_rgn[georgn_data$georgn_com == unit & georgn_data$predict_id == id] <- predict(mod, newdata=pred)

    ## leave one out model, rgn and year:
    mod_loo_yr <- lm(tonnes ~ usd + year, data = subset(tmp, year != year_loo))
    georgn_data$predict_rgn_year_loo[georgn_data$georgn_com == unit & georgn_data$predict_id == id] <- predict(mod_loo_yr, newdata=pred)

    ## standard model, rgn only:
    mod_yr <- lm(tonnes ~ usd + year, data = tmp)
    georgn_data$predict_rgn_year[georgn_data$georgn_com == unit & georgn_data$predict_id == id] <- predict(mod_yr, newdata=pred)
  }
}

plot(tonnes ~ predict_rgn_loo, data=georgn_data)
plot(tonnes ~ predict_rgn, data=georgn_data)
plot(tonnes ~ predict_rgn_year_loo, data=georgn_data)
plot(tonnes ~ predict_rgn_year, data=georgn_data)

### RMSE calculations
sqrt(mean((georgn_data$predict_rgn_loo - georgn_data$tonnes)^2))
sqrt(mean((georgn_data$predict_rgn - georgn_data$tonnes)^2))
sqrt(mean((georgn_data$predict_rgn_year_loo - georgn_data$tonnes)^2))
sqrt(mean((georgn_data$predict_rgn_year - georgn_data$tonnes)^2))


#####################################################
## Calculate error for global relationships
#####################################################

## within country relationship:
years_back <- 10 
lower_bound_year <- max(h$year) - years_back

## min number of paired data:
min_pairs <- 4

global_data <- h %>%
  filter(year > lower_bound_year) %>%
  filter(!(is.na(tonnes))) %>%
  group_by(commodity) %>%
  mutate(pairs = length(!is.na(tonnes))) %>%
  ungroup() %>%
  filter(pairs >= min_pairs) %>%
  mutate(predict_id = paste(rgn_id, year, sep="_")) %>%
  mutate(predict_rgn_loo = NA) %>%
  mutate(predict_rgn = NA) %>%
  mutate(predict_rgn_year_loo = NA) %>%
  mutate(predict_rgn_year = NA)

units <- unique(global_data$commodity)

for(unit in units){
   #unit <- "Miscellaneous corals and shells"
  
  tmp <- global_data[global_data$commodit == unit, ]
  id_tmp <- unique(tmp$predict_id)
  
  for(id in id_tmp){
    #  id <- '5_2002'
    pred <- subset(tmp, predict_id == id) 
    
    ## leave one out model, rgn only:
    mod_loo <- lm(tonnes ~ usd, data = subset(tmp, predict_id != id))
    global_data$predict_rgn_loo[global_data$commodity == unit & global_data$predict_id == id] <- predict(mod_loo, newdata=pred)
    
    ## standard model, rgn only:
    mod <- lm(tonnes ~ usd, data = tmp)
    global_data$predict_rgn[global_data$commodity == unit & global_data$predict_id == id] <- predict(mod, newdata=pred)
    
    ## leave one out model, rgn and year:
    mod_loo_yr <- lm(tonnes ~ usd + year, data = subset(tmp, year != year_loo))
    global_data$predict_rgn_year_loo[global_data$commodity == unit & global_data$predict_id == id] <- predict(mod_loo_yr, newdata=pred)
    
    ## standard model, rgn only:
    mod_yr <- lm(tonnes ~ usd + year, data = tmp)
    global_data$predict_rgn_year[global_data$commodity == unit & global_data$predict_id == id] <- predict(mod_yr, newdata=pred)
  }
}

plot(tonnes ~ predict_rgn_loo, data=global_data)
plot(tonnes ~ predict_rgn, data=global_data)
plot(tonnes ~ predict_rgn_year_loo, data=global_data)
plot(tonnes ~ predict_rgn_year, data=global_data)

### RMSE calculations
sqrt(mean((global_data$predict_rgn_loo - global_data$tonnes)^2))
sqrt(mean((global_data$predict_rgn - global_data$tonnes)^2))
sqrt(mean((global_data$predict_rgn_year_loo - global_data$tonnes)^2))
sqrt(mean((global_data$predict_rgn_year - global_data$tonnes)^2))

