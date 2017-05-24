###########################################################
## script to combine RAM and CMSY derived B/Bmsy values
## at the FAO/region scale
###########################################################

library(dplyr)
library(tidyr)
library(zoo)
library(stringr)


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
    dplyr::select(stock_id, year, bbmsy_q2.5,bbmsy_q97.5,bbmsy_sd, bbmsy_mean, prior, model) %>%
    arrange(stock_id, year) %>%
    group_by(stock_id) %>%
    mutate(mean_5year        = rollmean(bbmsy_mean, 5, align="right", fill=NA))
  write.csv(b_bmsy, sprintf('globalprep/fis/v2016/int/%s_b_bmsy_%s_mean5yrs.csv', method, unique(b_bmsy$prior)), row.names=FALSE)
} 

new_b_bmsy(cmsy, method="cmsy")
new_b_bmsy(comsir, method="comsir")
new_b_bmsy(sscom, method="sscom")

#---------------------------------------------------------------------------------

# The CMSY is the only one we end up using based on analysis in compare_bmsy_data.R

cmsy <- read.csv('globalprep/fis/v2016/int/cmsy_b_bmsy_constrained_mean5yrs.csv') %>%
  select(stock_id, year, cmsy_bbmsy=mean_5year)

## created in format_RAM_data.R
ram <- read.csv("globalprep/fis/v2016/int/ram_bmsy.csv")


## Mean catch data created in "meanCatch.R"
mean_catch <- read.csv("globalprep/fis/v2016/data/mean_catch.csv") %>%
  mutate(stock_id_taxonkey = as.character(stock_id_taxonkey)) %>%
  mutate(taxon_key = str_sub(stock_id_taxonkey, -6, -1)) %>%
  mutate(stock_id = substr(stock_id_taxonkey, 1, nchar(stock_id_taxonkey)-7)) 


## combine data
setdiff(cmsy$stock_id, mean_catch$stock_id)
setdiff(mean_catch$stock_id, cmsy$stock_id)
intersect(mean_catch$stock_id, cmsy$stock_id) #946

setdiff(ram$stock_id, mean_catch$stock_id)
setdiff(mean_catch$stock_id, ram$stock_id)
intersect(ram$stock_id, mean_catch$stock_id) #188

data <- mean_catch %>%
  left_join(ram, by=c('stock_id', "year")) %>%
  group_by(rgn_id, taxon_key, stock_id, year, mean_catch) %>%    ### some regions have more than one stock...these will be averaged
  summarize(ram_bmsy = mean(ram_bmsy, na.rm=TRUE),
            gapfilled = ifelse(all(is.na(gapfilled)), NA, max(gapfilled, na.rm=TRUE))) %>%
  left_join(cmsy, by=c("stock_id", "year")) %>%
  ungroup()


## select best data and indicate gapfilling
 data <- data %>%
   mutate(bmsy_data_source = ifelse(!is.na(ram_bmsy), "RAM", NA)) %>%
   mutate(bmsy_data_source = ifelse(is.na(bmsy_data_source) & !is.na(cmsy_bbmsy), "CMSY", bmsy_data_source)) %>%
   mutate(bbmsy = ifelse(is.na(ram_bmsy), cmsy_bbmsy, ram_bmsy)) %>%
   select(rgn_id, stock_id, taxon_key, year, bbmsy, bmsy_data_source, RAM_gapfilled=gapfilled, mean_catch) %>%
   filter(year >= 2001) %>%
   unique()
 
write.csv(data, "globalprep/fis/v2016/data/fis_bbmsy_gf.csv", row.names=FALSE) 
 
bbmsy <- data %>%
  select(rgn_id, stock_id, year, bbmsy) %>%
  filter(!is.na(bbmsy)) %>%
  unique()

write.csv(bbmsy, "globalprep/fis/v2016/data/fis_bbmsy.csv", row.names=FALSE) 

#---------------------------------------------------------------------------------

###START - DATA PREP FOR FISHERIES MODEL EXPLORATION - JA - 12-15-16 & 1-18-17 (adding mPRM)

## Prepping the final bbmsy data with output from each of the catch-only models for fisheries goal exploration. OHI 2016 only used CMSY.

comsir <- read.csv('globalprep/fis/v2016/int/comsir_b_bmsy_NA_mean5yrs.csv') %>%
  select(stock_id, year, comsir_bbmsy=mean_5year)

sscom <- read.csv('globalprep/fis/v2016/int/sscom_b_bmsy_NA_mean5yrs.csv') %>%
  select(stock_id, year, sscom_bbmsy=mean_5year)

mprm <- read.csv('~/github/fish_models/mPRM/mprm_b_bmsy_NA_mean5yrs.csv')%>%
          select(stock_id, year, mprm_bbmsy = mean_5year)

### The code below is copied from the section above to reproduce fis_bbmsy.csv for SSCOM, COMSIR and mPRM

### Including RAM

#### COMSIR

data <- mean_catch %>%
  left_join(ram, by=c('stock_id', "year")) %>%
  group_by(rgn_id, taxon_key, stock_id, year, mean_catch) %>%    ### some regions have more than one stock...these will be averaged
  summarize(ram_bmsy = mean(ram_bmsy, na.rm=TRUE),
            gapfilled = ifelse(all(is.na(gapfilled)), NA, max(gapfilled, na.rm=TRUE))) %>%
  left_join(comsir, by=c("stock_id", "year")) %>%
  ungroup()


## select best data and indicate gapfilling
bbmsy <- data %>%
  mutate(bmsy_data_source = ifelse(!is.na(ram_bmsy), "RAM", NA)) %>%
  mutate(bmsy_data_source = ifelse(is.na(bmsy_data_source) & !is.na(comsir_bbmsy), "COMSIR", bmsy_data_source)) %>%
  mutate(bbmsy = ifelse(is.na(ram_bmsy), comsir_bbmsy, ram_bmsy)) %>%
  select(rgn_id, stock_id, year,bbmsy)%>%
  filter(year >= 2001)  %>%
  filter(!is.na(bbmsy)) %>%
  unique()

write.csv(bbmsy, "globalprep/fis/v2016/data/fis_comsir_bbmsy.csv", row.names=FALSE) 


### SSCOM

data <- mean_catch %>%
  left_join(ram, by=c('stock_id', "year")) %>%
  group_by(rgn_id, taxon_key, stock_id, year, mean_catch) %>%    ### some regions have more than one stock...these will be averaged
  summarize(ram_bmsy = mean(ram_bmsy, na.rm=TRUE),
            gapfilled = ifelse(all(is.na(gapfilled)), NA, max(gapfilled, na.rm=TRUE))) %>%
  left_join(sscom, by=c("stock_id", "year")) %>%
  ungroup()


## select best data and indicate gapfilling
bbmsy <- data %>%
  mutate(bmsy_data_source = ifelse(!is.na(ram_bmsy), "RAM", NA)) %>%
  mutate(bmsy_data_source = ifelse(is.na(bmsy_data_source) & !is.na(sscom_bbmsy), "SSCOM", bmsy_data_source)) %>%
  mutate(bbmsy = ifelse(is.na(ram_bmsy), sscom_bbmsy, ram_bmsy)) %>%
  select(rgn_id, stock_id, year, bbmsy) %>%
  filter(year >= 2001) %>%
  unique()%>%
  filter(!is.na(bbmsy)) %>%
  unique()

write.csv(bbmsy, "globalprep/fis/v2016/data/fis_sscom_bbmsy.csv", row.names=FALSE) 

### mPRM

data <- mean_catch %>%
  left_join(ram, by=c('stock_id', "year")) %>%
  group_by(rgn_id, taxon_key, stock_id, year, mean_catch) %>%    ### some regions have more than one stock...these will be averaged
  summarize(ram_bmsy = mean(ram_bmsy, na.rm=TRUE),
            gapfilled = ifelse(all(is.na(gapfilled)), NA, max(gapfilled, na.rm=TRUE))) %>%
  left_join(mprm, by=c("stock_id", "year")) %>%
  ungroup()


## select best data and indicate gapfilling
bbmsy <- data %>%
  mutate(bmsy_data_source = ifelse(!is.na(ram_bmsy), "RAM", NA)) %>%
  mutate(bmsy_data_source = ifelse(is.na(bmsy_data_source) & !is.na(mprm_bbmsy), "mPRM", bmsy_data_source)) %>%
  mutate(bbmsy = ifelse(is.na(ram_bmsy), mprm_bbmsy, ram_bmsy)) %>%
  select(rgn_id, stock_id, year, bbmsy) %>%
  filter(year >= 2001) %>%
  unique()%>%
  filter(!is.na(bbmsy)) %>%
  unique()

write.csv(bbmsy, "globalprep/fis/v2016/data/fis_mprm_bbmsy.csv", row.names=FALSE) 


## Without RAM

### COMSIR

data <- mean_catch %>%
  group_by(rgn_id, taxon_key, stock_id, year, mean_catch) %>%    ### some regions have more than one stock...these will be averaged
  left_join(comsir, by=c("stock_id", "year")) %>%
  ungroup()%>%
  mutate(bbmsy = comsir_bbmsy) %>%
  select(rgn_id, stock_id, taxon_key, year, bbmsy, mean_catch) %>%
  filter(year >= 2001) %>%
  unique()%>%
  select(rgn_id, stock_id, year, bbmsy) %>%
  filter(!is.na(bbmsy)) %>%
  unique()

write.csv(data, "globalprep/fis/v2016/data/fis_comsir_bbmsy_noRAM.csv", row.names=FALSE) 

### CMSY

data <- mean_catch %>%
  group_by(rgn_id, taxon_key, stock_id, year, mean_catch) %>%    ### some regions have more than one stock...these will be averaged
  left_join(cmsy, by=c("stock_id", "year")) %>%
  ungroup()%>%
  mutate(bbmsy = cmsy_bbmsy) %>%
  select(rgn_id, stock_id, taxon_key, year, bbmsy, mean_catch) %>%
  filter(year >= 2001) %>%
  unique()%>%
  select(rgn_id, stock_id, year, bbmsy) %>%
  filter(!is.na(bbmsy)) %>%
  unique()

write.csv(data, "globalprep/fis/v2016/data/fis_cmsy_bbmsy_noRAM.csv", row.names=FALSE) 

### SSCOM

data <- mean_catch %>%
  group_by(rgn_id, taxon_key, stock_id, year, mean_catch) %>%    ### some regions have more than one stock...these will be averaged
  left_join(sscom, by=c("stock_id", "year")) %>%
  ungroup()%>%
  mutate(bbmsy = sscom_bbmsy) %>%
  select(rgn_id, stock_id, taxon_key, year, bbmsy, mean_catch) %>%
  filter(year >= 2001) %>%
  unique()%>%
  select(rgn_id, stock_id, year, bbmsy) %>%
  filter(!is.na(bbmsy)) %>%
  unique()

write.csv(data, "globalprep/fis/v2016/data/fis_sscom_bbmsy_noRAM.csv", row.names=FALSE) 


### mPRM

data <- mean_catch %>%
  group_by(rgn_id, taxon_key, stock_id, year, mean_catch) %>%    ### some regions have more than one stock...these will be averaged
  left_join(mprm, by=c("stock_id", "year")) %>%
  ungroup()%>%
  mutate(bbmsy = mprm_bbmsy) %>%
  select(rgn_id, stock_id, taxon_key, year, bbmsy, mean_catch) %>%
  filter(year >= 2001) %>%
  unique()%>%
  select(rgn_id, stock_id, year, bbmsy) %>%
  filter(!is.na(bbmsy)) %>%
  unique()

write.csv(data, "globalprep/fis/v2016/data/fis_mprm_bbmsy_noRAM.csv", row.names=FALSE) 

### END DATA PREP FOR FISHERIES MODEL EXPLORATION - JA - 12-15-16 & 1-18-17

#---------------------------------------------------------------------------------

 ### Exploring the data (CMSY only)
 
 # Finding stocks with high B/Bmsy values and are heavily fished by multiple regions (i.e., influential stocks)

 high_bmsy <- filter(data, bbmsy>1.5 & year==2010) %>%
   group_by(stock_id) %>%
   mutate(regions_high = length(bbmsy)) %>%
   ungroup() %>%
   filter(regions_high > 1) %>%  #ignore stocks with high bbmsy values only found in one ohi region
   filter(stock_id == "Scomberomorus_cavalla-31") %>%
   select(rgn_id, stock_id, year, regions_high, bbmsy) %>%
   mutate(high_bmsy = "yes")
 
 data.frame(unique(select(high_bmsy, stock_id, regions_high, bbmsy)) %>%
   arrange(-regions_high))

 highs <- data %>%
   filter(year==2010) %>%
   left_join(high_bmsy, by= c('rgn_id', 'stock_id', 'year')) %>%
   mutate(high_bmsy = ifelse(is.na(high_bmsy), "no", high_bmsy)) %>%
   group_by(rgn_id, high_bmsy) %>%
   summarize(mean_catch = sum(mean_catch)) %>%
   ungroup() 
 
 highs_filled <- highs %>%
   spread(high_bmsy, mean_catch) %>%
   gather(high_bmsy, "sum_mean_catch", 2:3) %>%
   mutate(sum_mean_catch = ifelse(is.na(sum_mean_catch), 0, sum_mean_catch)) %>%
   group_by(rgn_id) %>%
   mutate(prop_high_bmsy = sum_mean_catch[high_bmsy=="yes"]/sum(sum_mean_catch)) %>%
   mutate(prop_high_bmsy = round(prop_high_bmsy*100, 1)) %>%
   arrange(rgn_id)
 
 hist(highs_filled$prop_high_bmsy, main="Scomberomorus_cavalla-31")

 # list of B/Bmsy values over time for influential stocks with high B/Bmsy data
 
 tmp <- filter(data, stock_id %in% c("Katsuwonus_pelamis-71", "Clupea_harengus-27",
"Trachurus_capensis-47", "Sardinella_aurita-34", "Scomberomorus_cavalla-31")) %>%
select(stock_id, year, bbmsy, bmsy_data_source) %>%
unique()
 
write.csv(tmp, "globalprep/fis/v2016/int/influential_high_bbmsy.csv", row.names=FALSE)
 
#########################################
## checking data for toolbox

# data should now go to 2001 and I checked a couple regions that went wrong previously
# this looks good

b <- read.csv('globalprep/fis/v2016/data/fis_bbmsy.csv')
summary(b)
filter(b, stock_id=="Katsuwonus_pelamis-27" & year==2010)
filter(b, stock_id=="Katsuwonus_pelamis-71" & year==2010)

c <- read.csv('globalprep/fis/v2016/data/mean_catch.csv')
summary(c)

ram <- read.csv("globalprep/fis/v2016/data/fis_bbmsy_gf.csv") 
ram %>% filter(stock_id=="Engraulis_encrasicolus-37" & year=='2010')
ram %>% filter(stock_id=="Sardina_pilchardus-37" & year=='2010')
ram %>% filter(stock_id=="Xiphias_gladius-37" & year=='2010')


#### Checking country rankings.
### Why does Thailand score so well?

data <- read.csv('globalprep/fis/v2016/data/fis_bbmsy_gf.csv') %>%
  filter(rgn_id == 25 & year==2010) %>%
  arrange(-mean_catch)
data.frame(data[1:20,])
dim(data)
median(data$bbmsy, na.rm=TRUE)
table(data$bmsy_data_source)

sum(data$mean_catch[is.na(data$bmsy_data_source)])/ 
  (sum(data$mean_catch[!is.na(data$bmsy_data_source)]) + sum(data$mean_catch[is.na(data$bmsy_data_source)]))
