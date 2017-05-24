library(dplyr)
library(tidyr)
library(ggplot2)

################ 
#### Correction: didn't end up gapfilling missing RAM data
#### Correct on line  153
#### this should be: select(year, ram_bmsy, gapfilled, stock_id)
####  select(year, ram_bmsy_predict, gapfilled, stock_id)
#### should also select OHI regions and merge with catch at FAO and region 
#### see compare_bbmsy_data.R for an approach that will take advantage
#### of the higher resolution information we have for RAM


# -------------------------------------------------------------------
### Read in RAM data and format
# -----------------------------------------------------------------

#### In this section, I am just getting a list of the species that are in the RAM data, but 
#### not in the SAUP catch data.  I am hand checking these to find potential synonyms
#### and creating a synonym master file that can be used to align the datasets.
ram_sp <- read.csv('globalprep/fis/v2016/ram/ram_extended.csv', stringsAsFactors = FALSE) %>%
  dplyr::select(scientificname) %>%
  unique() %>%
  arrange(scientificname)

# get a list of the ram species that are not in the catch data 
# some of these were hand matched based on synonyms...these are saved in:
# RAM_species_to_SAUP.csv
# tmp <- sort(setdiff(ram_sp$scientificname, bmsy_fao_rgn$Scientific_Name))
# write.csv(tmp, "globalprep/fis/v2016/int/unmatched_RAM_species.csv", row.names=FALSE)
ram_name_corr <- read.csv("globalprep/fis/v2016/int/RAM_species_to_SAUP.csv", stringsAsFactors = FALSE) %>%
  filter(!is.na(SAUP_species))  # SAUP to RAM name conversion


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

## filter out the regions that are not in an eez
ram <- filter(ram, rgn_id<250)
length(unique(ram$stockid_ram))  

### This dataset has the actual B/Bmsy values:
ram_bmsy <- read.csv('globalprep/fis/v2016/ram/ram_timeseries.csv') %>%
  dplyr::filter(tsid == "BdivBmsytouse-dimensionless") %>%
  dplyr::filter(!is.na(tsvalue)) %>%
  select(stockid_ram = stockid, year=tsyear, ram_bmsy = tsvalue) 

## gapfill ram_bmsy
ram_gf_check <- ram_bmsy %>%
  filter(year >= 2001) %>%
  spread(year, ram_bmsy) 
## based on this it seams reasonable to gap-fill missing 2010 values with lm

ram_bmsy_gf <- ram_bmsy %>%
  filter(year >= 2001 & year <= 2010) %>%
  group_by(stockid_ram) %>%
  mutate(years_data = length(ram_bmsy[year >= 2005])) %>%
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

setdiff(ram$stockid_ram, ram_bmsy_gf$stockid_ram) # these got cut due to not having at least 3 years of data from 2005-2010

ram_data <- ram %>% 
  left_join(ram_bmsy_gf, by="stockid_ram") %>%
  mutate(stock_id = paste(species, fao_id, sep="-")) %>%
  mutate(stock_id = gsub(" ", "_", stock_id)) %>%
  filter(!is.na(year)) %>%   ## these are the ones that didn't have enough ram data to make an accurate guess
  ungroup() %>%
  select(year, ram_bmsy, gapfilled, stock_id) %>%
  unique()


write.csv(ram_data, "globalprep/fis/v2016/int/ram_bmsy.csv", row.names=FALSE)


