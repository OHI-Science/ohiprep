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

#### Ultimately, there will be three B/Bmsy values calculated using different methods
### These will be averaged.  But, in the meantime, I will do the b/bmsy values
### estimated using CMSY methods

### NOTE: 5 year running average of cmsy derived b/bmsy values do seem to perform 
### slightly better.
## cor.coefficients
##                     no_avg     5_year_avg
## resilience based    0.04       0.007
## uniform             0.01       -0.03
## constrained         0.14       0.12

# -------------------------------------------------------------------
## Taking the 5 year running average of b/bmsy values to smooth data
# -------------------------------------------------------------------

constrained <- read.csv('globalprep/fis/v2016/int/cmsy_bbmsy.csv') %>%
  mutate(prior = 'constrained') %>%
  filter(!is.na(bbmsy_mean))
uniform <- read.csv('globalprep/fis/v2016/int/cmsy_bbmsy_uni_prior.csv') %>%
  mutate(prior = "uniform") %>%
  filter(!is.na(bbmsy_mean))
comsir <- read.csv('globalprep/fis/v2016/int/comsir_bbmsy.csv') %>%
  mutate(prior = NA) %>%
  filter(!is.na(bbmsy_mean))

new_b_bmsy <- function(b_bmsy=constrained, method = "comsir"){
  b_bmsy <- b_bmsy %>%
    dplyr::select(stock_id, year, bbmsy_mean, prior, model) %>%
    arrange(stock_id, year) %>%
    group_by(stock_id) %>%
    mutate(mean_5year = rollmean(bbmsy_mean, 5, align="right", fill=NA))
  write.csv(b_bmsy, sprintf('globalprep/fis/v2016/int/%s_b_bmsy_%s_mean5yrs.csv', method, unique(b_bmsy$prior)), row.names=FALSE)
} 

new_b_bmsy(constrained, method="cmsy")
new_b_bmsy(uniform, method="cmsy")
new_b_bmsy(comsir, method="comsir")


#--------------------------------------------------------------------
## CMSY b/bmsy data selection based on stock resilience scores
# B/bmsy data is generated using different models 
# depending on the resilience score of the stock
# -------------------------------------------------------------------

# resilience scores to select the appropriate b/bmsy 
res <- read.csv("globalprep/fis/v2016/int/stock_resil_06cutoff_2016.csv")


b_bmsy_uniform <- read.csv('globalprep/fis/v2016/int/cmsy_b_bmsy_uniform_mean5yrs.csv')
b_bmsy_uniform <- b_bmsy_uniform %>%
  select(stock_id, year, b_bmsy_cmsy_uni = mean_5year) 

b_bmsy_constrained <- read.csv('globalprep/fis/v2016/int/cmsy_b_bmsy_constrained_mean5yrs.csv')
b_bmsy_constrained <- b_bmsy_constrained %>%
  select(stock_id, year, b_bmsy_cmsy_con = mean_5year) 

b_bmsy_comsir <- read.csv('globalprep/fis/v2016/int/comsir_b_bmsy_NA_mean5yrs.csv')
b_bmsy_comsir <- b_bmsy_comsir %>%
  select(stock_id, year, b_bmsy_comsir=mean_5year) 

setdiff(res$stock_id, b_bmsy_constrained$stock_id) # these did not converge in this b/bmsy analysis
setdiff(b_bmsy_constrained$stock_id, res$stock_id)  # these are in saup156 which overlaps some of our other regions and includes disputed zones that we don't use
setdiff(res$stock_id, b_bmsy_uniform$stock_id) 
setdiff(b_bmsy_uniform$stock_id, res$stock_id)

bmsy <- b_bmsy_uniform %>%
  left_join(b_bmsy_constrained, by=c("stock_id", "year")) %>%
  left_join(b_bmsy_comsir, by=c("stock_id", "year")) %>%
  left_join(res, by="stock_id")

bmsy <- bmsy %>%
  mutate(b_bmsy_cmsy = ifelse(unif_prior==1, b_bmsy_cmsy_uni, b_bmsy_cmsy_con)) %>%
  rowwise() %>%
  mutate(mean_cmsy_comsir = mean(c(b_bmsy_cmsy, b_bmsy_comsir), na.rm=TRUE)) %>%
  dplyr::select(-final_score, -unif_prior, -test, -test2)

#--------------------------------------------------------------------
#### getting b/bmsy data to the correct spatial scale
#-----------------------------------------------------------------------
catch <- read.csv('globalprep/fis/v2016/data/mean_catch.csv')
catch <- separate(catch, taxon_key_stock, c("TaxonKey", "species", "fao_id"), sep="-") %>%
  mutate(stock_id = paste(species, fao_id, sep = "-")) %>%
  mutate(fao_id = as.numeric(fao_id)) %>%
  mutate(rgn_id = as.numeric(rgn_id)) %>%
  mutate(TaxonKey = as.numeric(TaxonKey)) %>%
  filter(TaxonKey >= 600000) %>%
  mutate(species = gsub("_", " ", species))

dim(unique(catch))

intersect(catch$stock_id, bmsy$stock_id)
setdiff(catch$stock_id, bmsy$stock_id)
setdiff(bmsy$stock_id, catch$stock_id)

bmsy_fao_rgn <- catch %>%
  left_join(bmsy, by=c('stock_id', 'year'))
head(bmsy_fao_rgn)
summary(bmsy_fao_rgn)


# -------------------------------------------------------------------
### Read in RAM data and format
# -----------------------------------------------------------------
#correcting mismatches in names: figure out mismatches and make correction csv file
ram_sp <- read.csv('globalprep/fis/v2016/ram/ram_extended.csv', stringsAsFactors = FALSE) %>%
  dplyr::select(scientificname) %>%
  unique()

tmp <- sort(setdiff(ram_sp$scientificname, bmsy_fao_rgn$species))
#write.csv(tmp, "globalprep/fis/v2016/int/unmatched_RAM_species.csv", row.names=FALSE)
sort(intersect(bmsy_fao_rgn$species, ram_sp$scientificname))

tmp <- catch %>%
  dplyr::select(TaxonKey, species) %>%
  unique() %>%
  arrange(species)
#write.csv(tmp, "globalprep/fis/v2016/int/SAUP_species.csv", row.names=FALSE)


## read in the ram data and substitute names
ram_name_corr <- read.csv("globalprep/fis/v2016/int/unmatched_RAM_species_to_SAUP.csv", stringsAsFactors = FALSE) %>%
  filter(!is.na(SAUP_species))  # SAUP to RAM name conversion

## This file includes all the eez regions that correspond to an FAO region:
ram <- read.csv('globalprep/fis/v2016/ram/ram_extended.csv', , stringsAsFactors = FALSE) %>%
  dplyr::select(stockid, RAM_species=scientificname, rgn_id=OHI_rgn, fao_id=FAO) %>%
  left_join(ram_name_corr, by="RAM_species") %>%
  mutate(species = ifelse(!is.na(SAUP_species), SAUP_species, RAM_species)) %>%
  select(stockid_ram = stockid, rgn_id, fao_id, species)
length(unique(ram$stockid_ram))  #237 stocks

## filter out the species that have no match in the SAUP data
ram <- filter(ram, species %in% bmsy_fao_rgn$species)
length(unique(ram$stockid_ram))  #203 stocks that match our species

## filter out the regions that are not in an eez
ram <- filter(ram, rgn_id<250)
length(unique(ram$stockid_ram))  #202 stocks that match our species and in the eez regions

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


## checking error and model fit for stocks
ram_bmsy_gf_check1 <- ram_bmsy_gf %>%
  group_by(stockid_ram) %>%
  do({
    mod <- lm(ram_bmsy ~ year, data= filter(., year < 2010))  ### change year to test different accuracy levels based on # years of data used in lm
    ram_bmsy_predict <- predict(mod, newdata=.[c('year')])
    data.frame(., ram_bmsy_predict)
  })

ggplot(data = filter(ram_bmsy_gf_check1, year==2010), aes(y=ram_bmsy_predict, x=ram_bmsy)) +
         geom_point() +
         theme_bw() +
         geom_abline(slope=1, intercept=0, color="red")
summary(lm(ram_bmsy_predict ~ ram_bmsy, data=filter(ram_bmsy_gf_check1, year==2010)))

ram_bmsy_gf_check2 <- ram_bmsy_gf %>%
  group_by(stockid_ram) %>%
  do({
    mod <- lm(ram_bmsy ~ year, data= filter(., year < 2009))  ### change year to test different accuracy levels based on # years of data used in lm
    ram_bmsy_predict <- predict(mod, newdata=.[c('year')])
    data.frame(., ram_bmsy_predict)
  })

ggplot(data = filter(ram_bmsy_gf_check2, year==2010), aes(y=ram_bmsy_predict, x=ram_bmsy)) +
  geom_point() +
  theme_bw() +
  geom_abline(slope=1, intercept=0, color="red")
summary(lm(ram_bmsy_predict ~ ram_bmsy, data=filter(ram_bmsy_gf_check2, year==2010)))

ram_bmsy_gf_check3 <- ram_bmsy_gf %>%
  group_by(stockid_ram) %>%
  do({
    mod <- lm(ram_bmsy ~ year, data= filter(., year < 2008))  ### change year to test different accuracy levels based on # years of data used in lm
    ram_bmsy_predict <- predict(mod, newdata=.[c('year')])
    data.frame(., ram_bmsy_predict)
  })

ggplot(data = filter(ram_bmsy_gf_check3, year==2010), aes(y=ram_bmsy_predict, x=ram_bmsy)) +
  geom_point() +
  theme_bw() +
  geom_abline(slope=1, intercept=0, color="red")
summary(lm(ram_bmsy_predict ~ ram_bmsy, data=filter(ram_bmsy_gf_check3, year==2010)))


#### end of checking gapfilling

ram_bmsy_gf <- ram_bmsy_gf %>%
  mutate(ram_bmsy = ifelse(is.na(ram_bmsy), ram_bmsy_predict, ram_bmsy)) %>%
  select(stockid_ram, year, ram_bmsy, gapfilled)

setdiff(ram_bmsy_gf$stockid_ram, ram$stockid_ram) # these are the ones with no species match in the SAUP data...ok
setdiff(ram$stockid_ram, ram_bmsy_gf$stockid_ram) # these got cut due to not having at least 4 years of data from 2005-2010

ram_data <- ram %>% 
  left_join(ram_bmsy_gf, by="stockid_ram") %>%
  group_by(rgn_id, fao_id, species, year) %>%  # sometimes a region will have multiple stocks of the same species, average these!
  summarize(ram_bmsy = mean(ram_bmsy, na.rm=TRUE),
            gapfilled = ifelse(all(is.na(gapfilled)), NA, max(gapfilled, na.rm=TRUE))) %>%  ## slightly overestimating gapfilling for a few species....oh-well
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
### compare b/bmsy values
# -----------------------------------------------------------------
library(ggplot2)
data <- filter(bmsy_dl_ram, ram_bmsy<2.5)

###
ggplot(data, aes(x=ram_bmsy, y=b_bmsy_cmsy_con)) +
  annotate("rect", xmin=0, xmax=0.5, ymin=0, ymax=0.5, fill = "green", alpha=0.5) + 
  annotate("rect", xmin=0, xmax=1, ymin=0, ymax=1, fill = "yellow", alpha=0.5) + 
  annotate("rect", xmin=1, xmax=2.5, ymin=1, ymax=2.5, fill = "green", alpha=0.5) + 
  geom_point() +
  labs(x='RAM B/Bmsy', y='CMSY B/Bmsy, constrained') +
  theme_bw()

hist(bmsy_dl_ram$b_bmsy_cmsy_con)

mod <- lm(ram_bmsy ~ b_bmsy_cmsy_con, data=data)
summary(mod)
var(data$b_bmsy_cmsy_con, data$ram_bmsy, na.rm=TRUE)

###

ggplot(data, aes(x=ram_bmsy, y=b_bmsy_cmsy_uni)) +
  annotate("rect", xmin=0, xmax=0.5, ymin=0, ymax=0.5, fill = "green", alpha=0.5) + 
  annotate("rect", xmin=0, xmax=1, ymin=0, ymax=1, fill = "yellow", alpha=0.5) + 
  annotate("rect", xmin=1, xmax=2.5, ymin=1, ymax=2.5, fill = "green", alpha=0.5) + 
  geom_point() +
  labs(x='RAM B/Bmsy', y='CMSY B/Bmsy, uniform') +
  theme_bw()


mod <- lm(ram_bmsy ~ b_bmsy_cmsy_uni, data=data)
summary(mod)
var(data$b_bmsy_cmsy_uni, data$ram_bmsy, na.rm=TRUE)

###


ggplot(data, aes(x=ram_bmsy, y=b_bmsy_cmsy)) +
  annotate("rect", xmin=0, xmax=0.5, ymin=0, ymax=0.5, fill = "green", alpha=0.5) + 
  annotate("rect", xmin=0, xmax=1, ymin=0, ymax=1, fill = "yellow", alpha=0.5) + 
  annotate("rect", xmin=1, xmax=2.5, ymin=1, ymax=2.5, fill = "green", alpha=0.5) + 
  geom_point() +
  labs(x='RAM B/Bmsy', y='CMSY B/Bmsy, uniform or constrained') +
    theme_bw()

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

mod2 <- lm(ram_bmsy ~ b_bmsy_cmsy_con, data=data)
summary(mod2)

mod3 <- lm(ram_bmsy ~ b_bmsy_comsir + b_bmsy_cmsy_con, data=data)
summary(mod3)

mod4 <- lm(ram_bmsy ~ mean_cmsy_comsir, data=data)
summary(mod4)

AIC(mod1, mod2, mod3, mod4)

# -------------------------------------------------------------------
### final formatting of data
# -----------------------------------------------------------------

bbmsy_data <- bmsy_dl_ram %>%
  mutate(bbmsy = ifelse(is.na(ram_bmsy), b_bmsy_cmsy_con, ram_bmsy)) %>%
  filter(!is.na(bbmsy)) %>%
  mutate(stock_id = paste(species, fao_id, sep="-")) %>%
  select(rgn_id, stock_id, year, bbmsy, gapfilled) %>%
  filter(year >= 2005)
write.csv(bbmsy_data, 'globalprep/fis/v2016/data/fis_bbmsy_gf_ram.csv', row.names = FALSE)

bbmsy_data <- bmsy_dl_ram %>%
  mutate(bbmsy = ifelse(is.na(ram_bmsy), b_bmsy_cmsy_con, ram_bmsy)) %>%
  filter(!is.na(bbmsy)) %>%
  mutate(stock_id = paste(species, fao_id, sep="-")) %>%
  select(rgn_id, stock_id, year, bbmsy) %>%
  filter(year >= 2005)
write.csv(bbmsy_data, 'globalprep/fis/v2016/data/fis_bbmsy.csv', row.names = FALSE)

# -------------------------------------------------------------------
### summary of data
# -----------------------------------------------------------------

bmsy_dl_ram %>%
  mutate(ram_data = ifelse(is.na(ram_bmsy), "no", "yes")) %>%
  mutate(bbmsy = ifelse(is.na(ram_bmsy), b_bmsy_cmsy_con, ram_bmsy)) %>%
  filter(!is.na(bbmsy)) %>%
  group_by(ram_data, year) %>%
  summarize(sum_mean_catch = sum(mean_catch)) %>%
  group_by(year) %>%
  summarize(prop_catch = sum_mean_catch[ram_data=="yes"]/(sum(sum_mean_catch)))
# ~35% of catch with b/bmsy estimates

bmsy_dl_ram %>%
  mutate(ram_data = ifelse(is.na(ram_bmsy), "no", "yes")) %>%
  mutate(bbmsy = ifelse(is.na(ram_bmsy), b_bmsy_cmsy_con, ram_bmsy)) %>%
  group_by(ram_data, year) %>%
  summarize(sum_mean_catch = sum(mean_catch)) %>%
  group_by(year) %>%
  summarize(prop_catch = sum_mean_catch[ram_data=="yes"]/(sum(sum_mean_catch)))


tmp <- bmsy_dl_ram %>%
  mutate(ram = ifelse(is.na(ram_bmsy), "no", "yes")) %>%
  group_by(rgn_id, ram, year) %>%
  summarize(sum_mean_catch = sum(mean_catch)) %>%
  spread(ram, sum_mean_catch) %>%
  mutate(yes = ifelse(is.na(yes), 0, yes)) %>%
  gather("ram", "sum_mean_catch", 3:4) %>%
  group_by(rgn_id, year) %>%
  summarize(prop_catch = ifelse(is.na(sum_mean_catch[ram=="yes"]), 0, sum_mean_catch[ram=="yes"])/(sum(sum_mean_catch))) %>%
  filter(year==2010)

hist(tmp$prop_catch)
