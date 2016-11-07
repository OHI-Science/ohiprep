# data_prep.R for Tourism & Recreation - master data_prep.R file
# Jul2015: Casey O'Hara - combining multiple scripts and data sets into one
#   master script.  
#   Supplemental scripts are located in TourismRecreation/R.

#   Outputs:
#   * tr_unemployment.csv
#     * rgn_id, year, percent
#     * Percent unemployment (0-100%)
#   * tr_sustainability.csv
#     * rgn_id, score (no year value - only current year)
#     * TTCI score, not normalized (1-7)
#   * tr_jobs_tourism.csv
#     * rgn_id, year, jobs_ct (individuals)
#     * Number of jobs, direct employment in tourism
#   * tr_jobs_pct_tourism.csv
#     * rgn_id, year, jobs_pct
#     * Percent of direct tourism jobs
#   * tr_jobs_total.csv
#     * rgn_id, year, count (individuals)
#     * Total jobs

##############################################################################=
### setup -----
##############################################################################=

library(devtools)
devtools::install_github("ohi-science/ohicore@dev") 
#devtools::install_github("ohi-science/ohicore@master")
#install_github('rCharts', 'ramnathv')
library(ohicore)

source('src/R/common.R')
library(readr)

year_max    <- 2014

goal     <- 'globalprep/tr'
scenario <- 'v2016'
dir_git  <- file.path(goal, scenario)
dir_data <- file.path(dir_git, 'output')
dir_int  <- file.path(dir_git, 'intermediate')
# dir_git points to TourismRecreation; dir_data and dir_int point to v201X/data and v201x/intermediate
#   within the TourismRecreation directory.

source(file.path(goal, scenario, 'R/tr_fxns.R'))

###################################################################
### Step 1: Get WTTC data (# direct employed in tourism and % workforce in tourism)
### See mazu: globalprep/_raw_data/WTTC/README.md for instructions
### Cleaned with: R/process_WTTC.R (saves: intermediate/wttc_empd_rgn.csv)
#####################################################################

# NOTE: probably good to go line by line through code to check,
# but this will run automatically

## describe where the raw data are located:
dir_wttc <- file.path(dir_M, 'git-annex/globalprep/_raw_data/WTTC/d2016/raw')
## processing script
source(file.path(dir_git, 'R/process_WTTC.R'), local = TRUE)


###################################################################
### Step 2: Get World Economic Forum data (Travel and Tourism Competitiveness Report)
### http://reports.weforum.org/travel-and-tourism-competitiveness-report-2015/downloads/
### see mazu: _raw_data/WEF-Economics/
### Cleaned with: R/process_WEF.R (saves: intermediate/wef_ttci_2015.csv)
### As of 7/15/2016: no new data for 2016 
### NOTE: same data is used for all years of analysis
#####################################################################

# NOTE: probably good to go line by line through code to check,
# but this will run automatically

## describe location of raw data:
#dir_wef  <- file.path(dir_M, 'git-annex/globalprep/_raw_data/WEF-Economics/d2015/raw/WEF_TTCR_Dataset_2015.csv')
## processing the script (NOTE: needs to be updated to run correctly)
#source(file.path(dir_git, 'R/process_WEF.R'), local = TRUE)


###################################################################
### Step 3: Get Travel Advisory data
### See instructions in raw/README.md
### Clean and calculate scores with the following:
#####################################################################

scores = data.frame(category = c("inform", "risk", "avoid_nonessential", "avoid_all", "gtfo"),
                    score = c(0, 0.25, 0.75, 1, 1))

warn <- read.csv('globalprep/tr/v2016/raw/tr_travelwarnings_2016.csv') %>%
  select(year = assess_year, rgn_name, inform, risk, avoid_nonessential, avoid_all, gtfo, regional) %>%
  gather("category", "n", 3:7)  %>%
  filter(!is.na(n)) %>%
  select(-n) %>%
  left_join(scores, by="category") %>%
  group_by(year, rgn_name) %>%
  mutate(regions = n()) 

warn2 <- warn %>%
  mutate(score = ifelse(regions %in% 1 & regional %in% 1, score*0.5, score)) %>%
  summarize(score = mean(score)) %>%
  mutate(multiplier = 1-score) %>%
  select(year, rgn_name, multiplier) %>%
  data.frame()

data.frame(filter(warn2, year==2015)) # checked to make sure I got conversions correct, looks good!

warn_rgn <- name_2_rgn(df_in = warn2, 
                       fld_name='rgn_name', 
                       flds_unique=c('rgn_name','year'))

warn_rgn <- warn_rgn %>%
  select(rgn_id, year, multiplier)

write.csv(warn_rgn, 'globalprep/tr/v2016/output/tr_travelwarnings.csv', row.names=FALSE)

###################################################################
### NOTE: Script for this is now in "Supplementary_data"
### Step 4: Download and clean the World Bank data: total labor force and unemployment and ppppcgdp
### see mazu: globalprep/_raw_data/WorldBank/d2016 README.md for information
### Clean with: R/process_WorldBank.R (saves: intermediate/xxx.csv, files)
### Data Downloaded: 7/20/2016
### NOTE: The idea behind downloading the labor and unemployment data
### is that it could be used as an alternative method of calculating employment in tourism
### (this is how it was done in the past, now we just use WTTC proportion of employment in tourism:
### E = Ed/(L - (L x U))   )
### however, because the WTTC data is more complete, and the few missing values can't be
### estimated anyway because there is no corresponding Ed data, in future renditions, we should not 
### bother getting/cleaning these data!
###
### Will still want to get the ppppcgdp data, which is used to gapfill missing Sustainability data
#####################################################################

## describe location of raw data:
dir_wb <- file.path(dir_M, 'git-annex/globalprep/_raw_data/WorldBank/d2016/raw')

## get list of files
wb_file_list <- list.files(path = dir_wb, pattern=glob2rx('*csv'), full.names = TRUE)


source(file.path(dir_git, 'R/process_WorldBank.R'), local = TRUE)

######################################################################################
## Download these data into raw folder:
## https://www.cia.gov/library/publications/the-world-factbook/rankorder/2004rank.html
## used for filling in missing World Bank data (this is more complete - but only includes most recent year)
## Ultimately used to gapfill sustainability data
#######################################################################################
cia_gdp <- read.csv('globalprep/tr/v2016/raw/cia_gdp_pc_ppp.csv', stringsAsFactors = FALSE)

splits <- data.frame(Country = "Saint Helena, Ascension, and Tristan da Cunha", Country2 = c("Saint Helena",
                                                                                             "Ascension",
                                                                                             "Tristan da Cunha")) %>%
  mutate(Country = as.character(Country),
         Country2 = as.character(Country2))

cia_gdp <- cia_gdp %>%
  left_join(splits, by='Country') %>%
  mutate(Country2 = ifelse(is.na(Country2), Country, Country2)) %>%
  mutate(year = year_max) %>%
  select(Country=Country2, year, pcgdp_cia = gdppcppp)


cia_gdp_rgn <- name_2_rgn(df_in = cia_gdp, 
                       fld_name='Country')

## population weighted average of duplicate regions:
pop <- read.csv(file.path(dir_int, 'wb_country_total_pop.csv')) %>%
  filter(year==2014) %>%
  select(Country=country, year, w_popn)

cia_gdp_rgn <- cia_gdp_rgn %>%
  left_join(pop, by=c("Country", "year")) %>%
  group_by(rgn_id, year) %>%
  summarize(pcgdp_cia = weighted.mean(pcgdp_cia, w_popn, na.rm=TRUE))

cia_gdp_rgn <- cia_gdp_rgn %>%
  select(rgn_id, pcgdp_cia)

write.csv(cia_gdp_rgn, "globalprep/tr/v2016/intermediate/wb_rgn_cia_GDPPCPPP.csv", row.names=FALSE)

##############################################################################=
### Process data and layers ----
### MRF Note: I think this could be moved up in the above sections
##############################################################################=
## This selects the relevant variables from each dataset and saves the datafile with the name listed in tr_layers!

tr_data_files <- c(unem      = file.path(dir_int, 'wb_rgn_UEM.csv'),
                   jobs_tot  = file.path(dir_int, 'wb_rgn_TLF.csv'),
                   jobs_tour = file.path(dir_int, 'wttc_empd_rgn.csv'))

tr_layers <- c(unem     = file.path(dir_int, 'tr_pregap_unemployment.csv'),
               jobs_tot = file.path(dir_int, 'tr_pregap_jobs_total.csv'),
               jobs_tour     = file.path(dir_int, 'tr_pregap_jobs_tourism.csv'),
               jobs_pct_tour = file.path(dir_int, 'tr_pregap_jobs_pct_tourism.csv'))

tr_prep_layers(tr_layers, tr_data_files, reload = TRUE)


##############################################################################=
### Assembling the data from layers -----
##############################################################################=

tr_unem          <- read.csv(tr_layers[['unem']],          stringsAsFactors = FALSE)
tr_sust          <- read.csv('globalprep/tr/v2015/intermediate/tr_pregap_sustainability.csv', stringsAsFactors = FALSE)
tr_jobs_tour     <- read.csv(tr_layers[['jobs_tour']],     stringsAsFactors = FALSE)
tr_jobs_pct_tour <- read.csv(tr_layers[['jobs_pct_tour']], stringsAsFactors = FALSE)
tr_jobs_tot      <- read.csv(tr_layers[['jobs_tot']],      stringsAsFactors = FALSE)

rgn_names        <- read.csv('../ohi-global/eez2013/layers/rgn_global.csv', stringsAsFactors = FALSE) %>%
  rename(rgn_name = label)

rgn_names <- rgn_names %>%
  left_join(data.frame(rgn_id = rep(1:max(rgn_names$rgn_id), each = 25),
                       year   = rep(c((year_max-24):year_max), max(rgn_names$rgn_id))),
            by = 'rgn_id')


tr_data_raw <- rgn_names %>%
  full_join(tr_jobs_tour %>%
              rename(Ed = jobs_ct),
            by = c('rgn_id', 'year')) %>%
  full_join(tr_jobs_pct_tour %>%
              rename(Ep = jobs_pct) %>%
              mutate(Ep = Ep/100,
                     Ep = ifelse(Ep > 1, NA, Ep)),
            by = c('rgn_id', 'year')) %>%
  full_join(tr_jobs_tot %>%
              rename(L = count),
            by = c('rgn_id', 'year')) %>%
  full_join(tr_unem %>%
              rename(U = percent) %>%
              mutate(U = U/100),
            by = c('rgn_id', 'year'))  %>%
  full_join(tr_sust %>%
              rename(S_score = score),
            by = c('rgn_id'))  %>%
  filter(year <= year_max) %>%
  filter(!is.na(rgn_name))

## nothing to be gained by alternative estimate...just use the Ep data!
## Next year: all this can be simplified:
tmp <- filter(tr_data_raw, is.na(Ed) & !is.na(L))
tmp ## for this to be worth while, we need Ed, L, and U data

##### gapfill missing data

### Attach georegions and per-capita GDP info for various gapfilling
georegions       <- read.csv('../ohi-global/eez2013/layers/rgn_georegions.csv', na.strings='')
georegion_labels <- read.csv('../ohi-global/eez2013/layers/rgn_georegion_labels.csv')

tr_data_raw <- tr_data_raw %>%
  left_join(georegion_labels %>%
              spread(level, label) %>%
              select(-r0),
            by = 'rgn_id') %>%
  filter(rgn_id != 255) # ditch disputed regions...

### world bank gdp data
gdppcppp <- read.csv(file.path(dir_int, 'wb_rgn_GDPPCPPP.csv')) %>%
  select(rgn_id, year, pcgdp = intl_dollar) %>%
  filter(year == year_max) %>%
  select(rgn_id, pcgdp)

tr_data_raw <- tr_data_raw %>%
  left_join(gdppcppp, by = c('rgn_id'))

### cia gdp data
gdppcppp2 <- read.csv('globalprep/tr/v2016/intermediate/wb_rgn_cia_GDPPCPPP.csv')

### Use WB data, but if missing, use pcgdp_cia
tr_data_raw <- tr_data_raw %>%
  left_join(gdppcppp2, by = c('rgn_id')) %>%
  mutate(pcgdp2 = ifelse(is.na(pcgdp), pcgdp_cia, pcgdp)) %>%
  select(rgn_id, rgn_name, year, Ep, S_score, r1, r2, pcgdp=pcgdp2)

tr_data_raw <- gdp_gapfill(tr_data_raw)

mod <- lm(Ep ~ r2 + pcgdp, data=filter(tr_data_raw, year==2013))
summary(mod)
anova(mod)


tr_data_raw_gf <- tr_data_raw %>%
  group_by(year, r2) %>%
  mutate(Ep_pred_r2 = mean(Ep, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(year, r1) %>%
  mutate(Ep_pred_r1 = mean(Ep, na.rm=TRUE)) %>%
  ungroup()

tr_data <- tr_data_raw_gf %>%
  mutate(Ep = ifelse(is.na(Ep), Ep_pred_r2, Ep)) %>%
  mutate(Ep = ifelse(is.na(Ep), Ep_pred_r1, Ep)) %>%
  select(rgn_id, year, Ep=Ep)

write.csv(tr_data, "globalprep/tr/v2016/output/tr_jobs_pct_tourism.csv", row.names=FALSE)









## gapfill pcgdp
mod <- lm(pcgdp2 ~ r2, data=tr_data_raw)
summary(mod)
anova(mod)
for (i in 1:dim(tr_data_raw)[1]){ #i=85
  
  tt <- tryCatch(predict(mod, newdata=tr_sust[i,]),error=function(e) e, warning=function(w) w)
  
  if(is(tt, "error")){
    tr_sust$gdppcppp_pred1[i] <-NA
  } else {
    tr_sust$gdppcppp_pred1[i] <- predict(mod, newdata=tr_sust[i,])
  }
}


mod2 <- lm(pcgdp2 ~ r1, data=tr_data_raw)
summary(mod2)
anova(mod2)
for (i in 1:dim(tr_sust)[1]){ #i=85
  
  tt <- tryCatch(predict(mod2, newdata=tr_sust[i,]),error=function(e) e, warning=function(w) w)
  
  if(is(tt, "error")){
    tr_sust$gdppcppp_pred2[i] <-NA
  } else {
    tr_sust$gdppcppp_pred2[i] <- predict(mod2, newdata=tr_sust[i,])
  }
}

arrange(tr_data_raw, r1, r2)

tr_data_raw <- tr_data_raw %>%
  mutate(gdppcppp = ifelse(is.na(pcgdp2), gdppcppp_pred1, pcgdp2)) %>%
  mutate(gdppcppp = ifelse(is.na(gdppcppp), gdppcppp_pred2, gdppcppp)) %>%
  select(rgn_id, rgn_name, r1, r2, gdppcppp, Ep, S_score)




ep <- tr_data_raw %>%
  select(rgn_id, year, Ep)

write.csv(ep, 'globalprep/tr/v2016/output/tr_jobs_pct_tourism.csv', row.names=FALSE)

#####################################
##### Finalizing sustainability data
#### gapfilling sustainability data:
tr_sust          <- read.csv('globalprep/tr/v2015/intermediate/tr_pregap_sustainability.csv', stringsAsFactors = FALSE)

rgn_names        <- read.csv('../ohi-global/eez2013/layers/rgn_global.csv', stringsAsFactors = FALSE) %>%
  rename(rgn_name = label)

tr_sust <- rgn_names %>%
           left_join(tr_sust) %>%
            rename(S_score = score)


### don't need to gapfill data without tourism data:
ep_gf <- ep %>%
  filter(year==2014) %>%
  select(rgn_id, Ep) %>%
  filter(!is.na(Ep))

tr_sust <- tr_sust %>%
  left_join(ep_gf, by="rgn_id")

### Add gapfill flag variable 

tr_sust_gf <- tr_sust %>%
  mutate(gapfilled = ifelse(is.na(S_score) & !is.na(Ep), "gapfilled", NA)) %>%
  mutate(method = ifelse(is.na(S_score) & !is.na(Ep) & is.na(pcgdp2), "lm georegion + gdppcppp, with est. gdppcppp", NA)) %>%
  mutate(method = ifelse(is.na(S_score) & !is.na(Ep) & !is.na(pcgdp2), "lm georegion + gdppcppp", method)) %>%
  select(rgn_id, gapfilled, method)
write.csv(tr_sust_gf, "globalprep/tr/v2016/output/tr_sustainability_gf.csv", row.names=FALSE)

  
##############################################################################=
### Gapfilling ----
##############################################################################=


### Gapfill S using r1 and/or r2 regional data and PPP-adjusted per-capita GDP

mod3 <- lm(S_score ~ r2 + gdppcppp, data=tr_sust)
summary(mod3)
anova(mod3)

for (i in 1:dim(tr_sust)[1]){ #i=85
  
  tt <- tryCatch(predict(mod3, newdata=tr_sust[i,]),error=function(e) e, warning=function(w) w)
  
  if(is(tt, "error")){
    tr_sust$S_score_pred1[i] <-NA
  } else {
    tr_sust$S_score_pred1[i] <- predict(mod3, newdata=tr_sust[i,])
  }
}

mod4 <- lm(S_score ~ r1 + gdppcppp, data=tr_sust)
summary(mod4)
anova(mod4)

for (i in 1:dim(tr_sust)[1]){ #i=85
  
  tt <- tryCatch(predict(mod4, newdata=tr_sust[i,]),error=function(e) e, warning=function(w) w)
  
  if(is(tt, "error")){
    tr_sust$S_score_pred2[i] <-NA
  } else {
    tr_sust$S_score_pred2[i] <- predict(mod4, newdata=tr_sust[i,])
  }
}


tr_sust <- tr_sust %>%
  mutate(S_score_2 = ifelse(is.na(S_score), S_score_pred1, S_score)) %>%
  mutate(S_score_2 = ifelse(is.na(S_score_2), S_score_pred2, S_score_2)) %>%
  select(rgn_id, S_score=S_score2)

### slight difference in values from Casey's due to gapfilling - but basically the same...just go with last year's data for now
### Could be rounding....also, Casey's method accidentally skips the factor that serves as the intercept

