# data_prep.R. 
# Prep World Bank WGI (World Governance Indicators) data using World Bank Development Indicators API
# J. Lowndes: lowndes@nceas.ucsb.edu, April 2015
# See also ohiprep/Global/WorldBank-WGI_v2013 for previous analyses without the API. 

# Steps involve:
# Access WGI data from 
# Add rgn_ids  
# gapfilling: sovereignty (parent-children) gapfilling with gapfill_georegions: flag sovereign


# setup ----
library(ohicore) # devtools::install_github('ohi-science/ohicore@dev')
library(tools)
library(dplyr)
library(tidyr)
devtools::install_github("hadley/lazyeval", build_vignettes = FALSE)
devtools::install_github("rstudio/ggvis", build_vignettes = FALSE)
library(testthat) # install.packages('testthat')
library(WDI) # install.packages('WDI')

dir_wgi = 'globalprep/worldbank_wgi' #MRF on neptune
dir_wgi = '~/github/ohiprep/globalprep/worldbank_wgi'
setwd(dir_wgi)

# check website to see what years are available: http://info.worldbank.org/governance/wgi/index.aspx#home
yr_start = 1996
yr_end   = 2013


## access data ----

## get description of variables:
indicators <-  data.frame(WDI_data[[1]])
indicators[grep("VA.EST", indicators$indicator), ]
indicators[grep("PV.EST", indicators$indicator), ]
indicators[grep("GE.EST", indicators$indicator), ]
indicators[grep("RQ.EST", indicators$indicator), ]
indicators[grep("RL.EST", indicators$indicator), ]
indicators[grep("CC.EST", indicators$indicator), ]

# identify the six indicators
# WDIsearch('violence')# general search
key_voice = WDI(
  WDIsearch('Voice and Accountability: Estimate', field='name')['indicator'],
  country='all',start = yr_start, end=yr_end)

key_polst = WDI(
  WDIsearch('Political Stability and Absence of Violence/Terrorism: Estimate', field='name')['indicator'],
  country='all',start = yr_start, end=yr_end)

key_gvtef = WDI(
  WDIsearch('Government Effectiveness: Estimate', field='name')['indicator'],
  country='all',start = yr_start, end=yr_end)

key_regqt = WDI(
  WDIsearch('Regulatory Quality: Estimate', field='name')['indicator'],
  country='all',start = yr_start, end=yr_end)

key_rolaw = WDI(
  WDIsearch('Rule of Law: Estimate', field='name')['indicator'],
  country='all',start = yr_start, end=yr_end)

key_corrp = WDI(
  WDIsearch('Control of Corruption: Estimate', field='name')['indicator'],
  country='all',start = yr_start, end=yr_end)


## combine all indicators ----
d = key_voice %>% 
  select(country, year, VA.EST) %>%
  left_join(key_polst %>% select(-iso2c), by=(c('country', 'year'))) %>%
  left_join(key_gvtef %>% select(-iso2c), by=(c('country', 'year'))) %>%
  left_join(key_regqt %>% select(-iso2c), by=(c('country', 'year'))) %>%
  left_join(key_rolaw %>% select(-iso2c), by=(c('country', 'year'))) %>%
  left_join(key_corrp %>% select(-iso2c), by=(c('country', 'year'))); head(d); summary(d); sapply(d, class)  
# archived record of raw data used for eez2015: write.csv(d, file.path(dir_wgi, 'raw', 'worldbank_wgi_from_wdi_api.csv'), row.names=F)


d <- gather(d, "indicator", "value", VA.EST:CC.EST)

d_gap_fill  <- d %>%
  group_by(country, year) %>%
  mutate(NA_count_c_y = sum(is.na(value))) %>%  # get count of NA values
  ungroup() %>%
  group_by(country, indicator) %>%              # this section gap-fills with the mean of values across years within the same region/indicator
  mutate(ind_mean_c_i = mean(value, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(value = ifelse(is.na(value), ind_mean_c_i, value)) %>%
  group_by(country, year) %>%
  mutate(NA_count_post_gf1 = sum(is.na(value)))     #count NA values after last gap-fill

data.frame(filter(d_gap_fill, country=="Niue"))
data.frame(filter(d_gap_fill, NA_count_post_gf1 > 3))

## get list of countries with no data:
countries_no_data <- d_gap_fill %>%
  filter(NA_count_post_gf1 > 3)

countries_no_data <- unique(countries_no_data$country)

# In this case, the countries with minimal data (< 3 indicators ever calculated) have sovereign countries.  
# These will be gap-filled later on if they are deleted now.
d_gap_fill <- d_gap_fill %>%
  filter(!(country %in% countries_no_data))

## wgi calculations and within country gapfill ----
## To rescale from 0-1: from Nature 2012 SOM: ..."6 dimensions of governance that range in value from approximately -2.5 to 2.5"
wgi_range = c(-2.5, 2.5)


d_calcs  <-  d_gap_fill %>%
  group_by(country, year) %>%
  summarize(score_wgi_scale = mean(value, na.rm=T),
            NA_start = mean(NA_count_c_y),
            NA_post_gf_1 = mean(NA_count_post_gf1)) %>%
  ungroup() %>%
  mutate(score_wgi_scale = ifelse(NA_post_gf_1 > 3, NA, score_wgi_scale))


d_calcs <- d_calcs %>%
  mutate(score =  (score_wgi_scale - wgi_range[1]) / (wgi_range[2] - wgi_range[1])) %>%
  ungroup(); head(d_calcs); summary(d_calcs)

## save intermediate file of wgi scores pre-gapfilling (for OHI+ use)
write.csv(d_calcs %>%
            select(country, year, score_wgi_scale, score_ohi_scale = score), 
          file.path(dir_wgi, 'intermediate/wgi_combined_scores_by_country.csv'),
          row.names = FALSE)
          

## d_calcs file follows two branches:  the first one determines the within region gap-filling and the second one calculates scores
## In both branches, the Antilles are divided into separate regions and names are converted to regions.

##  This no longer works with the updated data, but we should update because these are good checks.
# # the idea here is that the lowest scoring regions (score_wgi_scale) should make
# # sense compared to the lowest scoring regions by all 6 indicators. These won't
# # necessarily match perfectly because we're ordering by the different
# # indicators, but the regions in all should be familiar.
# print('compare the following to see if the lowest scoring countries make sense compared with their individual indicators')
# d_calcs %>% 
#   arrange(score_wgi_scale) %>% head(10)
# d_calcs %>% 
#   arrange(VA.EST, PV.EST, GE.EST) %>% head(10)
# d_calcs %>% 
#   arrange(PV.EST, RL.EST, VA.EST) %>% head(10)
# d_calcs %>% 
#   arrange(CC.EST, RQ.EST, PV.EST) %>% head(10)
# 
# # do the same as above for the highest scoring
# print('compare the following to see if the highest scoring countries make sense compared with their individual indicators')
# d_calcs %>% 
#   arrange(desc(score_wgi_scale)) %>% head(10)
# d_calcs %>% 
#   arrange(desc(VA.EST), desc(PV.EST), desc(GE.EST)) %>% head(10)
# d_calcs %>% 
#   arrange(desc(PV.EST), desc(RL.EST), desc(VA.EST)) %>% head(10)
# d_calcs %>% 
#   arrange(desc(CC.EST), desc(RQ.EST), desc(GE.EST)) %>% head(10)
# 
# 
# # make sure 'score_wgi_scale' matches last year's calculations:
# print('compare the following to see if ordered countries make sense compared with data from last year')
# d_calcs %>% select(country, year, score_wgi_scale) %>%
#   arrange(score_wgi_scale) %>% head(10)
# read.csv('~/github/ohiprep/Global/WorldBank-WGI_v2013/raw/GL-WorldBank-WGI_v2011-cleaned.csv') %>%
#   arrange(score) %>% head(10)
# # read.csv('../../Global/WorldBank-WGI_v2013/raw/GL-WorldBank-WGI_v2011-cleaned.csv') %>%
# #   arrange(score) %>% head(10)
# read.csv('../../Global/WorldBank-WGI_v2013/raw/GL-WorldBank-WGI_v2011-cleaned.csv') %>%
#   arrange(desc(score)) %>% head(10)
# 
# 
# d_calcs %>% select(country, year, score_wgi_scale) %>%
#   arrange(desc(score_wgi_scale)) %>% head(10)
# read.csv('~/github/ohiprep/Global/WorldBank-WGI_v2013/raw/GL-WorldBank-WGI_v2011-cleaned.csv') %>%
#   arrange(desc(score)) %>% head(10)
# 
# readline("Look through the error checking above; press return to continue") 
# 
# #### GoogleVis plot to look at the data
# # library(googleVis)
# # library(tidyr)
# # 
# # d_calcs_vis  <- d_calcs %>%
# #   select(country, year, voice_and_accountability = VA.EST,
# #                         political_stability = PV.EST,
# #                         government_effectiveness = GE.EST,
# #                         regulatory_quality =  RQ.EST,
# #                         rule_law = RL.EST,
# #                         control_of_corruption = CC.EST,
# #                         mean_wgi = score_wgi_scale,
# #                         score) 
# # # %>%
# # #    gather("WGI_data", "wgi_score", voice_and_accountability:score)
# # p <- gvisMotionChart(d_calcs_vis, idvar="country", timevar="year")
# # plot(p)
# # print(p, file="rawScoresGoogleVisChart.html")

########################################################################
# d_calcs: Branch 1: Preparing the first gap-filling dataset (within region gap-filling)----
########################################################################
# STEP 1: Antilles divided
# STEP 2: countries to regions

d_gap_1 <- d_calcs %>%
  mutate(gap_fill = NA_start - NA_post_gf_1,
         gap_fill = ifelse(is.na(score), 0, gap_fill)) 

d_gap_1[d_gap_1$gap_fill>0, ]     
d_gap_1[d_gap_1$country == "New Caledonia", ]  # no data, was deleted earlier
d_gap_1[d_gap_1$country == "Niue", ] # should have gap-fill values between 0-6


### This here should be put into a function given that we use it below.
d_gap_1  <- d_gap_1 %>%
  select(country, year, gap_fill)

d_gap_1_ant = d_gap_1 %>%
  filter(country == 'Netherlands Antilles') %>% 
  ungroup() %>%
  mutate(
    'Bonaire'        = gap_fill,    # Aruba reported separately
    'Curacao'        = gap_fill,
    'Saba'           = gap_fill,
    'Sint Maarten'   = gap_fill,
    'Sint Eustatius' = gap_fill) %>%
  select(-country, -gap_fill) %>%
  gather(country, gap_fill, -year) %>%
  select(year, country, gap_fill)  # putting these in the correct order
d_gap_1_ant


d_gap_1 = rbind(d_gap_1 %>%
                  filter(country != 'Netherlands Antilles'),
                d_gap_1_ant)

##  add rgn_id using name_to_rgn ----
# if this gives an error make sure you are working with ohicore@dev
d_gap_1_rgn = name_to_rgn(d_gap_1, fld_name='country', flds_unique=c('country','year'), 
                          fld_value='gap_fill', add_rgn_name=T, collapse_fxn = 'mean',
                          dir_lookup = "../../../ohiprep/src/LookupTables"); head(d_gap_1_rgn); summary(d_gap_1_rgn) 


write.csv(d_gap_1_rgn, 'data/wgi_gap_fill_1_attr.csv', row.names=FALSE)

####################################################
## d_calcs: Branch 2: Preparing scores datasets ----
####################################################
# STEP 1: Antilles divided
# STEP 2: countries to regions
# STEP 3: apply scores from sovereign countries
# STEP 4: Partition data into correct years

d_wgi = d_calcs %>%
  select(country, year, score); head(d_wgi); summary(d_wgi)


# partition Netherland Antilles

d_ant = d_wgi %>%
  filter(country == 'Netherlands Antilles') %>% 
  ungroup() %>%
  mutate(
    'Bonaire'        = score,    # Aruba reported separately
    'Curacao'        = score,
    'Saba'           = score,
    'Sint Maarten'   = score,
    'Sint Eustatius' = score) %>%
  select(-score, -country) %>%
  gather(country, score, -year); head(d_ant)


d_wgi2 = rbind(d_wgi %>%
                 filter(country != 'Netherlands Antilles'),
               d_ant)

## FYI: weighting data used to weight China and Virgin Islands/Puerto Rico
population_weights <- read.csv('../../../ohiprep/src/LookupTables/Pop_weight_ChinaSAR_USVIslPRico.csv')

##  add rgn_id using name_to_rgn ----
# if this gives an error make sure you are working with ohicore@dev
m_d = name_to_rgn(d_wgi2, fld_name='country', flds_unique=c('country','year'), 
                  fld_value='score', add_rgn_name=T, collapse_fxn = 'weighted.mean', 
                  collapse_csv = '../../../ohiprep/src/LookupTables/Pop_weight_ChinaSAR_USVIslPRico.csv',
                  dir_lookup = "../../../ohiprep/src/LookupTables"); head(m_d); summary(m_d) 
m_d[m_d$rgn_id == 209,] # check that these look correct.
m_d[m_d$rgn_id == 116,]

m_d_unique <- m_d %>%
  select(rgn_id, rgn_name) %>%
  unique() %>%
  arrange(rgn_id)


## sovereign gapfilling with gapfill_georegions.r ----
# use gapfill_georegions: lookup table that has sov_ids and weight the 'parent' country with 1, others with 0

# read in lookups
sovregions = read.csv('../../../ohiprep/src/LookupTables/eez_rgn_2013master.csv', na.strings='') %>%  ## Mel on Neptune
  #sovregions = read.csv('~/github/ohiprep/src/LookupTables/eez_rgn_2013master.csv', na.strings='') %>% 
  select(rgn_id = rgn_id_2013,
         r2 = sov_id) %>%               # r2 is actually rgn_ids of sovereign regions
  group_by(rgn_id) %>%                  # remove duplicated countries from this rgn_id list                    
  summarize(r2 = mean(r2, na.rm=T)) %>% # duplicates always have the same sov_id (r2 value)
  mutate(r1 = r2, 
         r0 = r2,
         fld_wt = as.integer(rgn_id == r2)) %>%  # flag the 'parent' rgn_id with 1, others with 0 
  filter(rgn_id < 255, rgn_id != 213); head(sovregions)

# join fld_wt weighting to m_d
m_d = m_d %>% 
  left_join(sovregions %>%
              select(rgn_id, fld_wt),
            by = 'rgn_id'); head(m_d)

# gapfill_georegions
attrsave  = file.path(dir_wgi, 'data', 'rgn_wb_wgi_2014a_attr.csv')

setwd('../..')
# source('../ohicore/R/gapfill_georegions.R')
d_g_a = gapfill_georegions(
  data = m_d %>%
    filter(!rgn_id %in% c(213,255)) %>%
    select(rgn_id, year, score, fld_wt),
  fld_id = c('rgn_id'),
  fld_weight = 'fld_wt',
  georegions = sovregions %>%
    select(-fld_wt),
  r0_to_NA = TRUE, 
  attributes_csv = (attrsave)) 

# investigate attribute tables
head(attr(d_g_a, 'gapfill_georegions'))  # or to open in excel: system(sprintf('open %s', attrsave))
tail(attr(d_g_a, 'gapfill_georegions'))  # or to open in excel: system(sprintf('open %s', attrsave))

d_g_a %>%
  filter(year==2013) %>%
  arrange(score) %>%
  data.frame()

# select data for layers
d_g = d_g_a %>%
  select(rgn_id, year, score) %>%
  arrange(rgn_id, year); head(d_g); summary(d_g) # d_g[duplicated(d_g[, c('rgn_id', 'year')]),] 


## error checking post gapfill----

print('compare the following to see if the lowest scoring countries make sense pre and post gapfilling')
m_d %>% 
  arrange(score) %>% head(10)
d_g %>% 
  arrange(score) %>% head(10)

# do the same as above for the highest scoring
m_d %>% 
  arrange(desc(score)) %>% head(10)
d_g %>% 
  arrange(desc(score)) %>% head(10)

readline("Look through the error checking above; press return to continue") 


## save for scenarios separately----

# for each scenario separately
yr_max = max(d_g %>% select(year))

scenarios = list('2015a' = yr_max,
                 '2014a' = yr_max - 1,
                 '2013a' = yr_max - 2,
                 '2012a' = yr_max - 3)

for (scen in names(scenarios)){ # scen = names(scenarios)[1]
  
  yr = scenarios[[scen]]
  cat(sprintf('\nScenario %s using year == %d\n', scen, yr))
  
  ## save gapfilled layer
  d_g_yr = d_g %>%
    filter(year == yr) %>% # only keep scenario year
    select(rgn_id, score); summary(d_g_yr)
  
  layersave = file.path(dir_wgi, 'data', sprintf('rgn_wb_wgi_%s.csv', scen))
  write.csv(d_g_yr, layersave, na = '', row.names=FALSE)
  
  
  # calculate inverse file and save ----
  d_g_inverse = d_g_yr %>%
    mutate(score_inverse = (1-score)) %>%
    select(rgn_id,
           score = score_inverse); summary(d_g_inverse)
  
  layersave2 = file.path(paste(file_path_sans_ext(layersave), '_inverse.csv', sep=''))
  write.csv(d_g_inverse, layersave2, na = '', row.names=FALSE)
  
}

## confirm data behaving normally ----

# plot prep from OHI-2014 (ie eez2014 prepared last year) with the current information

# rank original data with rescaled data



