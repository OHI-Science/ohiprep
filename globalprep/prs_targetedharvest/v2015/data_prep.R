# data_prep.r

# Prepare FAO fisheries data for targeted harvest pressure.
# updated for eez2015 by @jules32, based on ohiprep/Global/FAO-TargetedHarvest_v2012/data_prep.r; removes old commenting
# updates include using @oharac's code for cleaning FAO data codes


# setup ----

# load libraries. Note dplyr, tidyr, stringr are loaded later in common.R
library(ohicore) # devtools::install_github('ohi-science/ohicore@dev') # may require uninstall and reinstall


# get paths and functions
# source('src/R/common.R') # set dir_neptune_data ## MRF: seemed to run better on Neptune without this due to issues with updating the stringr package which requires the coll function in the newer stringr version 
# library(dplyr)
# library(stringr)
# library(tidyr)
# source('src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()
source('src/R/fao_fxn.R') # by @oharac

# directory where data are stored
dir_in = 'globalprep/FAO_captureproduction' 
dir_out = 'globalprep/FAO_targetedharvest'
dir_old = 'Global/FAO-TargetedHarvest_v2012' 

# # running on neptune:
# dir_in = 'globalprep/FAO_captureproduction' 
# dir_out = 'globalprep/FAO_targetedharvest'
# dir_old = 'Global/FAO-TargetedHarvest_v2012' 


# raw FAO data
fis_fao_csv = file.path(dir_in, 'raw/FAO_captureproduction_1950_2013.csv')


# species list 
sp2grp_csv = file.path(dir_out, 'species2group.csv')
# file.copy(file.path(dir_old, 'species2group.csv'), sp2grp_csv) # copied over from eez2014 assessment (one-time only)
sp2grp = read.csv(sp2grp_csv, na.strings='') %>%
  filter(incl_excl == 'include') %>%
  select(target, species); head(sp2grp)


## data read in and clean ----

d = read.csv(fis_fao_csv, check.names=F, strip.white=F); head(d)
# units <- c('tonnes','usd')[str_detect(fis_fao_csv, c('quant','value'))] # using American English, lowercase

# rename and gather
suppressWarnings({ # warning: attributes are not identical across measure variables; they will be dropped
   m = d %>%
     dplyr::rename(country         = `Country (Country)`,
                   species_ISSCAAP = `Species (ISSCAAP group)`,
                   species_common  = `Common_Name_ASFIS_species`,
                   species_sci     = `Scientific_Name_ASFIS_species`,
                   species_order   = `Species (Order)`, 
                   species_family  = `Species (Family)`, 
                   area            = `Fishing area (FAO major fishing area)`,
                   measure         = `Measure (Measure)`) %>%
     select(-species_ISSCAAP, -species_sci, -species_order, -species_family, -measure) %>%
     gather(year, value, -country, -species_common, -area) %>%
     dplyr::rename(species = species_common)
}); head(m)
 

# clean FAO data using fao_clean_data()
m <- m %>%
  filter(!country %in% c('Totals', 'Yugoslavia SFR')) %>%
  fao_clean_data() # NOTE: optional parameter 'sub_0_0' can be passed to control how a '0 0' code is interpreted.

### get list of countries with FAO data
country <- unique(m$country)
country <- data.frame(country=country, value=0)
rgns = name_to_rgn(country, fld_name='country', flds_unique=c('country'), fld_value='value', add_rgn_name=T,
                  dir_lookup = "src/LookupTables")
write.csv(rgns, "globalprep/FAO_targetedharvest/FAO_regions.csv", row.names=FALSE)


# check for commodities in data not found in lookup, per product by keyword
spgroups = sort(as.character(unique(m$species)))
keywords = c(
  'turtle'   = 'turtles', 
  'seal'     = 'seals', 
  'whale'    = 'whales', 
  'sea lion' = 'sea lions')
for (i in 1:length(keywords)){ # i=1
  spp = names(keywords)[i]
  keyword = keywords[i]
  d_missing_l = setdiff(
    spgroups[str_detect(spp, coll(keyword, ignore.case=T))], 
    subset(sp2grp, species==spp, target, drop=T))
  if (length(d_missing_l)>0){
    cat(sprintf("\nMISSING in the lookup the following species in target='%s' having keyword='%s' in data file %s:\n    %s\n", 
                spp, keyword, basename(fis_fao_csv), paste(d_missing_l, collapse='\n    ')))
  }
}

# check for species in lookup not found in data
l_missing_d = anti_join(sp2grp, m, by='species')
if (length(l_missing_d)>0){
  cat(sprintf('\nMISSING: These species in the lookup are not found in the data %s:\n    ', basename(fis_fao_csv)))
  print(l_missing_d)
}
# MISSING: These species in the lookup are not found in FAO_captureproduction_1950_2013.csv or FAO_raw_captureprod_1950_2012.csv:
#         target                  species incl_excl
# 1 cetacean      Spotted dolpins nei   include
# 2 cetacean Nothern bottlenose whale   include
# 3 cetacean    Fervais' beaked whale   include
# 4 cetacean     Commerson's dolphin    include


## filter data within turtle and cetacean categories ----
m2 = m %>%
  filter(species %in% sp2grp$species)
unique(m2$area) # confirm these are all marine


# antilles: break up Netherlands Antilles
stopifnot( sum(c('Sint Maarten', 'Curacao', 'Bonaire', 'Saba', 'Sint Eustasius', 'Aruba') %in% m2$country) == 0 )
stopifnot( sum('Netherlands Antilles' %in% m2$country) == 0 ) # not here at all 

# spread wide to expand years
m_w = m2 %>%
  spread(year, value) %>%
  # compared to eez2014 approach, same results:
  # reshape2::dcast(country + species  ~ year, fun.aggregate = sum) 
  left_join(sp2grp, by='species'); head(m_w)


# gather long by target
m_l = m_w %>%
  select(-area) %>%
  gather(year, value, -country, -species, -target, na.rm=T) %>%
  # compared to eez2014 approach, same results with the na.rm=T term removed from above:
  # reshape2::melt(id=c('country','target'), variable='year') %>%
  mutate(year = as.integer(as.character(year))) %>%
  arrange(country, target, year); head(m_l)

# explore Japan[210]
m_l %>% 
  group_by(country, target, year) %>%
  summarize(value = sum(value)) %>% 
  filter(country == 'Japan', target == 'cetacean', year >= 2000) 

# summarize across target for totals per region per year
m_sum = m_l %>%
  group_by(country, year) %>%
  summarize(value = sum(value)) %>%
  filter(value != 0); head(m_sum) # remove zeros 


## add rgn_ids: name_to_rgn ----
m_r = name_to_rgn(m_sum, fld_name='country', flds_unique=c('country','year'), fld_value='value', add_rgn_name=T,
                  dir_lookup = "src/LookupTables") %>%
  select(rgn_name, rgn_id, year, value) %>%
  arrange(rgn_name, year)  ### JSL come back and look into Netherlands Antilles, etc. ### 
m_r$year = as.numeric(as.character(factor(m_r$year))); head(m_r)
stopifnot(anyDuplicated(m_r[,c('rgn_id', 'year', 'rgn_name')]) == 0)
# m_r[duplicated(m_r[,c('rgn_id', 'rgn_name', 'year')]),]

## for each scenario: identify maximum year, rescale and save pressures layer ----

# identify  max and min years for each scenario and overall
maxyear_all = max(m_r$year, na.rm=T)
scenario_maxyear = c('eez2015' = maxyear_all,  
                     'eez2014' = maxyear_all - 1,
                     'eez2013' = maxyear_all - 2,
                     'eez2012' = maxyear_all - 3)
minyear_all = scenario_maxyear[length(scenario_maxyear)]

# calculate and save for each scenario
for (i in 1:length(names(scenario_maxyear))) { # i=1
  
  maxyear = scenario_maxyear[i]
  scen = as.character(str_extract(names(scenario_maxyear)[i], "\\d{4}"))
  
  m_f = m_r %>%
    filter(year >= minyear_all & year <= maxyear) %>%
    mutate(score = value / quantile(value, 0.95, na.rm = T)) %>% # * 1.10:  don't multiply by 1.10 since comparing to the max across all scenarios
    mutate(score = ifelse(score>1, 1, score))
  
#   library(ggplot2)
#   ggplot(m_f, aes(score, fill=as.factor(year))) +
#     geom_histogram() 
   
  head(m_f); summary(m_f)
  
  m_f_max = m_f %>%
    filter(value == max(value, na.rm = TRUE))
  
  m_f_quantile_95  <- quantile(m_f$value, 0.95, na.rm=TRUE)
   

  message(sprintf('\n%s pressures scores for %d regions are rescaled to the 95th quantile in harvest since %s (%d-%d):', 
                  names(scenario_maxyear)[i], length(unique(m_f$rgn_id)), names(minyear_all), minyear_all, maxyear))
  message(sprintf('%s in %s: %d marine mammals and sea turtles harvested, and the 95th quantile is: %s ', 
                  m_f_max$rgn_name, m_f_max$year, m_f_max$value, m_f_quantile_95))
  # output displayed below  
  
  m_f = m_f %>%
    filter(year == maxyear) %>%
    select(rgn_id, score) %>%
    arrange(rgn_id); head(m_f); summary(m_f)
  
  # any regions that did not have a catch should have score = 0 
  rgns = read.csv('../ohi-global/eez2014/layers/rgn_global.csv') %>%
    select(rgn_id)  %>%
    filter(rgn_id < 255) %>%
    arrange(rgn_id); head(rgns)
  
  m_f_fin = rbind(m_f, 
                  rgns %>%
                    anti_join(m_f, by = 'rgn_id') %>%
                    mutate(score = 0)) %>%
    arrange(rgn_id); head(m_f_fin); summary(m_f_fin)
  stopifnot(length(m_f_fin$rgn_id) == 221)
  
  filesave = sprintf('rgn_fao_targeted_%sa.csv', scen)
  write.csv(m_f_fin, file.path(dir_out, 'data', filesave), row.names = F)
  
}

# exploration figure
source('~/github/ohiprep/src/R/compareVis_layers.R')
scatterPlot(csv_orig   = file.path(dir_out, 'data', 'rgn_fao_targeted_2012a.csv'), 
            csv_new    = file.path(dir_out, 'data', 'rgn_fao_targeted_2015a.csv'), 
            title_text = 'targeted_harvest',
            fig_save   = file.path(dir_out, 'data', paste0(title_text,'_scatterPlot.png')))


## from eez2015 run: 

# eez2015 pressures scores for 31 regions are rescaled to the max harvest since eez2012 (2010-2013):
# Japan in 2010: 7489 marine mammals and sea turtles harvested
# 
# eez2014 pressures scores for 31 regions are rescaled to the max harvest since eez2012 (2010-2012):
# Japan in 2010: 7489 marine mammals and sea turtles harvested
# 
# eez2013 pressures scores for 30 regions are rescaled to the max harvest since eez2012 (2010-2011):
# Japan in 2010: 7489 marine mammals and sea turtles harvested
# 
# eez2012 pressures scores for 24 regions are rescaled to the max harvest since eez2012 (2010-2010):
# Japan in 2010: 7489 marine mammals and sea turtles harvested


## from eez2014 run:

# eez2014 pressures scores for 30 regions are rescaled to the max harvest since eez2012 (2010-2012):
#   Japan in 2010: 7489 marine mammals and sea turtles harvested
# 
# eez2013 pressures scores for 29 regions are rescaled to the max harvest since eez2012 (2010-2011):
#   Japan in 2010: 7489 marine mammals and sea turtles harvested
# 
# eez2012 pressures scores for 22 regions are rescaled to the max harvest since eez2012 (2010-2010):
#   Japan in 2010: 7489 marine mammals and sea turtles harvested


## --- fin ---


