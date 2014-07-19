# data_prep.r

# Prepare FAO fisheries data for targeted harvest pressures.
#  based on clean_FAOtargeted.r: FAO data (by JStewart Jul2013)

#   read in quant/value files
#   remove Totals, Yugoslavia rows
#   translate FAO data codes (F, ..., -, 0 0)
#   carry previous year's value forward if value for max(year) is NA
#   merge with commodities and collapse to product
#   add rgn_id using new cbind_rgn function (@bbest updated add_rgn_id())
#   save single files for each commodity 

# setup ----

# debug> options(warn=2); options(error=recover) # options(warn=0); options(error=NULL)

# load libraries
library(stringr)
library(zoo)  
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall


# get paths and functions
source('src/R/common.R') # set dir_neptune_data
source('src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()
dir_d = 'Global/FAO-TargetedHarvest_v2012' 


fis_fao = file.path('Global/FAO-Fisheries_v2012/raw', 'FAO_raw_captureprod_1950_2012.csv')
sp2grp = read.csv(file.path(dir_d, 'species2group.csv'), na.strings='') %>%
  filter(incl_excl == 'include') %>%
  select(target, species); head(sp2grp)

## data read in and clean ----

d = read.csv(fis_fao, check.names=F, strip.white=TRUE); head(d)
# units = c('tonnes','usd')[str_detect(f, c('quant','value'))] # using American English, lowercase

# melt and clean up
suppressWarnings({ # warning: attributes are not identical across measure variables; they will be dropped
  m = d %.%    
    rename(c(
      'Country (Country)'                     = 'country',
      'Species (ASFIS species)'               = 'species',
      'Fishing area (FAO major fishing area)' = 'area',
      'Measure (Measure)'                     = 'measure')) %.%
    melt(id=c('country','species','area', 'measure'), variable='year')})
m = m %.%
  filter(!country %in% c('Totals', 'Yugoslavia SFR')) %.%
  mutate(  
    value = str_replace(value, fixed( ' F'),    ''), # FAO denotes with F when they have estimated the value using best available data
    value = str_replace(value, fixed('0 0'), '0.1'), # FAO denotes something as '0 0' when it is > 0 but < 1/2 of a unit. 
    value = str_replace(value, fixed(  '-'),   '0'), # FAO's 0
    value = str_replace(value, fixed('...'),    NA), # FAO's NA
    value = str_replace(value, fixed('.'),      NA),
    value = ifelse(value =='', NA, value),  
    value = as.numeric(as.character(value)),
    year  = as.integer(as.character(year))) %.%       # search in R_inferno.pdf for "shame on you"
  select(country, species, area, year, value) %.%
  arrange(country, species, area, is.na(value), year) %.%
  group_by(country, species, area) %.%
  mutate(
    # gapfill: Carry previous year's value forward if value for max(year) is NA.
    #   This gives wiggle room for regions still in production but not able to report by the FAO deadline.            
    year_max   = max(year),    # note: currenly year_max is always max year of whole dataset b/c input is wide format
    year_prev  = lag(year,  order_by=year),
    value_prev = lag(value, order_by=year),
    value_ext  =        is.na(value) & year==year_max & year_prev==year-1,
    value      = ifelse(is.na(value) & year==year_max & year_prev==year-1, value_prev, value),
    # extend all other NAs as zeros after year_beg
    year_beg   = first(year),  # since ordered by is.na(value) before year, should pickup first non-NA year      
    value      = ifelse(is.na(value) & year>year_beg, 0, value)) %.%
  # drop NAs before year_beg
  filter(!is.na(value)) %.%
  ungroup() %.%
  arrange(country, species, area, year)

# show extended values
cat('\nExtended values:\n')
m_x = filter(m, value_ext==T)
print(m_x)  

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
    spgroups[str_detect(spp, ignore.case(keyword))], 
    subset(sp2grp, species==spp, target, drop=T))
  if (length(d_missing_l)>0){
    cat(sprintf("\nMISSING in the lookup the following species in target='%s' having keyword='%s' in data file %s:\n    %s\n", 
                spp, keyword, basename(fis_fao), paste(d_missing_l, collapse='\n    ')))
  }
}

# check for commodities in lookup not found in data
l_missing_d = anti_join(sp2grp, m, by='species')
if (length(l_missing_d)>0){
  cat(sprintf('\nMISSING: These commodities in the lookup are not found in the data %s:\n    ', basename(fis_fao)))
  print(l_missing_d)
}
# MISSING: These commodities in the lookup are not found in the data FAO_raw_captureprod_1950_2012.csv:
#         target                  species incl_excl
# 1 cetacean      Spotted dolpins nei   include
# 2 cetacean Nothern bottlenose whale   include
# 3 cetacean    Fervais' beaked whale   include
# 4 cetacean     Commerson's dolphin    include


## filter data within turtle and cetacean categories ----
m2 = m %>%
  filter(species %in% sp2grp$species)
unique(m2$area) # confirm these are all marine


# antilles: break up Netherlands Antilles into the 4 of 6 regions not already included ('Aruba','Curaçao')
stopifnot( sum(c('Sint Maarten', 'Curacao', 'Bonaire','Saba', 'Sint Eustasius', 'Aruba') %in% m2$country) == 0 )
m_ant = m2 %.%
  filter(country == 'Netherlands Antilles') 
m_ant # not here at all 

# cast wide to expand years
m_w = m2 %.%
  select(country, species, year, year_beg, value) %.%
  dcast(country + species + year_beg ~ year, fun.aggregate = sum) %>%
  left_join(sp2grp, by='species'); head(m_w)

# melt long by target and apply 0's where NA since first available year
m_l = m_w %.%
  select(-species) %>%
  melt(id=c('country','target', 'year_beg'), variable='year') %.%
  mutate(year = as.integer(as.character(year))) %.%
  arrange(country, target, year) %.%
  filter(!is.na(value)); head(m_l)

# just looking into Japan = 210
# filter(m2, country == 'Japan', year == 2005) # year == 2011 # mostly dolphins, not whales
# a = m_l %>% 
#   group_by(country, target, year) %>%
#   summarize(value = sum(value)) %>% 
#   filter(country == 'Japan', target == 'cetacean', year >= 2005) 
# a


# summarize across target for totals per region per year
m_l2 = m_l %>%
  group_by(country, year) %>%
  summarize(value = sum(value)); head(m_l2)

## add rgn_ids: name_to_rgn ----
# source('src/R/ohi_clean_fxns.R')
m_r = name_to_rgn(m_l2, fld_name='country', flds_unique=c('country','year'), fld_value='value', add_rgn_name=T) %.%
  select(rgn_name, rgn_id, year, value) %.%
  arrange(rgn_name, year)
m_r$year = as.numeric(as.character(factor(m_r$year))); head(m_r)


## for each scenario: id year, rescale and save pressures layer ----
# makes obsolete: ohiprep:src/R/ohi_clean_fxns.R:: save_pressure_layers_2012a_2013a.r 
#                 neptune_data:model/GL-FAO-TargetedHarvest_v2011/export_layers.R 

scenario = c('2014' = 0,
             '2013' = 1,
             '2012' = 2)
#              '2011' = 3, # just to look into 
#              '2010' = 4,
#              '2009' = 5)

for (i in 1:length(names(scenario))) { # i=1
  
  yr_max = max(m_r$year) - as.numeric(as.character(factor(scenario[i])))
  
  m_f_tmp = m_r %>%
    filter(year == yr_max) %>%
    mutate(score = value/(max(value) * 1.10)) # this was done in 2013a: neptune_data:model/GL-FAO-TargetedHarvest_v2011/export_layers.R
  hi_rgn = filter(m_f_tmp, value == max(value)) 
  
  m_f = m_f_tmp %>%  
    select(rgn_id, score) %>%
    arrange(rgn_id); head(m_f); summary(m_f)
  
  message(sprintf('\n  for %sa, pressures will use yr_max == %d', names(scenario)[i], yr_max))
  message(sprintf('\n  rescaled scores based on max(value) == %d counts of targeted harvest (catch from %s)', max(m_f_tmp$value), hi_rgn$rgn_name))
 
  filesave = paste('rgn_fao_targeted_', names(scenario)[i], 'a.csv', sep='')
  write.csv(m_f, file.path(dir_d, 'data', filesave), row.names = F)

}

#   for 2014a, pressures will use yr_max == 2012
#   rescaled scores based on max(value) == 3231 counts of targeted harvest (catch from Greenland)
# 
#   for 2013a, pressures will use yr_max == 2011
#   rescaled scores based on max(value) == 4094 counts of targeted harvest (catch from Greenland)
# 
#   for 2012a, pressures will use yr_max == 2010
#   rescaled scores based on max(value) == 7489 counts of targeted harvest (catch from Japan)



## --- fin ---


