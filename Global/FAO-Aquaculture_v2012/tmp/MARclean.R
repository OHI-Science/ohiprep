############################## MARICULTURE LAYERS PREP #######################################################################
# load libraries, set directories
source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)
library(stringr)
# library(tidyr)

dir_d = '../ohiprep/Global/FAO-Aquaculture_v2012'

##############################################
# PART 1 - get raw MAR data and clean it
##############################################
d <- read.csv(file.path(dir_d,'raw','FAO_raw_aquaculture_quant_1950_2012.csv'),check.names=F, stringsAsFactors=F) ; head(d)

# a) melt into long format from wide format
# b) rename, deal with the weird FAO format with the ...'s and 0 0's, remove Yugoslavia, U.S.S.R., occupied Palestine (don't do this for nat prods: you may miss the peak fi you remove early years)
# c) use max(year-1) value if max(year) is NA
# d) deal with country disag: disag Netherlands Antilles (in this case: assign all yield to Curaçao, based on lit search (Aruba reports separately)) 


# melt and clean up
  m = d %.%    
    rename(c(
      'Country (Country)'       = 'country',
      'Species (ASFIS species)' = 'species',
      'Aquaculture area (FAO major fishing area)'   = 'fao',
      'Environment (Environment)' = 'environment')) %.%
    melt(id=c('country','species','fao','environment'), variable='year') # can't get tidyr to do this with 'gather' so left the reshape2 command
  m = m %.%
  filter(!country %in% c('Totals', 'Yugoslavia SFR', 'Palestine, Occupied Tr.', 'Un. Sov. Soc. Rep.'), environment!='Freshwater') %.%    
  mutate(  
    value = str_replace(value, fixed( ' F'),    ''), # FAO denotes with F when they have estimated the value using best available data
    value = str_replace(value, fixed('0 0'), '0.1'), # FAO denotes something as '0 0' when it is > 0 but < 1/2 of a unit. 
    value = str_replace(value, fixed(  '-'),   '0'), # FAO's 0
    value = str_replace(value, fixed('...'),    NA), # FAO's NA
    value = str_replace(value, fixed('.'),      NA),
    value = ifelse(value =='', NA, value),  
    value = as.numeric(as.character(value)),
    year  = as.integer(as.character(year))) %.%       # search in R_inferno.pdf for "shame on you"
  select(country, species, fao, environment, year, value) %.%
  arrange(country, species, fao, environment, is.na(value), year) %.%
  group_by(country, species, fao, environment)  %.%
  mutate (tot = sum(value, na.rm=T)) %.%  # identify and eliminate empty time-series
  filter(tot>0)  %>% 
  # gapfill: Carry previous year's value forward if value for max(year) is NA.
  #   This gives wiggle room for regions still in production but not able to report by the FAO deadline. 
  mutate (value_is0   =  value==0 | is.na(value) , 
          year_beg = min(year[value_is0==FALSE]) , # first year of non-0 (or NA) harvest
          year_max = max(year[value_is0==FALSE]) , # last year of non-0 (or NA) harvest
          year_prev  = lag(year,  order_by=year),
          value_prev = lag(value, order_by=year),
          value_ext  =        is.na(value) & year==year_max, # removed  '& year_prev==year-1," as unnecessary
          value      = ifelse(is.na(value) & year==year_max , value_prev, value), # gapfill step # removed  '& year_prev==year-1" as unnecessary
          value      = ifelse(is.na(value) & year>year_beg, 0, value)) %.% # NAs turned to 0 after the first year of non-null harvest
  filter(!is.na(value)) %.% # drop NAs before year_beg
  ungroup() %.%
  arrange(country, species, fao, environment, year, value)

# turns out no years needed gapfilling
# dim(unique(m[,1:4])) # [1] 1353    4
# length(m$year[m$year==2012]) # [1] 1353

#####################################################################################################
# PART 2 - change species and country names to match resilience scores - exclude species not for food
#####################################################################################################

#### a) SPECIES NAMES

# write.csv(m,file.path(dir_d,'tmp/MARdata_JUL312014.csv'),row.names=F)
# I saved 'm' and did the following steps by hand in Excel - need to code this up for next year
# change species names so they match last year's names -> to know which ones to keep, and to join to the resilience scores
## 1) load last year's list of species names + keep/discard field: 'exclude' (=boolean)
# 2) join this to a list of unique species names in the new data, join by 'species' field
## 3) of those names that don't match, hand check whether to keep (exclude seaweeds and species harvested for ornamental/medicinal) and if synonyms to rename them by
# 4) generate a 'rename' field (=boolean) and a column for the new name, and the boolean for whether to exclude
## 5) generate the updated 2014_sp_list with fields: sp_name_2014, rename (=boolean), new.name, exclude (=boolean)

sp_nm <- read.csv(file.path(dir_d,'raw', 'updated 2014_sp_list.csv'), stringsAsFactors=F)
name_change <- sp_nm %>% filter (rename==1) %>% select (sp_name_2014, new.name) %>% rename (c('sp_name_2014'='species', 'new.name' = 'new.species') )                      
m2 <- join(m, name_change) # Joining by: species
m2$species <- ifelse (is.na(m2$new.species) , m2$species , as.character(m2$new.species)) # replace species names where necessary
m2 <- m2 %>% select (country, species, fao, environment, year, value, value_ext) # remove the new.species field, 'value_ext' is FALSE in all cases
sp_exclude <- sp_nm %>% select(new.name, exclude) %>% rename(c('new.name'='species','exclude'='not_food'))# same name for join field, avoid confusion with 'exclude' field from before
m2 <- left_join(m2, sp_exclude) # Joining by: species
m2 <- m2 %>% filter(not_food==0, year>2002) %>% select(country, species, fao, environment, year, value, value_ext)

#### b) COUNTRY NAMES

# rename countries -> to match Trujillo scores country names, check correct matches to ohi reporting regions
## 1) load last year's list of country names, with corresponding ohi reporting region names and ids
# 2) join to current data by 2013 country names (NOT by ohi reporting regions) 
## 3a) hand check mismatching records -> spell changes: e.g. Cote d'Ivoire, Reunion, Curacao ('country synonyms file' can help) 
# 3b) disaggregate: Netherland Antilles - lit search to match harvest with country (of Bonaire, Curacao, S Eustatius, Saba, Sinta Maarten - Aruba gen. reports separately)
## 3c) name change: Serbia and Montenegro = Montenegro (if need to find peak in data, should also match Yugoslavia SFR to Croatia, Serbia, BHI, and Un. Sov. Soc. Rep. to Russia)
## 3d) groups: Guadeloupe + Martinique = Guadeloupe and Martinique; Zanzibar + Tanzania = Tanzania, Channel Islands + UK = UK; Northern Mariana Islands + Guam =  Northern Mariana Islands and Guam
## 3e) new countries: match to ohi reporting regions  ('country synonyms file' can help)

cn_nm <- read.csv(file.path(dir_d,'raw/updated 2014_country_names.csv'), stringsAsFactors=F) ; head(cn_nm)
cn_nm <- rename(cn_nm,c('country_MARdata_2014' = 'country')) ; head(cn_nm)
cn_nm$country[which(cn_nm$country=="R\x8eunion")]<-"R\xe9union"  # for this time I can ignore "C\x99te d'Ivoire" because it's absent from the MAR data
m3 <- left_join(m2,cn_nm) # Joining by: "country"
m3 <- m3 %>% mutate (country = ifelse (name_change==1, as.character(country.new_2014), as.character(country)) )
m3 <- rename (m3, c('rgn_nam_2013' = 'rgn_nm', 'rgn_id_2013' = 'rgn_id'))
# sum tonnes from countries that get grouped (na.rm=T)
m3 <- m3 %>% select(rgn_nm , rgn_id, country, species, fao, environment, year, value_ext, value) %>% group_by(rgn_nm , rgn_id, country, species, fao, environment, year)  %>% summarise(yield = sum(value, na.rm=T))

# dim(m3[is.na(m3$yield),]) # check that NA/0 padding doesn't need to be redone after this grouping

#### c) SUSTAINABILITY SCORES
# Trujillo scores for country-spp pairs not in 2013 data: if new species -> have to assign to a taxon group, if new country, or new country-spp pairs, have to use species, or if not available, taxon averages
# did this by hand in Excel - next time:
# load Trujillo score tables (original, with converted names; spp average, group1 avg, group2 avg, sp_env+group1+group2_table)
# add rows to sp+group1+group2_table, if new species_env reported

m_s<-read.csv(file.path(dir_d, 'tmp/MARdata_JUL312014_sust.csv'), stringsAsFactors=F) ; head(m_s)

# get an average sustainability score per country-species pair (the code is set up so there's a unique resil for country-spp pair, this is untrue: there may be several)
# dim(unique(m_s[,3:4])) ; dim(unique(m_s[,1:5])) # 1007 against 1063 (in some cases the values are identical, but in several cases they are not)
# short-term fix: average the sustainability score to fit the code

m_s$final_sust <- as.numeric(as.character(m_s$final_sust))
m_s<- m_s %>% group_by(rgn_nm, rgn_id, country, species) %>% summarise(sust = mean(final_sust))

############################################
# create layers:

# mar_sustainability_score_lyr.csv
# rgn_id  species	sust_coeff

# mar_trend_years_lyr.csv
# rgn_id	trend_yrs (= one of '5_yr', if value_ext = F or '4_yr', if value_ext = T)
# join rgn_id w 5_yr
# 
# mar_harvest_tonnes_lyr.csv
# rgn_id	species_code	year	tonnes
# 
# mar_harvest_species_lyr.csv
# species_code	species (species code currently up to 1183, one number per species name - this is not a unique code!!)



# rgn_id: country to rgn_id  # source('src/R/ohi_clean_fxns.R')
m_r = name_to_rgn(m_l, fld_name='country', flds_unique=c('country','commodity','year'), fld_value='value', add_rgn_name=T) %.%
  select(rgn_name, rgn_id, commodity, year, value) %.%
  arrange(rgn_name, commodity, year)



# check for duplicates
stopifnot( sum(duplicated(m_s[,c('rgn_id', 'product', 'year')])) == 0 )

# units: rename value field to units based on filename
m_u = rename(m_s, setNames(units, 'value'))  

# output
f_out = sprintf('%s/data/%s_%s.csv', dir_d, basename(dir_d), units)
write.csv(m_u, f_out, row.names=F, na='')
}

## save as different scenarios ----

# this code was stolen from ohiprep/Global/WorldBank-Statistics_v2012/data_prep.R as an example of how to


# example data
lf = data.frame(rgn_id = 1:20, year = 1994:2013, count = sample(1:100, 20))

# identify scenarios and max years
scenarios = list('2012a'= max(lf$year)-2,
                 '2013a'= max(lf$year)-1,
                 '2014a'= max(lf$year))

# loop through and save
for (scen in names(scenarios)){ # scen = names(scenarios)[1]
  
  yr = scenarios[[scen]]
  cat(sprintf('\nScenario %s using year == %d\n', scen, yr))
  
  lf_yr = lf %>%
    filter(year <= yr) # remove any years greater than the scenario
  stopifnot(anyDuplicated(lf_yr[,c('rgn_id', 'year')]) == 0)
  
  csv = sprintf('rgn_id_fao_mar_%s_.csv', scen) 
  write.csv(lf_yr, file.path(dir_d, 'data', csv), row.names=F)
  
}
