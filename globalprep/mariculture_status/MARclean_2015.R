############################## MARICULTURE LAYERS PREP #######################################################################
# load libraries, set directories
# source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)
library(dplyr)
library(stringr)
library(tidyr)

dir_d = '../ohiprep/globalprep/mariculture_status'

##############################################
# PART 1 - get raw MAR data and clean it
##############################################

###### d <- read.csv(file.path(dir_d,'raw','FAO_raw_aquaculture_quant_1950_2012.csv'), check.names=F, stringsAsFactors=F) ; head(d)
## MAR_data_2015_nofreshwater.csv was queried from FAO using FishStatJ on May 20, 2015; 
## special symbol settings: estimates prefixed with 'F ', duplicates with 'R ', 0 as '-', values << 0.5 as '0 0', NAs as '...' 
d <- read.csv(file.path(dir_d,'raw','MAR_data_2015_nofreshwater.csv'), check.names=F, stringsAsFactors=F) ; head(d)

# a) melt into long format from wide format (using 'gather')
# b) rename, deal with the weird FAO format with the ...'s and 0 0's, remove Yugoslavia, U.S.S.R., occupied Palestine (don't do this for nat prods: you may miss the peak if you remove early years)
# c) eliminate time-series with all 0s
# d) deal with country disag: disag Netherlands Antilles (in this case: split among Aruba, Bonaire and Curaçao, based on lit search) 
# e) deal with countries with non-ASCII characters, as these won't allow joining with rgn_id via country names
# f) exclude taxa that are not relevant to the mariculture goal

# a) melt and clean up
  m = d %>%    
    select(
      country = `Country (Country)` ,
      species = `Species (ASFIS species)` ,
      fao = `Aquaculture area (FAO major fishing area)` ,
      environment = `Environment (Environment)` ,
      5:ncol(d)) %>%    # using 'select' instead of 'rename' to avoid dependency from 'plyr' package
    gather(year, value, -country, -species, -fao, -environment)

  # b) # this could be added to mar_fxs since it's a little different than other fao data
m = m %>%
  filter(!country %in% c('Totals', 'Yugoslavia SFR', 'Palestine, Occupied Tr.', 'Un. Sov. Soc. Rep.'), environment!='Freshwater') %>%    
  mutate(  
    value = str_replace(value, fixed( 'F '),    ''), # FAO denotes with F when they have estimated the value using best available data
    value = str_replace(value, fixed('0 0'), '0.1'), # FAO denotes something as '0 0' when it is > 0 but < 1/2 of a unit. 
    value = str_replace(value, fixed(  '-'),   '0'), # FAO's 0
    value = str_replace(value, fixed('...'),    'NA'), # FAO's NA, replaced with text
    value = ifelse(value =='NA', NA, value), # 'NA" text strings turned into NA (done this way because direct replace w NA didn't work on new R version)
    value = ifelse(value =='', NA, value),  
    value = as.numeric(as.character(value)),
    year  = as.integer(as.character(year))) %>%       # search in R_inferno.pdf for "shame on you"
  arrange(country, species, fao, environment, is.na(value), year) 

# c) identify and eliminate any empty time-series
full = dim(m)[1] ; full
m = m %>%
  group_by(country, species, fao, environment)  %>%
  mutate (tot = sum(value, na.rm=T)) %>%  
  filter(tot>0)  %>%
  select (-tot) %>% # clean up
  ungroup()  # remove grouping so subsequent funs work properly (i.e. mar_split_antilles)
eliminated = full - dim(m)[1] # tally number of eliminated records
eliminated # check how many - 0 records excluded in 2015
 
### Special case: user-defined function deals with 
# d)  breaking up Antilles into separate reported rgns

source(file.path(dir_d,'mar_fxs.R'))
m <- mar_split_antilles(m) # in './R/mar_fxs.R' # edit path, remember it will only work properly if m not grouped

test <- m %>% filter (country == 'Netherlands Antilles') ; dim(test) # check it worked
test2 <- m %>% filter (country == 'Bonaire/S.Eustatius/Saba') ; dim(test2) # check it worked

# e) change country names without ascii characters
countries_d <- unique(m$country)
replace <- grep("I_WAS_NOT_ASCII", iconv(countries_d, "latin1", "ASCII", sub="I_WAS_NOT_ASCII")) 
countries_d[replace] # check the countries: "C\x99te d'Ivoire" "R\x8eunion" 
# find replacements (by hand for now - get synonym and region names tables using code below)
m$country[which(m$country==countries_d[replace[1]])]<-"Ivory Coast" 
m$country[which(m$country== countries_d[replace[2]])]<-"Reunion"  

# f) exclude taxa that aren't relevant to mariculture goal
# 1) load last year's list of species names that includes a keep/discard field and rename field (=booleans)
sp_nm <- read.csv(file.path(dir_d,'raw', 'updated 2014_sp_list_v2.csv'), stringsAsFactors=F)
names(sp_nm) [1] <- 'species'
# 2) join this to a list of unique species names in the new data to see if there are new species this year that need assigning to a taxon group, join by 'species' field
sp_nm_2015 <- as.data.frame(unique(m$species),stringsAsFactors = F)
names(sp_nm_2015) <- 'species'
new.spp <- anti_join(sp_nm_2015,sp_nm, copy=T)
dim(new.spp) # check: if dim has 0 rows it means all match
# if length(new.spp) >0 , hand check whether to keep (exclude seaweeds and species harvested for ornamental/medicinal), check if synonyms match Trujillo names

# 3) Now remove and rename species appropriately from the harvest dataset
m2 <- left_join (m, sp_nm) # Joining by: species
m2 <- filter (m2, exclude==0) # remove flagged species = dim(m)[1]-dim(m2)[1]
# 4) change names using new.name and the species alias
m2$species <- ifelse (m2$match.alias==1, m2$alias, m2$new.name) 
# 5) remove extra fields, and restore table name
m = m2 %>% select(country, species, fao, environment, year, value, Taxon_code)

##########################################
#### PART 2 - add OHI region identifiers
##########################################

 # this could be added to mar_fxs since it's a little different than other fao data
# a) prepare the OHI region lookup table with country names and synonyms to match region ids
dpath = 'src/LookupTables'
rgn_master.csv   = file.path(dpath, 'eez_rgn_2013master.csv')
rgn_synonyms.csv = file.path(dpath, 'rgn_eez_v2013a_synonyms.csv')
rk = read.csv(rgn_master.csv); head(rk) # master file by BB
rk2 = read.csv(rgn_synonyms.csv); head(rk2) # synonym file by JS
rk = rk %>%
  filter(rgn_id_2013 < 255) %>% # remove open ocean and disputed
  mutate(rgn_typ = 'ohi_region') %>%
  arrange(rgn_id_2013)
rkb  = data.frame(rk$rgn_id_2013,  rk$rgn_key_2013,  rk$rgn_nam_2013,  rk$region_id_2012,  rk$rgn_typ) 
rk2b = data.frame(rk2$rgn_id_2013, rk2$rgn_key_2013, rk2$rgn_nam_2013, rk2$region_id_2012, rk2$rgn_typ)
names(rkb)  = c('rgn_id_2013', 'rgn_key_2013', 'rgn_nam_2013', 'region_id_2012', 'rgn_typ')
names(rk2b) = c('rgn_id_2013', 'rgn_key_2013', 'rgn_nam_2013', 'region_id_2012', 'rgn_typ')
regionkey = rbind(rkb, rk2b)
regionkey$rgn_nam_2013 = as.character(regionkey$rgn_nam_2013)
regionkey = regionkey[!duplicated(regionkey), ] # remove duplicate rows in regionkey. this is what SELECT DISTINCT rgn_nam_2013, rgn_key_2013, rgn_id_2013, rgn_typ did with SQlite

# b) prep the MAR data: get the position of the 'country' column in m, and rename it to match the OHI region lookup table
col_num = grep('country', names(m), ignore.case = TRUE)
names(m)[col_num] = 'rgn_nam'

# c) join the mariculture data with the OHI rgn lookup
m_regionkey = regionkey %>%
  select(rgn_id = rgn_id_2013,
         rgn_nam = rgn_nam_2013, 
         rgn_typ) %>%
  inner_join(m, by = 'rgn_nam'); head(m_regionkey) 

# d) only keep ohi_regions
m_rgn = m_regionkey %>%
  filter(rgn_typ == 'ohi_region' | is.na(rgn_typ)); head(m_rgn) 

# e) check 
# indicate which were removed
RemovedRegions = m_regionkey %>%
  filter(rgn_typ == 'landlocked' | rgn_typ == 'largescale' | rgn_typ == 'disputed') %>%
  select(rgn_nam)
print(unique(data.frame(RemovedRegions)))
# indicate which still need to be assigned:
m_rgn.na = m_rgn %>%
  filter(is.na(rgn_typ))
print(unique(data.frame(m_rgn.na$country_id)))
# any duplicates?
table(duplicated(m_rgn))
# final check
print(dim(m_rgn)[1] + dim(RemovedRegions)[1] == dim(m_regionkey)[1]) # make sure this is TRUE

### output to .csv
f_out <- sprintf('%s/raw/%s_%s.csv', dir_d, basename(dir_d),'raw')
write.csv(m_rgn, f_out, row.names=F, na='')

# f) tidy up: remove rgn_typ, rename country
m <- m_rgn %>% select (id = rgn_id, country = rgn_nam, species, fao, environment, year, value, Taxon_code) %>%
  arrange(country, species, fao, environment, is.na(value), year) 

####################################################################################
###### PART 3: Gap-filling ---------------------------------------------------------
####################################################################################

#######
# gapfill type 1: Carry previous year's value forward if value for max(year) is NA.
#######
  #   This gives wiggle room for regions still in production but not able to report by the FAO deadline. 
m = m %>% group_by (country, species, fao, environment) %>%
  mutate (value_is0   =  value==0 | is.na(value) , 
          year_beg = min(year[value_is0==FALSE]) , # first year of non-0 (or NA) harvest
          year_max = max(year[value_is0==FALSE]) , # last year of non-0 (or NA) harvest
          year_prev  = lag(year,  order_by=year),
          value_prev = lag(value, order_by=year),
          year_update = max(year), # the most recent year in the time-series
          value_ext  =  is.na(value) & year_max==year_update-1 & year==year_update, # this records whether the previous year gap-fill was used, when value_ext=TRUE
          value = ifelse(value_ext==TRUE, value_prev, value),
          value = ifelse(value =='NA', NA, value), # gapfill step # removed  '& year_prev==year-1" as unnecessary
          value      = ifelse(is.na(value) & year>year_beg, 0, value)) %>% # NAs turned to 0 after the first year of non-null harvest
  filter(!is.na(value)) %>% # drop NAs before year_beg
  ungroup() %>%
  arrange( country, species, fao, environment, year, value)

# remove time series with less than 4 datapoints
m2 = m %>% 
  group_by(country,species, fao, environment) %>%
  mutate (not_0 = length(value[value>0])) %>% 
  filter (not_0>3) %>%
  ungroup() %>% 
  select(id, country,  species, fao, environment, year, value, value_is0, value_ext, Taxon_code)

# add a unique identifier per cultivated stock
identifier = m2 %>% select(country,species, fao, environment) %>% unique() %>% mutate(species_code = 1:n())
m2 = left_join(m2,identifier)
  
# keep a record of removed time-series 
removed = m %>% 
  group_by(country,species, fao, environment) %>%
  mutate (not_0 = length(value[value>0])) %>% 
  filter (not_0<=3) %>%
  select(country,species, fao, environment,not_0, Taxon_code) %>%
  unique %>%
  ungroup() ; dim(removed) ; head(removed)

####################
## gapfill type 2: gapfill the Trujillo sustainability scores
####################

# a) create labels to match FAO data to Trujillo scores
# b) match sustainability scores for every combination of species, country, environment, fao region, and for every level of taxonomic aggregation (species-country, species, genus, broad taxon)
# c) obtain a sustainability score for each record, and a book-keeping column of whether it's actual or gap-filled
# d) sum harvest for duplicated layers, once all unique combinations were assigned their sustainability score

## a) create labels to match FAO data to the Trujillo scores by species, country, environment, region, etc
m3 = m2 %>% mutate(c_sp_env = paste(country,species, environment, sep='_'), c_sp_rgn = paste(country,species, fao, sep='_'), c_sp = paste(country,species,sep='_') )
t_lab = read.csv(file.path(dir_d,'raw','Truj_label_sust.csv'), stringsAsFactors = F,na.strings = NA) # upload file with trujillo scores by label

# check all country names match between Trujillo file and FAO data
c_no_match = anti_join(t_lab[t_lab$country!="",],m3,by=c('country'='country')) ; dim(c_no_match) ; unique(c_no_match$country) # no longer values for Poland
# check the fao spp list in the Trujillo sustainability file matches FAO mariculture species
t_nm =  read.csv(file.path(dir_d,'raw','Truj_sp_names.csv'), stringsAsFactors = F)
s_no_match = anti_join(t_lab[t_lab$country!="",],m3,by=c('species_fao'='species')) ; dim(s_no_match)
s_no_match = unique(s_no_match$species_fao) ;  s_no_match  # all excluded species or species that no longer occur in mariculture database

# view the types of combinations with unique resilience scores
table(t_lab$match_type)
# view the types of gap_filling methods
table(t_lab$gapfill)

## b) match sustainability scores for every combination
# 1) first match all the country-species-env combos, 
c_sp_env = t_lab %>% filter(match_type == 'c_sp_env') %>% select(label_fao, c_sp_env_Sust = Maric_sustainability) ; head(c_sp_env)
m3 = left_join(m3, c_sp_env, by=c('c_sp_env' = 'label_fao') )
m3 = m3 %>% select(id,species, species_code, year, value, value_is0, value_ext, Taxon_code, c_sp_env,c_sp_rgn,c_sp,c_sp_env_Sust)
# 2) then match the country-species-fao combos
c_sp_rgn = t_lab %>% filter(match_type == 'c_sp_rgn') %>% select(label_fao, c_sp_rgn_Sust = Maric_sustainability) ; head(c_sp_rgn)
m3 = left_join(m3, c_sp_rgn, by=c('c_sp_rgn' = 'label_fao') )
# 3) then match the remaining country-species combos
c_sp = t_lab %>% filter(match_type == 'c_sp') %>% select(label_fao, c_sp_Sust = Maric_sustainability) ; head(c_sp)
m3 = left_join(m3, c_sp, by=c('c_sp' = 'label_fao') )
# 4) merge all the sustainability scores based on actual values into the same column
m3 = m3 %>% mutate(Sust = ifelse(is.na(c_sp_env_Sust), 
                                             ifelse (is.na(c_sp_rgn_Sust), c_sp_Sust, c_sp_rgn_Sust),
                                       c_sp_env_Sust)) %>% 
                  select(id, species, species_code, year, value, value_is0, value_ext, Taxon_code, Sust)
                                             
# 5) match gapfilling values by species
sp = t_lab %>% filter(match_type == 'species'& gapfill == 'sp_average') %>% select(species = species_fao, sp_Sust = Maric_sustainability) ; head(sp)
m3 = left_join(m3, sp, by=c('species' = 'species') )

# 6)  match gapfilling values by genus
gen = t_lab %>% filter(match_type == 'species' & gapfill == 'genus_average') %>% select(species = species_fao, gen_Sust = Maric_sustainability) ; head(gen)
m3 = left_join(m3, gen, by=c('species' = 'species') ) ; head(m3[!is.na(m3$gen_Sust),])

# 7)  match gapfilling values by coarse taxon
tax =  t_lab %>% filter(match_type == 'taxon') %>% select(Taxon_code = taxon, tax_Sust = Maric_sustainability) ; head(tax)
m3 = left_join(m3,tax, by = c('Taxon_code' = 'Taxon_code') ) 

# c) obtain a sustainability score for each record, and a book-keeping column of whether it's actual or gap-filled
m3 = m3 %>% mutate( 
                      gapfill = ifelse(is.na(Sust), 
                                       ifelse(is.na(sp_Sust),
                                              ifelse(is.na(gen_Sust), 'tax_average', 'gen_average'),
                                              'sp_average'),
                                       'actuals'),
                      Sust_all =  ifelse(is.na(Sust), 
                                         ifelse(is.na(sp_Sust),
                                                ifelse(is.na(gen_Sust), tax_Sust, gen_Sust),
                                                sp_Sust),
                                         Sust)
                            )

# d) sum duplicate records (don't need to separate by fao area or envrionment anymore, now that I matched with sust scores)
m4 <- m3 %>% group_by(id, species, species_code, year, Sust_all, gapfill) %>% summarize( tonnes = sum(value)) %>% ungroup()
# test = unique(m4[,c(1:2,5)]) ; dim(test) # unique id-species-sust.score combos: [1] 926   3
# dim(unique(m4[,1:2])) # unique id-species combos: [1] 924   2
# test2 = test[duplicated(test[,1:2]),]; test2 # find out which regions have different sustainability scores for the same species
# test[test$species == 'Barramundi' & test$id == 14 |test$species == 'Atlantic salmon' & test$id == 218,] # view them:
# two species country pairs have two distinct sustainability scores
# Source: local data frame [4 x 3]
# 
# id         species Sust_all
# 1  14      Barramundi     0.17
# 2  14      Barramundi     0.23
# 3 218 Atlantic salmon     0.17
# 4 218 Atlantic salmon     0.27

###################################################################
####### PART 4:create layers
###################################################################

mar_harvest_tonnes = m4 %>% select(rgn_id = id, species_code = species_code, year = year, tonnes = tonnes)
anyDuplicated(mar_harvest_tonnes)
mar_harvest_species = m4 %>% select(species_code = species_code, species = species) %>% unique()
anyDuplicated(mar_harvest_species)
mar_sustainability_score = m4 %>% select(rgn_id = id, species = species, sust_coeff = Sust_all) %>% unique()
anyDuplicated(mar_sustainability_score)
# code to know whether the most recent 5 years or 4 should be used for trend calc (one of '5_yr', if value_ext = F or '4_yr', if value_ext = T)
mar_trend_years =  m3 %>% group_by(id) %>% summarize(trend_yrs = ifelse('TRUE' %in% value_ext,'4_yr','5_yr')) %>% ungroup()
anyDuplicated(mar_trend_years)

# save outputs
write.csv(mar_harvest_species, file.path(dir_d, 'data/mar_harvest_species_2015a_lyr.csv'), row.names=F)
write.csv(mar_trend_years, file.path(dir_d, 'data/mar_trend_years_2015a_lyr.csv'), row.names=F)
write.csv(mar_sustainability_score, file.path(dir_d, 'data/mar_sustainability_score_2015a_lyr.csv'), row.names=F)
write.csv(mar_harvest_tonnes, file.path(dir_d, 'data/mar_harvest_tonnes_2015a_lyr.csv'), row.names=F)
