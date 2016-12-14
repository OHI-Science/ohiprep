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
d <- read.csv(file.path(dir_d,'raw','FAO_raw_aquaculture_quant_1950_2012.csv'), check.names=F, stringsAsFactors=F) ; head(d)

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

Ta <- read.csv(file.path(dir_d, 'raw/Truj_sp_cn_sust.csv'), stringsAsFactors=F, check.names=F, ) ; head(Ta) # Trujillo scores
# change country names to match FAO - (would be good to merge this with 'updated 2014_country_names.csv' next time)
Tfn <- read.csv(file.path(dir_d, 'raw/Truj_fao_c_names.csv'),stringsAsFactors=F) ; head(Tfn)
Tfn <- rename(Tfn, c('cn_nm_Truj' = "Country")) # join by Trujillo country name
Ta <- left_join(Ta, Tfn) # Joining by: "Country"
Ta <- Ta %>% mutate( country = ifelse (is.na(cn_nm_fao), Country, cn_nm_fao)) %>% select (country, Sp, Maric_sustainability) %>% 
        rename(c('Sp' = 'species', 'Maric_sustainability' = 'sust'))

# join Trujillo species names to mariculture data, by species-country-fao_region
Tsfao <- read.csv(file.path(dir_d, 'raw/fao_sp_nm_byFAOarea.csv'),stringsAsFactors=F) ; head(Tsfao)
Tsfao <- Tsfao %>% rename(c("species_fao" = 'species',  "fao_area" = 'fao' ))
m4 <- join(m3,Tsfao) # Joining by: country, species, fao

# join Trujillo species names to mariculture data, by species-country-environment
Tsen <- read.csv(file.path(dir_d, 'raw/fao_sp_nm_byEnv.csv'), stringsAsFactors=F) ; head(Tsen)
Tsen <- Tsen %>% rename(c("species_fao" = 'species', 'species_Truj' = 'species_Truj_env'))
m4 <- join(m4, Tsen) # Joining by: country, species, environment
m4 <- m4 %>% mutate (new.species = ifelse( is.na(species_Truj), ifelse( is.na(species_Truj_env),species, species_Truj_env), species_Truj)) %>% 
        select(rgn_nm, rgn_id, country, new.species, year, yield) %>% rename(c('new.species' = 'species'))

# substitute some of the species names (would be good to merge this with 'updated 2014_sp_list.csv' next time)
Tsn <- read.csv(file.path(dir_d, 'raw/Truj_sp_nm_replace.csv'),stringsAsFactors=F) ; head(Tsn)
Tsn <- Tsn %>% rename(c('Row.Labels' = 'species')) %>% select( species, Alias)
m5 <- left_join(m4, Tsn) %>% mutate( species = ifelse(is.na(Alias), species, Alias) )  # Joining by: species
# no matches where Alias=='REMOVE', otherwise would filter them out
  
# join with Trujillo country-spp sust scores
m5 <- join(m5, Ta) %>% select (rgn_nm, rgn_id, country, species, year, sust, yield) # Join by: country, species

# sum duplicate records (don't need to separate by fao area or envrionment anymore, now that I matched with sust scores)
m5 <- m5 %>% group_by(rgn_nm, rgn_id, country, species, year, sust) %>% summarise( yield = sum(yield))
# anyDuplicated(m5[,1:6]) # check for duplicates
# anyDuplicated (m5[,1:5]) # check for sustainability duplicates

# add in species level sustainability
Tss <- read.csv(file.path (dir_d, 'raw/Truj_sp_sust.csv'), stringsAsFactors=F, check.names=F) ; head(Tss)
names(Tss) <- c('species', 'sp_sust', 'whence')
m6 <- left_join(m5, Tss[,1:2]) # Joining by: "species"

# add in taxon names and join taxon level sustainability 
tn <- read.csv(file.path (dir_d, 'raw/Truj_tax_nm_replace.csv'), stringsAsFactors=F, check.names=F) ; head(tn)
names(tn) <- c('species', 'taxon', 'added')
Tts <- read.csv(file.path (dir_d, 'raw/Truj_tax_sust.csv'), stringsAsFactors=F, check.names=F) ; head(Tts)
names(Tts)[2:3] <- c( 'taxon', 'tax_sust')
m7 <- left_join( m6, tn[,1:2]) # Joining by: "species"
m7 <- left_join( m7, Tts[,2:3]) # Joining by: "taxon"

# select the highest taxonomic level of sustainability score possible (highest priority to country-species matches)
m7 <- m7 %>% mutate (sust.all = ifelse( is.na(sust),  
                                    ifelse(is.na(sp_sust), tax_sust, sp_sust),
                                      sust)
                     ) 
m7 <- ungroup(m7) %>% select (rgn_nm, rgn_id, country, species, year, sust.all, yield) %>% rename(c( 'sust.all' = 'sust_coeff' , 'yield' = 'tonnes'))

############################################
# create layers:

# mar_harvest_species_lyr.csv : species_code  species (species code up to 1183 in ohi2013, one number per species name )
m7 <- m7 %>% mutate (sp_lab = paste(country, species, sep='_')) 
temp <- filter(m7, year >2006) %>% group_by( country, species) %>% summarise(sp_lab = unique(sp_lab))  %>% 
          ungroup() %>% mutate (species_code = 1:length(sp_lab))
mar_harvest_species_2014a <- temp %>% select (species_code,  species)
mar_harvest_species_2013a <- filter(m7, year %in% 2005:2011) %>% group_by( country, species) %>% summarise(sp_lab = unique(sp_lab))  %>% 
                                ungroup() %>% mutate (species_code = 1:length(sp_lab))  %>% select (species_code,  species)

anyDuplicated(mar_harvest_species_2014a)
anyDuplicated(mar_harvest_species_2013a)

# mar_trend_years_lyr.csv: rgn_id  trend_yrs (= one of '5_yr', if value_ext = F or '4_yr', if value_ext = T)
mar_trend_years_2014a <- m7 %>% group_by (rgn_id)  %>% summarise (rgn_ids = unique(rgn_id)) %>% 
                              mutate( trend_yrs = rep('5_yr') )  %>% select( rgn_id, trend_yrs )
anyDuplicated(mar_trend_years_2014a)

# mar_sustainability_score_lyr.csv: rgn_id  species	sust_coeff
mar_sustainability_score_2014a <- filter(m7, year>2006 ) %>% group_by(rgn_id, species) %>% summarise (sust_coeff = unique(sust_coeff))
anyDuplicated(mar_sustainability_score_2014a)
  
# mar_harvest_tonnes_lyr.csv: rgn_id	species_code	year	tonnes
mar_harvest_tonnes_2014a <- left_join ( m7, temp) # Joining by: c("country", "species", "sp_lab")
mar_harvest_tonnes_2014a <- mar_harvest_tonnes_2014a %>% select(rgn_id,  species_code,	year,	yield) %>% rename ( c( 'yield' = 'tonnes') )

anyDuplicated(mar_harvest_tonnes_2014a) #not0! it's Brunei's fault: groupers nei got duplicted when generating m6 (why??)
mar_harvest_tonnes_2014a < -mar_harvest_tonnes_2014a [ !duplicated(mar_harvest_tonnes_2014a), ] #removed
# head(mar_harvest_tonnes_2014a[anyDuplicated(mar_harvest_tonnes_2014a),])

  
# save outputs
write.csv(mar_harvest_species_2014a, file.path(dir_d, 'data/mar_harvest_species_2014a_lyr.csv'), row.names=F)
write.csv(mar_harvest_species_2013a, file.path(dir_d, 'data/mar_harvest_species_2013a_lyr.csv'), row.names=F)
write.csv(mar_trend_years_2014a, file.path(dir_d, 'data/mar_trend_years_2014a_lyr.csv'), row.names=F)
write.csv(mar_sustainability_score_2014a, file.path(dir_d, 'data/mar_sustainability_score_2014a_lyr.csv'), row.names=F)
write.csv(mar_harvest_tonnes_2014a, file.path(dir_d, 'data/mar_harvest_tonnes_2014a_lyr.csv'), row.names=F)
