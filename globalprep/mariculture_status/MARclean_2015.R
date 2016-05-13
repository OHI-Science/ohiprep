
#### Future improvments:
# 1) the FAO clean function should now work with these data due to Casey's updates
# 2) Update the "updated 2014_sp_list_v2: get rid of alias variables and make new.name the alias when necessary, 
#    combine with MAR_species_seaweeds (for some reason seaweeds are not excluded here)
# 3) name to rgn for both FAO and Trujillo data...rather than changing some country names after name to rgn.

############################## MARICULTURE LAYERS PREP #######################################################################
# load libraries, set directories
# source('../ohiprep/src/R/common.R') # set dir_neptune_data
library(ohicore)  #devtools::install_github('ohi-science/ohicore@dev')
library(dplyr)
library(stringr)
library(tidyr)

####
### Load FAO-specific user-defined functions
source('globalprep/mariculture_status/mar_fxs.R')


dir_d = '../ohiprep/globalprep/mariculture_status'

##############################################
# PART 1 - get raw MAR data and clean it
##############################################
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
  m  <-  d %>%    
    select(
      country = `Country (Country)` ,
      species = `Species (ASFIS species)` ,
      fao = `Aquaculture area (FAO major fishing area)` ,
      environment = `Environment (Environment)` ,
      5:ncol(d)) %>%  
    gather(year, value, -country, -species, -fao, -environment)

  
# remove seaweeds/algae/plants (these are not always used for food sources, so included in NP instead)
  seaweed <- read.csv(file.path(dir_d,'raw/MAR_species_seaweeds.csv')) %>%
    filter(seaweed == "yes") %>%
    select(species)
  
m <- m %>%
  filter(!(species %in% seaweed$species))
  
    ## tried to use the fao_clean_data function - but in commodities they put " F" at the end - here it is at the beginning.
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
 arrange(country, species, fao, environment, year) 
  
filter(m, country=="Albania", species=="European seabass")
filter(m, country=="Algeria", species=="Common sole")
  
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
m <- mar_split_antilles(m) # in './R/mar_fxs.R', the conch in Netherlands Antilles was cultivated for restoration purposes in a joint program across these 3 countries
# also splits up the 'Bonaire/S.Eustatius/Saba that gets reported the same.

test <- m %>% filter (country == 'Netherlands Antilles') ; dim(test) # check it worked
test2 <- m %>% filter (country == 'Bonaire/S.Eustatius/Saba') ; dim(test2) # check it worked
filter(m, country=="Bonaire")
filter(m, country=="Sint Eustatius")
filter(m, country=="Saba")
test3 <- m %>% filter (country == 'Channel Islands') ; dim(test3) # check it worked
filter(m, country=="Guernsey")
filter(m, country=="Jersey")

# f) exclude taxa that aren't relevant to mariculture goal
# 1) load last year's list of species names that includes a keep/discard field and rename field (=booleans)
sp_nm <- read.csv(file.path(dir_d,'raw', 'updated 2014_sp_list_v2.csv'), stringsAsFactors=F)
names(sp_nm) [1] <- 'species'
# 2) see if there are new species this year that need assigning to a taxon group, join by 'species' field
new.spp <- setdiff(m$species, sp_nm$species)
new.spp # check: if dim has 0 rows it means all match
# if length(new.spp) >0 , hand check whether to keep (exclude seaweeds and species harvested for ornamental/medicinal), check if synonyms match Trujillo names

# 3) Now remove and rename species appropriately from the harvest dataset
m2 <- left_join (m, sp_nm, by="species") # Joining by: species
m2 <- filter (m2, exclude==0) # remove flagged species = dim(m)[1]-dim(m2)[1]
# 4) change names using new.name and the species alias
m2$species <- ifelse (m2$match.alias==1, m2$alias, m2$new.name) 

# 5) sum duplicates after name change (this also gets rid of the NA values)
m2 <- m2 %>%
  filter(!is.na(value)) %>%
  group_by(country, species, fao, environment, year, Taxon_code) %>%
    summarize(value = sum(value)) %>% 
  ungroup()

# 6) remove extra fields, and restore table name
m = m2 %>% 
  select(country, species, fao, environment, year, value, Taxon_code)

##########################################
#### PART 2 - add OHI region identifiers
##########################################

source('../ohicore/R/name_to_rgn.R')

m_rgn <- m %>% 
  name_to_rgn(fld_name='country', flds_unique=c('country', 'species', 'fao', 'environment', 'Taxon_code', 'year'), 
              fld_value='value', add_rgn_name=T, dir_lookup = 'src/LookupTables')


# f) tidy up: remove rgn_typ, rename country
m <- m_rgn %>% 
  select (id = rgn_id, country = rgn_name, species, fao, environment, year, value, Taxon_code) %>%
  arrange(country, species, fao, environment, year) 

####################################################################################
###### PART 3: Gap-filling ---------------------------------------------------------
####################################################################################

####################
## gapfill type 1: fill in missing years after first harvest with 0 values
####################
m <- m %>%
  group_by(id, country, species, fao, environment, Taxon_code) %>%
  mutate(year1 = min(year)) %>%
  ungroup()

m_spread <- spread(m, year, value)
m_gather <- gather(m_spread, "year", "value", 8:dim(m_spread)[2])

m <- m_gather %>%
  mutate(year = as.numeric(as.character(year))) %>%
  filter(year >= year1) %>%
  mutate(gap_0_fill = ifelse(is.na(value), "NA_to_zero", "0")) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  select(-year1)

filter(m, country=="Albania", species=="European seabass")
data.frame(filter(m, country=="Algeria", species=="Common sole"))


# remove time series with less than 4 datapoints
m2 = m %>% 
  group_by(id, country,species, fao, environment) %>%
  mutate (not_0 = length(value[value>0])) %>% 
  filter (not_0>3) %>%
  ungroup() %>% 
  select(id, country,  species, fao, environment, year, value, Taxon_code, gap_0_fill)

# add a unique identifier per cultivated stock
identifier = m2 %>% 
  select(id, country, species, fao, environment) %>% 
  unique() %>% 
  mutate(species_code = 1:n())
m2 = left_join(m2,identifier)

### save this cleaned file to estimate total MAR yield per country
write.csv(m2, 'globalprep/mariculture_status/data/MAR_FP_data.csv', row.names=FALSE)


data.frame(m2 %>%
             filter(id==16) %>%
             arrange(species, fao, environment))
data.frame(m2 %>%
             filter(species_code==1))

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
t_lab = read.csv(file.path(dir_d,'raw','Truj_label_sust.csv'), stringsAsFactors = F,na.strings = NA) # upload file with trujillo scores by label

## there are mismatches between our official country names and the names in the Trujillo data.
## ideally these should be changed in the Trujillo data, but there are many columns that use the country names and they would also have to be changed
## so, for now, I will change our country names to match what is in the Trujillo data
setdiff(t_lab$country, m2$country)
setdiff(m2$country, t_lab$country)
m2$country[m2$country == 'Taiwan'] <- 'Taiwan Province of China'
m2$country[m2$country == "Faeroe Islands"] <- "Faroe Islands"
m2$country[m2$country == "Iran"] <- "Iran (Islamic Rep. of)"
m2$country[m2$country == "Iran"] <- "Iran (Islamic Rep. of)"
m2$country[m2$country == "North Korea"] <- "Korea, Dem. People's Rep"
m2$country[m2$country == "South Korea"] <- "Korea, Republic of"
m2$country[m2$country == "Russia"] <- "Russian Federation"
m2$country[m2$country == "United States"] <- "United States of America"
m2$country[m2$country == "Venezuela"] <- "Venezuela, Boliv Rep of"
m2$country[m2$country == "Vietnam"] <- "Viet Nam"
setdiff(t_lab$country, m2$country) # missing Poland is fine
setdiff(m2$country, t_lab$country)

## a) create labels to match FAO data to the Trujillo scores by species, country, environment, region, etc
m3 = m2 %>% mutate(c_sp_env = paste(country,species, environment, sep='_'), 
                   c_sp_rgn = paste(country,species, fao, sep='_'), 
                   c_sp = paste(country,species,sep='_') )


# check the fao spp list in the Trujillo sustainability file matches FAO mariculture species
setdiff(t_lab$species_fao[t_lab$country!=""], m3$species) # species that are no longer harvested
setdiff(m3$species, t_lab$species_fao[t_lab$country!=""]) # species with no Trujillo data

# view the types of combinations with unique resilience scores
table(t_lab$match_type)
# view the types of gap_filling methods
table(t_lab$gapfill)
table(t_lab$gapfill, t_lab$match_type)

filter(t_lab, species_fao=="Barramundi" & country=='Taiwan Province of China')
data.frame(filter(m3, species=="Barramundi" & id==14))

Escapees <- m3

## b) match sustainability scores for every combination
# 1) first match all the country-species-env combos, 
c_sp_env = t_lab %>% 
  filter(match_type == 'c_sp_env') %>% 
  select(label_fao, c_sp_env_Sust = Maric_sustainability) ; head(c_sp_env)

#these mismatches do not appear in the data (are ok)
setdiff(c_sp_env$label_fao, m3$c_sp_env)
sort(setdiff(m3$c_sp_env, c_sp_env$label_fao))

m3 = left_join(m3, c_sp_env, by=c('c_sp_env' = 'label_fao') )

m3 = m3 %>% 
  select(id, species, species_code, year, value, Taxon_code, c_sp_env,c_sp_rgn,c_sp,c_sp_env_Sust)

# 2) then match the country-species-fao combos
c_sp_rgn = t_lab %>% 
  filter(match_type == 'c_sp_rgn') %>% 
  select(label_fao, c_sp_rgn_Sust = Maric_sustainability) ; head(c_sp_rgn)

#these mismatches do not appear in the data (are ok)
setdiff(c_sp_rgn$label_fao, m3$c_sp_rgn)
sort(setdiff(m3$c_sp_rgn, c_sp_env$label_fao))

m3 = left_join(m3, c_sp_rgn, by=c('c_sp_rgn' = 'label_fao') )

# 3) then match the remaining country-species combos
c_sp = t_lab %>% 
  filter(match_type == 'c_sp') %>% 
  select(label_fao, c_sp_Sust = Maric_sustainability) %>%
  unique(); head(c_sp)

setdiff(c_sp$label_fao, m3$c_sp)
sort(setdiff(m3$c_sp, c_sp$label_fao))

m3 = left_join(m3, c_sp, by=c('c_sp' = 'label_fao') )

# 4) merge all the sustainability scores based on actual values into the same column
m3 = m3 %>% 
  mutate(Sust = ifelse(is.na(c_sp_env_Sust), 
                  ifelse (is.na(c_sp_rgn_Sust), c_sp_Sust, c_sp_rgn_Sust),
                                       c_sp_env_Sust)) %>% 
                  select(id, species, species_code, year, value, Taxon_code, Sust)
                                             
# 5) match gapfilling values by species
sp = t_lab %>% 
  filter(match_type == 'species'& gapfill == 'sp_average') %>% 
  select(species = species_fao, sp_Sust = Maric_sustainability) ; head(sp)
m3 = left_join(m3, sp, by=c('species' = 'species') )

# 6)  match gapfilling values by genus
gen = t_lab %>% 
  filter(match_type == 'species' & gapfill == 'genus_average') %>% 
  select(species = species_fao, gen_Sust = Maric_sustainability) ; head(gen)

m3 = left_join(m3, gen, by=c('species' = 'species') ) ; head(m3[!is.na(m3$gen_Sust),])

# 7)  match gapfilling values by coarse taxon
tax =  t_lab %>% 
  filter(match_type == 'taxon') %>% 
  select(Taxon_code = taxon, tax_Sust = Maric_sustainability) ; head(tax)
m3 = left_join(m3,tax, by = c('Taxon_code' = 'Taxon_code') ) 

# c) obtain a sustainability score for each record, and a book-keeping column of whether it's actual or gap-filled
m3 = m3 %>% 
  mutate( 
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

filter(m3, id==82, species=="European seabass")
data.frame(filter(m3, id==84, species=="Common sole"))

# d) sum duplicate records (don't need to separate by fao area or envrionment anymore, now that I matched with sust scores)
m4 <- m3 %>% 
  group_by(id, species, species_code, year, Sust_all, gapfill) %>% 
  summarize( tonnes = sum(value)) %>% 
  ungroup()

test = unique(m4[,c(1:2,5)]) ; dim(test)
dim(unique(m4[,1:2]))
test2 = test[duplicated(test[,1:2]),]; test2
test[test$species == 'Barramundi' & test$id==14,]
test[test$species == 'Atlantic salmon' & test$id==218,]



###########################################################
#####  PART 3b: Genetic escapes
###########################################################


## b) match sustainability scores for every combination
# 1) first match all the country-species-env combos, 
c_sp_env = t_lab %>% 
  filter(match_type == 'c_sp_env') %>% 
  select(label_fao, c_sp_env_Esc = Genetic.escapees) ; head(c_sp_env)

#these mismatches do not appear in the data (are ok)
setdiff(c_sp_env$label_fao, m3$c_sp_env)
sort(setdiff(m3$c_sp_env, c_sp_env$label_fao))

Escapees = left_join(Escapees, c_sp_env, by=c('c_sp_env' = 'label_fao') )

Escapees = Escapees %>% 
  select(id, species, species_code, year, value, Taxon_code, c_sp_env,c_sp_rgn,c_sp,c_sp_env_Esc)

# 2) then match the country-species-fao combos
c_sp_rgn = t_lab %>% 
  filter(match_type == 'c_sp_rgn') %>% 
  select(label_fao, c_sp_rgn_Esc = Genetic.escapees) ; head(c_sp_rgn)

#these mismatches do not appear in the data (are ok)
setdiff(c_sp_rgn$label_fao, m3$c_sp_rgn)
sort(setdiff(m3$c_sp_rgn, c_sp_env$label_fao))

Escapees = left_join(Escapees, c_sp_rgn, by=c('c_sp_rgn' = 'label_fao') )

# 3) then match the remaining country-species combos
c_sp = t_lab %>% 
  filter(match_type == 'c_sp') %>% 
  select(label_fao, c_sp_Esc = Genetic.escapees) %>%
  unique(); head(c_sp)

setdiff(c_sp$label_fao, m3$c_sp)
sort(setdiff(m3$c_sp, c_sp$label_fao))

Escapees = left_join(Escapees, c_sp, by=c('c_sp' = 'label_fao') )

# 4) merge all the sustainability scores based on actual values into the same column
Escapees = Escapees %>% 
  mutate(Escapees = ifelse(is.na(c_sp_env_Esc), 
                       ifelse (is.na(c_sp_rgn_Esc), c_sp_Esc, c_sp_rgn_Esc),
                       c_sp_env_Esc)) %>% 
  select(id, species, species_code, year, value, Taxon_code, Escapees)

# 5) match gapfilling values by species
sp = t_lab %>% 
  filter(match_type == 'species'& gapfill == 'sp_average') %>% 
  select(species = species_fao, sp_Esc = Genetic.escapees) ; head(sp)
Escapees = left_join(Escapees, sp, by=c('species' = 'species') )

# 6)  match gapfilling values by genus
gen = t_lab %>% 
  filter(match_type == 'species' & gapfill == 'genus_average') %>% 
  select(species = species_fao, gen_Esc = Genetic.escapees) ; head(gen)

Escapees = left_join(Escapees, gen, by=c('species' = 'species') ) ; head(Escapees)

# 7)  match gapfilling values by coarse taxon
tax =  t_lab %>% 
  filter(match_type == 'taxon') %>% 
  select(Taxon_code = taxon, tax_Esc = Genetic.escapees) ; head(tax)
Escapees = left_join(Escapees,tax, by = c('Taxon_code' = 'Taxon_code') ) 

# c) obtain a sustainability score for each record, and a book-keeping column of whether it's actual or gap-filled
Escapees = Escapees %>% 
  mutate( 
    gapfill = ifelse(is.na(Escapees), 
                     ifelse(is.na(sp_Esc),
                            ifelse(is.na(gen_Esc), 'tax_average', 'gen_average'),
                            'sp_average'),
                     'actuals'),
    Escapees_all =  ifelse(is.na(Escapees), 
                       ifelse(is.na(sp_Esc),
                              ifelse(is.na(gen_Esc), tax_Esc, gen_Esc),
                              sp_Esc),
                       Escapees)
  )


# d) sum duplicate records (don't need to separate by fao area or envrionment anymore, now that I matched with sust scores)
Escapees <- Escapees %>% 
  group_by(id, species, species_code, year, Escapees_all, gapfill) %>% 
  summarize( tonnes = sum(value)) %>% 
  ungroup()

genEscapes <- Escapees %>%
  group_by(id, year) %>%
  summarize(genEscapes = weighted.mean(Escapees_all, tonnes, na.rm=TRUE))

genEscapes_gf <- Escapees %>%
  mutate(gapfill = ifelse(gapfill=="actuals", 1, 0)) %>%
  group_by(id, year) %>%
  summarize(genEscapes = weighted.mean(gapfill, tonnes, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(year==2013) %>%
  select(rgn_id=id, pressures.score=genEscapes) %>%
  mutate(pressures.score=ifelse(pressures.score=="NaN", NA, pressures.score)) %>%
  data.frame()
write.csv(genEscapes_gf, 'globalprep/mariculture_status/data/GenEsc_v2013a_gf.csv', row.names=FALSE)


###################################################################
####### PART 4:create layers
###################################################################

mar_harvest_tonnes = m4 %>% 
  select(rgn_id = id, species_code, year, tonnes)
anyDuplicated(mar_harvest_tonnes)

mar_harvest_species = m4 %>% 
  select(species_code, species) %>% unique()
anyDuplicated(mar_harvest_species)

mar_sustainability_score = m4 %>% 
  select(rgn_id = id, species_code, sust_coeff = Sust_all) %>% 
  unique()
anyDuplicated(mar_sustainability_score)


# save outputs
write.csv(mar_harvest_species, file.path(dir_d, 'data/mar_harvest_species_2015a_lyr.csv'), row.names=F)
write.csv(mar_sustainability_score, file.path(dir_d, 'data/mar_sustainability_score_2015a_lyr.csv'), row.names=F)
write.csv(mar_harvest_tonnes, file.path(dir_d, 'data/mar_harvest_tonnes_2015a_lyr.csv'), row.names=F)
write.csv(m4, file.path(dir_d, 'data/mar_sustainability_gap_filling.csv'), row.names = FALSE)
