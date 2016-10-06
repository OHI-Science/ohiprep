############################
## Exploring fisheries
## MRF Sep - 7 2016
############################

# load libraries, set directories
library(ohicore)  #devtools::install_github('ohi-science/ohicore@dev')
library(dplyr)
library(stringr)
library(tidyr)

## comment out when knitting
setwd("globalprep/fis/v2016")


### Load FAO-specific user-defined functions
source('../../../src/R/fao_fxn.R') # function for cleaning FAO files
source('../../../src/R/common.R') # directory locations


##############################################################
##
##
##  Comparing the % unknown catch for different datasets
##
##
##############################################################

# Summary: more unks in 2016 SAUP data vs. previous data.
# Impossible to compare SAUP and FAO because there are so many differences!

######################################
## get the FAO data ----
######################################

## FAO capture production data
fis_fao_csv = read.csv(file.path(dir_M, 'git-annex/globalprep/_raw_data/FAO_capture/d2016/Global_capture_production_Quantity_1950-2014.csv'))

m <- fis_fao_csv %>%
  rename(country = Country..Country.,
         species = Species..ASFIS.species.,
         area = Fishing.area..FAO.major.fishing.area.,
         Unit = Measure..Measure.) %>%
  filter(Unit == "Quantity (tonnes)") %>%
  filter(!(species %in% c("Freshwater fishes nei", "Red seaweeds", " Marine shells nei", " Hard corals, madrepores nei",
                      "Brown seaweeds", "Aquatic plants nei", "Trochus shells nei", "Frogs", "Green seaweeds",
                      "River and lake turtles nei", "Giant kelps nei", "Gracilaria seaweeds", "Seaweeds nei",
                      "North European kelp", "Midway deep-sea coral", "Aka coral", "Wakame", "Freshwater drum",
                      "Freshwater perches nei", "Freshwater river stingray nei", "Sea lettuces nei", "Sponges",
                      "Spirulina nei", "Freshwater breams nei", "Freshwater crustaceans nei", "Freshwater fishes nei",
                      "Freshwater gobies nei", "Freshwater minnow", "Freshwater molluscs nei",  "Freshwater mussel shells",
                      "Freshwater prawns, shrimps nei", "Freshwater siluroids nei", "Freshwater sponge", "Gelidium seaweeds",
                      "Bubble gum coral", "Bushy hard coral", "Gigartina seaweeds nei", "Japanese kelp", "Bull kelp",
                      "Aquatic plants nei", "Carpet shells nei", "Chilean kelp", "Lacy sea lettuce", "Seaweeds nei",
                      "Turban shells nei", "Trochus shells nei", "Pod razor shell", "Nori nei", "Marine shells nei",
                      "Giant kelps nei"))) %>%
  select(-Unit, -area)

m %>%
  filter(country=="Niue") %>%
  select(country, species, X2010)

m %>%
  filter(country=="Turks and Caicos Is.") %>%
  select(country, species, X2010)

# sort(table(m$species))
# sort(unique(m$species))

m <- m %>%
  gather("year", "value", 3:(ncol(m))) %>%
  mutate(year = gsub("X", "", year)) %>%
  fao_clean_data() %>%
  filter(!is.na(value))

m <- m %>%
  mutate(category = ifelse(species %in% c("Marine fishes nei", "Aquatic invertebrates nei"), "unidentified", "identified")) %>%
  group_by(country, year, category) %>%
  summarize(value = sum(value)) %>%
  ungroup() 

m <- m %>%
  mutate(country = as.character(country)) %>%
  mutate(country = ifelse(country == "C\xf4te d'Ivoire", "Ivory Coast", country))


### Function to convert to OHI region ID
m_rgn <- name_2_rgn(df_in = m, 
                        fld_name='country', 
                        flds_unique=c('year', 'category'))

# Duplicates are summed:
m_rgn <- m_rgn %>%
  group_by(rgn_id, rgn_name, category, year) %>%
  summarize(value = sum(value)) %>%
  ungroup()

m_rgn <- m_rgn %>%
  group_by(rgn_id, rgn_name, year) %>%
  mutate(total_value = sum(value)) %>%
  ungroup() %>%
  mutate(per_unidentifed_FAO = value/total_value * 100) %>%
  filter(category == "unidentified") %>%
  select(rgn_id, rgn_name, year, per_unidentifed_FAO) %>%
  filter(year==2010)


######################################
## get the SAUP 2016 data ----
######################################
saup_2016_raw <- read.csv(file.path(dir_M,'git-annex/globalprep/fis/v2016/int/spatial_catch_saup.csv')) %>%
  rename(common = Common_Name, fao_id = fao_rgn, species=Scientific_Name)
summary(saup_2016_raw)

saup_2016_raw %>%
  filter(rgn_id==154 & year==2010) %>%
  arrange(tons)

saup_2016_raw %>%
  filter(rgn_id==111 & year==2010) %>%
  arrange(tons)

## filter out non ohi eez regions
saup_2016 <- saup_2016_raw %>%
  filter(!is.na(rgn_id)) %>%
  filter(!is.na(fao_id)) %>%
  filter(rgn_id <= 250) %>%
  filter(rgn_id != 213) %>%
  mutate(species = as.character(species))

sort(table(saup_2016$species[saup_2016$TaxonKey<=200000]))

unknowns <- c("Cnidaria", "Miscellaneous diadromous fishes", "Marine groundfishes not identified",
              "Miscellaneous aquatic invertebrates", "Marine finfishes not identified", "Miscellaneous marine crustaceans",
              "Mollusca", "Marine pelagic fishes not identified", "Marine fishes not identified")
saup_2016 <- saup_2016 %>%
  dplyr::select(year, rgn_id, species, tons) %>%
  mutate(category = ifelse(species %in% unknowns, "unidentified", "identified"))

saup_2016 <- saup_2016 %>%
  group_by(rgn_id, year, category) %>%
  summarize(value = sum(tons)) %>%
  ungroup() %>%
  group_by(rgn_id, year) %>%
  mutate(total_value = sum(value)) %>%
  ungroup() %>%
  mutate(per_unidentifed_SAU_2016 = value/total_value * 100) %>%
  filter(category == "unidentified") %>%
  select(rgn_id, year, per_unidentifed_SAU_2016) %>%
  filter(year==2010)

combined <- saup_2016 %>%
  left_join(m_rgn, by=c("rgn_id", "year"))

plot(combined$per_unidentifed_FAO, combined$per_unidentifed_SAU_2016)
filter(combined, per_unidentifed_SAU_2016>70)
abline(0,1, col="red")

#####################################
## SAUP 2015 data
#####################################

data <- read.csv(file.path(dir_M, 'git-annex/globalprep/SAUP_FIS_data/v2015/tmp/Catch_v16072015_summary.csv')) 
data <- data %>%
  mutate(EEZID = ifelse(EEZID==910, 0, EEZID)) %>%
  group_by(EEZID, FAOAreaID, TaxonKey, Year) %>%
  summarize(catch = sum(catch)) %>%
  ungroup()

## SAUP to OHI region data
region <- read.csv("../../../src/LookupTables/new_saup_to_ohi_rgn.csv")
dups <- region[duplicated(region$saup_id), ]
region[region$saup_id %in% dups$saup_id, ] #duplicates, 
###### dups occur because some SAUP regions have lower resolution than OHI regions.
###### This causes the sample size of the following merge to increase, but this is ok.  
###### They end up getting the same score.  

species <- read.csv(file.path(dir_M, 
                              'git-annex/globalprep/SAUP_FIS_data/v2015/raw/ohi_taxon.csv')) %>%
  select(TaxonKey=taxonkey, species=scientific.name)

saup_2015 <- data %>%
  left_join(species, by="TaxonKey")


##############################################################
## converting SAUP regions to OHI regions:
saup_2015 <- saup_2015 %>%
  left_join(region, by=c("EEZID"="saup_id")) %>%   #N increases here due to SAUP regions that correspond to multiple OHI regions
  mutate(ohi_id_2013 = ifelse(is.na(ohi_id_2013), 0, ohi_id_2013)) %>%  # All NA values are EEZID=0
  dplyr::select(rgn_id=ohi_id_2013, species, TaxonKey, Year, catch) %>%
  group_by(rgn_id, species, TaxonKey, Year) %>%
  summarize(catch=sum(catch)) %>%  # N decreases due to OHI regions that are comprised of multiple SAUP regions
  arrange(rgn_id, species, TaxonKey, Year) %>%
  ungroup()

data.frame(filter(saup_2015, TaxonKey <200000) %>%
  select(species, TaxonKey) %>%
  unique())

unknowns <- c("Cnidaria", "Miscellaneous diadromous fishes", "Marine groundfishes not identified",
              "Miscellaneous aquatic invertebrates", "Marine finfishes not identified", "Miscellaneous marine crustaceans",
              "Mollusca", "Marine pelagic fishes not identified", "Marine fishes not identified")

saup_2015 <- saup_2015 %>%
  dplyr::select(year=Year, rgn_id, species, tons=catch) %>%
  mutate(category = ifelse(species %in% unknowns, "unidentified", "identified"))

saup_2015 <- saup_2015 %>%
  group_by(rgn_id, year, category) %>%
  summarize(value = sum(tons)) %>%
  ungroup() %>%
  group_by(rgn_id, year) %>%
  mutate(total_value = sum(value)) %>%
  ungroup() %>%
  mutate(per_unidentifed_SAU_2015 = value/total_value * 100) %>%
  filter(category == "unidentified") %>%
  select(rgn_id, year, per_unidentifed_SAU_2015) %>%
  filter(year==2010)

combined <- combined %>%
  left_join(saup_2015, by=c("rgn_id", "year"))

plot(combined$per_unidentifed_SAU_2016.x, combined$per_unidentifed_SAU_2015, ylab="2015 SAUP", xlab="2016 SAUP")
abline(0,1, col="red")



##############################################################
##
##
##  Trying a more detailed method of gapfilling using taxonomic information
##
##
##############################################################

taxa_data <- read.csv('data/taxon_info.csv') %>%
  select(species, genus, family, order, class)

bmsy <- read.csv('data/fis_bbmsy.csv') %>%
  select(rgn_id, stock_id, year, bmsy=bbmsy)

## High B/Bmsy values that are unfairly penalizing some regions
high_bmsy <- c('Katsuwonus_pelamis-71', 'Clupea_harengus-27', 'Trachurus_capensis-47', 'Sardinella_aurita-34', 'Scomberomorus_cavalla-31')

bmsy <- bmsy %>%
  mutate(bmsy = ifelse(stock_id %in% high_bmsy, 1, bmsy))



catch <- read.csv('data/mean_catch.csv') %>%
  mutate(stock_id_taxonkey = as.character(stock_id_taxonkey)) %>%
  separate(stock_id_taxonkey, c("species", "junk"), sep="-") %>%
  mutate(species = gsub("_", " ", species)) %>%
  separate(junk, c("fao", "taxonkey"), sep="_") %>%
  mutate(stock_id = paste(species, fao, sep="-")) %>%
  mutate(stock_id = gsub(" ", "_", stock_id)) %>%
  left_join(taxa_data, by="species") %>%
  left_join(bmsy, by=c('rgn_id', 'stock_id', 'year'))

## convert bmsy to score
alpha <- 0.5
beta <- 0.25
lowerBuffer <- 0.95
upperBuffer <- 1.05

catch$score = ifelse(catch$bmsy < lowerBuffer, catch$bmsy,
                 ifelse (catch$bmsy >= lowerBuffer & catch$bmsy <= upperBuffer, 1, NA))
catch$score = ifelse(!is.na(catch$score), catch$score,  
                 ifelse(1 - alpha*(catch$bmsy - upperBuffer) > beta,
                        1 - alpha*(catch$bmsy - upperBuffer), 
                        beta))


# ### we would need to get the taxonomic information in the table for these as well:  
# unidentified_taxa <- catch %>%
#   filter(is.na(genus)) %>%
#   select(taxa = species) %>%
#   unique()
# 
# write.csv(unidentified_taxa, "int/unidentified_taxa.csv", row.names=FALSE)
# 

score_gf <- rbind(
catch %>%
  group_by(genus, fao, year) %>%
  summarize(median = median(score, na.rm=TRUE)) %>%
  select(year, fao, species=genus, median),
catch %>%
  group_by(family, fao, year) %>%
  summarize(median = median(score, na.rm=TRUE)) %>%
  select(year, fao, species=family, median),
catch %>%
  group_by(order, fao, year) %>%
  summarize(median = median(score, na.rm=TRUE)) %>%
  select(year, fao, species=order, median),
catch %>%
  group_by(class, fao, year) %>%
  summarize(median = median(score, na.rm=TRUE)) %>%
  select(year, fao, species=class, median)
) %>%
  arrange(year, fao, species)

score_gf <- score_gf %>%
  filter(!is.na(median))

catch_gf <- catch %>%
  left_join(score_gf, by=c("species", "fao", "year")) %>%
  rename(species_gf = median) %>%
  left_join(score_gf, by=c("genus"="species", "fao", "year")) %>%
  rename(genus_gf = median) %>%
  left_join(score_gf, by=c("family"="species", "fao", "year")) %>%
  rename(family_gf = median) %>%
  left_join(score_gf, by=c("order"="species", "fao", "year")) %>%
  rename(order_gf = median) %>%
  left_join(score_gf, by=c("class"="species", "fao", "year")) %>%
  rename(class_gf = median)
  
catch_gf <- catch_gf %>%
  mutate(score_gf = NA) %>%
  mutate(score_gf = ifelse(is.na(score_gf), species_gf, score_gf)) %>%
  mutate(score_gf = ifelse(is.na(score_gf), genus_gf, score_gf)) %>%
  mutate(score_gf = ifelse(is.na(score_gf), family_gf, score_gf)) %>%
  mutate(score_gf = ifelse(is.na(score_gf), order_gf, score_gf)) %>%
  mutate(score_gf = ifelse(is.na(score_gf), class_gf, score_gf)) %>%
  group_by(rgn_id, year) %>%
  mutate(score_gf = ifelse(is.na(score_gf), median(score, na.rm=TRUE), score_gf)) %>%
  ungroup() 

score_data <- catch_gf %>%
  select(rgn_id, species, fao, taxonkey, year, mean_catch, score, score_gf)

penaltyTable <- data.frame(TaxonPenaltyCode=1:6, 
                           penalty=c(0.1, 0.25, 0.5, 0.8, 0.9, 1))

score_data <- score_data %>%
  mutate(TaxonPenaltyCode = as.numeric(substring(taxonkey, 1, 1))) %>%
  left_join(penaltyTable, by='TaxonPenaltyCode') %>%
  mutate(score_gf_penalty = score_gf * penalty) %>%
  mutate(score_gapfilled = ifelse(is.na(score), "Median gapfilled", "none")) %>%
  mutate(score = ifelse(is.na(score), score_gf_penalty, score))

status_data <- score_data %>%
  select(rgn_id, species, fao, year, mean_catch, score=score_gf_penalty)

status_data <- status_data %>%
  group_by(year, rgn_id) %>%
  mutate(SumCatch = sum(mean_catch)) %>%
  ungroup() %>%
  mutate(wprop = mean_catch/SumCatch)

status_data <- status_data %>%
  group_by(rgn_id, year) %>%
  summarize(status = prod(score^wprop)) %>%
  ungroup()

names <- read.csv('../../../../ohi-global/eez2016/layers/rgn_labels.csv') %>%
  select(rgn_id, rgn_name = label)

old <- read.csv('../../../../ohi-global/eez2016/scores.csv') %>%
  filter(goal == "FIS") %>%
  filter(dimension == "status") %>%
  select(rgn_id = region_id, old_score = score)

new <- status_data %>%
  filter(year==2010) %>%
  mutate(score = round(status*100, 1)) %>%
  select(rgn_id, score) %>%
  left_join(old, by="rgn_id") %>%
  left_join(names, by="rgn_id") %>%
  mutate(new_minus_old = score - old_score) %>%
  arrange(new_minus_old)

write.csv(new, "int/new_vs_old_gapfilling.csv", row.names=FALSE)
plot(new$old_score, new$score, ylab="New gapfilling status", xlab="Old gapfilling status")
abline(0,1, col="red")


##############################################################
##
##                JAMIE'S EXPLORATIONS
##  Trying a more detailed method of gapfilling using taxonomic information
##
##
##############################################################

taxa_data <- read.csv('data/taxon_info.csv') %>%
  select(species, genus, family, order, class)

bmsy <- read.csv('data/fis_bbmsy.csv')%>%
          separate(stock_id,c('stock','fao'),sep = '-')%>%
          mutate(species = str_replace_all(stock, "_"," "))%>%
          select(-stock)%>%
          filter(year==2010)%>%
          left_join(taxa_data)

catch <- read.csv('data/mean_catch.csv') %>%
  mutate(stock_id_taxonkey = as.character(stock_id_taxonkey)) %>%
  separate(stock_id_taxonkey, c("species", "junk"), sep="-") %>%
  mutate(species = gsub("_", " ", species)) %>%
  left_join(taxa_data) 

### we would need to get the taxonomic information in the table for these as well:  
 # unidentified_taxa <- catch %>%
 #   filter(is.na(genus)) %>%
 #   select(taxa = species) %>%
 #   unique()

#write.csv(unidentified_taxa, "int/unidentified_taxa.csv", row.names=FALSE)

### figure out new Thailand score for 2010 data
catch_t <- catch %>%
  filter(rgn_id==25 & year==2010)%>%
  separate(junk,c('fao','junk'),sep = '_')

nrow(catch_t)
#566

non_sp <- catch_t%>%
            filter(is.na(genus))

nrow(non_sp)
#304 (there are 304 taxa we don't have bbmsy estimates for for thailand)


bmsy_func = function(sp,f){
  
  #filter bmsy table for all species with sp in the row and get median BBmsy
  
  bmsy_sub <- bmsy%>%
              filter(fao == f) #filter so fao area = f
  
  b_df <- bmsy_sub[which(apply(bmsy_sub,1, function(r) any(r == sp))),] #subset the bmsy table where the "species" name (anything from genus to class) is found in any column/row
  
  med <- median(b_df$bbmsy,na.rm=T) #get the median b/bmsy from all of the taxa
  
  return(med)
}

out <- non_sp%>%
  rowwise()%>%
       mutate(bbmsy = bmsy_func(species,fao))%>%
      full_join(catch_t)%>%
      select(rgn_id,species,fao,junk,year,mean_catch,bbmsy)%>%
      mutate(catch_prop = mean_catch/sum(.$mean_catch))

#how many new bbmsy values?
nrow(filter(out,!is.na(bbmsy)))
#110 out of 292

#how much additional catch are we getting bmsy for?

tot = sum(out$mean_catch)
non_sp_bbmsy_catch <- out%>%
                        filter(species %in% non_sp$species,
                               !is.na(bbmsy))

prop = sum(non_sp_bbmsy_catch$mean_catch)/tot
prop
#0.4255 (50% of catch)
