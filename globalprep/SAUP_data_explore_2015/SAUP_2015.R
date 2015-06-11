#######################################
## Exploring new SAUP data
## MRF: June 3 2015
#######################################
library(dplyr)
library(tidyr)
library(stringr)

source('C:/Users/Melanie/Github/ohiprep/src/R/fao_fxn.R') # by @oharac


#####################################################
#############Compare to FAO catch data ##############
#####################################################
### new data
catch <- read.csv("C:/Users/Melanie/Desktop/SAUP_data_2015/raw/ohi_main.csv")
taxon <- read.csv("C:/Users/Melanie/Desktop/SAUP_data_2015/raw/ohi_taxon.csv")
eez <- read.csv("C:/Users/Melanie/Desktop/SAUP_data_2015/raw/ohi_eez.csv")


## fao catch data
faoCatch <- read.csv("C:/Users/Melanie/Github/ohiprep/globalprep/FAO_captureproduction/raw/FAO_captureproduction_1950_2013.csv",
                     check.names=FALSE, strip.white=FALSE)

# rename and gather
  faoCatch = faoCatch %>%
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

head(faoCatch)

# clean FAO data using fao_clean_data()
faoCatch <- faoCatch %>%
  filter(!country %in% c('Totals', 'Yugoslavia SFR')) %>%
  fao_clean_data() # NOTE: optional parameter 'sub_0_0' can be passed to control how a '0 0' code is interpreted.

## make names consistent between two datasets:
regionlul <- read.csv("C:/Users/Melanie/Desktop/SAUP_data_2015/countrylul2.csv")

shared1 <- intersect(faoCatch$country, eez$Name)
eez[eez$Name %in% shared1, ]
eez$AdminCountry <-ifelse(eez$Name %in% shared1 & as.character(eez$Name) != as.character(eez$AdminCountry),
                           as.character(eez$Name), 
                           as.character(eez$AdminCountry))
eez <- left_join(eez, regionlul, by=c('AdminCountry' = 'SAUPCountry'))
eez  <- eez %>%
  mutate(AdminCountry = ifelse(is.na(eez$FAOCountry),
                                AdminCountry,
                                as.character(FAOCountry))) %>%
  filter(!(EEZID %in% c(907, 908, 909))) %>% # need to change the names at the FAO level here, but not necessary to deal with
  select(EEZID, Name, AdminCountry)

intersect(faoCatch$country, eez$AdminCountry)
setdiff(faoCatch$country, eez$AdminCountry)
setdiff(eez$AdminCountry, faoCatch$country)

catch_country_saup  <- catch %>%
  left_join(eez) %>%
  group_by(AdminCountry, TaxonKey, Year) %>%
  summarize(CatchAmount = sum(CatchAmount, na.rm=TRUE))

catch_country_fao <- faoCatch %>%
  group_by(country, species, year) %>%
  summarize(CatchAmount = sum(value, na.rm=TRUE))

## explore: Yellowfin tuna by country
yfFAO <- catch_country_fao %>%
  filter(species == "Yellowfin tuna")

yfSAUP <- catch_country_saup %>%
  filter(TaxonKey == 600143) %>%
  select(country=AdminCountry, year=Year, CatchAmount_saup=CatchAmount) %>%
  left_join(yfFAO)

#write.csv(yfSAUP, "C:/Users/Melanie/Desktop/SAUP_data_2015/FAOvsSAUP_yellowfin.csv", row.names=FALSE)

## explore: Yellowfin tuna by FAO (just remembered that the new SAUP data do not contain the open sea areas)
FAOnames <- read.csv("C:/Users/Melanie/Desktop/SAUP_data_2015/FAOname2number.csv")

catch_fao <- faoCatch %>%
  group_by(area, species, year) %>%
  summarize(CatchAmount = sum(value, na.rm=TRUE)) %>%
  left_join(FAOnames) %>%
  filter(!is.na(FAO)) %>%
  ungroup()

Ta_fao <- catch_fao %>%
  filter(species=="Yellowfin tuna") %>%
  mutate(dataSource="FAOdata") %>%
  select(dataSource, species, FAO, year, catch_FAO=CatchAmount, -area)
data.frame(Ta_fao)

catch_saup <- catch %>%
  group_by(FAOAreaID, Year, TaxonKey) %>%
  summarize(catch_SAUP = sum(CatchAmount, na.rm=TRUE)) %>%
  ungroup()

Ta_saup <- catch_saup %>%
  filter(TaxonKey==600143) %>%
  mutate(species="Yellowfin tuna") %>%
  mutate(dataSource="SAUPdata") %>%
  select(dataSource, species, FAO=FAOAreaID, year=Year, catch_SAUP)
#"Thunnus albacares"
#data <- left_join(Ta_saup, Ta_fao)

data <- rbind(Ta_fao, Ta_saup)


#####################################################
#############Compare to previous SAUP catch data ##############
#####################################################

# new data:
catch <- read.csv("C:/Users/Melanie/Desktop/SAUP_data_2015/raw/ohi_main.csv")
taxon_new <- read.csv("C:/Users/Melanie/Desktop/SAUP_data_2015/raw/ohi_taxon.csv")
#saup_region <- read.csv("C:/Users/Melanie/Desktop/SAUP_data_2015/raw/ohi_eez.csv")

##########################################################
## compare taxa lists to make sure they are consistent...
##########################################################
taxon_old <- read.csv('C:/Users/Melanie/Github/ohiprep/Global/NCEAS-Fisheries_2014a/tmp/TaxonLookup.csv') %>%
  select(taxonkey=Taxonkey, scientific.name.old=TaxonName, common.name_old=Common.name)

allTaxa <- merge(taxon_new, taxon_old, by="taxonkey", all=TRUE)
notSameName <- allTaxa[as.character(allTaxa$scientific.name.old) != as.character(allTaxa$scientific.name), ]
notSameName <- notSameName[!is.na(notSameName$common.name_old), ] #mostly these appear to be the same with slight changes to name (with a few possible exceptions)

newTaxa <- allTaxa[is.na(allTaxa$common.name_old), ] # 317 new taxa
retiredTaxa <- allTaxa[is.na(allTaxa$common.name), ] # 84 retired taxa

#these do not appear to be due to changes in ID #:
intersect(newTaxa$scientific.name, retiredTaxa$scientific.name.old)
intersect(newTaxa$common.name, retiredTaxa$common.name_old) #old id 690309 Tapes pullastra; new id 690630 Venerupis pullastra

#check to make sure new taxa are actually in catch data
setdiff(newTaxa$taxonkey, catch$TaxonKey) #yep!


############################ end taxa compare ################

saup2region_new <- read.csv("C:/Users/Melanie/Desktop/SAUP_data_2015/new_saup_to_rgn.csv") %>%
  filter(saup_id != 908) # this covers multiple EEZ regions

catch_new <- catch %>%
  left_join(saup2region_new, by=c('EEZID'='saup_id')) %>%
  select(Year, ohi_id_2013, TaxonKey, catch_new=CatchAmount) %>%
  group_by(Year, ohi_id_2013, TaxonKey) %>%
  summarize(catch_new = sum(catch_new, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(!is.na(ohi_id_2013))

#old data:
catch_old <- read.delim("N:/git-annex/Global/SAUP-Fisheries_v2011/raw/Extended catch data 1950-2011_18 July 2014.txt")
saup2region_old <- read.csv("C:/Users/Melanie/Github/ohiprep/Global/NCEAS-Fisheries_2014a/data/snk_fis_proparea_saup2rgn_lyr.csv")

catch_old$EEZ[catch_old$EEZ %in% c(277, 278)] <- 276
catch_old$EEZ[catch_old$EEZ %in% c(197, 198)] <- 196


catch_old <- catch_old %>%
  filter(EEZ != 0) %>%
  left_join(saup2region_old, by=c('EEZ'='saup_id')) %>%
  select(Year=IYear, ohi_id_2013=rgn_id, TaxonKey=Taxonkey, catch_old=Catch) %>%
  group_by(Year, ohi_id_2013, TaxonKey) %>%
  summarize(catch_old = sum(catch_old, na.rm=TRUE)) %>%
  ungroup() %>%
  filter(!is.na(ohi_id_2013))

### join the data
catchCompare <- merge(catch_new, catch_old, all=TRUE, by=c('Year', 'ohi_id_2013', 'TaxonKey'))
catchCompare <- catchCompare %>%
  filter(Year>1980)

catch_old_missing <- catchCompare[is.na(catchCompare$catch_old), ]
table(catch_old_missing$Year) #across all years
table(catch_old_missing$ohi_id_2013) #maybe missing 16, 171, 163, otherwise across regions
table(catch_old_missing$TaxonKey) #a lot of new taxa some retired taxa, but the numbers still seem to match with the same taxa..so matches should be equivalent
catch_new_missing <- catchCompare[is.na(catchCompare$catch_new), ] 
table(catch_new_missing$Year) #across all years, but fully missing 2011
table(catch_new_missing$ohi_id_2013) 
table(catch_new_missing$TaxonKey) #a lot of new taxa some retired taxa, but the numbers still seem to match with the same taxa..so matches should be equivalent

plot(catchCompare$catch_new, catchCompare$catch_old)
abline(0,1, col="red")
plot(log(catchCompare$catch_new), log(catchCompare$catch_old))
abline(0,1, col="red")
plot(log(catchCompare$catch_new[catchCompare$catch_old>1 & catchCompare$catch_new>1]), 
     log(catchCompare$catch_old[catchCompare$catch_old>1 & catchCompare$catch_new>1]))
abline(0,1, col="red")

######################################################
## Question: How influential is the open ocean data?
## Current data does not include these regions, but we have
## used it in the past to calculate b/bmsy
## # almost 10% of the taxa fished in eez regions have >20% of their catch in the open ocean 
#######################################################
catch_old <- read.delim("N:/git-annex/Global/SAUP-Fisheries_v2011/raw/Extended catch data 1950-2011_18 July 2014.txt")
saup2region_old <- read.csv("C:/Users/Melanie/Github/ohiprep/Global/NCEAS-Fisheries_2014a/data/snk_fis_proparea_saup2rgn_lyr.csv")

oo_taxa <- catch_old %>%
  filter(EEZ==0) %>%
  group_by(Taxonkey) %>%
  summarize(Catch_oo = sum(Catch, na.rm=TRUE)) %>%
  filter(substring(Taxonkey, 1, 1) == 6) %>%
  ungroup()
  
eez_taxa <- catch_old %>%
  filter(EEZ!=0) %>%
  group_by(Taxonkey) %>%
  summarize(Catch_eez = sum(Catch, na.rm=TRUE)) %>%
  filter(substring(Taxonkey, 1, 1) == 6) %>%
  ungroup()

length(unique(eez_taxa$Taxonkey)) #1089 species
shared_taxa <- intersect(oo_taxa$Taxonkey, eez_taxa$Taxonkey) #398 species
eez_only_taxa <- setdiff(eez_taxa$Taxonkey, oo_taxa$Taxonkey)

prop_catch <- eez_taxa %>%
  left_join(oo_taxa) %>%
  mutate(prop_open_ocean = Catch_oo / (Catch_eez+Catch_oo))

hist(prop_catch$prop_open_ocean, ylab="Number of species", 
     xlab="proportion of catch from open ocean", 
     main="")
sum(prop_catch$prop_open_ocean > 0.2, na.rm=TRUE)
100/dim(eez_taxa)[1]

######################################################
## Question: How big of a deal was taxon correction?
## We had these data in the past (SAUP reported at a differnet taxonomic level than FAO)
#######################################################
catch_old <- read.delim("N:/git-annex/Global/SAUP-Fisheries_v2011/raw/Extended catch data 1950-2011_18 July 2014.txt")

t_diff <- catch_old %>%
  mutate(Taxonkey_first = as.numeric(as.character(substring(Taxonkey, 1, 1)))) %>%
  mutate(SAUPgreaterFAO = ifelse(Taxonkey_first > TLevel,
                                 "yes", "no")) %>%
  mutate(SAUPgreaterFAO = ifelse(is.na(TLevel), "same", SAUPgreaterFAO)) %>%
  select(IYear, EEZ, Taxonkey, Taxonkey_first, Catch, TLevel, SAUPgreaterFAO) %>%
  filter(IYear > 1980)

sum(t_diff$SAUPgreaterFAO == "no") #58320
sum(t_diff$SAUPgreaterFAO == "yes") #10174
sum(t_diff$SAUPgreaterFAO == "same") #446388
(58320 + 10174) / (58320 + 10174 + 446388)

uniqueTaxa <- unique(t_diff$Taxonkey[t_diff$SAUPgreaterFAO == "yes" | t_diff$SAUPgreaterFAO == "no" ]) #150


#summarize catch for taxon change:
t_diff_summary  <- t_diff %>%
  group_by(IYear, EEZ, SAUPgreaterFAO) %>%
  summarize(Catch = sum(Catch, na.rm=TRUE)) %>%
  filter(IYear == 2010) %>%
  ungroup()


t_diff_summary_spread <- t_diff_summary %>%
  spread(SAUPgreaterFAO, Catch) %>%
  mutate(no = ifelse(is.na(no), 0, no)) %>%
  mutate(yes = ifelse(is.na(yes), 0, yes))

t_diff_summary_spread <- t_diff_summary_spread %>%
  mutate(InflatedScore = yes/(yes+no+same))  %>%
  mutate(Penalty = no/(yes+no+same))         

hist(t_diff_summary_spread$InflatedScore, ylab="SAUP regions",
     xlab="Proportion of catch", main="Correction prevented inflated scores")
hist(t_diff_summary_spread$Penalty, ylab="SAUP regions",
     xlab="Proportion of catch", main="Correction prevented unfair penalties")

######################################################
## Question: In the old case, is the cmsy data just the catch
## data summed by FAO region and formatted differently?
## Answer: Yes!
#######################################################

#old data:
catch_old <- read.delim("N:/git-annex/Global/SAUP-Fisheries_v2011/raw/Extended catch data 1950-2011_18 July 2014.txt")

taxaNames <- read.csv("C:/Users/Melanie/Github/ohiprep/Global/NCEAS-Fisheries_2014a/tmp/TaxonLookup.csv")

catch_fao <- catch_old %>%
  group_by(IYear, FAO, Taxonkey) %>%
  summarize(Catch=sum(Catch, na.rm=TRUE)) %>%
  ungroup() %>%
  left_join(taxaNames, by=c('Taxonkey')) %>%
  mutate(stock_id = paste(TaxonName, FAO, sep="_")) %>%
  select(yr=IYear, stock_id, Catch_catch=Catch)


# bbmsy data:
bmsy_old <- read.csv("C:/Users/Melanie/Github/ohiprep/Global/FIS_Bbmsy/raw/OHICatchHistoryCMSY_added0s_07_21_2014.csv") %>%
  select(stock_id, yr, ct)

combined_data <- bmsy_old %>%
  left_join(catch_fao, by=c('yr', 'stock_id'))

plot(combined_data$ct, combined_data$Catch_catch)
abline(0,1, col="red")
