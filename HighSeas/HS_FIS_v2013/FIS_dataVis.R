####################################################################
## Fisheries (FIS) High Seas:
## Data visualiztion/analysis/trouble-shooting 
####################################################################
library(dplyr)
library(ggplot2)
library(grid)

source("http://nceas.ucsb.edu/~frazier/myTheme.txt")

setwd("N:\\model\\GL-HS-AQ-Fisheries_v2013\\HighSeas")

country.level.data <- read.csv("raw\\Extension_redo_withFlag.csv")

# select only the EEZ values equal zero (Katie: "When the EEZ field is "0" it indicates open ocean)
country.level.data <- country.level.data[country.level.data$EEZ == 0,]

# Recode TaxonKey such that the FAO TaxonKey takes precedence over the Sea 
# Around Us TaxonKey to give credit to those who report at higher level 
# than was ultimately reported in the SAUP data. 
country.level.data$NewTaxonKey <- ifelse(is.na(country.level.data$TLevel),
                                         country.level.data$Taxonkey,
                                         100000*country.level.data$TLevel)

country.level.data$taxon_name_key <- paste(country.level.data$TaxonName, 
                                           as.character(country.level.data$NewTaxonKey), sep="_")
country.level.data <- rename(country.level.data, c(IYear="year", EEZ="saup_id", FAO="fao_id"))
country.level.data <- country.level.data[country.level.data$year >= 1980,]
country.level.data$TaxonGroup <- substr(country.level.data$Taxonkey, 1,1)

#############################################
## Total catch data
#############################################

## Total Catch per fao_id
totalCatch_fao <- country.level.data %.%
  filter(year==2011) %.%
  group_by(fao_id) %.%
  summarize(Catch_fao = sum(Catch))

totalCatch_fao_taxon <- country.level.data %.%
  filter(year==2011) %.%
  group_by(fao_id, TaxonGroup) %.%
  summarize(Catch_fao_taxon = sum(Catch)) %.%
  join(totalCatch_fao) %.% 
  filter(!(fao_id %in% c(88, 48, 58))) %.%
  mutate(propCatch_fao_taxon = round(Catch_fao_taxon/Catch_fao, 3)) %.%
  arrange(Catch_fao, TaxonGroup)

## plot
ggplot(totalCatch_fao_taxon, aes(x= as.factor(fao_id), y=Catch_fao_taxon, fill=TaxonGroup)) +
  geom_bar(stat = "identity") +
  labs(y = "Total Catch, 2011", x = "FAO region") + 
  myTheme
ggsave("catch_fao_taxon.png")

ggplot(totalCatch_fao_taxon, aes(x= as.factor(fao_id), y=propCatch_fao_taxon, fill=TaxonGroup)) +
  geom_bar(stat = "identity") +
  labs(y = "Total Catch, 2011", x = "FAO region") + 
  myTheme
ggsave("propCatch_fao_taxon.png")
              

#############################################
## Total species data
#############################################

## Total Catch per fao_id
totalSpecies_fao <- country.level.data %.%
  filter(year==2011) %.%
  group_by(fao_id) %.%
  summarize(species_fao = length(Catch))

totalSpecies_fao_taxon <- country.level.data %.%
  filter(year==2011) %.%
  group_by(fao_id, TaxonGroup) %.%
  summarize(Species_fao_taxon = length(Catch)) %.%
  join(totalSpecies_fao) %.% 
  filter(!(fao_id %in% c(88, 48, 58))) %.%
  mutate(propCatch_fao_taxon = round(Species_fao_taxon/species_fao, 3)) %.%
  arrange(species_fao, fao_id, TaxonGroup)

## plot
ggplot(totalSpecies_fao_taxon, aes(x= as.factor(fao_id), y=Species_fao_taxon, fill=TaxonGroup)) +
  geom_bar(stat = "identity") +
  labs(y = "Total species, 2011", x = "FAO region") + 
  myTheme
ggsave("species_fao_taxon.png")

ggplot(totalCatch_fao_taxon, aes(x= as.factor(fao_id), y=propCatch_fao_taxon, fill=TaxonGroup)) +
  geom_bar(stat = "identity") +
  labs(y = "Total Catch, 2011", x = "FAO region") + 
  myTheme
ggsave("propCatch_fao_taxon.png")
            