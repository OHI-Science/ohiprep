##############################################
## Figuring out coastal countries that signed 
## The CBD (originally, just the CBD convention, now the Nagoya Convention)
## April 8 2014
## MRF
##############################################
library(plyr)
library(dplyr)
library(stringr)

rm(list=ls())

setwd("C:\\Users\\Melanie\\Desktop\\CBD")

## ocean EEZ's
countries <- read.csv("C:\\Users\\Melanie\\Desktop\\GL-NCEAS-Regions_v2014\\eez_rgn_2013master.csv")
eez <- countries %.%
  filter(rgn_typ == "eez") %.%
  filter(rgn_nam_2013 != "DISPUTED") %.%
  filter(eez_iso3 != "TWN",
         sov_nam != "Antarctica") %.%                #Taiwan can't sign
  select(ISO=eez_iso3, rgn_nam_2013, sov_nam) %.%
  unique() %.%
  arrange(rgn_nam_2013)
eez <- rename(eez, c(rgn_nam_2013="Country"))


countriesList <- unique(eez$sov_nam)  #153 countries
length(countriesList)
countriesDetails <- eez[eez$Country %in% countriesList, ]


## signatories
sign <- read.csv("raw\\CBD_Nagoya.csv", strip.white=TRUE)
sign <- sign %.%
  filter(Signed..approved..accession..or.ratification..1.yes.%in% 1) %.%
  select(Country=Country..Name) %.%
  mutate(Country=as.factor(str_trim(Country))) %.%
  arrange(Country)
dim(sign) #101 original signers

# change a few names to match other list:
sign$Country  <- as.character(sign$Country)
sign$Country[sign$Country == "Comoros"] <- "Comoro Islands"
sign$Country[sign$Country == "Congo"] <- "Republique du Congo"
sign$Country[sign$Country == "Côte d'Ivoire"] <- "Ivory Coast"
sign$Country[sign$Country == "Guinea-Bissau"] <- "Guinea Bissau"
sign$Country[sign$Country == "Micronesia (Federated States of)"] <- "Micronesia"
sign$Country[sign$Country == "Republic of Korea"] <- "South Korea"
sign$Country[sign$Country == "Syrian Arab Republic"] <- "Syria"
sign$Country[sign$Country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
sign$Country  <- as.factor(sign$Country)


## Join with country data to get ISO data
sign = sign %.%
  left_join(countriesDetails, by="Country") %.%
  arrange(Country)

## check out countries not assigned an ISO
dim(sign[is.na(sign$ISO),])
# should be 20: 19 landlocked, 1 European Union (countries already represented)

sign  <- sign[!is.na(sign$ISO),]

dim(sign)

setdiff(sign$ISO, countriesDetails$ISO)
length(setdiff(sign$ISO, countriesDetails$ISO))
tmp <- setdiff(countriesDetails$ISO, sign$ISO)
length(tmp)
tmp
countriesDetails[countriesDetails$ISO %in% tmp, ]
intersect(countriesDetails$ISO, sign$ISO) #81 of 153 countries are signatories
length(intersect(countriesDetails$ISO, sign$ISO)) 

81/153


