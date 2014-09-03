####################################################################
## Fisheries (FIS) calculation (Part 1):
## Data preparation 
####################################################################
library(plyr)
setwd("N:\\model\\GL-HS-AQ-Fisheries_v2013\\HighSeas")

#---------------------------------------------------------
## Species estimates of b/bmsy
#---------------------------------------------------------
## Using b_bmsy values from OHI 2013 data.
## Now, we are using resilience to determine which
## method of calculating b/bmsy to use.

cmsy.ohi.df <- read.csv("N:\\model\\GL-NCEAS_FIS_v2013a\\RevisingFIS\\data\\fnk_fis_b_bmsy_lyr.csv")


head(cmsy.ohi.df)


#---------------------------------------------------------
## Catch data
#---------------------------------------------------------

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

# Calculate mean catch over all years per taxon and saup_id/fao_id
MeanCatch <- ddply(country.level.data, .(saup_id, fao_id, Taxonkey, TaxonName), 
                   summarize, mean_catch=mean(Catch), .progress = "text")

country.level.data<-join(country.level.data, MeanCatch, by=c("saup_id","fao_id","Taxonkey","TaxonName"))
head(country.level.data)

# remove mean catch == 0
country.level.data <- country.level.data[country.level.data$mean_catch != 0, ]
country.level.data <- country.level.data[country.level.data$year>2005,]

country.level.data$fao_saup_id <- paste(country.level.data$fao_id, 
                            country.level.data$saup_id, sep="_")

country.level.data <- subset(country.level.data, select=c("fao_saup_id", "taxon_name_key", 
                                                          "year", "mean_catch"))

#write.csv(country.level.data, "tmp\\cnk_fis_meancatch.csv", row.names=FALSE)

