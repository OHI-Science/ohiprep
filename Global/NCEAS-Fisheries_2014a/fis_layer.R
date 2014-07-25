####################################################################
## Fisheries (FIS) calculation (Part 1):
## Data preparation 
####################################################################
library(plyr)
setwd("C:\\Users\\Melanie\\Desktop\\Revising FIS")

#---------------------------------------------------------
## Species estimates of b/bmsy
#---------------------------------------------------------
load("raw\\cmsy_ohi_results_table.RData")
head(cmsy.ohi.df)

# Interpolate Euphausia superba_88 for 1988 and 1989 for FAO 88 to solve NA problem
interp1988 <- data.frame(stock_id="Euphausia superba_88",yr=1988,
                         b_bmsy = cmsy.ohi.df$b_bmsy[cmsy.ohi.df$stock_id=="Euphausia superba_88" & cmsy.ohi.df$yr=="1987"] + 
                           (1/3)*(cmsy.ohi.df$b_bmsy[cmsy.ohi.df$stock_id=="Euphausia superba_88" & cmsy.ohi.df$yr=="1990"] -
                              cmsy.ohi.df$b_bmsy[cmsy.ohi.df$stock_id=="Euphausia superba_88" & cmsy.ohi.df$yr=="1987"]))

interp1989 <- data.frame(stock_id="Euphausia superba_88",yr=1989,
                         b_bmsy = cmsy.ohi.df$b_bmsy[cmsy.ohi.df$stock_id=="Euphausia superba_88" & cmsy.ohi.df$yr=="1987"] + 
                           (2/3)*(cmsy.ohi.df$b_bmsy[cmsy.ohi.df$stock_id=="Euphausia superba_88" & cmsy.ohi.df$yr=="1990"] -
                                    cmsy.ohi.df$b_bmsy[cmsy.ohi.df$stock_id=="Euphausia superba_88" & cmsy.ohi.df$yr=="1987"]))

cmsy.ohi.df <- rbind.fill(cmsy.ohi.df, interp1988, interp1989)

#Produce a dataframe that has just the id, year and BvBmsy
cmsy.ohi.df <- cmsy.ohi.df[,c("stock_id","yr","b_bmsy")]

# Deparse the name to get the fao_id region
cmsy.ohi.df$fao_id <- sapply(strsplit(as.character(cmsy.ohi.df$stock_id), "_"), function(x)x[2])
cmsy.ohi.df$TaxonName <- sapply(strsplit(as.character(cmsy.ohi.df$stock_id), "_"), function(x)x[1])
cmsy.ohi.df <- rename(cmsy.ohi.df, c(yr="year"))
cmsy.ohi.df <- subset(cmsy.ohi.df, select=c("fao_id", "TaxonName", "year", "b_bmsy"))

#write.csv(cmsy.ohi.df, "data\\fnk_fis_b_bmsy.csv", row.names=FALSE)

#---------------------------------------------------------
## Catch data
#---------------------------------------------------------

country.level.data <- read.csv("raw\\Extension_redo_withFlag.csv")

# Need to fix Clupea harengus from FAO 27 (2 catch streams for same species in the same FAO - sum them together)
ClupeaHarengus27 <- country.level.data[country.level.data$TaxonName == "Clupea harengus" 
                                       & country.level.data$FAO==27, ]

SummedCH27 <- ddply(ClupeaHarengus27,.(IYear,EEZ,FAO,Fishing_area,TaxonName,CommonName,LH,Resilience,CHANGE,TLevel),
                    summarise,Catch=sum(Catch),Taxonkey=mean(Taxonkey))

country.level.data <- rbind.fill(country.level.data[!(country.level.data$TaxonName == "Clupea harengus" &
                                                 country.level.data$FAO == 27), ], SummedCH27)

# Recode TaxonKey such that the FAO TaxonKey takes precedence over the Sea 
# Around Us TaxonKey to give credit to those who report at higher level 
# than was ultimately reported in the SAUP data. 
country.level.data$NewTaxonKey <- ifelse(is.na(country.level.data$TLevel)==TRUE,
                                         country.level.data$Taxonkey,
                                   100000*country.level.data$TLevel)
country.level.data$TaxonName_TaxonKey <- paste(country.level.data$TaxonName, 
                            as.character(country.level.data$NewTaxonKey), sep="_")
country.level.data <- rename(country.level.data, c(IYear="year", EEZ="saup_id", FAO="fao_id"))
country.level.data <- country.level.data[country.level.data$saup_id>0,] # clean Data
country.level.data <- country.level.data[country.level.data$year >= 1980,]

# Calculate mean catch over all years per taxon and saup_id/fao_id
MeanCatch <- ddply(country.level.data, .(saup_id, fao_id, Taxonkey, TaxonName), 
                   summarize, MeanCatch=mean(Catch), .progress = "text")

country.level.data<-join(country.level.data, MeanCatch, by=c("saup_id","fao_id","Taxonkey","TaxonName"))
head(country.level.data)

# remove mean catch == 0
country.level.data <- country.level.data[country.level.data$MeanCatch != 0, ]
country.level.data <- country.level.data[country.level.data$year>2005,]

country.level.data$fao_saup_id <- paste(country.level.data$fao_id, 
                            country.level.data$saup_id, sep="_")

country.level.data <- subset(country.level.data, select=c("fao_saup_id", "TaxonName_TaxonKey", 
                                                          "year", "MeanCatch"))

write.csv(country.level.data, "data\\cnk_fis_meancatch.csv", row.names=FALSE)


#---------------------------------------------------------
## Reporting zones
#---------------------------------------------------------
# OHI reporting regions are often comprised of multiple saup regions.
# The proportional area of each saup region within each OHI reporting region
# is used to weight the calculated status within each OHI reporting region.

rgn_eez_v2013a <- read.csv("raw\\rgn_2013master.csv")# upload complete list of 2013 regions WITHOUT duplicates (Note: piped lists for some of the region and eez names)  
rgn_eez_v2013a <- rename(rgn_eez_v2013a, c(rgn_nam_2013 = "rgn_nm_2013"))
rgn_eez_v2013a <- rgn_eez_v2013a[rgn_eez_v2013a$rgn_id_2013 < 255,] # exclude disputed regions and open oceans
rgn_eez_v2013a <- rgn_eez_v2013a[rgn_eez_v2013a$rgn_id_2013 != 213,] #exclude Antarctica


saup_v2013a <- read.csv("raw\\saup_eez_v2013a.csv") # saup to ohi 2013 conversion list
saup_v2013a <- rename(saup_v2013a, c(OHI_2013_EEZ_ID="rgn_id_2013", 
                                     OHI_2013_reporting_region="rgn_nm_2013",
                                     SAUP_C_NUM="saup_id")) # same names for join columns
saup_v2013a <- saup_v2013a[!is.na(saup_v2013a$km2), ]

combined_v2013a <- join(subset(rgn_eez_v2013a, select=c("rgn_id_2013", "rgn_nm_2013")),
                        subset(saup_v2013a, select=c(-rgn_nm_2013))) # this is a complete list of ohi 2013a regions

combined_v2013a <- rename(combined_v2013a, c(SAUP_C_NAME="saup_nm", km2="saup_km2"))
combined_v2013a <- subset(combined_v2013a, select=c(rgn_id_2013, rgn_nm_2013, saup_id, saup_nm, saup_km2))
Tot_KmData <- ddply(.data = combined_v2013a, .(rgn_id_2013), summarize, total_km2_rgn = sum(saup_km2)) 
combined_v2013a <- merge(combined_v2013a, Tot_KmData)

# calculate relative area to be used as weight
combined_v2013a$propArea <- combined_v2013a$saup_km2/combined_v2013a$total_km2
combined_v2013a <- subset(combined_v2013a, select=c("saup_id",
                                                    "rgn_id_2013", 
                                                    "propArea"))

write.csv(combined_v2013a, "data\\snk_fis_propArea_saup2rgn.csv", row.names=FALSE)


# ------------------------------------------------------------------------
# Make a fake layers data set to get things going 
# ------------------------------------------------------------------------
fileName <- "cnk_fis_meancatch"
CatchSpecies <- read.csv(paste("data\\", fileName, ".csv", sep=""))
head(CatchSpecies)
names(CatchSpecies) <- c("id_num", "category", "year", "value_num")
CatchSpecies_lyr <- data.frame(layer=fileName,
                               id_num=CatchSpecies$id_num,
                               id_chr=NA,
                               category=CatchSpecies$category,
                               year=CatchSpecies$year,
                               value_num=CatchSpecies$value_num,
                               value_chr=NA)
CatchSpecies_lyr[,1:5] <- apply(CatchSpecies_lyr[,1:5], 2, function(x)as.character(x))

##########################################################################
### SpeciesStatus data
### Yearly stock assessments (B/BMSY) of taxa identified to species were obtained 
### using the catch data at the FAO scale.  
fileName <- "fnk_fis_b_bmsy"
bmsy <- read.csv(paste("data\\", fileName, ".csv", sep=""))
head(bmsy)
names(bmsy) <- c("id_num", "category", "year", "value_num")
bmsy_lyr <- data.frame(layer=fileName,
                       id_num=bmsy$id_num,
                       id_chr=NA,
                       category=bmsy$category,
                       year=bmsy$year,
                       value_num=bmsy$value_num,
                       value_chr=NA)

bmsy_lyr[,1:5] <- apply(bmsy_lyr[,1:5], 2, function(x)as.character(x))

#############################################################
### saup2ohi
### Proportion of area of each of the saups comprising each 
### OHI reporting region.  These data were used to convert 
### the status calculated at the saup scale to OHI reporting scale
fileName <- "snk_fis_propArea_saup2rgn"
saup2ohi <- read.csv(paste("data\\", fileName, ".csv", sep=""))
head(saup2ohi)
names(saup2ohi) <- c("id_num", "category", "value_num")
saup2ohi_lyr <- data.frame(layer=fileName,
                           id_num=saup2ohi$id_num,
                           id_chr=NA,
                           category=saup2ohi$category,
                           year=NA,
                           value_num=saup2ohi$value_num,
                           value_chr=NA)
saup2ohi_lyr[,1:5] <- apply(saup2ohi_lyr[,1:5], 2, function(x)as.character(x))


layers_data <- rbind(CatchSpecies_lyr, bmsy_lyr, saup2ohi_lyr)
layers_data$value_num <- as.numeric(layers_data$value_num)
write.csv(layers_data, "data\\layers_data.csv", row.names=FALSE)
