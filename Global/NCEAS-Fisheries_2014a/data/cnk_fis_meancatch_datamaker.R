##############################################################################
############### prep SAUP data to provide weights for FIS calculation ########
##############################################################################
## modified from FIS_dataPrep.R created by Mel##
# setup
library(gdata)
library(stringr)
library(ohicore)
library(plyr)
library(dplyr)
library(tidyr)

source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)

dir_d = '../ohiprep/Global/NCEAS-Fisheries_2014a' # set folder where files are saved

## Step 1. ## get files sourcing CMSY script
source('Global/NCEAS-Fisheries_2014a/tmp/CMSY data prep.R') # upload directory names and upload SAUP data in object newSAUP

########################################################################
## Step 1. ## get files ex novo
# load the new catch data
# dir_FIS_data = 'git-annex/Global/SAUP-Fisheries_v2011/raw'
# /Volumes/data_edit/git-annex/Global/SAUP-Fisheries_v2011/raw/Extended catch data 1950-2011_Common name_25 July2014.txt
# file_1<- 'Extended catch data 1950-2011_18 July 2014.txt'
# nS <- read.delim(file.path(dir_neptune_data, dir_FIS_data, file_1 )) ; head(nS)
# 
# # load species names lookup table - it is important that this be the same as CMSY data prep
# file_2<- 'TaxonLookup.csv'
# tax <- read.csv(file.path(dir_d, 'tmp', file_2 )) ; head(tax)
# nS<-left_join(nS,tax[,1:2]) ; head(nS) # add species names # Joining by: "Taxonkey" # if I want to set it up to source 'CMSY data prep.R' I'll ahve to change this variable name
# 
# # create a unique stock id name for every species/FAO region pair
# nS$stock_id <- paste(nS$TaxonName,nS$FAO,sep='_')
########################################################################

## Step 2. ## rearrange the dataset
nS.eez <- newSAUP[newSAUP$EEZ != 0,] # select eez data

names(nS.eez)[names(nS.eez)=="IYear"]<-"year" # rename variable
nS.eez<-nS.eez[nS.eez$year>=1980,] ; dim(nS.eez) # only use data from 1980 or later to obtain average catch
nS.eez<-nS.eez[nS.eez$Catch!=0,] ; dim(nS.eez) # remove taxa with no catch since 1980

# ISSUE1: need to sum catch to remove duplicate 'Marine fishes not identified', 'Shrimps and prawns', 'Sharks rays and chimaeras' for the same year, saup_id
# multiple taxonkeys (but unique common.names!) are associated with each of these 3 taxonnames - 
# the data was prepared using taxonName, Taxonkey was joined later, then TaxonName was removed)
# ISSUE2: need to keep 'TLevel' as a grouping variable in case part of the catch assigned to a taxon should be given a new taxonkey (see step 3)
nS.eez<-nS.eez %>% group_by(year, stock_id_eez, EEZ, FAO, stock_id, Taxonkey, TaxonName, TLevel) %>% summarise (Catch2=sum(Catch))
# dim(nS2[is.na(nS2$Catch2),]) # check how many NAs added : 482283
names(nS2)[names(nS2)=='Catch2']<-'Catch'

## Step 3 # Recode TaxonKey such that the FAO TaxonKey takes precedence over SAUP TaxonKey
# to give credit to those who report at higher level than was ultimately reported in the SAUP data. 
country.level.data <- nS.eez

country.level.data$NewTaxonKey <- ifelse(is.na(country.level.data$TLevel),
                                         country.level.data$Taxonkey,
                                         100000*country.level.data$TLevel)

country.level.data$taxon_name_key <- paste(country.level.data$TaxonName, 
                                           as.character(country.level.data$NewTaxonKey), sep="_")
country.level.data <- rename(country.level.data, c(EEZ="saup_id", FAO="fao_id"))

# some eezs overlap >1 fao region so I need a stock identifier that cobines: taxonkey, fao_id, saup_id
country.level.data$taxon_name_key_id<-paste(country.level.data$taxon_name_key,country.level.data$fao_id,country.level.data$saup_id,sep='_')

# Step 4 ## years of missing data should be treated as 0 catch, this affects the average!
# create a full matrix of years for each stock and pad missing records with 0s
yrs<-as.data.frame(cbind("year"=1980:2011,'join'=rep(1,times=2011-1980+1)))
stockss<-as.data.frame(cbind('taxon_name_key_id'=unique(country.level.data$taxon_name_key_id),'join'=rep(1,times=length(unique(country.level.data$taxon_name_key_id)))))
yr_stck<-join(yrs,stockss)# Joining by: "join"

nS2<-join(yr_stck[,c(1,3)],country.level.data) ; head(nS2) # Joining by: year, taxon_name_key_id

min_yr<-ddply(nS2, .(taxon_name_key_id),summarise,min_year=min(year[!is.na(Catch)]))
nS3<-join(nS2,min_yr) # Joining by: taxon_name_key_id
nS3$Catch<-ifelse(is.na(nS3$Catch),ifelse(nS3$year<nS3$min_year,NA,0),nS3$Catch) # pad with 0s
nS3<-nS3[!is.na(nS3$Catch),] # rm NAs (the rows before first catch aren't necessary anymore)

# Calculate mean catch over all years per taxon and saup_id/fao_id (remember: due to new taxonkey, some stock_ids correspond to multiple taxa)
MeanCatch <- ddply(country.level.data, .(saup_id,fao_id, stock_id, taxon_name_key), # equivalent to .(stock_id, taxon_name_key_id)
                   summarise, mean_catch=mean(Catch), .progress = "text")

country.level.data<-join(country.level.data, MeanCatch, by=c("saup_id","fao_id","stock_id","taxon_name_key"))
head(country.level.data)

# remove mean catch == 0
country.level.data <- country.level.data[country.level.data$mean_catch != 0, ]
country.level.data <- country.level.data[country.level.data$year>2005,]

 country.level.data$fao_saup_id <- paste(country.level.data$fao_id, 
                                       country.level.data$saup_id, sep="_")


cnk_fis_meancatch <- subset(country.level.data, select=c("fao_saup_id", "taxon_name_key", 
                                                         "year", "mean_catch")) 

# same process for high seas
# HS.data <- nS[nS$EEZ == 0,] # select eez data
# MeanCatch should be by fao_id isntead of saup_id

# are there duplicate stocks ids per taxon-year-region?
stopifnot( sum(duplicated(cnk_fis_meancatch[,c('fao_saup_id', 'taxon_name_key', 'year')])) == 0 )


