##############################################################################
############### prep SAUP data to provide weights for FIS calculation ########
##############################################################################
## modified from FIS_dataPrep.R created by Mel##
# setup
library(gdata)
library(stringr)
library(ohicore)

source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)

dir_d = '../ohiprep/Global/NCEAS-Fisheries_2014a' # set folder where files are saved

## Step 1. ## read in files
# load the new catch data
dir_FIS_data = 'git-annex/Global/SAUP-Fisheries_v2011/raw'
file_1<- 'Extended catch data 1950-2011_18 July 2014.txt'
nS <- read.delim(file.path(dir_neptune_data, dir_FIS_data, file_1 )) ; head(nS)

# load species names lookup table
file_2<- 'TaxonLookup.csv'
tax <- read.csv(file.path(dir_d, 'tmp', file_2 )) ; head(tax)
nS<-left_join(nS,tax[,1:2]) ; head(nS) # add species names # Joining by: "Taxonkey"

# create a unique stock id name for every species/FAO region pair
nS$stock_id <- paste(nS$TaxonName,nS$FAO,sep='_')

## Step 2. ## rearrange the dataset

nS.eez <- nS[nS$EEZ != 0,] # select eez data

names(nS.eez)[names(nS.eez)=="IYear"]<-"year" # rename variable
nS.eez<-nS.eez[nS.eez$year>=1980,] # only use data from 1980 or later to obtain average catch
dim(nS.eez)
nS.eez<-nS.eez[nS.eez$Catch!=0,] # remove taxa with no catch since 1980
dim(nS.eez) # check

# create a full matrix of years for each stock
yrs<-as.data.frame(cbind("year"=1980:2011,'join'=rep(1,times=2011-1980+1)))
stockss<-as.data.frame(cbind('stock_id'=unique(nS.eez$stock_id),'join'=rep(1,times=length(unique(nS.eez$stock_id)))))
yr_stck<-join(yrs,stockss) # Joining by: "join"
nS2<-join(yr_stck[,c(1,3)],nS.eez) ; head(nS2) # Joining by: year, stock_id
# dim(nS2[is.na(nS2$Catch),]) # check how many NAs added

min_yr<-ddply(nS2, .(stock_id),summarize,min_year=min(year[!is.na(Catch)]))
nS3<-join(nS2,min_yr) # Joining by: stock_id
nS3$Catch<-ifelse(is.na(nS3$Catch),ifelse(nS3$year<nS3$min_year,NA,0),nS3$Catch) # pad with 0s
nS3<-nS3[!is.na(nS3$Catch),] # rm NAs (the rows before first catch aren't necessary anymore)

## Step 3 # Recode TaxonKey such that the FAO TaxonKey takes precedence over SAUP TaxonKey
# to give credit to those who report at higher level than was ultimately reported in the SAUP data. 
country.level.data <- nS3

country.level.data$NewTaxonKey <- ifelse(is.na(country.level.data$TLevel),
                                         country.level.data$Taxonkey,
                                         100000*country.level.data$TLevel)

country.level.data$taxon_name_key <- paste(country.level.data$TaxonName, 
                                           as.character(country.level.data$NewTaxonKey), sep="_")
 country.level.data <- rename(country.level.data, c(EEZ="saup_id", FAO="fao_id"))

# Calculate mean catch over all years per taxon and saup_id/fao_id
MeanCatch <- ddply(country.level.data, .(saup_id,fao_id, Taxonkey, TaxonName), 
                   summarize, mean_catch=mean(Catch), .progress = "text")

country.level.data<-join(country.level.data, MeanCatch, by=c("saup_id","fao_id","Taxonkey","TaxonName"))
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




