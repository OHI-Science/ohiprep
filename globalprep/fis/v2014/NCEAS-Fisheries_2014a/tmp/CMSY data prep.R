#####################################################################################
########### Prepare SAUP data for CMSY model runs --- July 21-2014 ######
### step.1. load file and rename variables (fields must be: id  stock_id  res  ct  yr)
### step 2. join resilience and life history parameters from old SAUP data file
### step 3. add a unique id code for each stock-FAO area pair
### step 4. select only time-series with at least 10 years of data
#####################################################################################

source('src/R/common.R')
## Step 1. ##
# load the new catch data
dir_FIS_data = 'git-annex/Global/SAUP-Fisheries_v2011/raw'
file_1<- 'Extended catch data 1950-2011_18 July 2014.txt'
newSAUP <- read.delim(file.path(dir_neptune_data, dir_FIS_data, file_1 )) ; head(newSAUP)

# load species names lookup table
dir_lc_FIS = 'Global/NCEAS-Fisheries_2014a/tmp'
file_2<- 'TaxonLookup.csv'
tax <- read.csv(file.path(dir_lc_FIS, file_2 )) ; head(tax)
newSAUP<-left_join(newSAUP,tax[,1:2]) # add species names # Joining by: "Taxonkey"
# create a unique stock id name for every species/FAO region pair
newSAUP$stock_id <- paste(newSAUP$TaxonName,newSAUP$FAO,sep='_') #newSAUP is catch data for weighted means!

#Following gets species data prepped for CMSY
spSAUP <- newSAUP[newSAUP$Taxonkey>600000,] # remove non-species level taxa
names(spSAUP)[names(spSAUP)=="IYear"]<-"yr" # rename variable

# load old SAUP data to join the resilience traits
file_3<-'Extension_redo_withFlag.csv'
firstSAUP <-read.csv(file.path(dir_neptune_data,dir_FIS_data,file_3))  ; head(firstSAUP)
old_tax<-unique(firstSAUP[,c(3,5,9,10)]) ; head(old_tax)
old_tax<-old_tax[old_tax$LH!="",]

# the new SAUP data has no longer duplicate ids in TaxonKeys or TaxonNames ('Clupea harengus' corresponds to taxonkey 600024 when it's Atlantic herring, and 'Clupea harengus membras' is taxonkey 648134, Baltic herring)
# but the old SAUP data with the resilience values still has that issue, so need to join by Taxonkey not TaxonName to ensure both Clupea spp get their resil value

## Step 2. ##
nS2<-left_join(spSAUP,old_tax) # Joining by: c("FAO", "Taxonkey")
nS3<-summarise(group_by(nS2,stock_id,yr,Resilience),ct=sum(Catch, na.rm = TRUE)) # total catch per year per stock
names(nS3)[names(nS3)=='Resilience']<-"res" ; head(nS3)

## Step 3. ##
# generate a unique numeric id per taxon
alls<-unique(nS3$stock_id)
alls<-as.data.frame(cbind('stock_id'=alls,'id'=1:length(alls)))
nS3<-left_join(nS3,alls) # Joining by: "stock_id"

# subset columns
nS4<-select(nS3,id,stock_id,res,ct, yr) 

## step 4. ##
nS4<-nS4[!is.na(nS4$ct),] # remove NAs
nS5<-filter(group_by(nS4,stock_id),length(yr)>=10) %>% # remove time-series <10 years 
  arrange(stock_id,yr)
# any duplicates?
nrow(nS5[duplicated(nS5),])==0
# write.csv(nS5,'OHICatchHistoryCMSY_07_21_2014.csv')

################################################################
###### alternate version padded with 0s after first year of non-0 catch #################
## step 5. ## pad with 0s
min_yr<-ddply(nS5, .(stock_id),summarize,min_year=min(yr[!is.na(ct)]))
# create a full matrix of years for each stock
yrs<-as.data.frame(cbind("yr"=1950:2011,'join'=rep(1,times=2011-1950+1)))
stockss<-as.data.frame(cbind('stock_id'=unique(nS5$stock_id),'join'=rep(1,times=length(unique(nS5$stock_id)))))
yr_stck<-join(yrs,stockss) # Joining by: "join"
nS6<-join(yr_stck[,c(1,3)],nS5) # Joining by: yr, stock_id

nS6<-join(nS6,min_yr) # Joining by: stock_id
nS6$ct<-ifelse(is.na(nS6$ct),ifelse(nS6$yr<nS6$min_year,NA,0),nS6$ct)
nS6<-nS6[!is.na(nS6$ct),]

# re-create the resilience field
resil<-unique(nS2[,c(9,11)])
resil <- rename(resil, c("Resilience" = "res"))
nS6<-join(nS6[c(1:3,5:6)], resil) 

# re-create the id field
nS7<-join(nS6[,c(1:2,4:6)],alls)
nS7<-nS7[,c(6,2,5,3,1)]

nrow(nS7[duplicated(nS7),])==0

# write.csv(nS7,'OHICatchHistoryCMSY_added0s_07_21_2014.csv')
# check differences between the two time-series
# test<-nS5 %>% group_by(stock_id) %>% summarise (mean_ct_no_0=mean(ct, na.rm=T),n_yrs_no_0=n())
# test2<-nS7 %>% group_by(stock_id) %>% summarise (mean_ct_w_0=mean(ct, na.rm=T),n_yrs_w_0=n())
# write.csv(test3,file.path(dir_lc_FIS,'cfr_adding0toCatch.csv'),row.names=F)
