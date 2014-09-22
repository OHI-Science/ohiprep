##############################################################################
############### prep SAUP data to provide weights for FIS calculation ########
##############################################################################

# setup
library(gdata)
library(stringr)

source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)

dir_d = '../ohiprep/Global/NCEAS-Fisheries_2014a' # set folder where files are saved
data = file.path(dir_neptune_data, "model/GL-NCEAS-FIS_2014a")
#######################
## Mean catch ----
######################

# source(file.path(dir_d,'tmp/CMSY data prep.R'))  
## Step 1. ## get files ex novo
# load the new catch data
dir_FIS_data = 'git-annex/Global/SAUP-Fisheries_v2011/raw'
file_1<- 'Extended catch data 1950-2011_18 July 2014.txt'
nS <- read.delim(file.path(dir_neptune_data, dir_FIS_data, file_1 )) ; head(nS)

# Change a few of the Saup region ID's (these were updated from our previous saup2ohiRegion and caused a bit of weirdness: 
# email chain on 9/9/2014)
nS$EEZ[nS$EEZ %in% c(277, 278)] <- 276
nS$EEZ[nS$EEZ %in% c(197, 198)] <- 196


# load species names lookup table - it is important that this be the same as CMSY data prep
file_2<- 'TaxonLookup.csv'
tax <- read.csv(file.path(dir_d, 'tmp', file_2 )) ; head(tax)

newSAUP <- nS %>%
  left_join(tax[,1:2]) %>% # add species names # Joining by: "Taxonkey"
  mutate(stock_id = paste(TaxonName, FAO, sep="_")) # create a unique stock id name for every species/FAO region pair
#newSAUP is catch data for weighted means!


## Step 2. ## select eez data (EEZ != 0) and year>=1980 for averages
## and correct: 
# ISSUE1: need to sum catch to remove duplicate 'Marine fishes not identified', 'Shrimps and prawns', 'Sharks rays and chimaeras' for the same year, saup_id
# multiple taxonkeys (but unique common.names!) are associated with each of these 3 taxonnames - 
# the data was prepared using taxonName, Taxonkey was joined later, then TaxonName was removed)
# ISSUE2: need to keep 'TLevel' as a grouping variable in case part of the catch assigned to a taxon should be given a new taxonkey (see step 3)

nS.eez <- newSAUP %>%
  filter(EEZ != 0,
         IYear >= 1980) %>%
  group_by(IYear, EEZ, FAO, stock_id, Taxonkey, TaxonName, TLevel) %>%
  summarise(Catch=sum(Catch))

names(nS.eez)[names(nS.eez)=="IYear"]<-"year" # rename variable


## Step 3 # Recode TaxonKey such that the FAO TaxonKey takes precedence over SAUP TaxonKey
# to give credit to those who report at higher level than was ultimately reported in the SAUP data.
nS.eez$NewTaxonKey <- ifelse(is.na(nS.eez$TLevel),
                                         nS.eez$Taxonkey,
                                         100000*nS.eez$TLevel)

## some cleaning
nS.eez$taxon_name_key <- paste(nS.eez$TaxonName, 
                                           as.character(nS.eez$NewTaxonKey), sep="_")
nS.eez <- rename(nS.eez, c(EEZ="saup_id", FAO="fao_id"))

# some eezs overlap >1 fao region so I need a stock identifier that cobines: taxonkey, fao_id, saup_id
nS.eez$taxon_name_key_id<-paste(nS.eez$taxon_name_key,nS.eez$fao_id,nS.eez$saup_id,sep='_')

################################################
##OPTION 1: no zeros (option 2 with zeros is below)
# Calculate mean catch over all years per taxon and saup_id/fao_id (remember: due to new taxonkey, some stock_ids correspond to multiple taxa)
MeanCatch <- nS.eez %>%
  group_by(saup_id,fao_id, stock_id, taxon_name_key) %>% # equivalent to .(stock_id, taxon_name_key_id
  summarise(mean_catch=mean(Catch))

nS.eez_no0<-join(nS.eez, MeanCatch, by=c("saup_id","fao_id","stock_id","taxon_name_key"))
head(nS.eez_no0)

# remove mean catch == 0
nS.eez_no0 <- nS.eez_no0[nS.eez_no0$mean_catch != 0, ]
nS.eez_no0 <- nS.eez_no0[nS.eez_no0$year>2005,]

nS.eez_no0$fao_saup_id <- paste(nS.eez_no0$fao_id, 
                            nS.eez_no0$saup_id, sep="_")


cnk_fis_meancatch <- subset(nS.eez_no0, select=c("fao_saup_id", "taxon_name_key", 
                                             "year", "mean_catch")) 

# are there duplicate stocks ids per taxon-year-region?
stopifnot( sum(duplicated(cnk_fis_meancatch[,c('fao_saup_id', 'taxon_name_key', 'year')])) == 0 )

# no duplicates! Proceed to save the file
write.csv(cnk_fis_meancatch, file.path(dir_d, 'data/fnk_fis_meancatch_lyr.csv'), row.names=F, na='')

# #################################################
# ## OPTION 2: adding trailing zeros to replace NA data
# ### comment out the next part if trailing zeros do not need to be added:
# 
# # Step 4 ## years of missing data should be treated as 0 catch, this affects the average!
# # create a full matrix of years for each stock and pad missing records with 0s
# nS.eez <- data.frame(nS.eez)
# nS.eez  <- nS.eez %>%
#   select(year, taxon_name_key_id, Catch)
# 
# yr_stck <- expand.grid(year=1980:2011, taxon_name_key_id=unique(nS.eez$taxon_name_key_id))
# 
# nS2 <- join(yr_stck, nS.eez) ; head(nS2) # Joining by: year, taxon_name_key_id # same comment as above about plyr's join fn
# 
# min_yr <- ddply(nS2, .(taxon_name_key_id), summarise, min_year=min(year[!is.na(Catch)]))
# 
# # pad with zeros:
# nS3 <- left_join(nS2, min_yr, by="taxon_name_key_id") # Joining by: taxon_name_key_id
# nS3$TaxonName  <- sapply(strsplit(as.character(nS3$taxon_name_key_id), "_"), function(x)x[1])
# nS3$taxon_key  <- sapply(strsplit(as.character(nS3$taxon_name_key_id), "_"), function(x)x[2])
# nS3$fao_id  <- sapply(strsplit(as.character(nS3$taxon_name_key_id), "_"), function(x)x[3])
# nS3$saup_id  <- sapply(strsplit(as.character(nS3$taxon_name_key_id), "_"), function(x)x[4])
# 
# 
# nS3$Catch <- ifelse(is.na(nS3$Catch) & nS3$year>= nS3$min_year, 
#                     0, 
#                     nS3$Catch)
# 
# # nS3[nS3$TaxonName=='Seriola' & nS3$fao_id==37,]
# # 
# #  ggplot(subset(nS3, fao_id=='37'), aes(x=year, y=Catch)) +
# #    geom_point() +
# #    geom_line() + 
# #    facet_wrap( ~TaxonName)
# 
# nS3<-nS3[!is.na(nS3$Catch),] ; dim(nS3)# rm NAs (the rows before first catch aren't necessary anymore)
# 
# # Calculate mean catch over all years per taxon and saup_id/fao_id (remember: due to new taxonkey, some stock_ids correspond to multiple taxa)
# MeanCatch <- nS3 %>%
#   group_by(taxon_name_key_id) %>%
#   summarise(mean_catch = mean(Catch))
#   
# nS.eez_zero<-nS3 %>%
#   join(MeanCatch, by=c("taxon_name_key_id")) %>%
#   filter(mean_catch != 0,
#          year>2005) %>%
#   mutate(fao_saup_id = paste(fao_id, saup_id, sep="_")) %>%
#   mutate(taxon_name_key = paste(TaxonName, taxon_key, sep="_")) %>%
#   select(fao_saup_id, taxon_name_key, year, mean_catch) %>%
#   arrange(fao_saup_id, taxon_name_key, year)
#   
# 
# # are there duplicate stocks ids per taxon-year-region?
# stopifnot(sum(duplicated(nS.eez_zero[,c('fao_saup_id', 'taxon_name_key', 'year')])) == 0 )
#        
# # no duplicates! Proceed to save the file
# write.csv(nS.eez_zero,file.path(dir_d,'data/fnk_fis_meancatch_lyr.csv'),row.names=F, na='')
#        
#        
       
###############################################
## B-Bmsy data----
###############################################
# B/bmsy data is now generated using different models 
# depending on the resilience score of region of the stock
# B/bmsy scores are now based on a running 5 year average
# NA's are not replaced with zeros

# resilience scores to select the appropriate b/bmsy 
res <- read.csv("Global/FIS_Bbmsy/stock_resil_06cutoff_ALL.csv")

b_bmsy_uniform <- read.csv("Global/NCEAS-Fisheries_2014a/tmp/fnk_fis_b_bmsy_lyr_uniform_no0_runningMean.csv")
b_bmsy_uniform <- b_bmsy_uniform %>%
  select(stock_id, yr, b_bmsy_uniform=b_bmsy) 

b_bmsy_constrained <- read.csv("Global/NCEAS-Fisheries_2014a/tmp/fnk_fis_b_bmsy_lyr_constrained_no0_runningMean.csv")
b_bmsy_constrained <- b_bmsy_constrained %>%
  select(stock_id, yr, b_bmsy_constrained=b_bmsy)

# # 93 stocks without resilience 
# # some of these in arctic (18) and antarctic (48, 58, 88) don't count, 
# # remaining 45 have poor catch records
# setdiff(res$stock_id, b_bmsy_constrained$stock_id)
# setdiff(b_bmsy_constrained$stock_id, res$stock_id)
# 
# setdiff(res$stock_id, b_bmsy_uniform$stock_id)
# noRes <- data.frame(stock_id = setdiff(b_bmsy_uniform$stock_id, res$stock_id)) 
# noRes$TaxonName  <- sapply(strsplit(as.character(noRes$stock_id), "_"), function(x)x[1])
# noRes$fao_id  <- sapply(strsplit(as.character(noRes$stock_id), "_"), function(x)x[2])
# noRes <- noRes[!(noRes$fao_id %in% c(18, 48, 58, 88)),]
# write.csv(noRes, file.path(dir_d, "missingReslience.csv"), row.names=FALSE)
# 
# newSAUP[newSAUP$stock_id %in% noRes$stock_id[2], ]
# missingRes <- new_bmsy[new_bmsy$stock_id %in% noRes$stock_id,]
# ggplot(missingRes, aes(y=b_bmsy_uniform, x=yr, group=stock_id)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap( ~ stock_id, nrow=7) +
#   theme(strip.text=element_text(size=rel(0.7)))


new_bmsy <- b_bmsy_uniform %>%
  left_join(b_bmsy_constrained, by=c("stock_id", "yr")) %>%
  left_join(res, by="stock_id")

new_bmsy <- new_bmsy %>%
  mutate(b_bmsy = ifelse(unif_prior==1, b_bmsy_uniform, b_bmsy_constrained)) %>%
  mutate(taxon_name = sapply(strsplit(as.character(stock_id), "_"), function(x)x[1])) %>%
  mutate(fao_id = sapply(strsplit(as.character(stock_id), "_"), function(x)x[2])) %>%
  select(fao_id, taxon_name, year=yr, b_bmsy) %>%
  filter(!is.na(b_bmsy))

write.csv(new_bmsy, file.path(dir_d, 'data/fnk_fis_b_bmsy_lyr.csv'), row.names=F, na='')
       

####################################################################
## Saup to FAO conversion (from provious script) ----
## this doesn't need to be updated, but this is the script to do so
####################################################################
library(plyr)
setwd("C:\\Users\\Melanie\\Desktop\\Revising FIS")

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

##################################################################################################
############# MAKE mean catch for High Seas 
############# (the b_bmsy data used for eezs is used asis for high seas as well)
##################################################################################################

# setup
library(gdata)
library(stringr)
library(ohicore)
library(plyr)
library(dplyr)

source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)

dir_d = '../ohiprep/Global/NCEAS-Fisheries_2014a' # set folder where files are saved
data = file.path(dir_neptune_data, "model/GL-NCEAS-FIS_2014a")

## Step 1hs. ## get files sourcing CMSY script
# if newSAUP is already in the workspace, avoid next step as it's a bit time-consuming to regenerate
# source(file.path(dir_d,'tmp/CMSY data prep.R')) # upload directory names and upload SAUP data in object newSAUP

## Step 2hs. ## rearrange the dataset
 nS.hs <- newSAUP[newSAUP$EEZ == 0,] # select eez data
 nS.hs <- rename (nS.hs, c("IYear" = "year")) %>% filter(year>=1980, Catch!=0) ; dim(nS.hs) # rename, cut off pre-1980 data, exclude 0 data

# ISSUE1: need to sum catch to remove duplicate 'Marine fishes not identified', 'Shrimps and prawns', 'Sharks rays and chimaeras' for the same year, saup_id
# multiple taxonkeys (but unique common.names!) are associated with each of these 3 taxonnames - 
# the data was prepared using taxonName, Taxonkey was joined later, then TaxonName was removed)
# ISSUE2: need to keep 'TLevel' as a grouping variable in case part of the catch assigned to a taxon should be given a new taxonkey (see step 3)
 nS.hs<-nS.hs %>% group_by(year, FAO, stock_id, Taxonkey, TaxonName, TLevel) %>% summarise (Catch2=sum(Catch)) ; dim(nS.hs)
# # dim(nS.hs[is.na(nS.hs$Catch),]) # check how many NAs added : 482283
 names(nS.hs)[names(nS.hs)=='Catch2']<-'Catch'
 
 ## Step 3hs. # Recode TaxonKey such that the FAO TaxonKey takes precedence over SAUP TaxonKey
 # to give credit to those who report at higher level than was ultimately reported in the SAUP data. 
 nS.hs$NewTaxonKey <- ifelse(is.na(nS.hs$TLevel),
                                nS.hs$Taxonkey,
                                    100000*nS.hs$TLevel)

nS.hs$taxon_name_key <- paste(nS.hs$TaxonName, as.character(nS.hs$NewTaxonKey), sep="_")
nS.hs <- rename(nS.hs, c(FAO="fao_id") )

# eez data needed a stock identifier combining: taxonkey, fao_id, saup_id - not for hs
nS.hs$taxon_name_key_id <- paste(nS.hs$taxon_name_key , nS.hs$fao_id, sep="_") 

# Step 4hs. ## years of missing data should be treated as 0 catch, this affects the average!
# create a full matrix of years for each stock and pad missing records with 0s
yrs<-as.data.frame(cbind("year"=1980:2011,'join'=rep(1,times=2011-1980+1)))
stockshs<-as.data.frame(cbind('taxon_name_key_id'=unique(nS.hs$taxon_name_key_id),'join'=rep(1,times=length(unique(nS.hs$taxon_name_key_id)))))
yr_stck<-join(yrs,stockshs)# Joining by: "join" # beware: this uses plyr's 'join' instead of dplyr because I want rows for all multiple correspondences between years and ids

hs2 <- join (yr_stck[ ,c(1,3)], nS.hs) ; head(hs2) # Joining by: year, taxon_name_key_id
min_yr <- hs2 %>% group_by(taxon_name_key_id) %>% summarise(min_year = min( year[!is.na(Catch) ]) )
 hs3 <- join(hs2, min_yr) # Joining by: taxon_name_key_id
 hs3$Catch <- ifelse( is.na(hs3$Catch), ifelse(hs3$year<hs3$min_year, NA, 0), hs3$Catch ) ; dim(hs3) # pad with 0s
 hs3 <- hs3[!is.na(hs3$Catch), ] ; dim(nS3) # rm NAs (the rows before first catch aren't necessary anymore)
hs3 <- ungroup(hs3)
# Calculate mean catch over all years per taxon and fao_id
hs.MeanCatch <- hs3 %>% group_by(fao_id, stock_id, taxon_name_key) %>% summarise(mean_catch = mean(Catch))
hs3 <- join(hs3, hs.MeanCatch) ; head(hs3) # Joining by: fao_id, stock_id, taxon_name_key
 # remove mean catch == 0

 hs3 <- filter( hs3, mean_catch != 0, year>2005) %>% ungroup()
 hs3 <- hs3 %>% mutate( fao_saup_id = paste(fao_id, 0, sep="_") ) # the saup_id is always 0, because we're dealing with high seas

hs_cnk_fis_meancatch <- select(hs3, fao_saup_id, taxon_name_key, year, mean_catch)

 # are there duplicate stocks ids per taxon-year-region?
anyDuplicated(hs_cnk_fis_meancatch)
anyDuplicated(hs_cnk_fis_meancatch[,1:3])
 
 # no duplicates! Proceed to save the file

# write.csv(hs_cnk_fis_meancatch, file.path(dir_neptune_data, 'model/GL-HS-AQ-Fisheries_v2013/HighSeas','fnk_fis_meancatch_lyr.csv'), row.names=F)
# tried to save to Neptun high seas folder but have insufficient permissions - saving on github instead

write.csv(hs_cnk_fis_meancatch, '../ohiprep/HighSeas/GL_HS_FIS/data/fnk_fis_meancatch_lyr.csv', row.names=F)
