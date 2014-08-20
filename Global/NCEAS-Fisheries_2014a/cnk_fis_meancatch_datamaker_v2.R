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

source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)

dir_d = '../ohiprep/Global/NCEAS-Fisheries_2014a' # set folder where files are saved
data = file.path(dir_neptune_data, "model/GL-NCEAS-FIS_2014a")

## Step 1. ## upload directory names and upload SAUP data in object newSAUP (same as first 10 or so rows in CMSY script, could source that but it takes a while)
# source(file.path(dir_d,'tmp/CMSY data prep.R'))  
########################################################################
## Step 1. ## get files ex novo
# load the new catch data
dir_FIS_data = 'git-annex/Global/SAUP-Fisheries_v2011/raw'
file_1<- 'Extended catch data 1950-2011_18 July 2014.txt'
newSAUP <- read.delim(file.path(dir_neptune_data, dir_FIS_data, file_1 )) ; head(newSAUP)

# load species names lookup table - it is important that this be the same as CMSY data prep
file_2<- 'TaxonLookup.csv'
tax <- read.csv(file.path(dir_d, 'tmp', file_2 )) ; head(tax)
newSAUP<-left_join(newSAUP,tax[,1:2]) # add species names # Joining by: "Taxonkey"
# create a unique stock id name for every species/FAO region pair
newSAUP$stock_id <- paste(newSAUP$TaxonName,newSAUP$FAO,sep='_') #newSAUP is catch data for weighted means!
newSAUP <- rename(newSAUP, c('IYear' = 'yr') )

## Step 2. ## filter out unused years, regions, taxa, stocks, and get lists of stocks that are unique or shared among EEZs and HSs
newSAUP2 <- newSAUP %>% mutate (TL = substr(Taxonkey, 1, 1) ) %>% 
            filter (TL == 6, yr>=1980, !FAO %in% c(48, 58, 88)) %>% # exclude data prior to 1980 and belongin to Antarctic regions
            group_by (EEZ, FAO, yr, Taxonkey, CHANGE, TLevel, TaxonName, stock_id) %>%
            summarise (Catch = sum(Catch)) # remove duplicate 'Marine fishes not identified', 'Shrimps and prawns', 'Sharks rays and chimaeras' for the same year, saup_id
# sum catch per stock by major fishing area and by year, then get the list of stocks with at least 10 years of non-0 data
newSt <- newSAUP2 %>% group_by (FAO, stock_id, yr) %>% 
                    summarise (t_Catch = sum(Catch)) %>% 
                    summarise (N = length( t_Catch[t_Catch>0 ] )) %>%
                    filter (N > 9) # at least 10 non-0 data points
# use the list of stocks 
newSAUP3 <- left_join(newSt, newSAUP2) # Joining by: c("FAO", "stock_id") # excludes 6003 records
eez_stocks <- newSAUP3 %>% filter (EEZ != 0)
eez_stocks <- unique( eez_stocks$stock_id ) 
# length(eez_stocks) # 1840
hs_stocks <- newSAUP3 %>% filter (EEZ == 0) 
hs_stocks <- unique( hs_stocks$stock_id )
# length(hs_stocks) # 672
hs_alone <- hs_stocks [ !hs_stocks %in% eez_stocks ] 
# length(hs_alone) # 2
eez_alone <- eez_stocks [!eez_stocks %in% hs_stocks ]  
# length(eez_alone) # 1170
shared <- hs_stocks[hs_stocks %in% eez_stocks] 
# length(shared) #  670

####################################################################
# # exploring the number of overlapping stocks before and after applying: 1) the year filter, 2) the minimum 10 datapoints filter
# # newSAUP <- rename(newSAUP, c('IYear' = 'yr') )
# # test <- newSAUP %>% mutate (TL = substr(Taxonkey, 1, 1) ) %>% 
# #   filter (TL == 6, !FAO %in% c(48, 58, 88))
#
# eez_stocks <- newSAUP %>% filter (EEZ != 0)
# eez_stocks <- unique( eez_stocks$stock_id ) 
# length(eez_stocks) # 1840 (2197) ((2224))
# hs_stocks <- newSAUP %>% filter (EEZ == 0) 
# hs_stocks <- unique( hs_stocks$stock_id )
# length(hs_stocks) # 672 (798) ((813))
# hs_alone <- hs_stocks [ !hs_stocks %in% eez_stocks ] 
# length(hs_alone) # 2 (32) ((30)) 
# # interestingly, when including all years instead of just post 1980, there were 30 instead of 32 stocks only found in high seas 
# # the stocks are: "Istiophorus platypterus_21"    "Gobionotothen gibberifrons_41"
# # saved it using 'test': hs_alone_alltser <- hs_alone 
# # and save it using 'newSAUP' post 1980 but still including short time-series (<10 years): hs_alone_shtser <- hs_alone
# eez_alone <- eez_stocks [!eez_stocks %in% hs_stocks ]  
# # saved it using 'test': eez_alone_alltser <- eez_alone
# # and save it using 'newSAUP' post 1980 but still including short time-series (<10 years): eez_alone_shtser <- eez_alone
# length(eez_alone) # 1170 (1431) ((1441))
# shared <- hs_stocks[hs_stocks %in% eez_stocks] 
# length(shared) #  670 (766) ((783))
# # saved it using 'test': shared_alltser <- shared
# # and save it using 'newSAUP' post 1980 but still including short time-series (<10 years): shared_shtser <- shared
# 
# # turns out all the additional high seas stocks were excluded for having too few data points
# ##################################################################################################################

## Step 3 # get mean catch throughout the time-series (1980-2011) per stock and use it to calculate relative catch for shared stocks 

####################################################################################################################
# # Recode TaxonKey such that the FAO TaxonKey takes precedence over SAUP TaxonKey
# # to give credit to those who report at higher level than was ultimately reported in the SAUP data. 
# 
# newSAUP3$NewTaxonKey <- ifelse(is.na(newSAUP3$TLevel),
#                                newSAUP3$Taxonkey,
#                               100000*newSAUP3$TLevel)
# 
# newSAUP3$taxon_name_key <- paste( newSAUP3$TaxonName, as.character(newSAUP3$NewTaxonKey), sep = "_" )
# newSAUP3 <- rename(newSAUP3, c(EEZ = "saup_id", FAO = "fao_id") )
# newSAUP3$taxon_name_key_id <- paste( newSAUP3$taxon_name_key, newSAUP3$fao_id, newSAUP3$saup_id, sep = '_' )
# 
# # check whether there are multiple stock_ids for the same combination of Taxonkey and saup_id, due to the NewTaxonKey
# check <- unique(newSAUP3[,c('stock_id','saup_id','NewTaxonKey')])
# check[duplicated(check[,1:2]),] # there are 13 cases of duplicate records that have two NewTaxonKey fot the same stock_id and saup_id
####################################################################################################################

newSAUP4 <- newSAUP3 %>% ungroup() %>% group_by(stock_id, fao_id, saup_id) %>% summarise(av_ct = mean(Catch)) %>% mutate (rel_ct = av_ct/sum(av_ct) ) %>% arrange (fao_id, saup_id, stock_id)

# Step 4 ## get resilience scores per EEZ and per major fishing area, calculate a weighted mean score (relative catch from newSAUP4 as weights)
# where saup_id != 0, simply join the EEZ resil scores, 

dir_r = '../ohiprep/Global/GL-Resilience_Fish_Hab_2014a' # set folder where files are saved
file_r <- 'r_fishing_v2_eez_2013a.csv' # pick the file for OHI2013
eez_r <- read.csv(file.path(dir_r, 'data', file_r), stringsAsFactors = F)  
# EEZlookup to match ohi regions to saup ids
saup_ohi <- read.csv(file.path(dir_d, 'tmp', 'EEZlookup.csv'), stringsAsFactors = F)
saup_ohi <- saup_ohi %>% rename ( c('SAUP_C_NUM' = 'saup_id', 'OHI_2013_EEZ_ID' = 'rgn_id') ) %>% select (saup_id, rgn_id)

# join saup_ohi to assigna  resilience score to each saup id and thus to each protion of stock_id within EEZ boundaries
eez_r <- left_join(eez_r, saup_ohi)  # Joining by: c("rgn_id", "saup_id")
newSAUP5 <- left_join( newSAUP4, eez_r) # Joining by: "saup_id"

# two saup regions missing: 274, 891. Region 274 is missing from the EEZlookup (it's the Gaza strip, so correctly is excluded), 
# region 891 is in the EEZlookup,it's OHI rgn 186: Montenegro (ex Serbia and Montenegro), missing from OHI resilience file, why?
# gapfill it?

# I now have a resilience value per saup_id != 0
# multiply the res value by the rel_ct weight, check distr of values, adn teh countries they correspond to

# where saup_id ==0, get a list per fao region of stocks, rfmos, relative rfmo area
# get the list of scores per rfmo
dir_r2 = 'model/GL-HS-Resilience/RFMO'
rfmo <- read.csv(file.path(dir_neptune_data, dir_r2, 'data/rfmo_2013.csv'))

# 

# get the list of stocks for each rfmo: rfmo_id, TaxonName
# a) import each sheet per rfmo, cbind the rfmo name field: /Volumes/data_edit/model/GL-HS-Resilience/RFMO/raw/RFMO Info 4.1.14.xls
# b) rbind all rfmo names and species
# c) check  spp names match SAUP TaxonName

# get the rfmo score and join: rfmo_id, TaxonName, rfmo score
/Volumes/data_edit/model/GL-HS-Resilience/RFMO/raw/RFMOscores.csv

# get the rfmo to fao id lookup table: fao_id, rfmo_id, prop rfmo area in fao_id
/Volumes/data_edit/model/GL-HS-Resilience/RFMO/tmp/RFMOperFAO.csv
# join this to the rfmo table: fao_id, rfmo_id, prop rfmo area in fao_id,  TaxonName, rfmo score

# join this with the relative catch per stock per fao region (where saup id = 0, where saup id !=0, then prop rfmo area shoudl be set =0) 


# calculate the relative resilience score per stock, multiplying the res score by the rel prop of catch and by the rel prop of rfmo (not sure if I can do this in the same df as the eex scores)

# finalize calculation for shared stocks, stocks only in eezs, and stocks only in high seas (2)

# any stocks occurring in high seas should get no 0 resilience for that portion as they are likely unmanaged if not in rfmos

# step 5 ## pick a few stocks and plot catch, 2 cmsy versions, highlighting which one would be picked based on resilience



write.csv(hs_cnk_fis_meancatch, '../ohiprep/HighSeas/GL_HS_FIS/data/fnk_fis_meancatch_lyr.csv', row.names=F)
