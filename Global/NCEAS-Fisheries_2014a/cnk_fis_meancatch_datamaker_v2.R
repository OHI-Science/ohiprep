head(##############################################################################
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
            filter (TL == 6, yr>=1980) %>%
# alternate version excl Antarctica:             filter (TL == 6, yr>=1980, !FAO %in% c(48, 58, 88)) %>% # exclude data prior to 1980 and belongin to Antarctic regions
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


####################################################################################################################
# # Recode TaxonKey such that the FAO TaxonKey takes precedence over SAUP TaxonKey
# # to give credit to those who report at higher level than was ultimately reported in the SAUP data. 
# 
# newSAUP3$NewTaxonKey <- ifelse(is.na(newSAUP3$TLevel),
#                                newSAUP3$Taxonkey,
#                               100000*newSAUP3$TLevel)
# 
# newSAUP3$taxon_name_key <- paste( newSAUP3$TaxonName, as.character(newSAUP3$NewTaxonKey), sep = "_" )
# newSAUP3$taxon_name_key_id <- paste( newSAUP3$taxon_name_key, newSAUP3$fao_id, newSAUP3$saup_id, sep = '_' )
# 
# # check whether there are multiple stock_ids for the same combination of Taxonkey and saup_id, due to the NewTaxonKey
# check <- unique(newSAUP3[,c('stock_id','saup_id','NewTaxonKey')])
# check[duplicated(check[,1:2]),] # there are 13 cases of duplicate records that have two NewTaxonKey fot the same stock_id and saup_id
####################################################################################################################

## Step 3 ## get resilience scores per EEZ and per major fishing area, calculate a weighted mean score (relative catch from newSAUP4 as weights)
# where saup_id != 0, simply join the EEZ resil scores, 

dir_r = '../ohiprep/Global/GL-Resilience_Fish_Hab_2014a' # set folder where files are saved
file_r <- 'r_fishing_v2_eez_2013a_wMontenegro.csv' # pick the file for OHI2013
eez_r <- read.csv(file.path(dir_r, 'data', file_r), stringsAsFactors = F)  
# EEZlookup to match ohi regions to saup ids
saup_ohi <- read.csv(file.path(dir_d, 'tmp', 'EEZlookup.csv'), stringsAsFactors = F)
saup_ohi <- saup_ohi %>% rename ( c('SAUP_C_NUM' = 'saup_id', 'OHI_2013_EEZ_ID' = 'rgn_id') ) %>% select (saup_id, rgn_id)

# join saup_ohi to assign a resilience score to each saup id and thus to each protion of stock_id within EEZ boundaries
eez_r <- left_join(eez_r, saup_ohi)  # Joining by: c("rgn_id", "saup_id")

# get mean catch throughout the time-series (1980-2011) per stock and use it to calculate relative catch for shared stocks 
newSAUP3 <- rename(newSAUP3, c(EEZ = "saup_id", FAO = "fao_id") )
newSAUP4 <- newSAUP3 %>% ungroup() %>% group_by(stock_id, fao_id, saup_id) %>% summarise(av_ct = mean(Catch)) %>% mutate (rel_ct = av_ct/sum(av_ct) ) %>% arrange (fao_id, saup_id, stock_id)
newSAUP4 <- newSAUP4 %>% filter( saup_id != 274 )  # remove Gaza strip

# add resilience
newSAUP5 <- left_join( newSAUP4, eez_r) # Joining by: "saup_id"
r_eez <- newSAUP5 %>% filter( saup_id >0 ) %>% mutate(  # DO NOT exclude the Arctic
                        relarea = 1, rfmo = 0) %>% rename(
                          c('resilience.score' = 'Score')) %>% select(
                         fao_id, saup_id, rfmo, relarea, stock_id, Score)  # don't include relative catch for now

### get the HIGH SEAS resilience scores (saup_id ==0)
# get the list of scores per rfmo
dir_r2 = 'model/GL-HS-Resilience/RFMO'
hs_res <- read.csv(file.path(dir_neptune_data, dir_r2, 'data/rfmo_2013.csv'), stringsAsFactors = F)

# get the list of stocks for each rfmo: rfmo_id, TaxonName
rfmo_sp <- read.csv('../ohiprep/HighSeas/tmp/rfmo_species.csv', stringsAsFactors = F, check.names = F)
rfmo_sp <- unique(rfmo_sp[,3:4]) # remove duplicates and retain only the TaxonName

# get the rfmo score and join: rfmo_id, TaxonName, rfmo score
rfmo_sc <- read.csv(file.path(dir_neptune_data, dir_r2, 'raw/RFMOscores.csv'), stringsAsFactors = F)
# remove capital letters from rfmo names and rows with NAs
rfmo_sc <- rfmo_sc %>% mutate (rfmo = tolower(rfmo_sc$RFMO)) %>% select (rfmo, Score) %>% filter (!is.na(Score))

# get the rfmo to fao id lookup table: fao_id, rfmo_id, prop rfmo area in fao_id
rfmo_fao <- read.csv(file.path(dir_neptune_data, dir_r2, 'tmp/RFMOperFAO.csv'), stringsAsFactors = F)

# need table for fao_id to fao_name
fao_id_rgn <- read.csv('../ohiprep/tmp/install_local/ohi-global-master/highseas2014/layers/FAOregions.csv', stringsAsFactors = F)

# join this to the rfmo table: fao_id, rfmo_id, prop rfmo area in fao_id,  TaxonName, rfmo score
rfmo_fao <- left_join(rfmo_fao, fao_id_rgn) ; rfmo_fao # Joining by: "rgn_id" 
library(tidyr)

r_hs <- gather(rfmo_fao, key = rfmo, value = proparea, -rgn_id, -rgn_name, -total, -fao_id)
r_hs$rfmo <- as.character(r_hs$rfmo)

Ant_rgn <- data.frame( cbind ('rgn_id' = as.numeric(c(268, 271, 278)), 'rgn_name' = c('Antarctic', 'Antarctic', 'Antarctic'), 'total' = as.numeric(c(1, 1, 1)), 'fao_id' = as.numeric(c(48, 58, 88)), 'rfmo' = c('ccamlr', 'ccamlr', 'ccamlr'), 'proparea' = as.numeric(c(1, 1, 1)) ), stringsAsFactors = F)
Ant_rgn$proparea <- as.numeric(Ant_rgn$proparea)
Ant_rgn$total <- as.numeric(Ant_rgn$total)
Ant_rgn$fao_id <- as.numeric(Ant_rgn$fao_id)
r_hs <- rbind(Ant_rgn, r_hs)

r_hs <- r_hs %>% mutate( relarea = proparea/total) ; head(r_hs) # relarea will be NaN if the RFMO doesn't overlap the FAO region
# add in rfmo resilience scores 
r_hs <- left_join (r_hs, rfmo_sc) # Joining by: "rfmo"
# r_hs <-  filter( r_hs, fao_id!=18 ) # remove the Arctic

# join this with the species list per rfmo
r_hs2 <- left_join (r_hs, rfmo_sp) # Joining by: "rfmo"
r_hs2 <- r_hs2 %>% mutate (stock_id = paste( TaxonName, fao_id, sep = '_'), saup_id = 0) # make the stock_id and add the saup_id
r_hs2 <- r_hs2 %>% ungroup() %>% select(fao_id, saup_id, rfmo, relarea, stock_id, Score)

# rbind high seas with eez resilience scores
r_c <- rbind(r_hs2, r_eez) # unique(r_c$saup_id[is.na(r_c$Score)])  # check

# join relative catch by fao+saup+stock_id
newSAUP4$saup_id <- as.numeric( newSAUP4$saup_id )
newSAUP4$fao_id <- as.numeric( newSAUP4$fao_id )

# generate a lookup that has the correct rfmo assigned to each saup id (non to eezs, but all 14 to high seas, where saup_id=0)
rfmo_lookup <- unique(r_c[,1:4])
newSAUP4 <- left_join(newSAUP4, rfmo_lookup) 
r_all <- left_join (newSAUP4, r_c) %>% select (
                                      fao_id, saup_id, rfmo, relarea, stock_id, rel_ct, Score)
# %>% filter (
#                                       fao_id != 18)

# calculate the relative resilience score per stock, multiplying the res score by the rel prop of catch and rel rfmo area
# when an rfmo area has area for that stock (relarea>0) but Score is na, then relarea should be recalculated excluding that rfmo
r_all <- r_all %>% ungroup() %>% group_by (stock_id, saup_id) %>% mutate ( 
                                            new.relarea = ifelse( is.na(Score), 0, ifelse( relarea == 0, 0,
                                                        relarea/sum( relarea[which( !is.na(Score) ) ], na.rm =T ) 
                                                                                )
                                                                               ) 
                                                                              )
res_scores <- r_all %>% mutate ( 
                          part_score = rel_ct * new.relarea * Score ) %>% group_by (
                        stock_id) %>% summarise (
                          final_score = sum(part_score, na.rm =T)) %>% mutate (
                            unif_prior = ifelse( final_score > 0.6, 1, 0)
                            )


# the score for high seas is based only on management bodies present for specific species
# r_all[r_all$stock_id == 'Katsuwonus pelamis_67',] # example showing that the % of rfmo in the area doesn't work: since most of the area is covered by the salmon commission, the tunas appear under managed
# only if the final score is NA does it get turned to 0

write.csv(res_scores, '../ohiprep/Global/FIS_Bbmsy/stock_resil_06cutoff_ALL_v2.csv', row.names=F)

###########################################################################################################



# step 5 ## pick a few stocks and plot catch, 2 cmsy versions, highlighting which one would be picked based on resilience


trouble <- res_scores %>% mutate ( fao_id = str_split_fixed( stock_id, '_',2) [,2] ) %>% filter (fao_id %in% c(34, 51, 71) )
trouble <- left_join( trouble, fao_id_nm )

library(ggplot2)
ch <- ggplot(trouble, aes(x=final_score, fill = rgn_name)) + geom_histogram(binwidth=0.1,colour="white") 
ch1 <- ch + facet_wrap( ~ rgn_name) 
ch2 <- ch1 + geom_vline(data = trouble, aes(xintercept= 1), colour='blue', linetype="dashed", size=1)
ch2

trouble2 <- r_c 
#%>% filter (fao_id %in% c(34, 51, 71) )
trouble2 <- left_join(trouble2, saup_ohi)
ohi_rg_nm <- ohi_rg[,3:4] %>% rename(c('rgn_id_2013' = 'rgn_id'))
trouble2 <- left_join(trouble2, ohi_rg_nm) %>% mutate (type = ifelse (saup_id==0, 'hs', 'eez') )
 trouble2 <- left_join (trouble2, fao_id_nm)

# trouble3 <- trouble2 %>% group_by(fao_id, rgn_nam_2013) %>% summarise (med_score = median(Score))
# subset <- trouble2[trouble2$fao_id==34,]
ch <- ggplot(trouble2, aes(x=Score, fill = fao_id)) + geom_histogram(binwidth=0.1,colour="white") 
ch1 <- ch  + facet_grid( type  ~ fao_id)
ch2 <- ch1 + geom_vline(data = trouble2, aes(xintercept= 0.6), colour='blue', linetype="dashed", size=1)
ch2

ch <- ggplot(cmsy11.all.v, aes(x=b_bmsy, fill = rgn_name)) + geom_histogram(binwidth=0.1,colour="black") 
ch1 <- ch + facet_grid(cutoff ~ rgn_name) 
ch2 <- ch1 + geom_vline(data = cmsy11.all.v, aes(xintercept= 1), colour='blue', linetype="dashed", size=1)
ch2