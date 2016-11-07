##############################################################################
############### calculate governance score for each fisheries stock ##########
##############################################################################
## ReadMe : ##################################################################
## assigns a resilience score to each stock, based on the fisheries governance score
## of EEZs and high seas where the stock is caught, weighted by the relative proportion
## with witch it is caught in each jurisdiction (using mean catch since 1980). 
## For stocks that are exclusively coastal, it is simply based on the governance score of the respective EEZs.
## For stocks overlapping the high seas, it is based on the score of the RFMOs responsible for that HS area. 
## If none of the RFMOs overlapping a stock do not explicitly manage that species, 
## the stock is assumed to be unmanaged and gets a management score of 0.
## The resulting score is used to decide whether to assess that stock with the CMSY model
## that uses uniform priors (managed stock), or constrained priors (unmanaged stock).
## In 2014 the cutoff point for a stock to be considered managed was a score equal or greater than 0.6
##############################################################################
## NOTE: this requires SAUP fisheries data where castal(=eez) and high seas are combined (distinguished by saup_id == 0 for high seas) ##

# ************************ directories to update: ************************
# dir_r = where resilience score files are
# file_r = resil file name 
# saup_ohi = # EEZlookup to match ohi regions to saup ids (file.path(dir_d, 'tmp', 'EEZlookup.csv'))
# dir_r2  = folder containing the high seas resilience scores
# rfmo_sp = location of file assigning species to rfmos
# rfmo_sc = file with rfmo resilience scores
# rfmo_fao = location of rfmo_to_fao correspondence
# fao_id_rgn = lookup for fao ids to fao names
# ************************************************************************
# load('../ohiprep/.RData')
source('../ohiprep/src/R/common.R') # set dir_neptune_data
# library(dplyr) # already loaded by sourcing common.R
library(stringr)
library(tidyr)

########################################################################
## Step 1. ## get files

# load the new SAUP catch data for 2015
dir_FIS_data = 'git-annex/globalprep/SAUP_data_2015/tmp'
file_1 = 'Catch_Value_11062015_summary.csv'
newSAUP = read.csv(file.path(dir_neptune_data, dir_FIS_data, file_1 )) ; head(newSAUP)
save.image()
newSAUP =  newSAUP %>% rename(saup_id = EEZID, fao_id=FAOAreaID, yr=Year, Taxonkey=TaxonKey, tonnes=catch); head(newSAUP)
anyDuplicated(newSAUP[,1:4]) # should be 0

# load species names lookup table - it is important that this be the same as CMSY data prep
dir_d = '../ohiprep/globalprep/Fisheries_stock_gov_score' # where github file 'ohi_taxon.csv' is saved
file_2<- 'ohi_taxon.csv'
tax <- read.csv(file.path(dir_d, 'raw', file_2 ), stringsAsFactors = F) ; head(tax)
newSAUP<-left_join(newSAUP,tax[,1:2], by=c('Taxonkey' = 'tax_code')) # add species names # Joining by: "Taxonkey"
# create a unique stock id name for every species/FAO region pair
newSAUP = newSAUP %>% mutate(stock_id = paste(Taxonkey, fao_id, sep='_')) %>% select(saup_id, fao_id, Taxonkey, stock_id, yr, tonnes); head(newSAUP)

## Step 2. ## filter out early years, (Antarctica,) coarse taxa and count stocks

newSAUP2 = newSAUP %>% mutate (TL = substr(Taxonkey, 1, 1) ) %>% 
  filter (TL == 6, yr>=1980) 
  # alternate version excl Antarctica:  
  # filter (TL == 6, yr>=1980, !FAO %in% c(48, 58, 88)) %>% # exclude data prior to 1980 and belonging to Antarctic regions
anyDuplicated(newSAUP2) # should be 0 
# sum catch per stock by major fishing area and by year, then get the list of stocks with at least 10 years of non-0 data
newSt <- newSAUP2 %>% group_by (fao_id, stock_id, yr) %>% 
  summarise (t_Catch = sum(tonnes)) %>% 
  summarise (N = length( t_Catch[t_Catch>0 ] )) %>%
  # ** check this is not >=9 (ie check ==9 is empty)
  filter (N > 9) %>% 
  ungroup() # at least 10 non-0 data points
length(unique(newSAUP2$stock_id))-length(unique(newSt$stock_id)) # excludes -396 records
# use the filtered list of stocks to select only relevant records in newSAUP2 
newSAUP3 = left_join(newSt, newSAUP2) # Joining by: c("fao_id", "stock_id") 
newSAUP3 = newSAUP3 %>% filter( saup_id != 274 )  # remove Gaza strip

## count and check stocks in EEZs and HS
eez_stocks <- newSAUP3 %>% filter (saup_id != 0)
eez_stocks <- unique( eez_stocks$stock_id ) 
length(eez_stocks) # how many stocks in EEZs: 2692
hs_stocks <- newSAUP3 %>% filter (saup_id == 0) 
hs_stocks <- unique( hs_stocks$stock_id )
length(hs_stocks) # how many stocks in HS: 694
hs_alone <- hs_stocks [ !hs_stocks %in% eez_stocks ] 
length(hs_alone) # how many stocks only in HS: 14
eez_alone <- eez_stocks [!eez_stocks %in% hs_stocks ]  
length(eez_alone) # how many stocks only in EEZs: 2012
# check the stocks in both regions are total stocks - only EEZ - only HS (or: 680+14+2012=2706)
shared <- hs_stocks[hs_stocks %in% eez_stocks] 
length(shared) #  680

## Step 3 ##  check that each time-series is complete (i.e. missing values after the first recorded catch are 0s, not absent)
newSAUP3 = newSAUP3 %>% mutate(stock_saup_id = paste (stock_id, saup_id, sep='_'))
full_stock_yrs = expand.grid(yr=1980:2010, stock_saup_id = unique(newSAUP3$stock_saup_id), stringsAsFactors = F)
newSAUP4 = left_join(full_stock_yrs, newSAUP3) # Joining by: c("yr", "stock_saup_id")
length(unique(newSAUP3$stock_saup_id))*(2010-1980+1) == dim(newSAUP4)[1] # check that true
# get min(year) of tonnes>0 value per stock_saup_id, turn every NA after that to 0
newSAUP4 = newSAUP4 %>% group_by(stock_saup_id) %>% mutate(yr_beg =  min(yr[!is.na(tonnes)]))
newSAUP4  = newSAUP4 %>% mutate(tonnes2 = ifelse(is.na(tonnes) & yr>=yr_beg, 
                                       0, tonnes)) %>% ungroup()

# get mean catch throughout the time-series (1980-2011) per stock 
newSAUP5 <- newSAUP4 %>% group_by(saup_id, fao_id, stock_saup_id) %>% summarise(av_ct = mean(tonnes2, na.rm=T)) %>% mutate (rel_ct = av_ct/sum(av_ct) ) 

## Step 4 ## get resilience scores per EEZ and per major fishing area, calculate a weighted mean score (relative catch from newSAUP5 as weights)
# where saup_id != 0, simply join the EEZ resil scores, 
# where saup_id == 0, the resilience score is the average of the rfmos that are present and in charge for that stock (or 0, if none manage that species)

file_r <- 'r_fishing_v2_eez_2013a_wMontenegro.csv' # fisheries governance by EEZ from OHI2013 Mora scores for fis governance (layer unchanged)
eez_r <- read.csv(file.path(dir_d, 'raw', file_r), stringsAsFactors = F) ; head(eez_r) 

# get lookup to match ohi regions to saup ids, and then match resilience values per eez
saup_ohi <- read.csv(file.path('../ohiprep/src/LookupTables/new_saup_to_rgn_v2.csv'), stringsAsFactors = F) ; head(saup_ohi)
saup_ohi <- saup_ohi %>% rename ( rgn_id = ohi_id_2013)  %>% select (saup_id, rgn_id) ; head(saup_ohi)
rgn_master = read.csv('../ohiprep/src/LookupTables/eez_rgn_2013master.csv',stringsAsFactors = F)
eez_r <- left_join(eez_r, saup_ohi)  # Joining by: c("rgn_id")

# join resilience values to the catch data
newSAUP6 <- left_join( newSAUP5, eez_r) # Joining by: "saup_id"
res_eez <- newSAUP6 %>% filter( saup_id >0 ) %>% mutate(  # DO NOT exclude the Arctic
  relarea = 1, rfmo = 0) %>% rename(
    Score = resilience.score) %>% select(
      fao_id, saup_id, rfmo, relarea, stock_saup_id, Score)  # don't include relative catch calculated in newSAUP5 for now

### get the HIGH SEAS resilience scores (saup_id ==0)
# get the list of scores per rfmo
dir_r2 = '../ohiprep/HighSeas/HS_Resilience_v2014/RFMO'
hs_res <- read.csv(file.path(dir_r2, 'data/rfmo_2014.csv'), stringsAsFactors = F) ; head(hs_res)

###
###
###
# move /rfmo_species.csv RFMOscores.csv

# get the list of stocks for each rfmo: rfmo_id, TaxonName
rfmo_sp <- read.csv('../ohiprep/HighSeas/tmp/rfmo_species.csv', stringsAsFactors = F, check.names = F)
rfmo_sp <- unique(rfmo_sp[,3:4]) # remove duplicates and retain only the TaxonName

# rename some of the taxa that have different names for the same species in the saup data
rfmo_sp$TaxonName[rfmo_sp$TaxonName =="Longfin mako"] = "Isurus paucus" 
rfmo_sp$TaxonName[rfmo_sp$TaxonName == "Shortfin mako" ] = "Isurus oxyrinchus"
rfmo_sp$TaxonName[rfmo_sp$TaxonName == "Anguilla anguilla anguilla"] = "Anguilla anguilla"
rfmo_sp$TaxonName[rfmo_sp$TaxonName == "Loligo vulgaris vulgaris"] = "Loligo vulgaris"
rfmo_sp$TaxonName[rfmo_sp$TaxonName == "Mullus barbatus"] = "Mullus barbatus barbatus"
rfmo_sp$TaxonName[rfmo_sp$TaxonName == "Lepidonotothen kempi"] = "Lepidonotothen squamifrons"
rfmo_sp$TaxonName[rfmo_sp$TaxonName == "Caranx ruber"] = "Carangoides ruber"
rfmo_sp$TaxonName[rfmo_sp$TaxonName == "Alectis alexandrina"] = "Alectis alexandrinus"
rfmo_sp$TaxonName[rfmo_sp$TaxonName == "Kajikia audax"] = "Tetrapturus audax"
rfmo_sp$TaxonName[rfmo_sp$TaxonName == "Istiompax indica"] = "Makaira indica"
rfmo_sp$TaxonName[rfmo_sp$TaxonName == "Clupea Harengus"] = "Clupea harengus"
rfmo_sp$TaxonName[rfmo_sp$TaxonName == "Raja alba"] = "Rostroraja alba"

# get the rfmo score and join: rfmo_id, TaxonName, rfmo score
rfmo_sc <- read.csv(file.path(dir_r2, 'tmp/RFMOscores.csv'), stringsAsFactors = F) ; head(rfmo_sc)
# remove capital letters from rfmo names and rows with NAs
rfmo_sc <- rfmo_sc %>% mutate (rfmo = tolower(rfmo_sc$RFMO)) %>% select (rfmo, Score) %>% filter (!is.na(Score))

# get the rfmo to fao id lookup table: fao_id, rfmo_id, prop rfmo area in fao_id
rfmo_fao <- read.csv(file.path(dir_r2, 'tmp/RFMOperFAO.csv'), stringsAsFactors = F) ; head(rfmo_fao)

# need table for fao_id to fao_name
fao_id_rgn <- read.csv('../ohiprep/HighSeas/HS_other_v2014/FAOregions.csv', stringsAsFactors = F) ; head(fao_id_rgn)
# join this to the rfmo table: fao_id, rfmo_id, prop rfmo area in fao_id,  TaxonName, rfmo score
rfmo_fao <- left_join(rfmo_fao, fao_id_rgn[,c(1,4)], by=c("rgn_id" = "rgn_id_2013")) ; rfmo_fao # Joining by: "rgn_id" 
res_hs <- gather(rfmo_fao, key = rfmo, value = proparea, -rgn_id, -rgn_name, -total, -fao_id) ; head(res_hs)
res_hs$rfmo <- as.character(res_hs$rfmo)

Ant_rgn <- data.frame( cbind ('rgn_id' = as.numeric(c(268, 271, 278)), 'rgn_name' = c('Antarctic', 'Antarctic', 'Antarctic'), 'total' = as.numeric(c(1, 1, 1)), 'fao_id' = as.numeric(c(48, 58, 88)), 'rfmo' = c('ccamlr', 'ccamlr', 'ccamlr'), 'proparea' = as.numeric(c(1, 1, 1)) ), stringsAsFactors = F)
Ant_rgn$proparea <- as.numeric(as.character(Ant_rgn$proparea))
Ant_rgn$total <- as.numeric(as.character(Ant_rgn$total))
Ant_rgn$fao_id <- as.numeric(as.character(Ant_rgn$fao_id))
res_hs <- rbind(Ant_rgn, res_hs)

res_hs2 <- res_hs %>% mutate( relarea = proparea/total) ; head(res_hs2) # relarea will be NaN if the RFMO doesn't overlap the FAO region
# add in rfmo resilience scores 
res_hs2 <- left_join (res_hs2, rfmo_sc) # Joining by: "rfmo"
# res_hs <-  filter( res_hs, fao_id!=18 ) # remove the Arctic
# join the species list per rfmo
res_hs2 <- left_join (res_hs2, rfmo_sp) # Joining by: "rfmo"
# join the taxon numeric code
res_hs2 <- left_join (res_hs2, tax[,1:2], by = c("TaxonName"="tax_nm"))
res_hs2 <- res_hs2 %>% mutate (saup_id = 0, stock_saup_id = paste( tax_code, fao_id, saup_id, sep = '_')) # make the stock_id and add the saup_id
res_hs2 <- res_hs2 %>% ungroup() %>% select(fao_id, saup_id, rfmo, relarea, stock_saup_id, Score)

# rbind high seas with eez resilience scores
r_c <- rbind(res_hs2, res_eez) # unique(r_c$saup_id[is.na(r_c$Score)])  # check
# save.image()
# join relative catch by fao+saup+stock_id
# generate a lookup that has the correct rfmo assigned to each saup id (none to eezs, but all 14 to high seas, where saup_id=0)
rfmo_lookup <- unique(r_c[,1:4])
newSAUP6 <- left_join(newSAUP6, rfmo_lookup) 
r_all <- left_join (newSAUP6, r_c) %>% select (
  fao_id, saup_id, rfmo, relarea, stock_saup_id, rel_ct, Score)
# %>% filter (
#                                       fao_id != 18)

# calculate the relative resilience score per stock, multiplying the res score by the rel prop of catch and rel rfmo area
# when an rfmo area has area for that stock (rel_area>0) but Score is na, then relarea should be recalculated excluding that rfmo
r_all <- r_all %>% ungroup() %>% group_by (stock_saup_id, saup_id) %>% mutate ( 
  new.relarea = ifelse( is.na(Score), 0, ifelse( relarea == 0, 0,
                                                 relarea/sum( relarea[which( !is.na(Score) ) ], na.rm =T ) 
  )
  ) 
)
res_scores <- r_all %>% mutate ( 
  stock_id = paste(
    sapply(strsplit(stock_saup_id, "_"), function(x)x[1]),
    "_", sapply(strsplit(stock_saup_id, "_"), function(x)x[2]), sep=""),
  part_score = rel_ct * new.relarea * Score ) %>% group_by (
    stock_id) %>% summarise (
      final_score = sum(part_score, na.rm =T)) %>% mutate (
        unif_prior = ifelse( final_score > 0.6, 1, 0)
      )


# the score for high seas is based only on management bodies present for specific species
# r_all[r_all$stock_id == 'Katsuwonus pelamis_67',] # example showing that the % of rfmo in the area doesn't work: since most of the area is covered by the salmon commission, the tunas appear under managed
# only if the final score is NA does it get turned to 0

write.csv(res_scores, file.path(dir_d, 'data','stock_resil_06cutoff_2015.csv'), row.names=F)

################################################################################################################################
### check if any of the new stocks (i.e. stocks that occur in the new saup taxon list and didnt occur in the list from 2014) fall
# under RFMO jurisdiction
new_tax <- read.csv(file.path(dir_d, 'raw', 'new_taxa.csv' )) ; head(new_tax)
new_tax_catch = left_join(new_tax,newSAUP3[newSAUP3$saup_id==0,], by=c('tax_code'='Taxonkey'))
new_tax_catch = new_tax_catch %>% group_by(tax_code,tax_nm,common_nm,fao_id) %>% summarise(av_ct = mean(tonnes, na.rm=T))
new_tax_catch = new_tax_catch[!is.na(new_tax_catch$fao_id),]
new_tax_catch = new_tax_catch %>% arrange(fao_id, tax_nm)
print(tbl_df(new_tax_catch),n=25)
# only: 
# 600135 Scomberomorus semifasciatus Broad-barred king mackerel     71 
#is managed in rfmo: 
# sprfmo 

###########################################################################################################
###### not sure if the following should be kept #######


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