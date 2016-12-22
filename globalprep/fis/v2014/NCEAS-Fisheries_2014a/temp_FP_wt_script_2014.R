################ calculate the MAR sub-goal Status and Trend ####################################################
source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)
library('zoo')

# set path directories
MAR_data = '../ohiprep/Global/FAO-Aquaculture_v2012/data' # set path to "cleaned" FAO mariculture harvest data file 'MAR_data_AUG152013.csv'
FIS_data = '../ohiprep/Global/NCEAS-Fisheries_2014a/tmp'
dir_FIS_data = 'git-annex/Global/SAUP-Fisheries_v2011'

# read in files for MAR
MARdata<-read.csv(file.path(MAR_data, 'mar_harvest_tonnes_2013a_lyr.csv'), na='', stringsAsFactors=F) ; head(MARdata)
MARsust <- read.csv(file.path(MAR_data, 'mar_sustainability_score_2013a_lyr.csv'), na='', stringsAsFactors=F) ; head(MARsust)
MARsp <- read.csv(file.path(MAR_data, 'mar_harvest_species_2013a_lyr.csv'), na='', stringsAsFactors=F) ; head(MARsp)
MARsust <- join (MARsust, MARsp )
MARd <-  join (MARdata, MARsust)
MARd <-MARd %>% group_by(rgn_id, species_code, species, sust_coeff, year) %>% summarise(tonnes=sum(tonnes))
anyDuplicated(MARd[,1:5]) # no duplicate records 
MARd2 <- MARd %>% group_by(rgn_id, species_code) %>% arrange (rgn_id, species_code, year) %>% mutate(sm_tonnes=rollapply(tonnes, 4, mean, na.rm = TRUE, partial=T), wt_sm_data=sm_tonnes*sust_coeff)

# calculate total MAR catch per OHI region in 2012 (2014a)
MARd3 <- MARd2 %>% group_by(rgn_id, year) %>% filter(year>2003) %>% summarise(tot_ct_m=sum(sm_tonnes))

# calculate total FIS catch per OHI region in 2010 (2012a), 2011 (2013a, 2014a)
file_1<- 'raw/Extended catch data 1950-2011_18 July 2014.txt'
newSAUP <- read.delim(file.path(dir_neptune_data,dir_FIS_data, file_1 )) ; head(newSAUP)
newSAUP2 <- rename(newSAUP, c("IYear"="year","EEZ"="SAUP_C_NUM")) # this change to the 'newSAUP' file was performed in the CMSY data prep script

newSAUP2<-filter(newSAUP2,SAUP_C_NUM>0)
saup_to_ohi <- read.csv(file.path(FIS_data, 'EEZlookup.csv')) ; head(saup_to_ohi)
newSAUP2<-left_join(newSAUP2,select(saup_to_ohi,SAUP_C_NUM,OHI_2013_EEZ_ID)) # add species names 
newSAUP2<-newSAUP2[!is.na(newSAUP2$OHI_2013_EEZ_ID),]
FISdata3<- newSAUP2 %>% group_by(OHI_2013_EEZ_ID,year) %>% filter(year>2003) %>% summarise(tot_ct_f=sum(Catch)) ; head(FISdata3)
FISdata3<-rename(FISdata3,c('OHI_2013_EEZ_ID'='rgn_id'))

FISdata3 <- ungroup(FISdata3)

# create the weights files for 2012 and 2013 (note: use the same year of fisheries for both layers)
FIS_ct_2011 <- filter(FISdata3, year == 2011) %>% select(rgn_id, tot_ct_f)
MAR_y_2011 <- filter(MARd3, year == 2011) %>% select(rgn_id, tot_ct_m)
MAR_y_2012 <- filter(MARd3, year == 2012) %>% select(rgn_id, tot_ct_m)

FP2013a<-join(FIS_ct_2011,MAR_y_2011) # Joining by: rgn_id
FP2013a <- FP2013a %>% mutate (tot_ct_m = ifelse(is.na(tot_ct_m),0,tot_ct_m), w_fis = tot_ct_f / (tot_ct_f+tot_ct_m)) %>% select(rgn_id,w_fis) # replace NA with 0 - there are NAs only in the MAR data

FP2014a<-join(FIS_ct_2011,MAR_y_2012) # Joining by: rgn_id
FP2014a <- FP2014a %>% mutate (tot_ct_m = ifelse(is.na(tot_ct_m),0,tot_ct_m), w_fis = tot_ct_f / (tot_ct_f + tot_ct_m)) %>% select(rgn_id,w_fis) # replace NA with 0 - there are NAs only in the MAR data

# check for duplicates
anyDuplicated(FP2013a[,1])
anyDuplicated(FP2014a[,1])

dir_d = '../ohiprep/Global/NCEAS-Fisheries_2014a/data'
write.csv(FP2014a, file.path(dir_d, 'FP_mar_fis_weight_2014a.csv'), row.names=F)
write.csv(FP2013a, file.path(dir_d, 'FP_mar_fis_weight_2013a.csv'), row.names=F)
