##############################################################################
############### calculate governance score for each fisheries stock ##########
##############################################################################
## ReadMe : ##################################################################
## assigns a resilience score to each stock, based on the fisheries governance score
## of EEZs and high seas where the stock is caught, weighted by the relative proportion
## with which it is caught in each jurisdiction (using mean catch since 1980). 
## For stocks that are exclusively coastal, it is simply based on the governance score of the respective EEZs.
## For stocks overlapping the high seas, it is based on the score of the RFMOs responsible for that HS area. 
## If none of the RFMOs overlapping a stock do not explicitly manage that species, 
## the stock is assumed to be unmanaged and gets a management score of 0.
## The resulting score is used to decide whether to assess that stock with the CMSY model
## that uses uniform priors (managed stock), or constrained priors (unmanaged stock).
## In 2014 the cutoff point for a stock to be considered managed was a score equal or greater than 0.6
##############################################################################
## NOTE: this requires SAUP fisheries data where castal(=eez) and high seas are combined (distinguished by saup_id == 0 for high seas) ##

source('../ohiprep/src/R/common.R') # set dir_neptune_data
library(tidyr)

#################################
## Formatting new SAUP catch data
#################################

## Identify and select the stocks that b/bmsy was calculated for (cmsy method):

stocks <- read.csv('globalprep/SAUP_FIS/v2015/tmp/b_bmsy_v16072015.csv') %>%
  select(stock_id) %>%
  unique()

meanCatch <- read.csv('globalprep/SAUP_FIS/v2015/tmp/mean_catch_saup_fao.csv')

meanCatch  <- meanCatch %>%
  filter(EEZID != 274) %>% #remove Gaza strip
   mutate(stock_id = paste(TaxonKey, FAOAreaID, sep="_")) %>%
  filter(stock_id %in% stocks$stock_id) %>%
  select(stock_id, EEZID, FAOAreaID, TaxonKey, mean_catch) %>%
  unique()

# calculate relative catch within each eez/hs region for each stock
meanCatch <- meanCatch %>%
  mutate(stock_saup_id = paste(stock_id, EEZID, sep="_")) %>%
  group_by(TaxonKey, FAOAreaID) %>%
  mutate (rel_ct = mean_catch/sum(mean_catch)) %>%
  dplyr::select(stock_saup_id, stock_id, saup_id=EEZID, fao_id=FAOAreaID, TaxonKey, mean_catch, rel_ct) %>%
  ungroup()


################################################################
## converting spatial scale of Mora scores (ohi 2013 regions) to saup regions 
################################################################
# file to convert saup regions to ohi regions
saup_ohi <- read.csv(file.path('../ohiprep/src/LookupTables/new_saup_to_ohi_rgn.csv'), stringsAsFactors = F) %>% 
  select (saup_id, rgn_id = ohi_id_2013) ; head(saup_ohi)

# fisheries governance by EEZ from OHI2013 Mora scores for fis governance (layer unchanged)
eez_r <- read.csv('globalprep/Fisheries_stock_gov_score/raw/r_fishing_v2_eez_2013a_wMontenegro.csv', stringsAsFactors = F) %>%
  select(rgn_id,  Score=resilience.score); head(eez_r) 

eez_r <- eez_r %>%
  left_join(saup_ohi) %>%   #N=287
  group_by(saup_id) %>%
  summarize(Score = mean(Score))   #N=278, average score for cases where several ohi regions are represented by one saup region
head(eez_r)
eez_r[duplicated(eez_r$saup_id),]  #should be no duplicates

res_eez <- meanCatch %>%  
  filter(saup_id > 0) %>%  
  left_join(eez_r) %>%
  mutate(proparea = 1, rfmo = 0) %>%  
  select(fao_id, saup_id, rfmo, proparea, stock_saup_id, Score)  
head(res_eez); summary(res_eez) 
#N=15763
################################################################
## High seas: determining resilience scores (saup_id=0)
################################################################

#########################
## dealing with species:

# get the list of stocks protected by each rfmo: rfmo_id, TaxonName
rfmo_sp <- read.csv('../ohiprep/HighSeas/tmp/rfmo_species.csv', stringsAsFactors = FALSE, check.names = FALSE) %>%
  select(TaxonName, rfmo) %>%
  unique()
  
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

# get taxon key based on TaxonName
tax <- read.csv(file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_FIS_data/v2015/raw/ohi_taxon.csv')) %>%
  dplyr::select(Taxonkey=taxonkey, TaxonName=scientific.name); head(tax)

sort(setdiff(tax$TaxonName, rfmo_sp$TaxonName)) #lots of names on the taxon list that aren't in the RFMO protected species, not surprising
setdiff(rfmo_sp$TaxonName, tax$TaxonName) # some names that aren't in species list as catch

rfmo_sp <- rfmo_sp %>%
  left_join(tax) %>%
  filter(!(is.na(Taxonkey)))
head(rfmo_sp)

#############################################
## Scores for rfmos for protectiveness: 

### get the rfmo score and join: rfmo_id, TaxonName, rfmo score
rfmo_sc <- read.csv('HighSeas/HS_Resilience_v2014/RFMO/tmp/RFMOscores.csv', stringsAsFactors = F) ; head(rfmo_sc)

# remove capital letters from rfmo names and rows with NAs
rfmo_sc <- rfmo_sc %>% 
  mutate (rfmo = tolower(rfmo_sc$RFMO)) %>% 
  select (rfmo, Score) %>% 
  filter (!is.na(Score))

# Coverage of each rfmo in each fao_rgn
# need table to translate fao_rgn_id to fao_rgn
fao_id_rgn <- read.csv('../ohiprep/HighSeas/HS_other_v2014/FAOregions.csv', stringsAsFactors = F) %>%
  select(rgn_id_2013, fao_id); head(fao_id_rgn)

# get the prop rfmo area in fao_id and join by region ID
rfmo_fao <- read.csv('HighSeas/HS_Resilience_v2014/RFMO/tmp/RFMOperFAO.csv', stringsAsFactors = F) ; head(rfmo_fao)

rfmo_fao <- rfmo_fao %>%
  left_join(fao_id_rgn, by=c("rgn_id" = "rgn_id_2013")) ; rfmo_fao # Joining by: "rgn_id" 

rfmo_fao <- gather(rfmo_fao, key = rfmo, value = proparea, -rgn_id, -rgn_name, -total, -fao_id)
rfmo_fao$rfmo <- as.character(rfmo_fao$rfmo)
head(rfmo_fao)
# add Antarctica:
Ant_rgn <- data.frame( cbind ('rgn_id' = as.numeric(c(268, 271, 278)), 
                              'rgn_name' = c('Antarctic', 'Antarctic', 'Antarctic'), 
                              'total' = as.numeric(c(1, 1, 1)), 
                              'fao_id' = as.numeric(c(48, 58, 88)), 
                              'rfmo' = c('ccamlr', 'ccamlr', 'ccamlr'), 
                              'proparea' = as.numeric(c(1, 1, 1)) ), stringsAsFactors = F)
Ant_rgn$proparea <- as.numeric(as.character(Ant_rgn$proparea))
Ant_rgn$total <- as.numeric(as.character(Ant_rgn$total))
Ant_rgn$fao_id <- as.numeric(as.character(Ant_rgn$fao_id))
rfmo_fao <- rbind(Ant_rgn, rfmo_fao)
head(rfmo_fao)

res_hs <- rfmo_fao %>% 
#  mutate(relarea = proparea/total) %>% # rel area will be NaN if the RFMO doesn't overlap the FAO region, MRF: don't think this is needed
  left_join (rfmo_sc) %>%              # add in rfmo resilience scores, joining by: "rfmo"
  left_join (rfmo_sp) 
head(res_hs)

res_hs <- res_hs %>% 
  mutate (saup_id = 0, 
          stock_saup_id = paste(Taxonkey, fao_id, saup_id, sep = '_')) %>% # make the stock_id and add the saup_id
  select(fao_id, saup_id, rfmo, proparea, stock_saup_id, Score)
head(res_hs)

###############################
## Join scores with catch data
# rbind high seas with eez resilience scores
r_c <- rbind(res_hs, res_eez) # unique(r_c$saup_id[is.na(r_c$Score)])  # check

FinalRes <- meanCatch %>%
  left_join(r_c) %>%
#     dups <- FinalRes$stock_saup_id[duplicated(FinalRes$stock_saup_id)]
#     tmp2 <- FinalRes[FinalRes$stock_saup_id %in% dups, ]
#     tmp2[tmp2$stock_saup_id=="600094_47_0", ]
  mutate(part_score = rel_ct * proparea * Score) %>% # NAs are stocks not protected by any rfmo, these get a 0 score.
  group_by(stock_saup_id, stock_id, TaxonKey, fao_id, saup_id) %>%
  summarize(part_score = max(part_score, na.rm=TRUE)) %>%  # when a stock is protected by >1 rfmo within an FAO, select the highest score
  ungroup()
#NAs result for high seas taxa that are not protected by any RFMO

FinalRes <- FinalRes %>%
  group_by(stock_id) %>%
  summarize(final_score=sum(part_score, na.rm=TRUE)) %>%
  mutate (unif_prior = ifelse( final_score > 0.6, 1, 0))

head(FinalRes)
summary(FinalRes)
hist(FinalRes$final_score)

write.csv(FinalRes, 'globalprep/SAUP_FIS/v2015/tmp/stock_resil_06cutoff_2015.csv', row.names=FALSE)
