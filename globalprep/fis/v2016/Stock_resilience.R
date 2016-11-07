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
library(dplyr)

#################################
## Formatting new SAUP catch data
#################################

## Identify and select the stocks that b/bmsy was calculated for (cmsy method):
stocks <- read.csv("globalprep/fis/v2016/int/cmsy_bbmsy.csv") %>%
#  filter(!is.na(bbmsy_mean)) %>%  # thare are some that don't converge during cmsy analysis...but it seems better to keep them in because it seems they do for the uniform
  dplyr::select(stock_id) %>%
  unique()
# Umbrina_cirrosa-37
filter(stocks, stock_id=="Conger_myriaster-71")

## Mean eez catch data
meanCatch_ohi <- read.csv('globalprep/fis/v2016/data/mean_catch.csv') %>%
  separate(taxon_key_stock, c('TaxonKey', 'species', 'fao_id'), sep="-") %>%
  mutate(stock_id = paste(species, fao_id, sep="-")) %>%
  filter(stock_id %in% stocks$stock_id) %>%
  select(stock_id, rgn_id, fao_id, TaxonKey, mean_catch) %>%
  unique()
filter(meanCatch_ohi, stock_id == "Conger_myriaster-71")
## Mean hs catch data
meanCatch_hs <- read.csv('globalprep/fis/v2016/data/mean_catch_hs.csv') %>%
  filter(stock_id %in% stocks$stock_id) %>%
  select(stock_id, rgn_id=ohi_rgn, fao_id=fao_rgn, TaxonKey, mean_catch) %>%
  unique()
filter(meanCatch_hs, stock_id == "Conger_myriaster-71")
## bind eez and hs regions
meanCatch <- rbind(meanCatch_ohi, meanCatch_hs)

# calculate relative catch within each region for each stock
meanCatch <- meanCatch %>%
  group_by(stock_id) %>%
  mutate (rel_ct = mean_catch/sum(mean_catch)) %>%
  ungroup()


tmp <- filter(meanCatch, stock_id == 'Clupea_harengus-18')
tmp <- filter(meanCatch, stock_id == 'Salvelinus_alpinus_alpinus-18')
tmp <- filter(meanCatch, stock_id == 'Brama_australis-87')
tmp

##############################
## Focus on the eez fisheries: scores - based on Mora
##############################
# fisheries governance by EEZ from OHI2013 Mora scores for fis governance (layer unchanged)
eez_r <- read.csv('globalprep/Fisheries_stock_gov_score/raw/r_fishing_v2_eez_2013a_wMontenegro.csv', stringsAsFactors = F) %>%
  select(rgn_id,  Score=resilience.score); head(eez_r) 

res_eez <- meanCatch %>%  
  left_join(eez_r, by="rgn_id")

################################################################
## High seas: determining resilience scores
## this is fao-rgn and stock specific based on rfmo
################################################################

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
  left_join (rfmo_sc) %>%            # add in rfmo resilience scores, joining by: "rfmo"
  left_join (rfmo_sp) %>%
  filter(!is.na(TaxonName))  ## siofa has no protected species in the list...
head(res_hs)

res_hs <- res_hs %>% 
  mutate(TaxonName = sub(" ", "_", TaxonName),
          rgn_id = 0, 
          stock_id = paste(TaxonName, fao_id, sep = '-')) %>% 
  mutate(Score_hs = proparea * Score) %>% # make the stock_id and add the saup_id
  dplyr::select(stock_id, rgn_id, fao_id, Score_hs)
head(res_hs)

# when a stock is protected by >1 rfmo within an FAO, select the highest score
res_hs <- res_hs %>%
  group_by(stock_id, rgn_id, fao_id) %>%
 summarize(Score_hs = max(Score_hs, na.rm=TRUE))  

###############################
## Join hs scores with eez data

setdiff(res_hs$stock_id, res_eez$stock_id[res_hs$rgn_id==0])
setdiff(res_eez$stock_id[res_hs$rgn_id==0], res_hs$stock_id)
intersect(res_eez$stock_id[res_hs$rgn_id==0], res_hs$stock_id)

resilience <- res_eez %>%  
  mutate(fao_id = as.numeric(fao_id)) %>%
  left_join(res_hs, by=c("stock_id", "rgn_id", "fao_id")) %>%
  mutate(Score = ifelse(is.na(Score), Score_hs, Score)) %>%
  mutate(Score = ifelse(is.na(Score), 0, Score)) %>%   # assume these are unprotected stocks in the high seas and get a zero
  mutate(part_score = rel_ct * Score) %>%
  group_by(stock_id) %>%
  summarize(final_score = sum(part_score, na.rm=TRUE),
            test = sum(rel_ct, na.rm=TRUE),   ## should all be one
            test2 = sum(part_score, na.rm=TRUE)) %>%  ## should all be less than one
  ungroup() %>%
  mutate(unif_prior = ifelse(final_score > 0.6, 1, 0))


head(resilience)
summary(resilience)
hist(resilience$final_score)

write.csv(resilience, 'globalprep/fis/v2016/int/stock_resil_06cutoff_2016.csv', row.names=FALSE)
