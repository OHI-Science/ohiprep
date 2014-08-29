# prep AQ catch data for CMSY runs and for calculating mean catch ####
library(plyr)
library(dplyr)

dir_AQ <- '../ohiprep/Antarctica/AQ_FIS_2014a'

## Calculating: (1) catch time-series for CMSY and (2) weights for food provision FIS
CCAMLR_FIS<-read.csv(file.path(dir_AQ, "raw/CCAMLR_FIS.csv") ) # load CCAMLR data
length(unique(CCAMLR_FIS$SpeciesCode)) # count unique taxa: 225

# sum CCAMLR catch by year, CCAMLR area, and add species details
CCAMLR_FIS$SpeciesCode[which(CCAMLR_FIS$SpeciesCode=="kri")]<-"KRI" # assign correct name
CCAMLR_sum<-aggregate(CCAMLR_FIS[,3], by=list(CCAMLR_FIS[,4], CCAMLR_FIS[,6], CCAMLR_FIS[,2]), FUN="sum")
names(CCAMLR_sum)[1:3]<-names(CCAMLR_FIS)[c(4,6,2)]
names(CCAMLR_sum)[4]<-"totC"
# alternate using: library(plyr) # CCAMLR_sum<- ddply(CCAMLR_FIS, .(season.year, ASD, SpeciesCode),summarize, totC=sum(CatchWeight))
CCAMLR_spp<-read.csv(file.path(dir_AQ, "raw/CCAMLR_spp.csv") ) #load spp names for reference
CCAMLR_c<-merge(CCAMLR_sum, CCAMLR_spp, all.x=T)
table(is.na(CCAMLR_c$ScientificName)) # check for NAs
CCAMLR_c$FIS.target[CCAMLR_c$SpeciesCode=="SQA"]<-"y" # add Illex argentinus (Argentine shortfin squid)
CCAMLR_c$FIS.target[CCAMLR_c$SpeciesCode=="KCM"]<-"y" # add Lithodes murrayi (stone crab)

############################################################################################################
# ### EXPLORING SAUP DATA ####
# SAUP_K_eez<-read.csv("Ant_Data/Extension_redo_noFlag.csv") # load extended catch
# head(SAUP_K_eez)
# Ant_K_eez<-SAUP_K_eez[SAUP_K_eez$FAO %in% c(48,58,88),] # select Antarctic FAO regions
# Ant_K_CCAMLR<- Ant_K_eez[Ant_K_eez$EEZ==0,] # remove eez data
# unique(Ant_K_CCAMLR$CommonName) # count unique taxa: 69
# SAUP_Ant<-read.csv("Ant_Data/SAUP_Ant.csv") # load SAUP Antarctica data
# SAUP_Ant$CCAMLRArea2<-gsub('.','',SAUP_Ant$CCAMLRArea,fixed=T) # remove dots so that CCAMLR region codes match the saup file
# # compare values for a few regions/years/species
# SAUP5842_10<-SAUP_Ant[SAUP_Ant$Year==2010 & SAUP_Ant$CCAMLRArea2==5842,c(2,9,10,12,15)]
# CCAMLR5842_10<-CCAMLR_sum[CCAMLR_sum$season.year==2010 & CCAMLR_sum$ASD==5842,]
# CCAMLR5842_11<-CCAMLR_sum[CCAMLR_sum$season.year==2011 & CCAMLR_sum$ASD==5842,]
# CCAMLR881_10<-CCAMLR_sum[CCAMLR_sum$season.year==2010 & CCAMLR_sum$ASD==881,]
# SAUP5881_10<-SAUP_Ant[SAUP_Ant$Year==2010 & SAUP_Ant$CCAMLRArea2==881,c(2,9,10,12,15)]
# ##### the CCAMLR data appears more complete - now prep for CMSY and for weights #####
#############################################################################################

# 1 # CMSY
# 1a # join with spp df - exclude invertebrates per year
CCAMLR_y<-CCAMLR_c[CCAMLR_c$FIS.target=="y",]# exclude all non-fish except for King crabs and flying squids (Paralomis formosa, Paralomis spinosissima, Neolithodes diomedeae, Martialia hyadesi, Argentine shortfin squid, Lithodes murrayi)
length(unique(CCAMLR_y$ScientificName))# count unique taxa again: 169 (still higher than SAUP)
# 1b # exclude taxa coarser than species
CCAMLR_cs<-CCAMLR_c[CCAMLR_y$Tax_Lev==6,]
# 1c # sum catch per taxon and year, irrespective of area
# CCAMLR_t<-ddply(CCAMLR_cs, .(SpeciesCode, ScientificName, EnglishName, season.year),summarize, Catch=sum(totC)) # create the catch time series for CMSY

# the ccamlr region names have duplicates due to trailing spaces
CCAMLR_cs <- CCAMLR_cs %>% mutate( ASD = gsub(' ', '', ASD) )
CCAMLR_t <- CCAMLR_cs %>% rename (c('season.year' = 'yr')) %>% group_by(ASD, yr, ScientificName) %>% summarise (ct = sum (totC, na.rm =T) )

write.csv(CCAMLR_t, file.path(dir_AQ ,"data/CCAMLR_ct_rgn_Aug29_2014.csv"),row.names = F)