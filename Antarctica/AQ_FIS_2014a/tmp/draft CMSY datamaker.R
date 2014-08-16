######################################################
######## Prepping data for FIS ANTARCTICA ###########
######################################################

## Calculating: (1) catch time-series for CMSY and (2) weights for food provision FIS
source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)

dir_d = '../ohiprep/Antarctica/AQ_FIS_2014a' # set folder where files are saved
data = file.path(dir_neptune_data, "model/GL-NCEAS-FIS_2014a")


CCAMLR_FIS<-read.csv("Ant_Data/CCAMLR_FIS.csv") # load CCAMLR data
length(unique(CCAMLR_FIS$SpeciesCode)) # count unique taxa: 225

# sum CCAMLR catch by year, CCAMLR area, and add species details
CCAMLR_FIS$SpeciesCode[which(CCAMLR_FIS$SpeciesCode=="kri")]<-"KRI" # assign correct name
CCAMLR_sum<-aggregate(CCAMLR_FIS[,3], by=list(CCAMLR_FIS[,4], CCAMLR_FIS[,6], CCAMLR_FIS[,2]), FUN="sum")
names(CCAMLR_sum)[1:3]<-names(CCAMLR_FIS)[c(4,6,2)]
names(CCAMLR_sum)[4]<-"totC"
# alternate using: library(plyr) # CCAMLR_sum<- ddply(CCAMLR_FIS, .(season.year, ASD, SpeciesCode),summarize, totC=sum(CatchWeight))
CCAMLR_spp<-read.csv("Ant_Data/CCAMLR_spp.csv") #load spp names for reference
CCAMLR_c<-merge(CCAMLR_sum, CCAMLR_spp, all.x=T)
table(is.na(CCAMLR_c$ScientificName)) # check for NAs
CCAMLR_c$FIS.target[CCAMLR_c$SpeciesCode=="SQA"]<-"y" # add Illex argentinus (Argentine shortfin squid)
CCAMLR_c$FIS.target[CCAMLR_c$SpeciesCode=="KCM"]<-"y" # add Lithodes murrayi (stone crab)


# 1 # CMSY
# 1a # join with spp df - exclude invertebrates per year
CCAMLR_y<-CCAMLR_c[CCAMLR_c$FIS.target=="y",]# exclude all non-fish except for King crabs and flying squids (Paralomis formosa, Paralomis spinosissima, Neolithodes diomedeae, Martialia hyadesi, Argentine shortfin squid, Lithodes murrayi)
length(unique(CCAMLR_y$ScientificName))# count unique taxa again: 169 (still higher than SAUP)
# 1b # exclude taxa coarser than species
CCAMLR_cs<-CCAMLR_c[CCAMLR_y$Tax_Lev==6,]
# 1c # sum catch per taxon and year, irrespective of area
# CCAMLR_t<-ddply(CCAMLR_cs, .(SpeciesCode, ScientificName, EnglishName, season.year),summarize, Catch=sum(totC)) # create the catch time series for CMSY
CCAMLR_t<-aggregate(CCAMLR_cs[,4], by=list(CCAMLR_cs[,1],CCAMLR_cs[,5],CCAMLR_cs[,7],CCAMLR_cs[,2]), FUN=sum)
names(CCAMLR_t)[1:4]<-names(CCAMLR_cs)[c(1,5,7,2)]
names(CCAMLR_t)[5]<-"Catch"
# checking on the invert time-series
par(mfrow=c(2,3))
plot(CCAMLR_t$season.year[CCAMLR_t$ScientificName=="Paralomis formosa"], CCAMLR_t$Catch[CCAMLR_t$ScientificName=="Paralomis formosa"],main="King crab (P.formosa)",ylab="tons",xlab="",xlim=c(1989,2012))
plot(CCAMLR_t$season.year[CCAMLR_t$ScientificName=="Paralomis spinosissima"], CCAMLR_t$Catch[CCAMLR_t$ScientificName=="Paralomis spinosissima"],main="King crab (P.spinosissima)",ylab="",xlab="",xlim=c(1989,2012))
plot(CCAMLR_t$season.year[CCAMLR_t$ScientificName=="Neolithodes diomedeae"], CCAMLR_t$Catch[CCAMLR_t$ScientificName=="Neolithodes diomedeae"],main="King crab (N.diomedeae)",ylab="tons",xlab="",xlim=c(1989,2012))
plot(CCAMLR_t$season.year[CCAMLR_t$ScientificName=="Martialia hyadesi"], CCAMLR_t$Catch[CCAMLR_t$ScientificName=="Martialia hyadesi"],main="Sevenstar flying squid",ylab="",xlab="",xlim=c(1989,2012))
plot(CCAMLR_t$season.year[CCAMLR_t$ScientificName=="Illex argentinus"], CCAMLR_t$Catch[CCAMLR_t$ScientificName=="Illex argentinus"],main="Argentine shortfin squid",ylab="tons",xlab="year",xlim=c(1989,2012))
plot(CCAMLR_t$season.year[CCAMLR_t$ScientificName=="Lithodes murrayi"], CCAMLR_t$Catch[CCAMLR_t$ScientificName=="Lithodes murrayi"],main="Stone crab",ylab="",xlab="year",xlim=c(1989,2012))

write.csv(CCAMLR_t,"Ant_Outputs/CCAMLR_t.csv",row.names = F)

# 1d # check how many species are already assigned to a life-history category - can I assugn more by family? inverts count as small pelagics  
SAUP_K_all<-read.csv("Ant_Data/SAUP data including open oceans.csv") # load data that has life history traits adn resilience cateogries
SAUP_K_sub<-SAUP_K_all[,c(3,6,9,10)]
SAUP_K_LH<-unique(SAUP_K_sub)  
names(SAUP_K_LH)[2]<-"ScientificName"
# CCAMLR_t_LH<-merge(CCAMLR_t,SAUP_K_LH,all.x=T)
library(plyr); library(dplyr)
# CCAMLR_t_LHcorr<-merge(CCAMLR_t,unique(SAUP_K_LH[,2:3]),all.x=T)
CCAMLR_t_LHcorr <- left_join(CCAMLR_t, unique(SAUP_K_LH[,c(2,4)])) # Joining by: "ScientificName"
table(is.na(CCAMLR_t_LHcorr$Resilience)) # check how many spp I have codes for (484 codes, against 446 NAs)
# add life history code to inverts -> only for COstello method!!
# Invert_codes<-unique(CCAMLR_y$SpeciesCode[CCAMLR_y$Class=="C" & CCAMLR_y$Tax_Lev==6])
# CCAMLR_t_LH$LH[CCAMLR_t_LH$SpeciesCode %in% Invert_codes]<-"I" 
# CCAMLR_t_LHcorr$LH[CCAMLR_t_LHcorr$SpeciesCode %in% Invert_codes]<-"I"
# remove assessed species: TOA, TOP, ANI (KRI was already removed)
assessed<-c("TOA","TOP","ANI")
CCAMLR_t_LH3<-CCAMLR_t_LHcorr [!CCAMLR_t_LHcorr$SpeciesCode %in% assessed,] 
# dim(CCAMLR_t_LH2[CCAMLR_t_LH2$SpeciesCode=="TOA",]) # check they were removed
write.csv(CCAMLR_t_LH3,"Ant_Outputs/CCAMLR_t_LHcorr.csv",row.names = F)
AQ_ct <- CCAMLR_t_LH3 %>% group_by(ScientificName) %>% mutate(tl = n()) %>% filter (tl>9)  %>% select(ScientificName, season.year, Catch, Resilience) %>% rename (c('ScientificName' = 'stock_id', 'season.year' = 'yr', 'Catch' = 'ct',  'Resilience' = 'res'))
write.csv(AQ_ct,"Ant_Outputs/CCAMLR_no0s_w_resil.csv",row.names=F)

# 1e # # add 0s for missing catch in years beyond the min year with catch>0

# 1e.1 merge with a df of all years for all species
sp_list<-unique(CCAMLR_t_LH3$ScientificName)
sp_list<-as.data.frame(cbind(as.character(sp_list),rep(1,times=length(sp_list))))
names(sp_list)[1]<-"ScientificName"
y_list<-c(min(CCAMLR_t_LH3$season.year):max(CCAMLR_t_LH3$season.year))
y_list<-as.data.frame(cbind(as.numeric(y_list),rep(1,times=length(y_list))))
spy_list<-merge(y_list,sp_list)
names(spy_list)<-names(CCAMLR_t_LH3)[c(6,1,4)]
CCAMLR_t_LH4<-merge(CCAMLR_t_LH3,spy_list[,2:3],all.y=T)
# 1e.2 add the species-specific min year of catch
vec<-vector("numeric",length=0)
for (i in 1:length(CCAMLR_t_LH4$ScientificName)) {
  vec[i]<- min(CCAMLR_t_LH4$season.year[CCAMLR_t_LH4$Catch>0 & CCAMLR_t_LH4$ScientificName==CCAMLR_t_LH4$ScientificName[i]],na.rm=T)
}
CCAMLR_t_LH4$min.year<-vec
# 1e.3 replace catch with 0 for each year > first year with non-0 catch
CCAMLR_t_LH4$Catch<-ifelse(CCAMLR_t_LH4$season.year>CCAMLR_t_LH4$min.year,ifelse(is.na(CCAMLR_t_LH4$Catch),0,CCAMLR_t_LH4$Catch),CCAMLR_t_LH4$Catch)
CCAMLR_t_LH4<-CCAMLR_t_LH4[!is.na(CCAMLR_t_LH4$Catch),] # now can remove rows where catch is NA as these will only be prior to the start of that species' catch time-series

write.csv(CCAMLR_t_LH4,"Ant_Outputs/CCAMLR_t_LHcorr_v2.csv",row.names = F)

vec<-vector("numeric",0)
for (i in 1:length(unique(CCAMLR_t_LH4$ScientificName))) {
  vec[i]<- length(CCAMLR_t_LH4$Catch[CCAMLR_t_LH4$ScientificName==unique(CCAMLR_t_LH4$ScientificName)[i] & CCAMLR_t_LH4$Catch>0])
}

t_ser_list<-as.data.frame(cbind(as.character(unique(CCAMLR_t_LH4$ScientificName)),as.numeric(as.character(vec))))
names(t_ser_list)<-c("Scientific.Name","n_years")
t_ser_list$n_years<-as.numeric(as.character(t_ser_list$n_years))
t_ser7<-t_ser_list[t_ser_list$n_years>6,] # species with at least 7 non-0 catch records
#######################
# 2 # weights
# 2a # EXPLORATIONS: take product 1a, but this might exclude large portions of bycatch, so explore where it makes sense to include other groups
CCAMLR_t2<-aggregate(CCAMLR_c[,4], by=list(CCAMLR_c[,1],CCAMLR_c[,5],CCAMLR_c[,7],CCAMLR_c[,2],CCAMLR_c[,8],CCAMLR_c[,9],CCAMLR_c[,10]), FUN=sum)
names(CCAMLR_t2)[1:7]<-names(CCAMLR_c)[c(1,5,7,2,8,9,10)]
names(CCAMLR_t2)[8]<-"Catch"
#CCAMLR_t2<-ddply(CCAMLR_test, .(SpeciesCode, ScientificName, EnglishName, season.year),summarize, Catch=sum(totC)) 
unique(CCAMLR_t2$Class) # only fish and inverts reported (plants, mammals and seabirds are not reported in catch)
other_catch<-CCAMLR_t2[CCAMLR_t2$Class=="O",]# check there is no substantial time-series for any invertebrates in "other"
other_max<-ddply(other_catch, .(SpeciesCode, ScientificName, EnglishName, FIS.target),summarize, MaxCatch=max(Catch))
other_max2<-other_max[with(other_max, order(-MaxCatch)), ] # sort by descending max Catch to find biggest yearly harvests (must be at least 10^2 and 7 years to matter)
write.csv(other_max2,"Ant_Outputs/other_inverts.csv",row.names=F) # none of these represent potentially commercial bycatch (unless there's some commerce of sponges, corals, salps, jellies, that I'm unaware of) so they should not be assessed in FIS, but they do represent a likely undersetimated level of impact on soft bottoms

crustmoll_catch<-CCAMLR_t2[CCAMLR_t2$Class=="C",]
crustmoll_max<-ddply(crustmoll_catch, .(SpeciesCode, ScientificName, EnglishName, FIS.target),summarize, MaxCatch=max(Catch))
crustmoll_max2<-crustmoll_max[with(crustmoll_max, order(-MaxCatch)), ] # sort by descending max Catch to find biggest yearly harvests (must be at least 10^2 and 7 years to matter)
write.csv(crustmoll_max2,"Ant_Outputs/crustmoll.csv",row.names=F) # given they represent miscellaneous taxa to which the targeted taxa belong, and/or can on occasion be a significan proporiton of catch, include: "octopus", "crab", "squid", "krill", "y" but not: "prawns and lobsters" and "n"

# 2b # based on explorations, select taxa
w_FIS.target<-levels(CCAMLR_c$FIS.target)[c(1,2,4,6,7)]
CCAMLR_w<-CCAMLR_c[CCAMLR_c$FIS.target %in% w_FIS.target,]
length(unique(CCAMLR_w$ScientificName)) # 186 unique taxa: all the spp, plus some coarser taxa

# check no duplicates in catch per taxon per year per CCAMLR area
CCAMLR_w$label<-paste(CCAMLR_w$ScientificName,CCAMLR_w$ASD ,CCAMLR_w$season.year)
length(unique(label)) # 3236
dim(CCAMLR_w) # 3236   11 - none! 
write.csv(CCAMLR_w,"Ant_Outputs/CCAMLR_w.csv",row.names=F)
# old: 
# table(CCAMLR_w$label)[table(CCAMLR_w$label)>1] # the duplicate is "Euphausia superba 481 2000"
# look at E. superba t-series
# E_superba<-CCAMLR_c[CCAMLR_c$ScientificName=="Euphausia superba" & CCAMLR_c$ASD==481,]
# E_superba<-E_superba[with(E_superba, order(-season.year)), ] 
# CCAMLR_t_LH2[CCAMLR_t_LH2$SpeciesCode=="kri",]

# 2c # get list of EEZs per CCAMLR area, subtract respective EEZ catch based on "SAUP_K" data
Ant_K_eezs<- Ant_K_eez[Ant_K_eez$EEZ!=0,] # data to remove from the weight df
# CCAMLR_w$ASD2<-substr(CCAMLR_w$ASD, 1, 2) # extra column to check the matches by EEZ are correct
names(Ant_K_eezs)[c(1,3,6)]<-c("season.year","ASD2","ScientificName") # rename to prep for "merge"
# 2c.1 # make df of EEZs and respective CCAMLR areas 
C_excl<-unique(Ant_K_eezs[,2:3]) # list of S Ocean eezs for which we have data and corresp FAO area
C_excl$ASD<-c(5852, 587, 586, 5851, 483, 486) # found by matching by hand with saup EEZ id names (from "OHI_SAUP_regions_V12.xlsx")
C_excl<-C_excl[,-2]
Ant_K_eez2<-merge(Ant_K_eezs,C_excl,all.x=T)

CCAMLR_w2<-merge(CCAMLR_w,Ant_K_eez2[,c(1:2,9,6,8)],all.x=T)
CCAMLR_w2$update.Catch<-ifelse(is.na(CCAMLR_w2$Catch),CCAMLR_w2$totC,CCAMLR_w2$totC-CCAMLR_w2$Catch)
# select relevant columns
names(CCAMLR_w2)[5]<-"tot.Catch"
names(CCAMLR_w2)[5]<-"totC_old"
CCAMLR_w2<-CCAMLR_w2[,c(1,2,12,3,4,7,8,10,5,15)]
CCAMLR_w2$corr.Catch2<-CCAMLR_w2$corr.Catch/1000
CCAMLR_w2$corr.Catch2<-ifelse(CCAMLR_w2$corr.Catch2<0,0,CCAMLR_w2$corr.Catch2) # correct the weighting file by dividing by 1000 and cap negative values at 0
write.csv(CCAMLR_w2,"Ant_Outputs/CCAMLR_w_update_v2.csv",row.names=F)

# table(duplicated(CCAMLR_w2))
