#### Figuring out fisheries data

rm(list = ls())
library(dplyr)
library(ggplot2)
library(grid)
library(parallel)

source("http://nceas.ucsb.edu/~frazier/myTheme.txt")
source("~/ohiprep/src/R/common.R")

############################################################
## Running CMSY script on handful of species to compare data (July 16 2014)----
## NOTE: if doing this again, the code for the next set of species is
## probably more organized. 
############################################################

## selecting random fish for test b-bmsy run
# original data from Kristin, ran with uniform prior:
b_bmsy_kristin <- read.csv("raw/cmsy.ohi.df_Jul292014.csv")

catch <- read.csv("raw/OHICatchHistoryCMSY_added0s_07_21_2014.csv", stringsAsFactors=FALSE)

species_bmsy <- c('Istiophorus albicans_41', 'Sphyrna lewini_31', 'Psenopsis anomala_61',       
                  'Hippoglossus stenolepis_67', 'Parapenaeus longirostris_27', 
                  'Mallotus villosus_21', 'Epinephelus socialis_71', 'Aphanopus carbo_34',
                  'Clupea pallasii pallasii_77', 'Melanogrammus aeglefinus_21')

cdat <- catch %>% 
  filter(stock_id %in% species_bmsy) %>%
  #filter(Year >= 1980) %>%  #I think the earlier run restricted data to >=1980, but Kristin's did not seem to do this
  arrange(stock_id, yr) 


## Run bbmsy script
source('cmsy_uniform.R')

### run CMSY function:
#start the clock
Sys.time()
ptm <- proc.time()

b_bmsy <- data.frame()

for(i in 1:length(unique(cdat$stock_id))){  
  #for(i in 1:1){    ##troubleshooting    
  test <- runCMSY(cdat, stockNumber=i)
  new <- data.frame(taxon_name=test[[1]],
                    b_bmsy=test[[2]],
                    year=test[[7]])
  b_bmsy <- rbind(b_bmsy, new)
}
# Stop the clock
proc.time() - ptm
Sys.time()

#write.csv(b_bmsy, "Global/FIS_Bbmsy/my_bbmsy_Aug6_2014.csv", row.names=FALSE)

## Plots of the data
cdat$taxonName <- sapply(strsplit(as.character(cdat$stock_id), split="_"), function(x) x[1])
cdat$fao <- sapply(strsplit(as.character(cdat$stock_id), split="_"), function(x) x[2])
cdat$data <- sapply(strsplit(as.character(cdat$stock_id), split="_"), function(x) x[3])
cdat <- data.frame(cdat)

ggplot(cdat, aes(x=yr, y=ct, color=data, group=data)) +
  geom_point() +
  geom_line() +
  facet_wrap(taxonName ~ fao, scales="free") +
  theme_bw()

## comparing Kristin's data to mine:
mybmsy <- read.csv("Global/FIS_Bbmsy/my_bbmsy_Aug6_2014.csv")
kristin_bmsy <- read.csv("Global/FIS_Bbmsy/cmsy.ohi.df_Jul292014.csv")

bmsy <- mybmsy %>%
  select(stock_id=taxon_name, my_b_bmsy=b_bmsy, yr=year) %>%  
  left_join(kristin_bmsy, by=c("stock_id", "yr"))


ggplot(bmsy, aes(x=b_bmsy, y=my_b_bmsy)) +
  geom_point() +
  geom_abline(slope=1, intercept=0)

mod <- lm(my_b_bmsy~b_bmsy, data=bmsy)
summary(mod)



#####################################################
## Testing different models using parallel processing script
## Original and relaxed priors (this can be compared to
##  the uniform prior data that was generated above)
####################################################
# Parallel processing based on Ben B Code:
#   https://github.com/OHI-Science/ohiprep/blob/master/Global/NCEAS-SpeciesDiversity_v2014/ingest_iucn.R#L65

catch <- read.csv("Global/FIS_Bbmsy/OHICatchHistoryCMSY_added0s_07_21_2014.csv", stringsAsFactors=FALSE)

species_bmsy <- c('Istiophorus albicans_41', 'Sphyrna lewini_31', 'Psenopsis anomala_61',       
                  'Hippoglossus stenolepis_67', 'Parapenaeus longirostris_27', 
                  'Mallotus villosus_21', 'Epinephelus socialis_71', 'Aphanopus carbo_34',
                  'Clupea pallasii pallasii_77', 'Melanogrammus aeglefinus_21')

cdat <- catch %.% 
  filter(stock_id %in% species_bmsy) %.%
  #filter(Year >= 1980) %.%  #I think the earlier run restricted data to >=1980, but Kristin's did not seem to do this
  arrange(stock_id, yr) 


## Run bbmsy script
# source('Global/FIS_Bbmsy/cmsy_relaxed.R')
source('Global/FIS_Bbmsy/cmsy_constrained.R')

get_b_bmsy <- function(i){  
  test <- runCMSY(stockNumber=i, cdat=cdat)
  new <- data.frame(taxon_name=test[[1]],
                    b_bmsy=test[[2]],
                    year=test[[7]])
  return(new)
}

print(system.time({    
  r = mclapply(1:length(species_bmsy), get_b_bmsy, mc.cores=detectCores(), mc.preschedule=F) 
}))

r <- ldply(r)
#write.csv(r, "Global/FIS_Bbmsy/my_bbmsy_relaxed_Aug12_2014.csv", row.names=FALSE)
#write.csv(r, "Global/FIS_Bbmsy/my_bbmsy_original_Aug12_2014.csv", row.names=FALSE)

### compare different methods
relaxed <- read.csv("Global/FIS_Bbmsy/my_bbmsy_relaxed_Aug12_2014.csv")
relaxed <- relaxed %>%
  mutate(test="relaxed")

original <- read.csv("Global/FIS_Bbmsy/my_bbmsy_original_Aug12_2014.csv")
original <- original %>%
  mutate(test="original")

uniform <- read.csv("Global/FIS_Bbmsy/my_bbmsy_uniform_Aug6_2014.csv")
uniform <- uniform %>%
  mutate(test="uniform")

catch_zero <- cdat %>%
  select(taxon_name=stock_id, b_bmsy=ct, year=yr) %>%
  mutate(test="catch_ct") %>%
  group_by(taxon_name, test) %>%
  mutate(b_bmsy = b_bmsy/max(b_bmsy)) #not really b/bmsy but rather catch (just wanted to be able to merge thsi into other data)


data <- rbind(relaxed, original, uniform, catch_zero)
ggplot(data, aes(x=year, y=b_bmsy, group=test, color=test)) +
  geom_point() +
  geom_line() +
  facet_wrap(~taxon_name)


#####################################################
## Testing different models using parallel processing script
## Original and relaxed priors (this can be compared to
##  the uniform prior data that was generated above)
## same as above but selecting different taxa
####################################################

# identify taxa with increasing b/bmsy from 2010 to 2011
# (Kristin's run: based on the uniform prior and zeros added to catch)

b_bmsy_kristin <- read.csv("Global/FIS_Bbmsy/cmsy.ohi.df_Jul292014.csv")
b_bmsy_increase <- b_bmsy_kristin %>%
  select(taxon_name=stock_id, b_bmsy, year=yr) %>%
  mutate(test="increase") %>%
  filter(year>=2010) %>%
  group_by(taxon_name, test) %>%
  summarize(b_bmsy_diff = b_bmsy[year==2011] - b_bmsy[year==2010]) %>%
  filter(b_bmsy_diff>0)

b_bmsy_decrease <- b_bmsy_kristin %>%
  select(taxon_name=stock_id, b_bmsy, year=yr) %>%
  mutate(test="increase") %>%
  filter(year>=2010) %>%
  group_by(taxon_name, test) %>%
  summarize(b_bmsy_diff = b_bmsy[year==2011] - b_bmsy[year==2010]) %>%
  filter(b_bmsy_diff<0)

#species_bmsy <- sample(b_bmsy_increase$taxon_name, 15)
#species_bmsy <- sample(b_bmsy_decrease$taxon_name, 15)

#first 15 are increasing b/bmsy, second 15 are decreasing b/bmsy
species_bmsy <- c('Parapenaeus longirostris_34', 'Zygochlamys patagonica_87',
                  'Coregonus laurettae_18', 'Euthynnus alletteratus_31',
                  'Patagonotothen brevicauda brevicauda_48', 'Chlamys delicatula_81',
                  'Liza klunzingeri_51', 'Cheilodactylus variegatus_87',
                  'Dussumieria acuta_57', 'Etmopterus spinax_37',
                  'Thunnus alalunga_21', 'Chelidonichthys kumu_81',
                  'Myxine glutinosa_21', 'Decapterus macrosoma_71',
                  'Pandalus borealis_21', 'Lethrinus lentjan_51',
                  'Euthynnus lineatus_87', 'Makaira nigricans_27',
                  'Illex illecebrosus_27', 'Carangoides malabaricus_51',
                  'Tetrapturus angustirostris_51', 'Mercenaria mercenaria_27',
                  'Deania calcea_81', 'Epinephelus tauvina_51',
                  'Brama brama_27', 'Litopenaeus stylirostris_77',
                  'Seriola dumerili_31', 'Coryphaena hippurus_57', 
                  'Sillago sihama_51', 'Thunnus tonggol_51')

catch <- read.csv("Global/FIS_Bbmsy/OHICatchHistoryCMSY_added0s_07_21_2014.csv", stringsAsFactors=FALSE)

cdat <- catch %.% 
  filter(stock_id %in% species_bmsy) %.%
  #filter(Year >= 1980) %.%  #I think the earlier run restricted data to >=1980, but Kristin's did not seem to do this
  arrange(stock_id, yr) 


## Run bbmsy script
# source('Global/FIS_Bbmsy/cmsy_relaxed.R')
source('Global/FIS_Bbmsy/cmsy_constrained.R')

get_b_bmsy <- function(i){  
  test <- runCMSY(stockNumber=i, cdat=cdat)
  new <- data.frame(taxon_name=test[[1]],
                    b_bmsy=test[[2]],
                    year=test[[7]])
  return(new)
}


print(system.time({    
  r = mclapply(1:length(species_bmsy), get_b_bmsy, mc.cores=detectCores(), mc.preschedule=F) # took ~ 4 hrs on neptune
}))

r <- ldply(r)
#write.csv(r, "Global/FIS_Bbmsy/my_bbmsy_relaxed_Aug13_2014.csv", row.names=FALSE)
#write.csv(r, "Global/FIS_Bbmsy/my_bbmsy_original_Aug13_2014.csv", row.names=FALSE)

### compare different methods
relaxed <- read.csv("Global/FIS_Bbmsy/my_bbmsy_relaxed_Aug12_2014.csv")
relaxed <- relaxed %>%
  mutate(test="relaxed")

original <- read.csv("Global/FIS_Bbmsy/my_bbmsy_original_Aug12_2014.csv")
original <- original %>%
  mutate(test="original")

uniform <- read.csv("Global/FIS_Bbmsy/my_bbmsy_uniform_Aug6_2014.csv")
uniform <- uniform %>%
  mutate(test="uniform")

catch_zero <- cdat %>%
  select(taxon_name=stock_id, b_bmsy=ct, year=yr) %>%
  mutate(test="catch_ct") %>%
  group_by(taxon_name, test) %>%
  mutate(b_bmsy = b_bmsy/max(b_bmsy))


data <- rbind(relaxed, original, uniform, catch_zero)
ggplot(data, aes(x=year, y=b_bmsy, group=test, color=test)) +
  geom_point() +
  geom_line() +
  facet_wrap(~taxon_name)



##############################################################
## checking results when we remove zero values
##############################################################
catch <- read.csv("Global/FIS_Bbmsy/OHICatchHistoryCMSY_added0s_07_21_2014.csv", stringsAsFactors=FALSE)

species_bmsy <- c('Acanthurus dussumieri_71', 'Acanthurus lineatus_61', 'Acanthocybium solandri_61',       
                  'Anchoa choerostoma_31', 'Aprion virescens_71', 
                  'Aphareus rutilans_77', 'Ariomma indicum_61', 'Callinectes danae_31',
                  'Cetorhinus maximus_81', 'Chaceon quinquedens_41')

cdat_zeros <- catch %.% 
  filter(stock_id %in% species_bmsy) %>%
  #filter(Year >= 1980) %>%  #I think the earlier run restricted data to >=1980, but Kristin's did not seem to do this
  arrange(stock_id, yr) %>%
  select(stock_id, ct, yr) %>%
  mutate(ct=ifelse(ct==0, NA, ct)) 

cdat <- data.frame()
for(i in 1:length(species_bmsy)){
  #i=1
  spec <- species_bmsy[i]
  tmp <- cdat_zeros[cdat_zeros$stock_id %in% spec, ]
  tmp <- na.trim(tmp)
  cdat <- rbind(cdat,tmp)
}

cdat$ct[is.na(cdat$ct)] <- 0 


## Run bbmsy script
source('Global/FIS_Bbmsy/cmsy_uniform.R')

### run CMSY function:
#start the clock
Sys.time()
ptm <- proc.time()
b_bmsy <- data.frame()
for(i in 1:length(unique(cdat$stock_id))){  
  #for(i in 1:1){    ##troubleshooting    
  test <- runCMSY(cdat, stockNumber=i)
  new <- data.frame(taxon_name=test[[1]],
                    b_bmsy=test[[2]],
                    year=test[[7]])
  b_bmsy <- rbind(b_bmsy, new)
  i
}
# Stop the clock
proc.time() - ptm
Sys.time()  #8903

write.csv(b_bmsy, "Global/FIS_Bbmsy/my_bbmsy_Aug11_2014_zeroCheck .csv", row.names=FALSE)



## comparing Kristin's data to mine:
my_bmsy <- read.csv("Global/FIS_Bbmsy/my_bbmsy_Aug11_2014_zeroCheck .csv")
kristin_bmsy <- read.csv("Global/FIS_Bbmsy/cmsy.ohi.df_Jul292014.csv")

bmsy <- my_bmsy %>%
  select(stock_id=taxon_name, my_b_bmsy=b_bmsy, yr=year) %>%  
  left_join(kristin_bmsy, by=c("stock_id", "yr"))


ggplot(bmsy, aes(x=b_bmsy, y=my_b_bmsy, color=stock_id)) +
  geom_point() +
  geom_abline(slope=1, intercept=0)

ggplot(bmsy, aes(x=yr, y=my_b_bmsy, group=stock_id)) +
  geom_point() +
  geom_line() +
  facet_wrap(~stock_id)+
  geom_point(aes(x=yr, y=b_bmsy), col="red") +
  geom_line(aes(x=yr, y=b_bmsy), col="red")

mod <- lm(my_b_bmsy~b_bmsy, data=bmsy)
summary(mod)


##############################################################
## looking at high seas data
##############################################################
# look at old data to get list of stock

oldCatch <- read.csv(file.path(dir_neptune_data, 
                               'model/GL-HS-AQ-Fisheries_v2013/HighSeas/raw/Extension_redo_withFlag.csv'))
oldCatch <- oldCatch %>%
  filter(EEZ %in% 0,
         FAO %in% c(31, 34, 57, 61)) %>%
  mutate(stock_id = paste(TaxonName, FAO, sep="_")) %>%
  select(year=IYear, stock_id, ct=Catch) %>%
  group_by(stock_id) %>%
  mutate(ct_norm = ct/max(ct)) %>%
  mutate(data="old_catch") %>%
  arrange(stock_id, year)

old_bmsy <- read.csv(file.path(dir_neptune_data, 
                               'model/GL-HS-AQ-Fisheries_v2013/HighSeas/raw/fnk_fis_b_bmsy.csv'))
old_bmsy <- old_bmsy %>%
  mutate(stock_id = paste(taxon_name, fao_id, sep="_")) %>%
  filter(stock_id %in% oldCatch$stock_id) %>%
  select(year, stock_id, b_bmsy) %>%
  mutate(data = 'old_bmsy') %>%
  arrange(stock_id, year)

newCatch <- read.csv("Global/FIS_Bbmsy/OHICatchHistoryCMSY_added0s_07_21_2014.csv", stringsAsFactors=FALSE)

newCatch <- newCatch %>% 
  filter(stock_id %in% oldCatch$stock_id) %>%
  select(year=yr, stock_id, ct) %>%
  group_by(stock_id) %>%
  mutate(ct_norm = ct/max(ct)) %>%
  mutate(data="new_catch") %>%
  arrange(stock_id, year)

new_bmsy <- read.csv("Global/FIS_Bbmsy/cmsy.ohi.df_Jul292014.csv")

new_bmsy <- new_bmsy %>%
  filter(stock_id %in% oldCatch$stock_id) %>%
  select(year=yr, stock_id, b_bmsy) %>%
  mutate(data = 'new_Kristin_bmsy') %>%
  arrange(stock_id, year)

bmsy_data <- rbind(new_bmsy, old_bmsy)

stock <- unique(bmsy_data$stock_id)

ggplot(subset(bmsy_data, stock_id %in% stock[1:100]), aes(x=year, y=b_bmsy, color=data, group=data)) +
  geom_point(size=1) +
  geom_line() +
  facet_wrap(~stock_id, nrow=10, scale="free") +
  theme_bw()+
  theme(strip.text=element_text(size=rel(.55)))
#ggsave('Global/FIS_Bbmsy/FAO_oldvsnew_bmsy.png') 



catch_data <- rbind(newCatch,oldCatch)

catch_data  <- catch_data %>%
  filter(stock_id %in% stock)

ggplot(subset(catch_data, stock_id %in% stock[1:100]), aes(x=year, y=ct, color=data, group=data)) +
  geom_point(size=1) +
  geom_line() +
  facet_wrap(~stock_id, nrow=10, scale="free") +
  theme_bw()+
  theme(strip.text=element_text(size=rel(.55)))
#ggsave('Global/FIS_Bbmsy/FAO_oldvsnew_catch.png') 

ggplot(subset(catch_data, stock_id %in% stock[1:100]), aes(x=year, y=ct, color=data, group=data)) +
  geom_point(size=1) +
  geom_line() +
  facet_wrap(~stock_id, nrow=10, scale="free") +
  theme_bw()+
  theme(strip.text=element_text(size=rel(.55)))
#ggsave('Global/FIS_Bbmsy/FAO_oldvsnew_catch.png') 

ggplot(subset(catch_data, stock_id %in% stock[1:100]), aes(x=year, y=ct_norm, color=data, group=data)) +
  geom_point(size=1) +
  geom_line() +
  facet_wrap(~stock_id, nrow=10, scale="free") +
  theme_bw()+
  geom_point(data=subset(bmsy_data, stock_id %in% stock[1:100]), 
             aes(x=year, y=b_bmsy, color=data, group=data), size=1) + 
  geom_line(data=subset(bmsy_data, stock_id %in% stock[1:100]), 
             aes(x=year, y=b_bmsy, color=data, group=data)) +
  theme(strip.text=element_text(size=rel(.55)))
ggsave('Global/FIS_Bbmsy/FAO_oldvsnew_relcatch_bmsy.png') 

## running cmsy script
cdat <- catch_data %.% 
  filter(data=='new_catch') %>%
  select(stock_id, ct, yr=year) %>%
  arrange(stock_id, yr) 


## Run bbmsy script
# source('Global/FIS_Bbmsy/cmsy_relaxed.R')
source('Global/FIS_Bbmsy/cmsy_constrained.R')

get_b_bmsy <- function(i){  
  test <- runCMSY(stockNumber=i, cdat=cdat)
  new <- data.frame(taxon_name=test[[1]],
                    b_bmsy=test[[2]],
                    year=test[[7]])
  return(new)
}


print(system.time({    
  r = mclapply(1:length(stock), get_b_bmsy, mc.cores=detectCores(), mc.preschedule=F)
}))

r <- ldply(r)
#write.csv(r, "Global/FIS_Bbmsy/my_bbmsy_relaxed_Aug13_2014.csv", row.names=FALSE)


######################################################
## Testing the effects of cutting trailing zeros
##  Did this before, but want to test again
##   (results seemed surprising - and would be good to test)
######################################################
library(zoo)


### removed zeros
catch <- read.csv("Global/FIS_Bbmsy/OHICatchHistoryCMSY_added0s_07_21_2014.csv", stringsAsFactors=FALSE)
catch_lotszero <- catch %>%
  filter(yr>2005) %>%
  group_by(stock_id) %>%
  summarize(sumCatch=sum(ct)) %>%
  filter(sumCatch==0)


cdat_NA <- catch %.% 
  filter(stock_id %in% catch_lotszero$stock_id) %>%
  mutate(ct = ifelse(ct==0, NA, ct)) %>%
  select(stock_id, ct, yr) %>%
  arrange(stock_id, yr) 
 
cdat <- data.frame()  #fake data frame
for(i in 1:length(unique(cdat_NA$stock_id))){ 
  #i <- 1
  spec <- unique(cdat_NA$stock_id)[i]
  tmp <- cdat_NA[cdat_NA$stock_id %in% spec, ]
  tmp <- na.trim(tmp)
  cdat <- rbind(cdat, tmp)
}

cdat$ct[is.na(cdat$ct)] <- 0  
cdat <- subset(cdat, stock_id %in% catch_lotszero$stock_id[1:100])

ggplot(cdat, aes(x=yr, y=ct, group=stock_id)) +
  geom_point() +
  geom_line() +
  facet_wrap(~stock_id, nrow=10)

## Run bbmsy script
source('Global/FIS_Bbmsy/cmsy_constrained.R')

get_b_bmsy <- function(i){  
  test <- runCMSY(stockNumber=i, cdat=cdat)
  new <- data.frame(taxon_name=test[[1]],
                    b_bmsy=test[[2]],
                    year=test[[7]])
  return(new)
}

print(system.time({    
  r = mclapply(1:length(unique(cdat$stock_id)), get_b_bmsy, mc.cores=detectCores(), mc.preschedule=F) # took ~ 4 hrs on neptune
}))

 
r <- ldply(r)
write.csv(r, "Global/FIS_Bbmsy/my_bbmsy_original_nozeros_Aug14_2014.csv", row.names=FALSE)


### with zeros
catch <- read.csv("Global/FIS_Bbmsy/OHICatchHistoryCMSY_added0s_07_21_2014.csv", stringsAsFactors=FALSE)

cdat <- catch %.% 
  filter(stock_id %in% catch_lotszero$stock_id) %>%
  select(stock_id, ct, yr) %>%
  arrange(stock_id, yr) 

cdat <- subset(cdat, stock_id %in% catch_lotszero$stock_id[1:100])

ggplot(cdat, aes(x=yr, y=ct, group=stock_id)) +
  geom_point() +
  geom_line() +
  facet_wrap(~stock_id, nrow=10)

## Run bbmsy script
source('Global/FIS_Bbmsy/cmsy_constrained.R')

get_b_bmsy <- function(i){  
  test <- runCMSY(stockNumber=i, cdat=cdat)
  new <- data.frame(taxon_name=test[[1]],
                    b_bmsy=test[[2]],
                    year=test[[7]])
  return(new)
}

print(system.time({    
  r = mclapply(1:length(unique(cdat$stock_id)), get_b_bmsy, mc.cores=detectCores(), mc.preschedule=F) # took ~ 4 hrs on neptune
}))

r <- ldply(r)
write.csv(r, "Global/FIS_Bbmsy/my_bbmsy_original_zeros_Aug14_2014.csv", row.names=FALSE)



### compare data:
zeroData <- read.csv("Global/FIS_Bbmsy/my_bbmsy_original_zeros_Aug14_2014.csv")
noZeros <- read.csv("Global/FIS_Bbmsy/my_bbmsy_original_nozeros_Aug14_2014.csv")

zeroData  <- zeroData %>%
  mutate(test="ZeroesIncluded")
data <- noZeros %>%
  mutate(test="NoZeroes") %>%
  rbind(zeroData)


ggplot(data, aes(x=year, y=b_bmsy, group=test, color=test)) +
  geom_point(size=1) +
  geom_line() +
  theme_bw() +
  facet_wrap(~taxon_name, nrow=10, scales="free") +
  theme(strip.text=element_text(size=rel(.55)))
ggsave("Global/FIS_Bbmsy/zeroPaddingCompare_Aug142014.png")
