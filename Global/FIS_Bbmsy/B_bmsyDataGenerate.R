#### Figuring out fisheries data

rm(list = ls())

library(dplyr)
library(ggplot2)
library(grid)
source("http://nceas.ucsb.edu/~frazier/myTheme.txt")

############################################################
## Running CMSY script on handful of species to compare data (July 16 2014)----
## NOTE: if doing this again, the code for the next set of species is
## probably more organized. 
############################################################

## selecting fish for 1st b-bmsy run
OHInew <- read.csv("/var/data/ohi/model/GL-FIS-Bbmsy/Extended Catch Data 1950-2011.txt", sep="\t") #new SAUP data (although this is not the correct file)
OHIold <- read.csv("/var/data/ohi/model/GL-NCEAS_FIS_v2013a/RevisingFIS/raw/Extension_redo_withFlag.csv") #data used in 2013 analysis
fao <- read.csv('/var/data/ohi/model/GL-FIS-Bbmsy/FAO_clean_data_1950-2011.csv') #fao data

species_bmsy <- c("European conger", "Atlantic bonito", "Atlantic cod", "Patagonian toothfish", "Indo-Pacific sailfish")

## fao data
fao_bmsy <- fao %.% 
  filter(Species %in% species_bmsy) %.%
  filter(FAO %in% c(27, 21, 41, 77)) %.%
  filter(Year >= 1980) %.%
  group_by(Species, FAO, Year) %.%
  summarize(TotalCatch=sum(Catch, na.rm=TRUE)) 
fao_bmsy <- data.frame(fao_bmsy)
table(as.character(fao_bmsy$Species), fao_bmsy$FAO)

fao_bmsy <- fao_bmsy %.%
  mutate(stock_id = paste(Species, FAO, "fao", sep="_")) %.%
  select(stock_id, ct=TotalCatch, yr=Year)


## OHI old data
OHIold_bmsy <- OHIold %.%
  filter(CommonName %in% species_bmsy) %.%
  filter(FAO %in% c(27, 21, 41, 77)) %.%
  filter(IYear >= 1980) 
table(as.character(OHIold_bmsy$TaxonName), as.character(OHIold_bmsy$CommonName))

OHIold_bmsy <- OHIold_bmsy %.%
  mutate(stock_id = paste(CommonName, FAO, "OHIold", sep="_")) %.%
  select(stock_id, Catch, yr=IYear) %.%
  group_by(stock_id, yr) %.%
  summarize(ct=sum(Catch))

## OHI new data
OHInew$Taxonkey <- plyr::revalue(OHInew$Taxonkey, c('Conger conger' = 'European conger',
                                                    'Sarda sarda' = 'Atlantic bonito',
                                                    'Gadus morhua' = 'Atlantic cod',
                                                    'Istiophorus platypterus' = 'Indo-Pacific sailfish',
                                                    'Dissostichus eleginoides' = 'Patagonian toothfish'))


OHInew_bmsy <- OHInew %.%
  filter(Taxonkey %in% species_bmsy) %.%
  filter(FAO %in% c(27, 21, 41, 77)) %.%
  filter(IYear >= 1980) 
table(as.character(OHInew_bmsy$Taxonkey))

OHInew_bmsy <- OHInew_bmsy %.%
  mutate(stock_id = paste(Taxonkey, FAO, "OHInew", sep="_")) %.%
  select(stock_id, Catch, yr=IYear) %.%
  group_by(stock_id, yr) %.%
  summarize(ct=sum(Catch))

## merge everything....
data_bmsy <- rbind(fao_bmsy, OHIold_bmsy, OHInew_bmsy)
cdat <- data_bmsy %.%
  arrange(stock_id, yr)
cdat <- cdat[!(cdat$stock_id %in% c("Indo-Pacific sailfish_21_OHInew", "Indo-Pacific sailfish_21_OHIold")), ]

## Run bbmsy script
source('/var/data/ohi/model/GL-FIS-Bbmsy/cmsy.R')
 
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
Sys.time()

write.csv(b_bmsy, "C:\\Users\\Melanie\\Desktop\\FIS2014 cmsy\\dataRuns\\Compare_1980_v2.csv", row.names=FALSE)

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
ggsave('C:\\Users\\Melanie\\Desktop\\FIS2014 cmsy\\catch_1980start.png') 
