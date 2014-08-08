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

## selecting random fish for test b-bmsy run
b_bmsy_kristin <- read.csv("Global/FIS_Bbmsy/cmsy.ohi.df_Jul292014.csv")

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
source('Global/FIS_Bbmsy/cmsy.R')
 
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

write.csv(b_bmsy, "Global/FIS_Bbmsy/my_bbmsy_Aug6_2014.csv", row.names=FALSE)
#write.csv(b_bmsy, "C:\\Users\\Melanie\\Desktop\\FIS2014 cmsy\\dataRuns\\Compare_1980_v2.csv", row.names=FALSE)

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
