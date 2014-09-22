####################################################################
## Fisheries (FIS) High Seas:
## Data visualiztion/analysis/trouble-shooting 
####################################################################
library(dplyr)
library(ggplot2)
library(grid)
library(RColorBrewer)

source('../ohiprep/src/R/common.R') # set dir_neptune_data
source("http://nceas.ucsb.edu/~frazier/myTheme.txt")

data_path <- 'HighSeas/HS_FIS_v2014/data'

bmsy <- read.csv(file.path(data_path, 'fnk_fis_b_bmsy.csv')) %>%
  filter(year==2011) %>%
  select(fao_id, taxon_name, b_bmsy)

meanCatch <- read.csv(file.path(data_path, 'cnk_fis_meancatch.csv')) %>%
  mutate(fao_id = as.numeric(sapply(strsplit(as.character(fao_saup_id), "_"), function(x)x[1])),
         taxon_name = sapply(strsplit(as.character(taxon_name_key), "_"), function(x)x[1]),
         taxon_key = sapply(strsplit(as.character(taxon_name_key), "_"), function(x)x[2])) %>%
  filter(year==2011) %>%
  select(fao_id, taxon_name, taxon_key, mean_catch) %>%
  arrange(fao_id, mean_catch) %>%
  group_by(fao_id) %>%
  mutate(cumCatch=cumsum(mean_catch),
         totalCatch=sum(mean_catch)) %>%
  mutate(propCumCatch=round(cumCatch/totalCatch, 2)) %>%
  left_join(bmsy, by=c("fao_id", "taxon_name")) %>%
  ungroup()
  
data.frame(meanCatch[meanCatch$fao_id == 51,])
data.frame(meanCatch[meanCatch$fao_id == 34,])

bmsy.uniform <- read.csv("Global/NCEAS-Fisheries_2014a/tmp/fnk_fis_b_bmsy_lyr_uniform_no0_runningMean.csv") %>%
  filter(stock_id %in% c("Thunnus obesus_51", "Katsuwonus pelamis_51", "Thunnus albacares_51", 
                         "Thunnus obesus_34", "Katsuwonus pelamis_34", "Thunnus albacares_34"),
         yr==2011)

bmsy.constrained <- read.csv("Global/NCEAS-Fisheries_2014a/tmp/fnk_fis_b_bmsy_lyr_constrained_no0_runningMean.csv") %>%
  filter(stock_id %in% c("Thunnus obesus_51", "Katsuwonus pelamis_51", "Thunnus albacares_51", 
                         "Thunnus obesus_34", "Katsuwonus pelamis_34", "Thunnus albacares_34"),
         yr==2011)


status <- read.csv(file.path(data_path, "status.csv"))

bmsy <- bmsy %>%
  left_join(status)

col.brks = seq(0,100,length.out=11)
myPalette = colorRampPalette(brewer.pal(11, "Spectral"), space="Lab")
ggplot(bmsy, aes(b_bmsy)) +
  geom_histogram(aes(fill=Status)) +
  facet_wrap(~fao_id) +
  scale_fill_gradientn(colours=myPalette(100))
  
head(meanCatch)  
mutate(fao_id = as.numeric(sapply(strsplit(as.character(stock_id), "_"), function(x)x[2])),
       taxon_name = sapply(strsplit(as.character(stock_id), "_"), function(x)x[1])) %>%



     col=brewer.pal(10, 'RdYlBu')[cut(v, col.brks, labels=1:10, include.lowest=TRUE)])

## plot
ggplot(totalCatch_fao_taxon, aes(x= as.factor(fao_id), y=Catch_fao_taxon, fill=TaxonGroup)) +
  geom_bar(stat = "identity") +
  labs(y = "Total Catch, 2011", x = "FAO region") + 
  myTheme
ggsave("catch_fao_taxon.png")

ggplot(totalCatch_fao_taxon, aes(x= as.factor(fao_id), y=propCatch_fao_taxon, fill=TaxonGroup)) +
  geom_bar(stat = "identity") +
  labs(y = "Total Catch, 2011", x = "FAO region") + 
  myTheme
ggsave("propCatch_fao_taxon.png")
              

#############################################
## Total species data
#############################################

## Total Catch per fao_id
totalSpecies_fao <- country.level.data %.%
  filter(year==2011) %.%
  group_by(fao_id) %.%
  summarize(species_fao = length(Catch))

totalSpecies_fao_taxon <- country.level.data %.%
  filter(year==2011) %.%
  group_by(fao_id, TaxonGroup) %.%
  summarize(Species_fao_taxon = length(Catch)) %.%
  join(totalSpecies_fao) %.% 
  filter(!(fao_id %in% c(88, 48, 58))) %.%
  mutate(propCatch_fao_taxon = round(Species_fao_taxon/species_fao, 3)) %.%
  arrange(species_fao, fao_id, TaxonGroup)

## plot
ggplot(totalSpecies_fao_taxon, aes(x= as.factor(fao_id), y=Species_fao_taxon, fill=TaxonGroup)) +
  geom_bar(stat = "identity") +
  labs(y = "Total species, 2011", x = "FAO region") + 
  myTheme
ggsave("species_fao_taxon.png")

ggplot(totalCatch_fao_taxon, aes(x= as.factor(fao_id), y=propCatch_fao_taxon, fill=TaxonGroup)) +
  geom_bar(stat = "identity") +
  labs(y = "Total Catch, 2011", x = "FAO region") + 
  myTheme
ggsave("propCatch_fao_taxon.png")
            