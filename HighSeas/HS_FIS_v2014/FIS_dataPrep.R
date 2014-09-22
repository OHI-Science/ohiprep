####################################################################
## Fisheries (FIS) calculation (Part 1):
## Data preparation 
####################################################################

source('../ohiprep/src/R/common.R') # set dir_neptune_data

#---------------------------------------------------------
## Species estimates of b/bmsy
#---------------------------------------------------------
## Using b_bmsy values from OHI 2013 data.
## Now, we are using resilience to determine which
## method of calculating b/bmsy to use.
data_dir <- "HighSeas/HS_FIS_v2014"

# resilience scores to select the appropriate b/bmsy 
res <- read.csv("Global/FIS_Bbmsy/stock_resil_06cutoff_ALL.csv")

bmsy.uniform <- read.csv("Global/NCEAS-Fisheries_2014a/tmp/fnk_fis_b_bmsy_lyr_uniform_no0_runningMean.csv")
bmsy.uniform <- bmsy.uniform %>%
  mutate(fao_id = as.numeric(sapply(strsplit(as.character(stock_id), "_"), function(x)x[2])),
         taxon_name = sapply(strsplit(as.character(stock_id), "_"), function(x)x[1])) %>%
  select(stock_id, fao_id, taxon_name, year=yr, b_bmsy_uniform=b_bmsy)

bmsy.constrained <- read.csv("Global/NCEAS-Fisheries_2014a/tmp/fnk_fis_b_bmsy_lyr_constrained_no0_runningMean.csv")
bmsy <- bmsy.constrained %>%
  select(stock_id, year=yr, b_bmsy_constrained=b_bmsy) %>%
  left_join(bmsy.uniform, by=c("stock_id", "year")) %>%
  left_join(res, by="stock_id") #45 stock (N=744 catch records) do not have b/bmsy data, 
                                #these are data poor stock and this is fine

unique(bmsy$stock_id[is.na(bmsy$unif_prior)])

bmsy_scores <- bmsy %>%
   mutate(b_bmsy=ifelse(unif_prior==1, b_bmsy_uniform, b_bmsy_constrained)) %>%
   filter(!(is.na(unif_prior))) %>%
   select(fao_id, taxon_name, year, b_bmsy)

write.csv(bmsy_scores, file.path(data_dir, 'data/fnk_fis_b_bmsy.csv'), row.names=FALSE)

#---------------------------------------------------------
## Catch data
#---------------------------------------------------------

# load species names lookup table 
tax <- read.csv('../ohiprep/Global/NCEAS-Fisheries_2014a/tmp/TaxonLookup.csv') ; head(tax)

# source(file.path(dir_d,'tmp/CMSY data prep.R'))  
## Step 1. ## get files ex novo
# load the new catch data
country.level.data <- read.delim(file.path(dir_neptune_data,'git-annex/Global/SAUP-Fisheries_v2011/raw/Extended catch data 1950-2011_18 July 2014.txt'))

# select only the EEZ values equal zero (Katie: "When the EEZ field is "0" it indicates open ocean) and
# merge with species names and get rid of duplicates
country.level.data <- country.level.data %>%
  filter(EEZ==0 & IYear>=1980) %>%
  left_join(tax[,1:2]) %>%
#  mutate(stock_id = paste(TaxonName, FAO, sep="_")) %>% # create a unique stock id name for every species/FAO region pair
  mutate(NewTaxonKey = ifelse(is.na(TLevel), Taxonkey, 100000*TLevel)) %>% # Recode TaxonKey such that the FAO TaxonKey takes precedence over the Sea Around Us TaxonKey to give credit to those who report at higher level than was ultimately reported in the SAUP data. 
  select(year=IYear, saup_id=EEZ, fao_id=FAO, TaxonName, Taxonkey, NewTaxonKey, Catch) %>%
  group_by(year, saup_id, fao_id, NewTaxonKey, TaxonName) %>%
  summarize(Catch=sum(Catch)) %>%
  ungroup()

country.level.data <- country.level.data %>%
  group_by(saup_id, fao_id, NewTaxonKey, TaxonName) %>%
  mutate(mean_catch=mean(Catch)) %>%
  ungroup()
  
# Further clean up the data for the toolbox:
country.level.data  <- country.level.data %>%
  filter(mean_catch != 0,
         year > 2005) %>%
  mutate(fao_saup_id = paste(fao_id, saup_id, sep="_"),
         taxon_name_key = paste(TaxonName, NewTaxonKey, sep="_")) %>%
  select(fao_saup_id, taxon_name_key, year, mean_catch)

write.csv(country.level.data, file.path(data_dir, "data/cnk_fis_meancatch.csv"), row.names=FALSE)
tmp <- read.csv(file.path(data_dir, "data/cnk_fis_meancatch.csv"))
id <- paste(tmp$fao_saup_id, tmp$taxon_name_key, tmp$year, sep='_')
sum(duplicated(id))

