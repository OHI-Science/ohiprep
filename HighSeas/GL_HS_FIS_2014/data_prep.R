##############################################################################
############### Preparing data for high seas fishery analysis ########
##############################################################################
library(gdata)
library(stringr)

source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)

dir = '../ohiprep/HighSeas/GL_HS_FIS_2014' # set folder where files are saved
dir_nept  <- file.path(dir_neptune_data, 'git-annex/Global/SAUP-Fisheries_v2011/raw') #where raw catch data are located
dir_global <- '../ohiprep/Global/NCEAS-Fisheries_2014a' # where additional catch data are located
  
#############################################
## Mean catch data ----
#############################################


## Step 1. ## upload SAUP dat 
########################################################################
# load the new catch data
nS <- read.delim(file.path(dir_nept, 'Extended catch data 1950-2011_18 July 2014.txt')) ; head(nS)

# load species names lookup table - it is important that this be the same as CMSY data prep
tax <- read.csv(file.path(dir_global, 'tmp/TaxonLookup.csv')) ; head(tax)

nS <- nS %>%
  left_join(tax[,1:2]) %>% # add species names # Joining by: "Taxonkey"
  mutate(stock_id = paste(TaxonName, FAO, sep="_")) # create a unique stock id name for every species/FAO region pair
# N = 902,448
########################################################################
## Step 2. ## select eez data (EEZ == 0) and year>=1980 for averages
## and correct: 
# ISSUE1: need to sum catch to remove duplicate 'Marine fishes not identified', 'Shrimps and prawns', 'Sharks rays and chimaeras' for the same year, saup_id
# multiple taxonkeys (but unique common.names!) are associated with each of these 3 taxonnames - 
# the data was prepared using taxonName, Taxonkey was joined later, then TaxonName was removed)
# ISSUE2: need to keep 'TLevel' as a grouping variable in case part of the catch assigned to a taxon should be given a new taxonkey (see step 3)

nS <- nS %>%
  filter(EEZ == 0,
         IYear >= 1980) %>%  # N = 32,848
  select(year=IYear, saup_id=EEZ, fao_id=FAO, stock_id, Taxonkey, TaxonName, Catch, TLevel) %>%
  group_by(year, saup_id, fao_id, stock_id, Taxonkey, TaxonName, TLevel) %>%
  summarise(Catch=sum(Catch))  # N = 32,640

## Step 3 # Recode TaxonKey such that the FAO TaxonKey takes precedence over SAUP TaxonKey
# to give credit to those who report at higher level than was ultimately reported in the SAUP data.
nS$NewTaxonKey <- ifelse(is.na(nS$TLevel),
                             nS$Taxonkey,
                             100000*nS$TLevel)

## some cleaning
nS$taxon_name_key <- paste(nS$TaxonName, 
                               as.character(nS$NewTaxonKey), sep="_")

# some eezs overlap >1 fao region so I need a stock identifier that combines: taxonkey, fao_id, saup_id
nS$taxon_name_key_id<-paste(nS$taxon_name_key, nS$fao_id, nS$saup_id, sep='_')

################################################
# Calculate mean catch over all years per taxon and saup_id/fao_id 
# (remember: due to new taxonkey, some stock_ids correspond to multiple taxa)
# 

MeanCatch <- nS %>%
  group_by(saup_id, fao_id, stock_id, taxon_name_key) %>% # equivalent to .(stock_id, taxon_name_key_id
  summarise(mean_catch=mean(Catch)) #N = 1721

nS <- join(nS, MeanCatch, by=c("saup_id","fao_id","stock_id","taxon_name_key")); head(nS)

# Final cleaning up:
cnk_fis_meancatch  <- nS %>%
  filter(mean_catch != 0,
         year > 2005) %>%
  mutate(fao_saup_id = paste(fao_id, saup_id, sep="_")) %>%
  select(fao_saup_id, taxon_name_key, year, mean_catch)  # N=6952

# are there duplicate stocks ids per taxon-year-region?
stopifnot(sum(duplicated(cnk_fis_meancatch[,c('fao_saup_id', 'taxon_name_key', 'year')])) == 0 )

# no duplicates! Proceed to save the file
write.csv(cnk_fis_meancatch, file.path(dir, 'data/fnk_fis_meancatch_lyr.csv'), row.names=F, na='')


#############################################
## b/bmsy ----
#############################################
# realized that this will become complicated, but for now here is an intermediate version
## NOTE: all should access the b/bmsy data version from a b_bmsy file, rather than the global fisheries file (change in global eez as well):
##
b_bmsy <- read.csv("Global/NCEAS-Fisheries_2014a/data/fnk_fis_b_bmsy_lyr_constrained_w0_runningMean.csv")

# filter by catch (although, not sure this is necessary)
catch <- read.csv(file.path(dir, 'data/fnk_fis_meancatch_lyr.csv'))
catch$TaxonName  <- sapply(strsplit(as.character(catch$taxon_name_key), "_"), function(x)x[1])
catch$taxon_key  <- sapply(strsplit(as.character(catch$taxon_name_key), "_"), function(x)x[2])
catch$fao_id  <- sapply(strsplit(as.character(catch$fao_saup_id), "_"), function(x)x[1])
catch$saup_id  <- sapply(strsplit(as.character(catch$fao_saup_id), "_"), function(x)x[2])
catch$stock_id  <- paste(catch$TaxonName, catch$fao_id, sep="_")

b_bmsy <- b_bmsy %>%
  filter(stock_id %in% catch$stock_id) %>%
  mutate(taxon_name = sapply(strsplit(as.character(stock_id), "_"), function(x)x[1])) %>%
  mutate(fao_id = sapply(strsplit(as.character(stock_id), "_"), function(x)x[2])) %>%
  select(fao_id, taxon_name, year=yr, b_bmsy)
  
# are there duplicate stocks ids per taxon-year-region?
stopifnot(sum(duplicated(b_bmsy[,c('taxon_name', 'fao_id', 'year')])) == 0 )

# no duplicates! Proceed to save the file
write.csv(b_bmsy, file.path(dir, 'data/fnk_fis_b_bmsy_lyr.csv'), row.names=F, na='')
