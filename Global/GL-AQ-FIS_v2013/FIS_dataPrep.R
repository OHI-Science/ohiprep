####################################################################
## Fisheries (FIS) calculation (Part 1):
## Data preparation 
####################################################################
source('~/ohiprep/src/R/common.R')

library(dplyr)
library(reshape2)
library(stringr)

data_files <- "Global/GL-AQ-FIS_v2013"

#---------------------------------------------------------
## BMSY data ----
#---------------------------------------------------------
# b_bmsy values generated using cmsy_ohi.R script
b_bmsy_all <- read.csv(file.path(data_files, "tmp/b_bmsy_AQ_with_zeros.csv")) 


# We want to exclude taxa with <7 years of non-zero data (determine these species with these data):
b_bmsy_catch_data <- read.csv(file.path(data_files, "raw/CCAMLR_t_LHcorr_v2.csv"))
b_bmsy_catch_data <- b_bmsy_catch_data %.%
  filter(Catch != 0) %.%
  group_by(ScientificName) %.%
  summarize(count=length(season.year))

species <- b_bmsy_catch_data$ScientificName[b_bmsy_catch_data$count >= 10]

b_bmsy_all <- b_bmsy_all %.%
  select(taxon_name=stock_id, b_bmsy, year) %.%
  filter(taxon_name %in% species)

write.csv(b_bmsy_all, file.path(data_files, "tmp/fnk_fis_b_bmsy.csv"), row.names=FALSE)

#---------------------------------------------------------
## CMSY data: 3 additional taxa (these have c/cmsy data) ----
#---------------------------------------------------------
cmsy <- read.csv("raw/Ant_C_Cmsy .csv")
cmsy <- cmsy %.%
  select(taxon_name=ScientificName, fao_id=area, year=season.year, C_msy_assmt) %.%
  group_by(taxon_name, fao_id, year) %.%
  summarise(C_msy_assmt = mean(C_msy_assmt, na.rm=TRUE))  #some multiple scores that need to be averaged

cmsy <- data.frame(cmsy)

cmsy <- cmsy %.%
  mutate(fao_id = as.character(fao_id)) %.%
  mutate(fao_id = gsub("\\.", "", fao_id)) %.%
  mutate(fao_id = gsub("a", "1", fao_id)) %.%
  mutate(fao_id = as.numeric(gsub("b", "2", fao_id)))

## appears the best route is to use the 2011 data for both 2011 and 2012,
## (rather than linear model to predict 2012) 

## adding in 2012 ccmsy estimates based on 2011 data for taxon/regions
new <- data.frame(taxon_name="Champsocephalus gunnari", fao_id=c(483, 5852), year=2012, C_msy_assmt=c(0.18, 0.13))
cmsy <- rbind(cmsy, new)


write.csv(cmsy, "tmp\\fnk_fis_ccmsy.csv", row.names=FALSE)

#---------------------------------------------------------
## Catch data ----
#---------------------------------------------------------

catchData <- read.csv("raw\\CCAMLR_w_update_v2.csv") # N=3236

# some of the sites do not correspond with current sites.  Removing these:

catchData[catchData$ASD %in% c(41, 48, 58, 88, 584, 585, 4132, 5843, 5844),]

catchData  <- catchData %.%
  filter(!(ASD %in% c(41, 48, 58, 88, 584, 585, 4132, 5843, 5844))) %.% #N=3071
  select(year=season.year, ccamlr=ASD, ScientificName, Tax_Lev, corr.Catch2) %.%
  mutate(ccamlr = gsub("a", "1", ccamlr)) %.%
  mutate(ccamlr = as.numeric(gsub("b", "2", ccamlr))) %.%
  arrange(ccamlr, ScientificName, year)

### Gap-fill data so that missing years are 0 (after the taxa was first recorded as catch)

#make a data frame with all years ever sampled in all regions and order by year
year <- data.frame(year=unique(catchData$year))
year$year <- year$year[order(year$year)]

#get the unique list of ccamlr regions
ccamlrRegions <- unique(catchData$ccamlr)

country.level.data <- data.frame()
#loop through each ccmlrRegion and calculate values
for(i in 1:length(ccamlrRegions)){
#  i <- 1 #testing
  region <- ccamlrRegions[i]
  tmpdata <- subset(catchData,   ccamlr==region) #subset to each ccamlrRegion
  #merge subset to tmp to add all available years
  newtmpdata <- merge(year, tmpdata, all.x=T)
  
  #cast to a matrix of year by species
  castdata <- acast(newtmpdata, ScientificName + ccamlr + Tax_Lev ~ year, mean, value.var="corr.Catch2")
  
  for(j in 1:dim(castdata)[1]){ # for each row
    for(k in 2:dim(castdata)[2]){ #for each col
      #if previous and current are NA, keep current as NA
      if(is.na(castdata[j,k-1]) & is.na(castdata[j,k])) {
        castdata[j,k]<-NA
        next
      }
      #if previous is not an NA but current is NA, make current a 0
      if(!is.na(castdata[j,k-1]) & is.na(castdata[j,k])) {
        castdata[j,k] <- 0
      }
    }  
  }
  
  meltData <- melt(castdata)

  
meltData$fao_saup_id = paste(sapply(str_split(meltData$Var1, "_"), function(x)x[[2]]), 0, sep="_")
meltData$taxon_name = sapply(str_split(meltData$Var1, "_"), function(x)x[[1]])
meltData$key = sapply(str_split(meltData$Var1, "_"), function(x)x[[3]])
meltData$taxon_name_key = paste(meltData$taxon_name, meltData$key, sep="_")

meltData <- meltData %.%
  filter(!is.na(value)) %.%
  select(fao_saup_id, taxon_name_key, year=Var2, catch=value)
  
country.level.data <- rbind(country.level.data, meltData)
  
}       

# Just a check dataset:
#write.csv(country.level.data, "tmp\\CCAMLR_w_update_zeroPadded.csv", row.names=FALSE)


# Calculate mean catch over all years per taxon and saup_id/fao_id
MeanCatch <- country.level.data %.%
 group_by(fao_saup_id, taxon_name_key) %.% 
 summarize(mean_catch=mean(catch, na.rm=TRUE))

country.level.data <- merge(country.level.data, MeanCatch, by=c("fao_saup_id", "taxon_name_key"))

country.level.data <- country.level.data %.%
  filter(mean_catch != 0) %.% #cut catch with zero values
  filter(year>2005) %.%
  arrange(fao_saup_id, year, taxon_name_key)

write.csv(country.level.data, "tmp\\cnk_fis_meancatch.csv", row.names=FALSE)

