####################################################################
## Natural Products (NP) calculation (Part 1):
## Data preparation 
## Uses the same data as FIS, but includes only Eupausia superba (krill)

####################################################################
library(dplyr)
library(reshape2)
library(stringr)

rm(list=ls())
setwd("N:\\model\\GL-AQ-NaturalProducts_v2013")

## standardized region labels:
regions <- read.csv("N:/git-annex/Global/NCEAS-Antarctica-Other_v2014/rgn_labels_ccamlr.csv")

#---------------------------------------------------------
## CMSY data ----
#---------------------------------------------------------
## formatting and converting c_cmsy data to score:
cmsy <- read.csv("raw/compiled_C_Cmsy_krill.csv")
cmsy <- cmsy %.%
  select(taxon_name=SciName, fao_id=area, year=year, C_msy_assmt) %.%
  mutate(fao_id = as.character(fao_id)) %.%
  mutate(fao_id = gsub("\\.", "", fao_id)) %.%
  mutate(fao_id = gsub("a", "1", fao_id)) %.%
  mutate(fao_id = as.numeric(gsub("b", "2", fao_id))) %.%
  select(taxon_name, fao_id, year, C_msy_assmt) %.%
  arrange(taxon_name, fao_id, year) %.%
  filter(!(fao_id %in% "584"))

regions <- regions %.%
  select(fao_id = ccamlr_id, sp_id)

cmsy2 <- merge(cmsy, regions, by="fao_id", all.x=TRUE)
cmsy2 <- subset(cmsy2, select=c(sp_id, taxon_name, year, C_msy_assmt))

write.csv(cmsy2, "tmp\\fnk_np_ccmsy.csv", row.names=FALSE)

# ## experimenting with c/cmsy to score conversion
# eps <- .75 #0.5 original
# theta  <- 0.25 #0.25 originally
# tmp <- data.frame(C_msy_assmt= seq(0,2, by=0.01)) #used to test
# 
# tmp <- tmp %.%
#   mutate(score = ifelse(C_msy_assmt > 1.05, 2-C_msy_assmt, 
#                         ifelse(C_msy_assmt < 0.95, pmax(eps*(1/3+C_msy_assmt), theta),
#                                1)))
# plot(score ~ C_msy_assmt, data=tmp)
# 
# alpha <- 0.5 # a 0 indicates no penalty for underharvesting
# beta <- 0.25 # this is the lowest the underharvesting penalty can get.
# lowerBuffer <- 0.95
# upperBuffer <- 1.05
# 
# ## compare to b_bmsy:
# tmp <- data.frame(b_bmsy= seq(0,5, by=0.01), penalty=1)
# 
# score <- function(data, variable){
# #  data <- tmp
# #  variable <- "b_bmsy"
#   ifelse(data[ ,variable]*data[, "penalty"]<lowerBuffer,
#          data[ ,variable]*data[, "penalty"],
#          ifelse(data[ ,variable]*data[, "penalty"]>upperBuffer,
#                 ifelse(1-alpha*(data[ ,variable]*data[, "penalty"]
#                                 -upperBuffer)>beta,
#                        1-alpha*(data[ ,variable]*data[, "penalty"]-upperBuffer),beta),
#                 1))
# }
# 
# tmp$score <- score(data=tmp, variable="b_bmsy")
# plot(score ~ b_bmsy, data=tmp)

#---------------------------------------------------------
## Catch data ----
#---------------------------------------------------------

# regions with catch of 0 during past 2 years = NA

catchData <- read.csv("N:\\model\\GL-HS-AQ-Fisheries_v2013\\Antarctica\\raw\\CCAMLR_w_update_v2.csv") # N=3236

catchData <- catchData[catchData$ScientificName %in% "Euphausia superba" , ]

catchData  <- catchData %.%
  filter(!(ASD %in% c(41, 48, 58, 88))) %.% #N=3168
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

#write.csv(country.level.data, "tmp\\cnk_np_meancatch.csv", row.names=FALSE)

last2years <- country.level.data %.%
  filter(year %in% c(2009, 2010, 2011, 2012)) %.%
  group_by(fao_saup_id) %.%
  summarize(catch2011_2012=sum(catch))

zeroCount <- country.level.data %.%
  group_by(fao_saup_id) %.%
  summarize(totalRecords=length(catch),
            zeroRecords = sum(catch==0))


# ------------------------------------------------------------------
# Comparing catch to scores as a double check of data
# ------------------------------------------------------------------

c <- read.csv("tmp\\cnk_np_meancatch.csv")

status.year <- 2011
trend.years <- (status.year-4):status.year

# catch data
c$fao_id <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[1]))
c$saup_id <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[2]))
c$taxon_name <- sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[1])
c$TaxonKey <- as.numeric(sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[2]))

setdiff(b$taxon_name, c$taxon_name)
setdiff(c$taxon_name, b$taxon_name)
intersect(c$taxon_name, b$taxon_name)

scores <- merge(c, b, all.x=TRUE,
                by=c("taxon_name", "year", "fao_id"))
scores  <- scores %.%
  arrange(fao_id, year)

