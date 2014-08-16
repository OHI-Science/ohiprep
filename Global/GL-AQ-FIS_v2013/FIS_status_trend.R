####################################################################
## Calculate Status/Trend for Antarctica
## April 1 2014 
####################################################################
rm(list=ls())

### MeanCatch: The catch data averaged across years (>= 1980) for each Taxon/region. 
source('~/ohiprep/src/R/common.R')

library(plyr)
library(reshape2)
library(dplyr)

data_files <- "Global/GL-AQ-FIS_v2013"

regions <- read.csv(file.path(dir_neptune_data, 
                              "git-annex/Global/NCEAS-Antarctica-Other_v2014/rgn_labels_ccamlr.csv"))
c <- read.csv(file.path(data_files, "tmp/cnk_fis_meancatch.csv"))
b <- read.csv(file.path(data_files, "tmp/fnk_fis_b_bmsy.csv"))
extra_ccmsy <- read.csv(file.path(data_files, "tmp/fnk_fis_ccmsy.csv"))

status.year <- 2012
trend.years <- (status.year-4):status.year
 
# catch data
c$fao_id <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[1]))
c$saup_id <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[2]))
c$taxon_name <- sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[1])
c$TaxonKey <- as.numeric(sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[2]))

setdiff(b$taxon_name, c$taxon_name)
setdiff(c$taxon_name, b$taxon_name)
intersect(c$taxon_name, b$taxon_name)
    
# ------------------------------------------------------------------------
# STEP 1. Merge the species status data with catch data
#     AssessedCAtches: only taxa with catch status data
# -----------------------------------------------------------------------
AssessedCatches <- join(b, c, 
                        by=c("taxon_name", "year"), type="inner")

# include only taxa with species-level data
AssessedCatches <- AssessedCatches[as.numeric(AssessedCatches$TaxonKey)>=6, ]
AssessedCatches$penalty <- 1


# ------------------------------------------------------------------------
# STEP 2. Estimate status data for catch taxa without species status
#     UnAssessedCatches: taxa with catch status data
# -----------------------------------------------------------------------

UnAssessedCatches <- c[!(c$year %in% AssessedCatches$year &
                           c$taxon_name %in% AssessedCatches$taxon_name), ]

# 2a.  Join UnAssessedCatches data to the b_bmsy summaries for each FAO/Year

# Average status data for assessed stocks by FAO region for each year. 
# This is used as the starting estimate for unassesed stocks
# Here, the Median b_bmsy was chosen for TaxonKey >= 600000 
# and for TaxonKey < 600000

#  *************NOTE *****************************
#  Using the minimum B/BMSY score as an starting point
#  for the estimate of B/BMSY for unassessed taxa not
#  identified to species level is very conservative.
#  This is a parameter that can be changed.
#  ***********************************************

b_summary <- ddply(b, .(year), summarize,
                   Medianb_bmsy=quantile(as.numeric(b_bmsy), probs=c(0.5)), 
                   Minb_bmsy=min(as.numeric(b_bmsy))) 
#minimum b_bmsy was used in 2013 OHI data to provide a conservative estimate of the b_bmsy for taxa this
# could not be determined for using cmsy.  We now use the median to estimate b_bmsy for these taxa (OHI 2014, High seas, Antarctica).

UnAssessedCatches <- join(UnAssessedCatches, b_summary, by=c("year"),
                          type="left", match="all")


# 2c. Create a penalty table for taxa not identified to species level
#  *************NOTE *****************************
#  In some cases, it may make sense to alter the 
#  penalty for not identifying fisheries catch data to
#  species level.
#  ***********************************************

penaltyTable <- data.frame(TaxonKey=1:6, 
                           penalty=c(0.01, 0.25, 0.5, 0.8, 0.9, 1))
# 2d.Merge with data
UnAssessedCatches <- join(UnAssessedCatches, penaltyTable, by="TaxonKey")

# ------------------------------------------------------------------------
# STEP 3. Calculate score for all taxa based on status (b/bmsy) and taxa
# -----------------------------------------------------------------------

#  *************NOTE *****************************
#  These values can be altered
#  ***********************************************
alpha <- 0.5 # a 0 indicates no penalty for underharvesting
beta <- 0.25 # this is the lowest the underharvesting penalty can get.
lowerBuffer <- 0.95
upperBuffer <- 1.05


## Function to calculate score for different scenarios:
score <- function(data, variable){
  #data <- AssessedCatches
  #variable <- "bmsy"
  ifelse(data[ ,variable]*data[, "penalty"]<lowerBuffer,
         data[ ,variable]*data[, "penalty"],
         ifelse(data[ ,variable]*data[, "penalty"]>upperBuffer,
                ifelse(1-alpha*(data[ ,variable]*data[, "penalty"]
                                -upperBuffer)>beta,
                       1-alpha*(data[ ,variable]*data[, "penalty"]-upperBuffer),beta),
                1))
}

AssessedCatches$score <- score(data=AssessedCatches, variable="b_bmsy")

# Median is used to calculate score for species with Taxon 6 coding 
# Not really necessary to separate in this case because "Median_bmsy" is used in both
# cases, but will maintain code in case this approach changes:
UnAssessedCatchesT6 <- subset(UnAssessedCatches, penalty==1)
UnAssessedCatchesT6$score <- score(UnAssessedCatchesT6, "Medianb_bmsy")

UnAssessedCatches <- subset(UnAssessedCatches, penalty!=1)
UnAssessedCatches$score <- score(UnAssessedCatches, "Medianb_bmsy") #this used to be the Minb_bmsy 

scores <- rbind(AssessedCatches[,c("taxon_name", "TaxonKey", "year", "fao_id", "mean_catch","score")],
                UnAssessedCatchesT6[,c("taxon_name", "TaxonKey", "year", "fao_id", "mean_catch","score")],
                UnAssessedCatches[,c("taxon_name", "TaxonKey", "year", "fao_id", "mean_catch","score")])

# ------------------------------------------------------------------------
# these species have c/cmsy assessments and their scores are generated using a different method:
# ------------------------------------------------------------------------
## Fill in missing years/regions using mean of data
# determine mean for each species/year and apply to missing data
meanCMSY <- extra_ccmsy %.%
  group_by(taxon_name, year) %.%
  summarize(mean_cmsy = mean(C_msy_assmt, na.rm=TRUE))


scoresReplace <- scores %.%
  filter(taxon_name %in% c("Dissostichus mawsoni", "Champsocephalus gunnari", "Dissostichus eleginoides")) %.%
  mutate(fao_id = as.integer(fao_id)) %.%
  left_join(extra_ccmsy, by=c("taxon_name", "fao_id", "year")) %.%
  left_join(meanCMSY, by=c("taxon_name", "year"))

scoresReplace$C_msy_assmt2 <- ifelse(is.na(scoresReplace$C_msy_assmt), scoresReplace$mean_cmsy, scoresReplace$C_msy_assmt)


## calculate score based on c_cmsy
eps <- .25 
score_range  <- 1-0.25
value_range <- 0.90-0

scoresReplace$score = ifelse(scoresReplace$C_msy_assmt2 > 1.0, 2.0-scoresReplace$C_msy_assmt2, 
                        ifelse(scoresReplace$C_msy_assmt2 < 0.9, eps + score_range/value_range * scoresReplace$C_msy_assmt2, 1)) 

scoresReplace <- scoresReplace %.%
  select(taxon_name, TaxonKey, year, fao_id, mean_catch, score)

## replace old scores with newly calculated ccmsy

scores <- scores[!(scores$taxon_name %in% c("Dissostichus mawsoni", "Champsocephalus gunnari", "Dissostichus eleginoides")), ]

scores <- rbind(scores, scoresReplace)

scores <- scores[scores$year %in% trend.years, ]

# ------------------------------------------------------------------------
# STEP 4. Calculate status for each saup_id region
# -----------------------------------------------------------------------

# 4a. To calculate the weight (i.e, the relative catch of each stock per fao_id),
# the mean catch of taxon is divided by the   
# sum of mean catch of all species in region r, which is calculated as: 

smc <- ddply(.data = scores, .(year, fao_id), summarize, 
             SumCatch = sum(mean_catch)) 

scores<-join(scores,smc,by=c("year","fao_id"))

scores$wprop<-scores$mean_catch/scores$SumCatch 


#  4b. The "score" and "weight" values per taxon per SAUP region are used to  
#    calculate a geometric weighted mean across taxa for each saup_id region
StatusData <- ddply(.data = scores, .(fao_id, year), summarize, Status = prod(score^wprop)) 

### standardized region names
region <- regions %.%
  select(fao_id=ccamlr_id, sp_id)

StatusData <- StatusData %.%
  mutate(fao_id = as.integer(fao_id)) %.%
  left_join(region, by="fao_id") 

# ------------------------------------------------------------------------
# STEP 5. Status  
# -----------------------------------------------------------------------
# 2013 status is based on 2011 data (most recent data)
Status <- StatusData[StatusData$year==status.year, ]
Status$status <- round(Status$Status*100, 2)
Status <- subset(Status, select=c("sp_id", "status"))

write.csv(Status, file.path(data_files, "data/FISstatus.csv"), row.names=F, na="")

StatusData <- data.frame(StatusData)

# ------------------------------------------------------------------------
# STEP 6. Calculate trend  
# -----------------------------------------------------------------------
# NOTE: Status is rounded to 2 digits before trend is 
# calculated in order to match OHI 2013 results (is this what we want to do?)
TrendData = plyr::ddply(
  StatusData, "sp_id", 
  function(x) lm(Status ~ year, data=x)$coefficients[['year']] * 5)

TrendData <- TrendData %.%
  select(sp_id, trend=V1)

write.csv(TrendData, file.path(data_files, "data/FIStrend.csv"), row.names=F, na='')

