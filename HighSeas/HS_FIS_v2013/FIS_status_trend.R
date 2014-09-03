####################################################################
## Calculate Status/Trend for High Seas
## April 1 2014 
##
## Fisheries (FIS) calculation:
## STEP 1. Merge the species status data with the catch data
## STEP 2. Estimate status data for catch taxa without species status
## STEP 3. Calculate score for all taxa based on status 
## STEP 4. Calculate status 
## STEP 5. Convert from saup spatial scale to OHI spatial scale 
## STEP 6. Calculate trend
####################################################################

### MeanCatch: The catch data averaged across years (>= 1980) for each Taxon/region. 

rm(list=ls())
library(plyr)
library(reshape2)

data_dir <- "HighSeas/HS_FIS_v2013/data"

c <- read.csv(file.path(data_dir, "cnk_fis_meancatch.csv"))
b <- read.csv(file.path(data_dir, "fnk_fis_b_bmsy.csv"))

status_year <- 2011
trend_years <- (status_year-5):status_year
 
# catch data
c$fao_id <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[1]))
c$saup_id <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[2]))
c$taxon_name <- sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[1])
c$TaxonKey <- as.numeric(sapply(strsplit(as.character(c$taxon_name_key), "_"), function(x)x[2]))
#Create Identifier for linking assessed stocks with country-level catches
c$stock_id <- paste(as.character(c$taxon_name),
                    as.character(c$fao_id), sep="_")


# b_bmsy data
  # Identifier taxa/fao region:
  b$stock_id <- paste(b$taxon_name, b$fao_id, sep="_")
    
# ------------------------------------------------------------------------
# STEP 1. Merge the species status data with catch data
#     AssessedCAtches: only taxa with catch status data
# -----------------------------------------------------------------------
AssessedCatches <- join(b, c, 
                        by=c("stock_id", "year"), type="inner")

# include only taxa with species-level data
AssessedCatches <- AssessedCatches[as.numeric(AssessedCatches$TaxonKey)>=600000, ]
AssessedCatches$penalty <- 1

## Data exploration:
# table(AssessedCatches$fao_id, AssessedCatches$year)
# tmp_A <- AssessedCatches %.%
#   group_by(fao_id, year) %.%
#   summarize(assessed = sum(mean_catch))

# ------------------------------------------------------------------------
# STEP 2. Estimate status data for catch taxa without species status
#     UnAssessedCatches: taxa with catch status data
# -----------------------------------------------------------------------

UnAssessedCatches <- c[!(c$year %in% AssessedCatches$year &
                           c$stock_id %in% AssessedCatches$stock_id), ]
# table(UnAssessedCatches$fao_id, UnAssessedCatches$year)
# tmp_U <- UnAssessedCatches %.%
#   group_by(fao_id, year) %.%
#   summarize(unassessed = sum(mean_catch))
# 
# tmp <- merge(tmp_A, tmp_U, all.x=TRUE)
# tmp <- subset(tmp, year==2011)
# tmp$propUnAssessed <- tmp$unassessed/(tmp$unassessed + tmp$assessed) * 100
# write.csv(tmp, "N:\\model\\GL-HS-AQ-Fisheries_v2013\\HighSeas\\qa_qc\\ProportionUnassessed.csv", row.names=FALSE)


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

b_summary <- ddply(b, .(fao_id, year), summarize,
                   Medianb_bmsy=quantile(as.numeric(b_bmsy), probs=c(0.5)), 
                   Minb_bmsy=min(as.numeric(b_bmsy))) 
#minimum b_bmsy was used in 2013 OHI data to provide a conservative estimate of the b_bmsy for taxa this
# could not be determined for using cmsy.  We now use the median to estimate b_bmsy for these taxa (OHI 2014, High seas, Antarctica).
# For Fiji, the 25th quantile was used.

UnAssessedCatches <- join(UnAssessedCatches, b_summary, by=c("fao_id", "year"),
                          type="left", match="all")

# 2b.  Create a penalty variable based on taxa level:
UnAssessedCatches$TaxonPenaltyCode <- substring(UnAssessedCatches$TaxonKey,1,1)

# 2c. Create a penalty table for taxa not identified to species level
#  *************NOTE *****************************
#  In some cases, it may make sense to alter the 
#  penalty for not identifying fisheries catch data to
#  species level.
#  ***********************************************
penaltyTable <- data.frame(TaxonPenaltyCode=1:6, 
                           penalty=c(0.01, 0.25, 0.5, 0.8, 0.9, 1))

# 2d.Merge with data
UnAssessedCatches <- join(UnAssessedCatches, penaltyTable, by="TaxonPenaltyCode")

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
UnAssessedCatchesT6 <- subset(UnAssessedCatches, penalty==1)
UnAssessedCatchesT6$score <- score(UnAssessedCatchesT6, "Medianb_bmsy")

UnAssessedCatches <- subset(UnAssessedCatches, penalty!=1)
UnAssessedCatches$score <- score(UnAssessedCatches, "Medianb_bmsy") #this used to be the Minb_bmsy 

scores <- rbind(AssessedCatches[,c("taxon_name", "TaxonKey", "year", "fao_id", "mean_catch","score")],
                UnAssessedCatchesT6[,c("taxon_name", "TaxonKey", "year", "fao_id", "mean_catch","score")],
                UnAssessedCatches[,c("taxon_name", "TaxonKey", "year", "fao_id", "mean_catch","score")])


# ------------------------------------------------------------------------
# STEP 4. Calculate status for each saup_id region
# -----------------------------------------------------------------------

# 4a. To calculate the weight (i.e, the relative catch of each stock per fao_id),
# the mean catch of taxon i is divided by the   
# sum of mean catch of all species in region r, which is calculated as: 

smc <- ddply(.data = scores, .(year, fao_id), summarize, 
             SumCatch = sum(mean_catch)) 

scores<-join(scores,smc,by=c("year","fao_id"))

scores$wprop<-scores$mean_catch/scores$SumCatch 


#  4b. The "score" and "weight" values per taxon per SAUP region are used to  
#    calculate a geometric weighted mean across taxa for each saup_id region
StatusData <- ddply(.data = scores, .(fao_id, year), summarize, Status = prod(score^wprop)) 


# ------------------------------------------------------------------------
# STEP 5. Status  
# -----------------------------------------------------------------------
# 2013 status is based on 2011 data (most recent data)
Status <- StatusData[StatusData$year==status_year, ]
Status$Status <- round(Status$Status*100, 2)
Status <- subset(Status, select=c("fao_id", "Status"))

write.csv(Status, file.path(data_dir, "status.csv"), row.names=FALSE, na="")


# ------------------------------------------------------------------------
# STEP 6. Calculate trend  
# -----------------------------------------------------------------------
# NOTE: Status is rounded to 2 digits before trend is 
# calculated in order to match OHI 2013 results (is this what we want to do?)
trend = ddply(StatusData, .(fao_id), function(x){
  mdl = lm(Status ~ year, data=x)
  data.frame(
    score     = round(coef(mdl)[['year']] * 5, 2),
    dimension = 'trend')}) %.%
  select(region_id=fao_id, dimension, score)

write.csv(trend, file.path(data_dir, "trend.csv"), row.names=FALSE, na='')

# For quick view;
ids <- read.csv("C:\\Users\\Melanie\\Desktop\\GL-NCEAS-Regions_v2014\\FAOregions.csv")
StatusID <- merge(ids, Status, by="fao_id")
StatusID <- subset(StatusID, !(StatusID$rgn_nam %in% c("Atlantic, Antarctic", "Pacific, Antarctic")))

StatusID <- merge(StatusID, Trend, by="fao_id")
write.csv(StatusID, "data\\HighSeasFIS.csv", row.names=FALSE)

