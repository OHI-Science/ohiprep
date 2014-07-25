####################################################################
## Fisheries (FIS) calculation:
## STEP 1. Merge the species status data with the catch data
## STEP 2. Estimate status data for catch taxa without species status
## STEP 3. Calculate score for all taxa based on status 
## STEP 4. Calculate status 
## STEP 5. Convert from saup spatial scale to OHI spatial scale 
## STEP 6. Calculate trend
####################################################################

rm(list=ls())
library(plyr)
library(reshape2)

setwd("C:\\Users\\Melanie\\Desktop\\Revising FIS")


layers_data.csv = 'layers_data.csv'


calc.FIS = function(ld.csv=layers_data.csv, 
                    status.csv = file.path(dir.results, sprintf('FIS_status_%s.csv', sfx.scenario)),
                    trend.csv  = file.path(dir.results, sprintf('FIS_trend_%s.csv' , sfx.scenario)),
                      status.year="2011"){

# layers
  lyrs = list('rk'=c('cnk_fis_meancatch'= "catch", 
             'fnk_fis_b_bmsy' = "bmsy", 
             'snk_fis_propArea_saup2rgn'="prop_area"))
  layers = sub('(r|ry|rk)\\.','', names(unlist(lyrs)))
  
# obtain relevant data from layers: 
D = subset(read.csv(ld.csv, na.strings=''), layer %in% layers)

# isolate and organize individual data

# catch data
c  <- dcast(D, id_num + category + year ~ layer, 
                value.var='value_num', na.rm=T,
                subset = .(layer %in% layers[1]))
c <- rename(c, c("id_num"="fao_saup_id", "category"="TaxonNm_Key", lyrs[["rk"]][1])) 
# separate out the region ids:
c$fao_id <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[1]))
c$saup_id <- as.numeric(sapply(strsplit(as.character(c$fao_saup_id), "_"), function(x)x[2]))
c$TaxonName <- sapply(strsplit(as.character(c$TaxonNm_Key), "_"), function(x)x[1])
c$TaxonKey <- as.numeric(sapply(strsplit(as.character(c$TaxonNm_Key), "_"), function(x)x[2]))
c$catch <- as.numeric(c$catch)
c$year <- as.numeric(as.character(c$year))
#Create Identifier for linking assessed stocks with country-level catches
c$stock_id <- paste(as.character(c$TaxonName),
                               as.character(c$fao_id), sep="_")

# b_bmsy data
b  <- dcast(D, id_num + category + year ~ layer, 
            value.var='value_num', na.rm=T,
            subset = .(layer %in% layers[2]))
b <- rename(b, c("id_num"="fao_id", "category"="TaxonName", lyrs[["rk"]][2])) 
# Identifier taxa/fao region:
b$stock_id <- paste(b$TaxonName, b$fao_id, sep="_")
b$bmsy <- as.numeric(b$bmsy)
b$fao_id <- as.numeric(as.character(b$fao_id))
b$year <- as.numeric(as.character(b$year))

# area data for saup to rgn conversion
a  <- dcast(D, id_num + category  ~ layer, 
            value.var='value_num', na.rm=T,
            subset = .(layer %in% layers[3]))

a <- rename(a, c("id_num"="saup_id", "category"="rgn_id", lyrs[["rk"]][3])) 
a$prop_area <- as.numeric(a$prop_area)
a$saup_id <- as.numeric(as.character(a$saup_id))
a$rgn_id <- as.numeric(as.character(a$rgn_id))

# ------------------------------------------------------------------------
# STEP 1. Merge the species status data with catch data
#     AssessedCAtches: only taxa with catch status data
# -----------------------------------------------------------------------
AssessedCatches <- join(b, c, 
                        by=c("stock_id", "year"), type="inner")

# include only taxa with species-level data
AssessedCatches <- AssessedCatches[as.numeric(AssessedCatches$TaxonKey)>=600000, ]
AssessedCatches$penalty <- 1

# ------------------------------------------------------------------------
# STEP 2. Estimate status data for catch taxa without species status
#     UnAssessedCatches: taxa with catch status data
# -----------------------------------------------------------------------

UnAssessedCatches <- c[!(c$year %in% AssessedCatches$year &
                                      c$stock_id %in% AssessedCatches$stock_id), ]

# 2a.  Join UnAssessedCatches data to the b_bmsy summaries for each FAO/Year

# Average status data for assessed stocks by FAO region for each year. 
# This is used as the starting estimate for unassesed stocks
# Here, the Median b_bmsy was chosen for TaxonKey >= 600000 
# and Min b_bmsy for TaxonKey < 600000
#  *************NOTE *****************************
#  Using the minimum B/BMSY score as an starting point
#  for the estimate of B/BMSY for unassessed taxa not
#  identified to species level is very conservative.
#  This is a parameter that can be changed.
#  ***********************************************

b_summary <- ddply(b, .(fao_id, year), summarize,
                       Medianb_bmsy=quantile(as.numeric(bmsy), probs=c(0.5)), 
                       Minb_bmsy=min(as.numeric(bmsy)))

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
                           penalty=c(0.01, 0.1, 0.25, 0.5, 0.75, 1))
# 2d.Merge with data
UnAssessedCatches <- join(UnAssessedCatches, penaltyTable, by="TaxonPenaltyCode")

# ------------------------------------------------------------------------
# STEP 3. Calculate score for all taxa based on status (b/bmsy) and taxa
# -----------------------------------------------------------------------

#  *************NOTE *****************************
#  These values can be altered
#  ***********************************************
alpha <- 0.5
beta <- 0.25
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

AssessedCatches$score <- score(data=AssessedCatches, variable="bmsy")
  
# Median is used to calculate score for species with Taxon 6 coding
UnAssessedCatchesT6 <- subset(UnAssessedCatches, penalty==1)
UnAssessedCatchesT6$score <- score(UnAssessedCatchesT6, "Medianb_bmsy")

UnAssessedCatches <- subset(UnAssessedCatches, penalty!=1)
UnAssessedCatches$score <- score(UnAssessedCatches, "Minb_bmsy")
  
scores <- rbind(AssessedCatches[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")],
                         UnAssessedCatchesT6[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")],
                         UnAssessedCatches[,c("TaxonName", "TaxonKey", "year", "fao_id", "saup_id", "catch","score")])
                          


# ------------------------------------------------------------------------
# STEP 4. Calculate status for each saup_id region
# -----------------------------------------------------------------------

# 4a. To calculate the weight (i.e, the relative catch of each stock per saup_id),
# the mean catch of taxon i is divided by the   
# sum of mean catch of all species in region r, which is calculated as: 

smc <- ddply(.data = scores, .(year, saup_id), summarize, 
             SumCatch = sum(catch)) 

scores<-join(scores,smc,by=c("year","saup_id"))

scores$wprop<-scores$catch/scores$SumCatch 


#  4b. The "score" and "weight" values per taxon per SAUP region are used to  
#    calculate a geometric weighted mean across taxa for each saup_id region
geomMean <- ddply(.data = scores, .(saup_id, year), summarize, status_saup = prod(score^wprop)) 

# ------------------------------------------------------------------------
# STEP 5. Convert status from saup spatial scale to OHI spatial scale  
# -----------------------------------------------------------------------
# In many cases the ohi reporting regions are comprised of multiple saup regions.
# To correct for this, the proportion of each saup area of the total area of the 
# OHI region was calculated. This was used to calculate Status from the Status_saup.
# This type of adjustment is omitted if the data were collected at the same spatial 
# scale as the collecting region.

# Join region names/ids to Geom data
geomMean <- join(a, geomMean, type="inner", by="saup_id") # merge km2 of shelf area with status results

# weighted mean scores
StatusData <- ddply(.data = geomMean, .(rgn_id, year), summarize, Status = sum(status_saup*prop_area))

# 2013 status is based on 2011 data (most recent data)
Status <- StatusData[StatusData$year==status.year, ]
Status$Status <- round(Status$Status*100, 0)
Status <- subset(Status, select=c("rgn_id", "Status"))
OHIregions <- data.frame(rgn_id=unique(a$rgn_id))
Status <- merge(OHIregions, Status, all=TRUE, by="rgn_id")


write.csv(Status, status.csv, row.names=F, na="")


# ------------------------------------------------------------------------
# STEP 6. Calculate trend  
# -----------------------------------------------------------------------
# NOTE: Status is rounded to 2 digits before trend is 
# calculated in order to match OHI 2013 results (is this what we want to do?)
Trend = ddply(
  StatusData, .(rgn_id), summarize,
  Trend = lm(round(Status, 2) ~ year)$coefficients[['year']] * 5
)
Trend <- join(OHIregions, Trend)
write.csv(Trend, trend.csv, row.names=F, na='')

}



