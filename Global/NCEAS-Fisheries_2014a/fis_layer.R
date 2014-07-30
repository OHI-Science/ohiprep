####################################################################
## Fisheries (FIS) calculation:
## Data preparation 
## This calculates the saup to fao conversion (from earlier)
## this doesn't need to be updated, but this is the script to do so
####################################################################
library(plyr)
setwd("C:\\Users\\Melanie\\Desktop\\Revising FIS")

# OHI reporting regions are often comprised of multiple saup regions.
# The proportional area of each saup region within each OHI reporting region
# is used to weight the calculated status within each OHI reporting region.

rgn_eez_v2013a <- read.csv("raw\\rgn_2013master.csv")# upload complete list of 2013 regions WITHOUT duplicates (Note: piped lists for some of the region and eez names)  
rgn_eez_v2013a <- rename(rgn_eez_v2013a, c(rgn_nam_2013 = "rgn_nm_2013"))
rgn_eez_v2013a <- rgn_eez_v2013a[rgn_eez_v2013a$rgn_id_2013 < 255,] # exclude disputed regions and open oceans
rgn_eez_v2013a <- rgn_eez_v2013a[rgn_eez_v2013a$rgn_id_2013 != 213,] #exclude Antarctica


saup_v2013a <- read.csv("raw\\saup_eez_v2013a.csv") # saup to ohi 2013 conversion list
saup_v2013a <- rename(saup_v2013a, c(OHI_2013_EEZ_ID="rgn_id_2013", 
                                     OHI_2013_reporting_region="rgn_nm_2013",
                                     SAUP_C_NUM="saup_id")) # same names for join columns
saup_v2013a <- saup_v2013a[!is.na(saup_v2013a$km2), ]

combined_v2013a <- join(subset(rgn_eez_v2013a, select=c("rgn_id_2013", "rgn_nm_2013")),
                        subset(saup_v2013a, select=c(-rgn_nm_2013))) # this is a complete list of ohi 2013a regions

combined_v2013a <- rename(combined_v2013a, c(SAUP_C_NAME="saup_nm", km2="saup_km2"))
combined_v2013a <- subset(combined_v2013a, select=c(rgn_id_2013, rgn_nm_2013, saup_id, saup_nm, saup_km2))
Tot_KmData <- ddply(.data = combined_v2013a, .(rgn_id_2013), summarize, total_km2_rgn = sum(saup_km2)) 
combined_v2013a <- merge(combined_v2013a, Tot_KmData)

# calculate relative area to be used as weight
combined_v2013a$propArea <- combined_v2013a$saup_km2/combined_v2013a$total_km2
combined_v2013a <- subset(combined_v2013a, select=c("saup_id",
                                                    "rgn_id_2013", 
                                                    "propArea"))

write.csv(combined_v2013a, "data\\snk_fis_propArea_saup2rgn.csv", row.names=FALSE)


