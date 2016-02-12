###########################################
## Converting Molnar invasive data into 
## alien invasive pressure for Antarctica
## May 2 2014, MRF
###########################################

library(dplyr)

### Calculating invasive pressures for Antarctica ----
### see below for details of where data came from:
invasives <- read.csv("N:/git-annex/Global/NCEAS-Antarctica-Other_v2014/rgn_labels_ccamlr.csv")

invasives <- invasives %.%
  select(sp_id)

invasives$pressure_score[invasives$sp_id %in% c(248100, 248500)] <- 1/429.83 
invasives$pressure_score[is.na(invasives$pressure_score)] <- 0 
write.csv(invasives, "N:/model/GL-HS-AQ-PressuresSummary_v2013/data/sp_alien_AQ.csv", row.names=FALSE)


### Figuring out data----
data2013 <- read.csv("N:/model/GL-NCEAS-Pressures_v2013a/data/p_sp_alien_2013a.csv")

data2012 <- read.csv("N:/model/GL-NCEAS-Pressures_AlienInvasives/data/sp_alien_pressures.csv")

regionKey <- read.csv("C:/Users/Melanie/Desktop/GL-NCEAS-Regions_v2014/eez_rgn_2013master.csv")

regionKey <- regionKey %.%
  select(rgn_id_2013, rgn_id_2012=region_id_2012)

data2013 <- data2013 %.%
  select(rgn_id_2013=rgn_id, pressures.score) %.%
  left_join(regionKey)

data2012 <- data2012 %.%
  select(rgn_id_2012=id, pressures_unstand=pressures) %.%
  left_join(data2013)

maxval <- max(data2012$pressures_unstand) #429.83
1.455101e+00/maxval  #rgn_id2012=5, 3.385290e-03

1/maxval  #region all but 48.1
2/maxval  #region 48.1

### Yes, # of invasives was standardized by the highest value for the regions.  Which was 429.83
# 
# Note from Katie: I checked the Nature paper SOM and not much can be gleaned.
# Just that the total number of invasives (not just harmful invasives) was used
# from Molnar et al. You already found the raw data and Antarctica has 2
# species. Since one species is the King crabs found in Palmer Deep, and this
# area is in CCAMLR region 48.1, you can assign 1 species to all CCAMLR regions
# except 48.1 that gets 2.

# Summary of data (N:/model/GL-NCEAS-Pressures_AlienInvasives/data/): 
#   global_regions_ecoregions.csv = % of rgn in MEOW ecoregion (how the Molnar data is reported), variable = pct_ecoregion
#   Used to weight the Number of invasives in each MEOW (molnar2008.csv)
# 
# sp_alien_data.csv = n_allocated per OHI region
# 
# Then summed per OHI region = sp_alien_pressures.csv
# 
# Standardized data for 2013 (N:/model/GL-NCEAS-Pressures_AlienInvasives/data/)
# For 2013 data, values redistributed according to new regions and then 
# were standardized using the max value per eez (demonstrated above) = 429.83
