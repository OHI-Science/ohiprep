# Data Prep for Wastewater Data Layer 
# The Ocean Health Index
### Omar Malik, 28 May 2015, Santa Barbara, CA

# Install libraries:

devtools::install_github('ohi-science/ohicore@dev') # may require uninstall and reinstall
getwd()
setwd("./globalprep/EPI_wastewater")

library(ohicore)
library("dplyr")
library("readr")
library("tidyr")

# STEP: Define variables, and pull out what is necessary:

# NOTE: waste <- read.csv("YaleEPI_WastewaterTreatment_2014.csv", head = TRUE); head(waste) # raw file with time series for treatment % only; it's not normalized as index score;

waste <- read.csv("./raw/WASTECXN_2014.csv", head = TRUE); head(waste)

waste_sum_a <- waste %>% 
  select(iso, country, WASTECXN.2012) %>% 
  mutate(year = 2014, wastedec = WASTECXN.2012*.01) # Taking out country code and index score; making into long format; 

numc <- length(waste_sum_a[1,])
numr <- length(waste_sum_a[,1])
head(waste_sum_a)

# STEP: adding column names
colnames(waste_sum_a) <- c("ISO", "Country", "WasteIndex", "Year", "WasteIndexCoeff")

# STEP: removing NA rows, truncating text

waste_sum_b <- waste_sum_a[1:232,]
head(waste_sum_b)

# STEP: removing -9999s

waste_sum_b <- filter(waste_sum_b, WasteIndex > -1)

waste_sum_c <- select(waste_sum_b, ISO, Country, Year, WasteIndex, WasteIndexCoeff)

head(waste_sum_c); summary(waste_sum_c)

# NOTE: Rescaling - NA here unlesss you choose to use the raw data CSV instead; however, WasteIndex scores are normalized already from 0 - 1.
#waste_sum_e = waste_sum_d %>% select(iso, country, year, Treated) %>% mutate(rescale = (Treated)/max(Treated))
#  head(waste_sum_e);

# STEP: Save file in this directory - multiple files for different scenarios

write.csv(waste_sum_c, "po_wastewater_gl2015.csv"); # TO DO: Change to _d after the region collapse works
write.csv(waste_sum_c, "cw_wastewater_gl2015.csv"); # ibid.

# STEP: Clean region IDs, using ohicore function 'name_to_rgn()'. *USER: You should choose whether to use WasteIndex or WasteIndexCoeff:

## old: waste_sum_d <- ohicore::name_to_rgn(waste_sum_c, fld_name= 'Country', fld_value = 'WasteIndexCoeff')

## !ISSUE: after applying this code, a problem occured where China has > 100. This is probably because it's summing Hong Kong and/or Taiwan into the data. 
## Fixing the above issue through the below method of creating a lookup table:

population_weights <- read.csv('../../../ohiprep/src/LookupTables/Pop_weight_chn_hkg_mtq_glp.csv'); head(population_weights)

waste_sum_d <- ohicore::name_to_rgn(waste_sum_c, fld_name='Country', flds_unique='Country', 
                   fld_value='WasteIndex', add_rgn_name=T, collapse_fxn = 'weighted.mean',  # <- change "d_wgi2"; stays weighted mean; collapse_fxn; 
                   collapse_csv = '../../../ohiprep/src/LookupTables/Pop_weight_chn_hkg_mtq_glp.csv', # <- make new datasheet; use long names;
                   dir_lookup = "../../../ohiprep/src/LookupTables"); head(waste_sum_d); summary(waste_sum_d) # 

# !ISSUE: now getting error, `Error: length(fld) == 1 is not TRUE`  
#^ once the above is fixed, apply the below:

m_d[m_d$rgn_id == 140,] # check that these look correct.
m_d[m_d$rgn_id == 209,]

m_d_unique <- m_d %>%
  select(rgn_id, rgn_name) %>%
  unique() %>%
  arrange(rgn_id)


# !ISSUE-history: summary(waste_sum_d)
# rgn_id      WasteIndex      
# Min.   :  7   Min.   :  0.0000  
# 1st Qu.: 66   1st Qu.:  0.5312  
# Median :129   Median : 10.8688  
# Mean   :124   Mean   : 27.5250  
# 3rd Qu.:185   3rd Qu.: 49.4981  
# Max.   :255   Max.   :105.9668  # > 100 error

## NOTE: save as pressure layer: only scenario's recent year, but rescaled, including previous scenarios ----

#### II. Analysis

dim(waste_sum_d)
summary(waste_sum_d) 
 
# 141 regions matching.
# 
# 40 landlocked countries were removed:
# 
# 4 duplicates -> # Question - how does it collapse China, Hong Hong?
#   
#   tmp_name                   landlocked
# Afghanistan                       1
# Andorra                           1
# Armenia                           1
# Austria                           1
# Belarus                           1
# Bhutan                            1
# Bolivia                           1
# Botswana                          1
# Burkina Faso                      1
# Burundi                           1
# Central African Republic          1
# Chad                              1
# Czech Republic                    1
# Ethiopia                          1
# Hungary                           1
# Kazakhstan                        1
# Kyrgyzstan                        1
# Laos                              1
# Lesotho                           1
# Luxembourg                        1
# Macedonia                         1
# Malawi                            1
# Mali                              1
# Moldova                           1
# Mongolia                          1
# Nepal                             1
# Niger                             1
# Paraguay                          1
# Rwanda                            1
# Serbia                            1
# Slovakia                          1
# Swaziland                         1
# Switzerland                       1
# Tajikistan                        1
# Turkmenistan                      1
# Uganda                            1
# Uzbekistan                        1
# Zambia                            1
# Zimbabwe                          1
#   
#   DUPLICATES found. Resolving by collapsing rgn_id with collapse_fxn: sum_na after first removing all NAs from duplicates...
# rgn_id
# fld_name     140 209
# China        0   1
# Guadeloupe   1   0
# Hong Kong    0   1
# Martinique   1   0

View(waste_sum_d)

## Testing how it would look to have scores of CW status weighted by "Wastewater Index":

test <- read.csv("scores_test.csv")
colnames(test) = c("goal","dimension","rgn_id","score")
head(test);
waste_by_scores <- left_join(test,waste_sum_d);

waste_by_scores_b <- subset(waste_by_scores, goal == "CW" & dimension == "status"); head(waste_by_scores_b)
View(waste_by_scores)

scenario_a <- waste_by_scores_b %>% mutate(scorewaste = score*WasteIndexCoeff);

a <- as.data.frame(subset(scenario_a, !is.na(scorewaste == TRUE)))
head(a)
summary(a)

p <- qplot(score, scorewaste, data = a) # Compare CW status score against (statusscore*wasteindex));

## Testing it against JMP/WHO data

library(ggplot2)

test2 <- read.csv("Waste_ACSAT_exploration.csv"); head(test2)

testing1 <- test2 %>% select(country, ACSAT.2012, WASTECXN.2012); head(testing1)

qplot(ACSAT.2012, WASTECXN.2012, data = testing1);
