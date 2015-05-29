# Data Prep for Wastewater Data Layer 
# The Ocean Health Index
### Omar Malik, 28 May 2015, Santa Barbara, CA

# Install libraries:
getwd()
setwd("./globalprep/EPI_wastewater")

library("dplyr")
library("readr")
library("tidyr")

# STEP: Define variables, and pull out what is necessary:

# NOTE: waste <- read.csv("YaleEPI_WastewaterTreatment_2014.csv", head = TRUE); head(waste) # raw file with time series for treatment % only; it's not normalized as index score;

waste <- read.csv("./raw/WASTECXN_2014.csv", head = TRUE); head(waste)

waste_sum_a <- waste %>% 
  select(iso, country, WASTECXN.2012) %>% 
  mutate(year = 2014) # Taking out country code and index score; making into long format; 

numc <- length(waste_sum_a[1,])
numr <- length(waste_sum_a[,1])
head(waste_sum_a)

# STEP: adding column names
colnames(waste_sum_a) <- c("ISO", "Country", "WasteIndex", "Year")

# STEP: removing NA rows, truncating text

waste_sum_b <- waste_sum_a[1:232,]
head(waste_sum_b)

# STEP: removing -9999s

waste_sum_b <- filter(waste_sum_b, WasteIndex > -1)

# # WasteIndex_raw <- as.character(waste_sum_b[,3])
# # WasteIndex_clean <- gsub("-9999.000000", NA, WasteIndex_raw, perl = FALSE)
# # # View(WasteIndex)
# # # View(WasteIndex_clean)
# # as.numeric(WasteIndex_clean) -> WasteIndex
# 
# #### STEP: Rejoin columns here:
# 
# cbind(waste_sum_b, WasteIndex) -> waste_sum_c
# head(waste_sum_c)
# # View(waste_sum_c)
waste_sum_c <- select(waste_sum_b, ISO, Country, Year, WasteIndex)

head(waste_sum_c)

### NOTE: Rescaling - NA here unlesss you choose to use the raw data CSV instead; however, WasteIndex scores are normalized already from 0 - 1.
#waste_sum_e = waste_sum_d %>% select(iso, country, year, Treated) %>% mutate(rescale = (Treated)/max(Treated))
#  head(waste_sum_e);

#### STEP: Save file in this directory:

write.csv(waste_sum_c, "po_wastewater_gl2014.csv")

####### TO DO: Clean Region IDs

# add_rgn_id("po_wastewater_gl2014.csv")
# ?add_rgn_id; 

## NOTE: save as pressure layer: only scenario's recent year, but rescaled, including previous scenarios ----

# # rescale with all previous scenarios, to 110%:  Neptune: model/GL-NCEAS-CleanWatersPressures/pathogens/sanitation-population-combo/model.R
# sp_press = san_pop %>%
#   filter(include_prev_scenario == T) %>% 
#   mutate(pressure_score = propWO_x_pop_log / (max(propWO_x_pop_log, na.rm=T) * 1.1)) %>% # number of POOPERS, RESCALED
#   select(-include_prev_scenario)

