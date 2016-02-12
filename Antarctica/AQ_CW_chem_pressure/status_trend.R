####################################################################
## CleanWaters (CW) calculation
## May 6 2014, MRF
## Updated: Feb 12 2016, MRF
####################################################################
library(dplyr)
library(tidyr)

### This goal is calculated using the pressures data for chemicals and trash

###############################################
## TRASH: trend (status was extracted already)
###############################################

### In the previous assessment, the trash trend was assumed to be zero because pressures for trash were zero and
### we were able to assume a zero trend.  We have since started using a global marine plastics dataset,
### but there is no temporal data.  Consequently, I am going to use global population as a proxy for trash trend.

regions <- read.csv("Antarctica/Other/rgn_labels_ccamlr_lyr.csv") %>%
  select(sp_id)

pop <- read.csv('Antarctica/AQ_CW_chem_pressure/raw/UN_global_population.csv') %>%
  mutate(prop = global_population/global_population[year == max(year)])

assess_years <- c(2014,2015)

for(a_year in assess_years){
  # a_year = 2015
  data <- pop[pop$year %in% (a_year-4):a_year, ]
  data$prop.pop <- data$global_population/data$global_population[data$year == (a_year-4)] 
  model <- lm(prop ~ year, data=data)
  trend <- model$coefficients[[2]]*5
  pressure <- data.frame(sp_id = regions$sp_id,
                         trend = trend)
  write.csv(pressure, sprintf("Antarctica/AQ_CW_chem_pressure/data/cw_trash_trend_v%s.csv", a_year), row.names=FALSE, quote=FALSE)
}


####################################
## Chemical
####################################
chem <- read.csv("globalprep/PressuresRegionExtract/tmp/chemical_data.csv") %>%
  filter(sp_type == "eez-ccamlr") %>%
  gather("year", "pollution", starts_with("chemical"))


