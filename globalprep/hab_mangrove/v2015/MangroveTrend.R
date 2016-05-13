#################################################
## Calculating mangrove trend based on new data
## June 8 2015, MRF
## See issue #481 for more details
#################################################

## The original data file downloaded from the website was: 'MASTER TABLE - All Countries.csv'
## I hand changed the header names and removed a few variables and summary row/columns to prepare the data for analysis.
## This file name is 'MASTER TABLE - All Countries_modified.csv'

library(ohicore)
source('src/R/common.R')
data <- read.csv(file.path(dir_neptune_data, "git-annex/globalprep/Mangrove/v2015/raw/MASTER TABLE - All Countries_modified .csv"), stringsAsFactors=FALSE)

data <- gather(data, "category", "value", X2000_MF_km2:X2014_BIOME_percent)

### these are usually reported as three countries, but are grouped in these data.  I will give them the same trend value (note this gap-filling)
# "France (Martinique, Guiana, Guadeloupe, Mayotte)"
data_france_extras <- data %>%
  filter(country == "France (Martinique, Guiana, Guadeloupe, Mayotte)")
data_mg <- data_france_extras %>%
  mutate(country = "Guadeloupe and Martinique")
data_Guiana <- data_france_extras %>%
  mutate(country = "French Guiana")
data_Mayotte <- data_france_extras %>%
  mutate(country = "Mayotte")
data_france_extras <- rbind(data_mg, data_Guiana, data_Mayotte)

### filtering only the MF and km2 data for the analysis and other formatting
data <- data %>%
  filter(country != "") %>%
  filter(country != "France (Martinique, Guiana, Guadeloupe, Mayotte)") %>%
  bind_rows(data_france_extras) %>%
  filter(grepl("MF", category)) %>%
  filter(grepl("km2", category)) %>%
  mutate(year = as.numeric(substring(category, 2, 5))) %>%
  mutate(km2 = as.numeric(gsub(",", "", value))) %>%
  select(country, year, km2)


# #### Convert names to regions (in this case sum because working with area, change to average if working with percent data):
data_region = name_to_rgn(data, fld_name='country', flds_unique=c('country','year'), 
                          fld_value='km2', add_rgn_name=T, collapse_fxn = 'sum_na',
                          dir_lookup = "src/LookupTables"); head(data_region); summary(data_region) 


########################################################################
## calculate trend data for testing (this will be used to test, use function below to 
## generate data for different scenarios)
## NOTE: delete this after I get the extent data together...this has a few
## notes I would like to preserve until then.
########################################################################
data_region_trend <-  data_region %>%
  filter(year %in% 2008:2012) %>%  #dataset includes 2013 and 2014 but these are projected data
  group_by(rgn_id, rgn_name) %>%
  mutate(total_area = sum(km2)) %>% 
  mutate(km2_rel = km2/km2[year==2008]) %>%
  mutate(km2_rel = ifelse(total_area==0, 0, km2_rel)) %>%  # Mauritania (64) and Qatar (190) had mangrove in teh past but not past 2008
  do(mdl = lm(km2_rel ~ year, data=.)) %>%
  summarize(rgn_name = rgn_name,
            rgn_id = rgn_id,
            habitat = "mangrove",
            trend = coef(mdl)['year']*5) %>%
  mutate(trend = round(trend, 6)) %>%
  ungroup()

## compare to previous trend data
old <- read.csv('globalprep/hab_mangrove/v2012/data/habitat_trend_mangrove.csv')

## note compare these when we obtain the new extents...
#some regions in the old data are missing in the new (this is reflected in health as well, I think this might be due to dissagregation)
# Also check that health of 64 and 190 are zero
setdiff(old$rgn_id, data_region_trend$rgn_id) 
setdiff(data_region_trend$rgn_id, old$rgn_id)

old <- old %>%
  left_join(data_region_trend, by='rgn_id')

plot(old$trend.x, old$trend.y, ylab="new trend", xlab="old trend")

old_extent <- read.csv('globalprep/hab_mangrove/v2012/data/habitat_trend_mangrove.csv') 

##############Checking trend data #######################
# I didn't notice anything that looked wrong 
#########################################################
##Check region_id ==200
year=c(2008:2012)
km2=c(1, 0.9988869, 0.9924401, 0.9907240, 0.9920226)
summary(lm(km2~year))
-0.0024118*5
data_region_trend[data_region_trend$rgn_id %in% c(200), ]

## Check Puerto rico (116)
year=c(2008:2012)
km2=c(1, 1, 1, 0.9997883, 0.9997883)
summary(lm(km2~year))
-6.351e-05*5
data_region_trend[data_region_trend$rgn_id %in% c(116), ]

data_region_trend[data_region_trend$rgn_id %in% c(118), ]  #should be zero (or very close to zero)

## these should be zero 
data_region_trend[data_region_trend$rgn_id %in% c(64, 190), ] # Mauritania (64) and Qatar (190) should be zero

## save with region names for a check
data_check <- data_region_trend %>%
  arrange(trend)
write.csv(data_check, 'globalprep/hab_mangrove/v2015/data_explore/trend_check.csv', row.names=FALSE)

#############################################
## Final formatting and save
#############################################

# function that calculates trend for each scenario year and saves data
# NOTE: scenario 2015 = 2008:2012 years.  
#       scenario 2014 = 2007:2011, etc.

yearlyMangrove <- function(scenarioYear=2015){
  yearRange <- (scenarioYear-7):(scenarioYear-3)
  criteria <- ~year %in% yearRange

data_region_trend <-  data_region %>%
  filter_(criteria) %>%  #dataset includes 2013 and 2014 but these are projected data
  group_by(rgn_id, rgn_name) %>%
  mutate(total_area = sum(km2)) %>% 
  mutate(km2_rel = km2/km2[year==min(year)]) %>%
  mutate(km2_rel = ifelse(total_area==0, 0, km2_rel)) %>%  # Mauritania (64) and Qatar (190) had mangrove in teh past but not past 2008
  do(mdl = lm(km2_rel ~ year, data=.)) %>%
  summarize(rgn_name = rgn_name,
            rgn_id = rgn_id,
            habitat = "mangrove",
            trend = coef(mdl)['year']*5) %>%
  mutate(trend = round(trend, 6)) %>%
  ungroup()

final_data <- data_region_trend %>%
  select(rgn_id, habitat, trend)

write.csv(final_data, 
          sprintf('globalprep/hab_mangrove/v2015/tmp/habitat_trend_mangrove_v%s.csv', scenarioYear),
                  row.names=FALSE)
}

yearlyMangrove(scenarioYear = 2015)
yearlyMangrove(scenarioYear = 2014)
yearlyMangrove(scenarioYear = 2013)
yearlyMangrove(scenarioYear = 2012)
