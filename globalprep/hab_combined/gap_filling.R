###############################################
## Habitat gap-filling
###############################################
library(dplyr)
library(tidyr)

# mangrove gap-filling is done here: ohiprep/globalprep/hab_mangrove/v2015/MangroveFinalDataFormatting.R
# sea ice had no gap-filling

## regions
regions <- read.csv('../ohiprep/src/LookupTables/eez_rgn_2013master.csv') %>%
  select(rgn_id = rgn_id_2013) %>%
  filter(rgn_id < 255) %>%
  arrange(rgn_id) %>%
  unique()

## disaggregation regions
d_regions <- read.csv('../ohi-global/global2015/gapFilling/dissaggregated_gap_fill.csv') %>%
  select(rgn_id = rgn_id_2013, region_id_2012) %>%
  mutate(gap_fill_1 = "disagg2012_gap_fill")

## general information on what was gap-filled and how
## NOTE: 2012 region ids are reported here:
whence <- read.csv('/var/data/ohi/model/GL-NCEAS_Habitat_health-v2013a/tmp/Habitat_whence/input files and script/HabitatWhence.csv') %>%
  select(region_id_2012=id, metric, corals, salt_marshes, seagrasses, soft_bottom)
whence <- apply(whence, 2, function(x) gsub("^$|^ $", NA, x)) # replaces spaces with NA values
whence <- data.frame(whence)
whence$region_id_2012 <- as.numeric(as.character(whence$region_id_2012)) 

whence <- read.csv('../ohiprep/src/LookupTables/eez_rgn_2013master.csv') %>%
  select(rgn_id = rgn_id_2013, region_id_2012) %>%
  filter(rgn_id < 255) %>%
  unique() %>%
  left_join(whence) %>%
  filter(rgn_id <= 250) %>%
  filter(!is.na(metric))


############################################################################
## start with soft bottom, only has gap-filling related to disaggregation
############################################################################
table(whence$soft_bottom)

sb_extent <- read.csv('globalprep/hab_soft_bottom/v2012/data/habitat_extent_softbottom.csv')
summary(sb_extent)
sb_health <- read.csv('globalprep/hab_soft_bottom/v2012/data/habitat_health_softbottom.csv')
summary(sb_health)
sb_trend <- read.csv('globalprep/hab_soft_bottom/v2012/data/habitat_trend_softbottom.csv')
summary(sb_trend)

# step 1: see if there was gap-filling due to disaggregation:
left_join(d_regions, sb_extent) #I think there was due to disaggration, values aren't the same because it was scaled to eez area
left_join(d_regions, sb_health) #yes
left_join(d_regions, sb_trend) #yes

tmp <- left_join(regions, sb_extent) %>%
  left_join(sb_health) %>%
  left_join(sb_trend) %>%
  mutate(habitat="soft_bottom") %>%
  ## for health/trend all values are 0, except those with values and in the disagg list 
  ## also add contingency when no extent, but health/trend values, or
  ## when there is extent but no health/trend values 
  mutate(health = ifelse(!is.na(health), 0, health)) %>%
  mutate(health = ifelse(rgn_id %in% d_regions$rgn_id & !is.na(health), "disagg2012_gap_fill", health)) %>%
  mutate(health = ifelse(km2==0 | is.na(km2), NA, health)) %>% 
  mutate(health = ifelse(km2>0 & is.na(health), "mean of other habitats", health)) %>% 
  mutate(trend = ifelse(!is.na(trend), 0, trend)) %>%
  mutate(trend = ifelse(rgn_id %in% d_regions$rgn_id & !is.na(trend), "disagg2012_gap_fill", trend)) %>%
  mutate(trend = ifelse(km2==0 | is.na(km2), NA, trend)) %>% 
  mutate(trend = ifelse(km2>0 & is.na(trend), "mean of other habitats", trend)) %>%
  mutate(extent_gap_fill = ifelse(km2>0, 0, NA)) %>%
  mutate(extent_gap_fill = ifelse(rgn_id %in% d_regions$rgn_id & !is.na(km2), "disagg2012_gap_fill", extent_gap_fill)) 

extent <- tmp %>%
  mutate(variable = "extent") %>%
  select(rgn_id, habitat, variable, gap_fill = extent_gap_fill)
write.csv(extent, 'globalprep/hab_soft_bottom/v2012/data/extent_gap_fill.csv', row.names=FALSE)

health <- tmp %>%
  mutate(variable = 'health') %>%
  select(rgn_id, habitat, variable, gap_fill=health)
write.csv(health, 'globalprep/hab_soft_bottom/v2012/data/health_gap_fill.csv', row.names=FALSE)

trend <- tmp %>%
  mutate(variable = 'trend') %>%
  select(rgn_id, habitat, variable, gap_fill=trend)
write.csv(health, 'globalprep/hab_soft_bottom/v2012/data/trend_gap_fill.csv', row.names=FALSE)

############################################################################
##corals:
############################################################################
table(whence$corals)
table(whence$corals[whence$metric == "extent"])
table(whence$corals[whence$metric == "trend"])
table(whence$corals[whence$metric == "condition"])

c_whence <- whence %>%
  select(rgn_id, region_id_2012, metric, corals) %>%
  spread(metric, corals) %>%
  select(rgn_id, region_id_2012, extent, condition, trend_gaps=trend) %>%
  left_join(d_regions)
  
coral_extent <- read.csv('globalprep/hab_coral/v2012/data/habitat_extent_coral.csv')
summary(coral_extent)
coral_health <- read.csv('globalprep/hab_coral/v2012/data/habitat_health_coral.csv')
summary(coral_health)
coral_trend <- read.csv('globalprep/hab_coral/v2012/data/habitat_trend_coral.csv')
summary(coral_trend)

# step 1: see if there was gap-filling due to disaggregation:
left_join(d_regions, coral_extent) %>%
  arrange(rgn_id) #I believe so, scaled by eez area
left_join(d_regions, coral_health) %>%#yes
  arrange(region_id_2012)
left_join(d_regions, coral_trend) #yes


tmp <- left_join(c_whence, coral_extent) %>%
  left_join(coral_health) %>%
  left_join(coral_trend) %>%
  mutate(habitat="coral")

extent <- tmp %>%
  select(rgn_id, habitat, km2) %>%
  mutate(variable = "extent") %>%
  mutate(gap_fill = ifelse(km2>0, 0, gap_fill)) %>%   # all actual data, so anything with a value is 0, others are NA
  mutate(gap_fill = ifelse(rgn_id %in% d_regions$rgn_id & !is.na(km2), "disagg2012_gap_fill", gap_fill)) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(extent, 'globalprep/hab_coral/v2012/data/extent_gap_fill_coral.csv', row.names=FALSE)

### NOTE: I think prediction might be considered as non-gapfilled, otherwise everything is gap-filled!
health <- tmp %>%
  mutate(variable = "health") %>%
  mutate(condition = ifelse(condition %in% 'actuals', 0, condition)) %>%    # if actuals, it is a zero    
  mutate(gap_fill = paste(condition, gap_fill_1, sep=' and ')) %>%
  mutate(gap_fill = gsub(' and NA', '', gap_fill)) %>%         # clean up
  mutate(gap_fill = gsub('prediction and', '', gap_fill)) %>%         # clean up
  mutate(gap_fill = ifelse(is.na(km2) | km2==0, NA, gap_fill)) %>%      # get rid of any values without extents
  mutate(gap_fill = ifelse(!is.na(km2) & km2>0 & is.na(health), 'mean of other habitats', gap_fill)) %>%  # when there is an area, but no health, this ends up being dropped from the calculation
  mutate(gap_fill = ifelse(is.na(condition) & !is.na(health), "assume gap-fill", gap_fill)) %>% # when there is a health value, but no information on gap-filling
  mutate(gap_fill = ifelse(gap_fill ==  'prediction', 0, gap_fill)) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(health, 'globalprep/hab_coral/v2012/data/health_gap_fill_coral.csv', row.names=FALSE)

table(tmp$trend_gaps)
trend <- tmp %>%
  mutate(variable = "trend") %>%
  mutate(trend_gaps = ifelse(trend_gaps == 'actuals', 0, trend_gaps)) %>%  # convert non-gapfills to zero
  mutate(gap_fill = paste(trend_gaps, gap_fill_1, sep=' and ')) %>%
  mutate(gap_fill = ifelse(trend_gaps %in% 0 & gap_fill_1 %in% "disagg2012_gap_fill", gap_fill_1, trend_gaps)) %>%    # original based on estimate but gap-filled during disagregation
  mutate(gap_fill = ifelse(is.na(km2) | km2 %in% 0, NA, gap_fill)) %>%
  mutate(gap_fill = ifelse(km2>0 & is.na(gap_fill), 'mean of other habitats', gap_fill)) %>%    # na.rm in functions.R
  mutate(gap_fill = ifelse(is.na(trend_gaps) & !is.na(trend) , "assume gap-fill", gap_fill)) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(trend, 'globalprep/hab_coral/v2012/data/trend_gap_fill_coral.csv', row.names=FALSE)


############################################################################
##salt marshes ----
## all actual data and nothing too complicated
############################################################################
table(whence$salt_marshes)
table(whence$salt_marshes[whence$metric == "extent"])
table(whence$salt_marshes[whence$metric == "trend"])
table(whence$salt_marshes[whence$metric == "condition"])

sm_whence <- whence %>%
  select(rgn_id, region_id_2012, metric, salt_marshes) %>%
  spread(metric, salt_marshes) %>%
  select(rgn_id, region_id_2012, extent, condition, trend_gaps=trend) %>%
  left_join(d_regions)

sm_extent <- read.csv('globalprep/hab_saltmarsh/v2012/data/habitat_extent_saltmarsh.csv')
summary(sm_extent)
sm_health <- read.csv('globalprep/hab_saltmarsh/v2012/data/habitat_health_saltmarsh.csv')
summary(sm_health)
sm_trend <- read.csv('globalprep/hab_saltmarsh/v2012/data/habitat_trend_saltmarsh.csv')
summary(sm_trend)

# step 1: see if there was gap-filling due to disaggregation:
left_join(d_regions, sm_extent) #yes...I believe scaled to eez area
left_join(d_regions, sm_health) #yes
left_join(d_regions, sm_trend) #yes


tmp <- left_join(sm_whence, sm_extent) %>%
  left_join(sm_health) %>%
  left_join(sm_trend) %>%
  mutate(habitat="salt_marsh")

extent <- tmp %>%
  mutate(variable = "extent") %>%
  mutate(extent = ifelse(extent %in% 'actuals', 0, extent)) %>%    # if actuals, it is a zero    
  mutate(gap_fill = paste(extent, gap_fill_1, sep=' and ')) %>%
  mutate(gap_fill = gsub(' and NA', '', gap_fill)) %>%         # clean up
  mutate(gap_fill = ifelse(extent ==0 & gap_fill_1 %in% "disagg2012_gap_fill", gap_fill_1, gap_fill)) %>%    # original based on estimate but gap-filled during disagregation
  mutate(gap_fill = ifelse(is.na(km2) | km2==0, NA, gap_fill)) %>%      # get rid of any values without extents
  mutate(gap_fill = ifelse(!is.na(km2) & km2>0 & is.na(extent), 'mean of other habitats', gap_fill)) %>%
  mutate(gap_fill = ifelse(is.na(extent) & !is.na(gap_fill), "assume gap-fill", gap_fill)) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(extent, 'globalprep/hab_saltmarsh/v2012/data/extent_gap_fill_saltmarsh.csv', row.names=FALSE)


health <- tmp %>%
  mutate(variable = "health") %>%
  mutate(condition = ifelse(condition %in% 'actuals', 0, condition)) %>%    # if actuals, it is a zero    
  mutate(gap_fill = paste(condition, gap_fill_1, sep=' and ')) %>%
  mutate(gap_fill = gsub(' and NA', '', gap_fill)) %>%         # clean up
  mutate(gap_fill = ifelse(condition %in% 0 & gap_fill_1 %in% "disagg2012_gap_fill", gap_fill_1, condition)) %>%    # original based on estimate but gap-filled during disagregation
  mutate(gap_fill = ifelse(is.na(km2) | km2==0, NA, gap_fill)) %>%      # get rid of any values without extents
  mutate(gap_fill = ifelse(!is.na(km2) & km2>0 & is.na(health), 'mean of other habitats', gap_fill)) %>%
  mutate(gap_fill = ifelse(is.na(condition) & !is.na(health), "assume gap-fill", gap_fill)) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(health, 'globalprep/hab_saltmarsh/v2012/data/health_gap_fill_saltmarsh.csv', row.names=FALSE)

table(tmp$trend_gaps)
trend <- tmp %>%
  mutate(variable = "trend") %>%
  mutate(trend_gaps = ifelse(trend_gaps == 'actuals', 0, trend_gaps)) %>%  # convert non-gapfills to zero
  mutate(gap_fill = paste(trend_gaps, gap_fill_1, sep=' and ')) %>%
  mutate(gap_fill = ifelse(trend_gaps %in% 0 & gap_fill_1 %in% "disagg2012_gap_fill", gap_fill_1, trend_gaps)) %>%    # original based on estimate but gap-filled during disagregation
  mutate(gap_fill = ifelse(is.na(km2) | km2 %in% 0, NA, gap_fill)) %>%
  mutate(gap_fill = ifelse(km2>0 & is.na(gap_fill), 'mean of other habitats', gap_fill)) %>%    # na.rm in functions.R
  mutate(gap_fill = ifelse(is.na(trend_gaps) & !is.na(trend) , "assume gap-fill", gap_fill)) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(trend, 'globalprep/hab_saltmarsh/v2012/data/trend_gap_fill_saltmarsh.csv', row.names=FALSE)


############################################################################
##salt marshes ----
## only actuals
############################################################################

table(whence$salt_marshes)
table(whence$salt_marshes[whence$metric == "extent"])
table(whence$salt_marshes[whence$metric == "trend"])
table(whence$salt_marshes[whence$metric == "condition"])

sm_whence <- whence %>%
  select(rgn_id, region_id_2012, metric, salt_marshes) %>%
  spread(metric, salt_marshes) %>%
  select(rgn_id, region_id_2012, extent, condition, trend_gaps=trend) %>%
  left_join(d_regions)

sm_extent <- read.csv('globalprep/hab_saltmarsh/v2012/data/habitat_extent_saltmarsh.csv')
summary(sm_extent)
sm_health <- read.csv('globalprep/hab_saltmarsh/v2012/data/habitat_health_saltmarsh.csv')
summary(sm_health)
sm_trend <- read.csv('globalprep/hab_saltmarsh/v2012/data/habitat_trend_saltmarsh.csv')
summary(sm_trend)

# step 1: see if there was gap-filling due to disaggregation:
left_join(d_regions, sm_extent) #yes, but scaled to eez area
left_join(d_regions, sm_health) #yes
left_join(d_regions, sm_trend) #yes


tmp <- left_join(sm_whence, sm_extent) %>%
  left_join(sm_health) %>%
  left_join(sm_trend) %>%
  mutate(habitat="salt_marsh")

extent <- tmp %>%
  mutate(variable = "extent") %>%
  mutate(extent = ifelse(extent %in% 'actuals', 0, extent)) %>%    # if actuals, it is a zero    
  mutate(gap_fill = paste(extent, gap_fill_1, sep=' and ')) %>%
  mutate(gap_fill = gsub(' and NA', '', gap_fill)) %>%         # clean up
  mutate(gap_fill = ifelse(extent ==0 & gap_fill_1 %in% "disagg2012_gap_fill", gap_fill_1, gap_fill)) %>%    # original based on estimate but gap-filled during disagregation
  mutate(gap_fill = ifelse(is.na(km2) | km2==0, NA, gap_fill)) %>%      # get rid of any values without extents
  mutate(gap_fill = ifelse(!is.na(km2) & km2>0 & is.na(extent), 'mean of other habitats', gap_fill)) %>%
  mutate(gap_fill = ifelse(is.na(extent) & !is.na(gap_fill), "assume gap-fill", gap_fill)) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(extent, 'globalprep/hab_saltmarsh/v2012/data/extent_gap_fill_saltmarsh.csv', row.names=FALSE)

health <- tmp %>%
  mutate(variable = "health") %>%
  mutate(condition = ifelse(condition %in% 'actuals', 0, condition)) %>%    # if actuals, it is a zero    
  mutate(gap_fill = paste(condition, gap_fill_1, sep=' and ')) %>%
  mutate(gap_fill = ifelse(condition %in% 0 & gap_fill_1 %in% "disagg2012_gap_fill", gap_fill_1, condition)) %>%    # original based on estimate but gap-filled during disagregation
  mutate(gap_fill = ifelse(is.na(km2) | km2==0, NA, gap_fill)) %>%      # get rid of any values without extents
  mutate(gap_fill = gsub(' and NA', '', gap_fill)) %>%         # clean up
  mutate(gap_fill = ifelse(!is.na(km2) & km2>0 & is.na(health), 'mean of other habitats', gap_fill)) %>%
  mutate(gap_fill = ifelse(is.na(condition) & !is.na(health), "assume gap-fill", gap_fill)) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(health, 'globalprep/hab_saltmarsh/v2012/data/health_gap_fill_saltmarsh.csv', row.names=FALSE)

table(tmp$trend_gaps)
trend <- tmp %>%
  mutate(variable = "trend") %>%
  mutate(trend_gaps = ifelse(trend_gaps == 'actuals', 0, trend_gaps)) %>%  # convert non-gapfills to zero
  mutate(gap_fill = paste(trend_gaps, gap_fill_1, sep=' and ')) %>%
  mutate(gap_fill = ifelse(trend_gaps %in% 0 & gap_fill_1 %in% "disagg2012_gap_fill", gap_fill_1, trend_gaps)) %>%    # original based on estimate but gap-filled during disagregation
  mutate(gap_fill = ifelse(is.na(km2) | km2 %in% 0, NA, gap_fill)) %>%
  mutate(gap_fill = ifelse(km2>0 & is.na(gap_fill), 'mean of other habitats', gap_fill)) %>%    # na.rm in functions.R
  mutate(gap_fill = ifelse(is.na(trend_gaps) & !is.na(trend) , "assume gap-fill", gap_fill)) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(trend, 'globalprep/hab_saltmarsh/v2012/data/trend_gap_fill_saltmarsh.csv', row.names=FALSE)

############################################################################
## sea ice ----
############################################################################


table(whence$salt_marshes)
table(whence$salt_marshes[whence$metric == "extent"])
table(whence$salt_marshes[whence$metric == "trend"])
table(whence$salt_marshes[whence$metric == "condition"])

sm_whence <- whence %>%
  select(rgn_id, region_id_2012, metric, salt_marshes) %>%
  spread(metric, salt_marshes) %>%
  select(rgn_id, region_id_2012, extent, condition, trend_gaps=trend) %>%
  left_join(d_regions)
