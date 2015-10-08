###############################################
## Habitat gap-filling
###############################################
library(dplyr)
library(tidyr)

# mangrove gap-filling is done here: ohiprep/globalprep/hab_mangrove/v2015/MangroveFinalDataFormatting.R
# not sure where rocky reef is used.....don't have gap-filling data
## Bottom of script combines the habitat-gapfilling so it corresponds to the habitat layer


### General gap-fill function
gap_fill_function <- function(var, no_gap_fill, data){
  
  if(var=="health"){
    health <- data %>%
      mutate(variable = var) %>%
      mutate(condition = ifelse(condition %in% no_gap_fill, 0, condition)) %>%    # if actuals, it is a zero    
      mutate(gap_fill = paste(condition, gap_fill_1, sep=' and ')) %>%
      mutate(gap_fill = gsub(' and NA', '', gap_fill)) %>%         # clean up
      mutate(gap_fill = ifelse(condition %in% 0 & gap_fill_1 %in% "disagg2012_gap_fill", gap_fill_1, gap_fill)) %>%    # original based on estimate but gap-filled during disagregation
      mutate(gap_fill = ifelse(is.na(km2) | km2==0, NA, gap_fill)) %>%      # get rid of any values without extents
      mutate(gap_fill = ifelse(!is.na(km2) & km2>0 & is.na(health), 'mean of other habitats', gap_fill)) %>%  # when there is an area, but no health, this ends up being dropped from the calculation
      mutate(gap_fill = ifelse(is.na(condition) & !is.na(health), "assume gap-fill", gap_fill)) # when there is a health value, but no information on gap-filling
      #select(rgn_id, habitat, variable, gap_fill)
    return(health)
  }
  
  if(var=="extent"){
    extent <- tmp %>%
      select(rgn_id, habitat, km2) %>%
      mutate(variable = var) %>%
      mutate(gap_fill = ifelse(km2>0, 0, NA)) %>%   # all actual data, so anything with a value is 0, others are NA
      mutate(gap_fill = ifelse(rgn_id %in% d_regions$rgn_id & !is.na(km2), "disagg2012_gap_fill", gap_fill))
      #select(rgn_id, habitat, variable, gap_fill)
    return(extent)
  }
  
  if(var=="trend")  {
    trend <- tmp %>%
      mutate(variable = var) %>%
      mutate(trend_gaps = ifelse(trend_gaps == no_gap_fill, 0, trend_gaps)) %>%  # convert non-gapfills to zero
      mutate(gap_fill = paste(trend_gaps, gap_fill_1, sep=' and ')) %>%
      mutate(gap_fill = gsub(' and NA', '', gap_fill)) %>%         # clean up
      mutate(gap_fill = ifelse(trend_gaps %in% 0 & gap_fill_1 %in% "disagg2012_gap_fill", gap_fill_1, gap_fill)) %>%    # original based on estimate but gap-filled during disagregation
      mutate(gap_fill = ifelse(is.na(km2) | km2 %in% 0, NA, gap_fill)) %>%
      mutate(gap_fill = ifelse(km2>0 & is.na(trend), 'mean of other habitats', gap_fill)) %>%    # na.rm in functions.R
      mutate(gap_fill = ifelse(is.na(trend_gaps) & !is.na(trend) , "assume gap-fill", gap_fill))
      #select(rgn_id, habitat, variable, gap_fill)
    return(trend)
  }
}


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
## soft bottom ----
## only has gap-filling related to disaggregation
############################################################################
table(whence$soft_bottom)

sb_whence <- whence %>%
  select(rgn_id, region_id_2012, metric, soft_bottom) %>%
  spread(metric, soft_bottom) %>%
  select(rgn_id, region_id_2012, extent, condition, trend_gaps=trend) %>%
  left_join(d_regions)

sb_extent <- read.csv('globalprep/hab_soft_bottom/v2012/data/habitat_extent_softbottom.csv')
summary(sb_extent)
sb_health <- read.csv('globalprep/hab_soft_bottom/v2012/data/habitat_health_softbottom.csv')
summary(sb_health)
sb_trend <- read.csv('globalprep/hab_soft_bottom/v2012/data/habitat_trend_softbottom.csv')
summary(sb_trend)


# step 1: see if there was gap-filling due to disaggregation:
left_join(d_regions, sb_extent) %>%
  arrange(rgn_id) #I believe so, scaled by eez area
left_join(d_regions, sb_health) %>%#yes
  arrange(region_id_2012)
left_join(d_regions, sb_trend) #yes


tmp <- left_join(sb_whence, sb_extent) %>%
  left_join(sb_health) %>%
  left_join(sb_trend) %>%
  mutate(habitat="soft_bottom")


sb_extent <- gap_fill_function(var="extent", no_gap_fill = "actuals", data=tmp) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(sb_extent, 'globalprep/hab_soft_bottom/v2012/data/extent_gap_fill.csv', row.names=FALSE)

sb_health <- gap_fill_function(var="health", no_gap_fill = "actuals", data=tmp) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(sb_health, 'globalprep/hab_soft_bottom/v2012/data/health_gap_fill.csv', row.names=FALSE)

sb_trend <- gap_fill_function(var="trend", no_gap_fill = "actuals", data=tmp) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(sb_trend, 'globalprep/hab_soft_bottom/v2012/data/trend_gap_fill.csv', row.names=FALSE)

############################################################################
##corals ----
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

c_extent <- gap_fill_function(var="extent", no_gap_fill = "actuals", data=tmp) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(c_extent, 'globalprep/hab_coral/v2012/data/extent_gap_fill_coral.csv', row.names=FALSE)

c_health <- gap_fill_function(var="health", no_gap_fill = c("actuals", "prediction"), data=tmp) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(c_health, 'globalprep/hab_coral/v2012/data/health_gap_fill_coral.csv', row.names=FALSE)

c_trend <- gap_fill_function(var="trend", no_gap_fill = c("actuals"), data=tmp) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(c_trend, 'globalprep/hab_coral/v2012/data/trend_gap_fill_coral.csv', row.names=FALSE)


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


sm_extent <- gap_fill_function(var="extent", no_gap_fill = "actuals", data=tmp) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(sm_extent, 'globalprep/hab_saltmarsh/v2012/data/extent_gap_fill_saltmarsh.csv', row.names=FALSE)

sm_health <- gap_fill_function(var="health", no_gap_fill = "actuals", data=tmp) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(sm_health, 'globalprep/hab_saltmarsh/v2012/data/health_gap_fill_saltmarsh.csv', row.names=FALSE)

sm_trend <- gap_fill_function(var="trend", no_gap_fill = c("actuals"), data=tmp) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(sm_trend, 'globalprep/hab_saltmarsh/v2012/data/trend_gap_fill_saltmarsh.csv', row.names=FALSE)

############################################################################
##sea grass ----
############################################################################

table(whence$seagrasses)
table(whence$seagrasses[whence$metric == "extent"])
table(whence$seagrasses[whence$metric == "trend"])
table(whence$seagrasses[whence$metric == "condition"])

sg_whence <- whence %>%
  select(rgn_id, region_id_2012, metric, seagrasses) %>%
  spread(metric, seagrasses) %>%
  select(rgn_id, region_id_2012, extent, condition, trend_gaps=trend) %>%
  left_join(d_regions)

sg_extent <- read.csv('globalprep/hab_seagrass/v2012/data/habitat_extent_seagrass.csv')
summary(sg_extent)
sg_health <- read.csv('globalprep/hab_seagrass/v2012/data/habitat_health_seagrass.csv')
summary(sg_health)
sg_trend <- read.csv('globalprep/hab_seagrass/v2012/data/habitat_trend_seagrass.csv')
summary(sg_trend)

# step 1: see if there was gap-filling due to disaggregation:
left_join(d_regions, sg_extent) #yes, but scaled to eez area (?)
left_join(d_regions, sg_health) #yes
left_join(d_regions, sg_trend) #yes


tmp <- left_join(sg_whence, sg_extent) %>%
  left_join(sg_health) %>%
  left_join(sg_trend) %>%
  mutate(habitat="seagrasses")


sg_extent <- gap_fill_function(var="extent", no_gap_fill = "actuals", data=tmp) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(sg_extent, 'globalprep/hab_seagrass/v2012/data/extent_gap_fill_seagrass.csv', row.names=FALSE)

sg_health <- gap_fill_function(var="health", no_gap_fill = c('actuals-mixed', 'prediction'), data=tmp) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(sg_health, 'globalprep/hab_seagrass/v2012/data/health_gap_fill_seagrass.csv', row.names=FALSE)

sg_trend <- gap_fill_function(var="trend", no_gap_fill = c("actuals"), data=tmp) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(sg_trend, 'globalprep/hab_seagrass/v2012/data/trend_gap_fill_seagrass.csv', row.names=FALSE)


############################################################################
## sea ice ----
############################################################################
extent <- read.csv('globalprep/NSIDC_SeaIce/v2015/data/hab_ice_extent_eez.csv')

extent_gf <- extent %>%
  mutate(variable = "extent") %>%
  mutate(gap_fill = ifelse(km2>0, 0, NA)) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(extent_gf, 'globalprep/NSIDC_SeaIce/v2015/data/extent_gap_fill_seaice.csv', row.names=FALSE)

health_gf <- extent %>%
  mutate(variable = "health") %>%
  mutate(gap_fill = ifelse(km2>0, 0, NA)) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(health_gf, 'globalprep/NSIDC_SeaIce/v2015/data/health_gap_fill_seaice.csv', row.names=FALSE)

trend_gf <- extent %>%
  mutate(variable = "trend") %>%
  mutate(gap_fill = ifelse(km2>0, 0, NA)) %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(trend_gf, 'globalprep/NSIDC_SeaIce/v2015/data/trend_gap_fill_seaice.csv', row.names=FALSE)


############################################################################
## Combining gap-filling data for habitats ----
############################################################################

# Extent:
mangrove_extent <- read.csv('globalprep/hab_mangrove/v2015/data/extent_gap_fill.csv')
si_extent <- read.csv('globalprep/NSIDC_SeaIce/v2015/data/extent_gap_fill_seaice.csv')
coral_extent <- read.csv('globalprep/hab_coral/v2012/data/extent_gap_fill_coral.csv')
sm_extent <- read.csv('globalprep/hab_saltmarsh/v2012/data/extent_gap_fill_saltmarsh.csv')
sg_extent <- read.csv('globalprep/hab_seagrass/v2012/data/extent_gap_fill_seagrass.csv')
sb_extent <- read.csv('globalprep/hab_soft_bottom/v2012/data/extent_gap_fill.csv')

extent_gf <- rbind(mangrove_extent, si_extent, coral_extent, sm_extent, sg_extent, sb_extent)

table(extent_gf$gap_fill)

write.csv(extent_gf, 'globalprep/hab_combined/v2015/data/habitat_extent_gap_fill.csv', row.names = FALSE)

# Health:
mangrove <- read.csv('globalprep/hab_mangrove/v2015/data/health_gap_fill.csv')
si <- read.csv('globalprep/NSIDC_SeaIce/v2015/data/health_gap_fill_seaice.csv')
coral <- read.csv('globalprep/hab_coral/v2012/data/health_gap_fill_coral.csv')
sm <- read.csv('globalprep/hab_saltmarsh/v2012/data/health_gap_fill_saltmarsh.csv')
sg <- read.csv('globalprep/hab_seagrass/v2012/data/health_gap_fill_seagrass.csv')
sb <- read.csv('globalprep/hab_soft_bottom/v2012/data/health_gap_fill.csv')

health_gf <- rbind(mangrove, si, coral, sm, sg, sb)
summary(health_gf)

write.csv(health_gf, 'globalprep/hab_combined/v2015/data/habitat_health_gap_fill.csv', row.names = FALSE)

# Trend:
mangrove <- read.csv('globalprep/hab_mangrove/v2015/data/trend_gap_fill.csv')
si <- read.csv('globalprep/NSIDC_SeaIce/v2015/data/trend_gap_fill_seaice.csv')
coral <- read.csv('globalprep/hab_coral/v2012/data/trend_gap_fill_coral.csv')
sm <- read.csv('globalprep/hab_saltmarsh/v2012/data/trend_gap_fill_saltmarsh.csv')
sg <- read.csv('globalprep/hab_seagrass/v2012/data/trend_gap_fill_seagrass.csv')
sb <- read.csv('globalprep/hab_soft_bottom/v2012/data/trend_gap_fill.csv')

trend_gf <- rbind(mangrove, si, coral, sm, sg, sb)
summary(trend_gf)

write.csv(trend_gf, 'globalprep/hab_combined/v2015/data/habitat_trend_gap_fill.csv', row.names = FALSE)
