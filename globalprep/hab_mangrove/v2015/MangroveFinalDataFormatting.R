#########################################
## Final organization of mangrove data
#########################################
library(dplyr)
library(tidyr)

## The eez_and_land_km2 is the primary source for determining
## where mangroves are located.  Trend/health data will be made consistent 
## with these data.

all <- read.csv('globalprep/hab_mangrove/v2015/tmp/eez_and_land_km2.csv') %>%
  select(rgn_id = zone, mangrove = sum)

all <- all %>%
  filter(mangrove>0) %>%
  filter(rgn_id < 255)
sum(all$mangrove)
#total global mangrove from text: 81,849 km2, 82292.24 km2 is our amount - which seems like a reasonable amount of error
# also spot checked a couple countries and looked correct

regions <- read.csv('src/LookupTables/rgn_georegions_wide_2013b.csv')

#####################################################
### gap-fill and organize trend data
## Two countries: Republique of Congo (100) and Mauritania (64) have trend data, but no mangrove
## Mauritania has 0 in all trend years except maybe one. 
## I checked our Congo data and didn't see any mangrove raster cells in this region
## so I am going to assume that our 0 value is in fact correct.



for(year in 2012:2015){ #year <- '2012'
trend <- read.csv(sprintf('globalprep/hab_mangrove/v2015/tmp/habitat_trend_mangrove_v%s.csv', year))

print(sprintf('year: %s', year))
print('deleted: ') 
print(setdiff(trend$rgn_id, all$rgn_id))
print('gap-filled: ')
print(setdiff(all$rgn_id, trend$rgn_id))

tmp <- all %>%
  left_join(trend)  

trend_gaps <- tmp %>%
  mutate(gap_fill = ifelse(is.na(trend), 'regional (r2) mean', 0)) %>%   # these have extent data, but no trend data. Will use mean trend to gapfill
  mutate(gap_fill = ifelse(rgn_id %in% c(29, 140, 169), "three countries combined", gap_fill)) %>% # these regions' trends were reported as one trend
  mutate(habitat="mangrove") %>%
  mutate(variable="trend") %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(trend_gaps, 'globalprep/hab_mangrove/v2015/data/trend_gap_fill.csv',  row.names=FALSE)  #same for all years, so save only one

tmp  <- tmp %>%
  left_join(regions) %>%
  group_by(r2) %>%
  mutate(avg_trend = mean(trend, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(trend2 = ifelse(is.na(trend), avg_trend, trend)) %>%
  mutate(habitat = "mangrove") %>%
  select(rgn_id, habitat, trend=trend2)
summary(tmp)

write.csv(tmp, sprintf('globalprep/hab_mangrove/v2015/data/habitat_trend_mangrove_v%s.csv', year),
          row.names=FALSE)
}


#####################################################
### gap-fill and organize health data
health <- read.csv('globalprep/hab_mangrove/v2012/data/habitat_health_mangrove.csv')

setdiff(all$rgn_id, health$rgn_id) #mangrove extent data but no health score
setdiff(health$rgn_id, all$rgn_id) #these have health scores, but no mangrove data, I think this is an artifact of previous gapfilling

agg_gap_fill <- read.csv('../ohi-global/global2015/gapFilling/dissaggregated_gap_fill.csv') %>%
  select(rgn_id=rgn_id_2013) %>%
  left_join(health) %>%
  filter(!(is.na(health)))

tmp <- all %>%      #eliminates the regions with health scores but no mangroves
  left_join(health)  


tmp  <- tmp %>%
  left_join(regions) %>%
  group_by(r2) %>%
  mutate(avg_health_r2 = mean(health, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(r1) %>%
  mutate(avg_health_r1 = mean(health, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(health2 = ifelse(is.na(health), avg_health_r2, health)) %>%
  mutate(health3 = ifelse(is.na(health2), avg_health_r1, health2)) %>%
  mutate(habitat = "mangrove") %>%
  mutate(gap_fill = ifelse(is.na(health), "r2_gap_fill", NA)) %>%
  mutate(gap_fill = ifelse(is.na(health) & is.na(avg_health_r2), "r1_gap_fill", gap_fill)) %>%
  mutate(gap_fill = ifelse(rgn_id %in% agg_gap_fill$rgn_id, "disagg2012_gap_fill", gap_fill)) %>%
  mutate(gap_fill = ifelse(is.na(gap_fill), 0, gap_fill)) %>%
  select(rgn_id, habitat, gap_fill, health=health3)
summary(tmp)

### save summary of gap-filling:
health_gaps <- tmp %>%
  mutate(variable = "health") %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(health_gaps, 'globalprep/hab_mangrove/v2015/data/health_gap_fill.csv', row.names=FALSE)

### save health data:
health <- tmp %>%
  select(rgn_id, habitat, health)
write.csv(tmp, 'globalprep/hab_mangrove/v2015/data/habitat_health_mangrove.csv', row.names=FALSE)


####################################
## extent data and gap-filling

all <- read.csv('globalprep/hab_mangrove/v2015/tmp/eez_and_land_km2.csv') %>%
  select(rgn_id = zone, mangrove = sum)

extent_gf <- all %>%
  mutate(gap_fill = ifelse(mangrove > 0, 0, NA)) %>%
  mutate(habitat = "mangrove") %>%
  mutate(variable = "extent") %>%
  select(rgn_id, habitat, variable, gap_fill)
write.csv(extent_gf, 'globalprep/hab_mangrove/v2015/data/extent_gap_fill.csv', row.names=FALSE)

inland1km <- read.csv('globalprep/hab_mangrove/v2015/tmp/inland_1km_km2.csv') %>%
  select(rgn_id, mangrove_inland1km = inland_1k) %>%
  group_by(rgn_id) %>%
  summarize(mangrove_inland1km = sum(mangrove_inland1km, na.rm=TRUE)) %>%
  ungroup()

inland <- read.csv('globalprep/hab_mangrove/v2015/tmp/inland_km2.csv') %>%
  select(rgn_id = zone, inland = sum)

mangrove <- inland1km %>%
  left_join(all) %>%
  left_join(inland) %>%
  mutate(mangrove_offshore = mangrove - inland) %>%
  select(rgn_id, mangrove, mangrove_inland1km, mangrove_offshore)

mangrove <- gather(mangrove, 'habitat', 'km2', -rgn_id)
mangrove$km2[is.na(mangrove$km2)] <- 0

write.csv(mangrove, 'globalprep/hab_mangrove/v2015/data/habitat_extent_mangrove.csv', row.names=FALSE)




