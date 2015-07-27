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

#####################################################
### gap-fill and organize trend data
## Two countries: Republique of Congo (100) and Mauritania (199) have trend data, but no mangrove
## Mauritania has 0 in all trend years except maybe one. 
## I checked our Congo data and didn't see any mangrove raster cells in this region
## so I am going to assume that our 0 value is in fact correct.


for(year in 2012:2015){ #year <- '2012'
regions <- read.csv('src/LookupTables/rgn_georegions_wide_2013b.csv')
data <- read.csv(sprintf('globalprep/hab_mangrove/v2015/tmp/habitat_trend_mangrove_v%s.csv', year))

print(sprintf('year: %s', year))
print('deleted: ') 
print(setdiff(data$rgn_id, all$rgn_id))
print('gap-filled: ')
print(setdiff(all$rgn_id, data$rgn_id))

tmp <- all %>%
  left_join(data)  

trend_gaps <- tmp %>%
  mutate(gap_fill = ifelse(is.na(trend), "1", "0")) %>%
  select(rgn_id, habitat, gap_fill) %>%
  mutate(habitat="mangrove")
write.csv(trend_gaps, sprintf('globalprep/hab_mangrove/v2015/data/trend_gap_fill_v%s.csv', year), row.names=FALSE)

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
regions <- read.csv('src/LookupTables/rgn_georegions_wide_2013b.csv')

setdiff(all$rgn_id, health$rgn_id) #mangrove extent data but no health score
setdiff(health$rgn_id, all$rgn_id) #these have health scores, but no mangrove data, I think this is an artifact of previous gapfilling

tmp <- all %>%
  left_join(health)  

health_gaps <- tmp %>%
  mutate(gap_fill = ifelse(is.na(health), "1", "0")) %>%
  select(rgn_id, habitat, gap_fill) %>%
  mutate(habitat="mangrove")
write.csv(health_gaps, 'globalprep/hab_mangrove/v2015/data/health_gap_fill.csv', row.names=FALSE)

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
  select(rgn_id, habitat, health=health3)
summary(tmp)

write.csv(tmp, 'globalprep/hab_mangrove/v2015/data/habitat_health_mangrove.csv', row.names=FALSE)


####################################
## extent data

all <- read.csv('globalprep/hab_mangrove/v2015/tmp/eez_and_land_km2.csv') %>%
  select(rgn_id = zone, mangrove = sum)

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
