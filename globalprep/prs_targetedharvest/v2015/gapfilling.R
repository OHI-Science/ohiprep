################################################
## targeted harvest pressure gapfilling
################################################
# regions without FAO data were assumed to be 0.  I am going to consider this 
# to be gapfilling.


## These are the regions with FAO data:
regions <- read.csv("globalprep/FAO_targetedharvest/FAO_regions.csv")

## Here is the pressure data:
pressure <- read.csv('globalprep/FAO_targetedharvest/data/rgn_fao_targeted_2015a.csv') %>%
  left_join(regions) %>%
  mutate(score = ifelse(is.na(value), 1, 0)) %>%
  select(rgn_id, score)


write.csv(pressure, 'globalprep/FAO_targetedharvest/data/rgn_fao_targeted_2015a_gf.csv', row.names=FALSE)
