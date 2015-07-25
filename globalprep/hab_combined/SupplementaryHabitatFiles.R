########################################
## creating supplementary habitat files
########################################
library(dplyr)

# In future iterations, we should have these files generated
# within functions.R

################################################
### pressures cp: cp_habitat_extent_rank
## rgn_id, habitat, extent_rank
## habitat levels: coral, seagrass, saltmarsh, seaice_shoreline, mangrove
## mangrove includes the 1km inland and eez area 
## only includes areas > 0

## extent (km2) data are multiplied by the ranks in protectiveness

data <- read.csv('globalprep/hab_combined/v2015/data/habitat_extent_v2015.csv')

data <- data %>%
  filter(habitat %in% c('coral', 
                        'seagrass', 
                        'saltmarsh', 
                        'seaice_shoreline', 
                        'mangrove_inland1km', 
                        'mangrove_offshore')) %>%
  mutate(habitat = gsub('_inland1km', '', habitat)) %>%
  mutate(habitat = gsub('_offshore', '', habitat)) %>%
  group_by(rgn_id, habitat) %>%
  summarize(extent = sum(km2)) %>%
  ungroup() %>%
  filter(extent > 0)

habitat.rank <- data.frame(habitat = c('coral', 'mangrove', 'saltmarsh', 'seagrass', 'seaice_shoreline'),
                           rank = c(4, 4, 3, 1, 4))

data <- data %>%
  left_join(habitat.rank, by='habitat') %>%
  mutate(extent_rank = extent*rank) %>%
  select(rgn_id, habitat, extent_rank)

write.csv(data, 'globalprep/hab_combined/v2015/data/cp_habitat_extent_rank.csv', row.names=FALSE)
  
  
################################################
### pressures cs: cs_habitat_extent_rank
## rgn_id, habitat, extent_rank
## habitat levels: seagrass, saltmarsh, mangrove
## mangrove includes all the mangrove area
## only includes areas > 0

## extent (km2) data are multiplied by amount of carbon storage

data <- read.csv('globalprep/hab_combined/v2015/data/habitat_extent_v2015.csv')

data <- data %>%
  filter(habitat %in% c('seagrass', 
                        'saltmarsh', 
                        'mangrove')) %>%
  filter(km2 > 0)


habitat.rank <- data.frame(habitat = c('mangrove', 'saltmarsh', 'seagrass'),
                           rank = c(139, 210, 83))

data <- data %>%
  left_join(habitat.rank, by='habitat') %>%
  mutate(extent_rank = km2*rank) %>%
  select(rgn_id, habitat, extent_rank)

write.csv(data, 'globalprep/hab_combined/v2015/data/cs_habitat_extent_rank.csv', row.names=FALSE)
summary(data)
table(data$habitat)

################################################
### pressures HAB: hab_presence
## rgn_id, habitat, boolean
## habitat levels: coral, seaice_edge, soft_bottom, seagrass, saltmarsh, mangrove
## mangrove includes all the mangrove area
## boolean only includes 1 values

data <- read.csv('globalprep/hab_combined/v2015/data/habitat_extent_v2015.csv')

data <- data %>%
  filter(habitat %in% c('seagrass', 
                        'saltmarsh', 
                        'mangrove', 
                        'coral', 
                        'seaice_edge',
                        'soft_bottom')) %>%
  filter(km2 > 0) %>%
  mutate(boolean = 1) %>%
  select(rgn_id, habitat, boolean)

table(data$habitat)
head(data)
summary(data)

write.csv(data, 'globalprep/hab_combined/v2015/data/hab_habitat_presence.csv', row.names=FALSE)
