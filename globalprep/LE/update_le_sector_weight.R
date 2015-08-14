#############################
# excluding oil and gas 
# from sector weight data
#############################
source('src/R/common.R')

data <- read.csv(file.path(dir_neptune_data, 
                  'model/GL-NCEAS-Pressures_v2013a/data/le_sector_weight_disaggregateNature2012b.csv'))
data <- data %>%
  filter(sector != "og")

write.csv(data, 'globalprep/LE/data/le_sector_weight.csv', row.names=FALSE)
