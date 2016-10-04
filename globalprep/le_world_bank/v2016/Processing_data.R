
### Download and clean the World Bank data
### see mazu: globalprep/_raw_data/WorldBank/d2016 README.md for information
### Clean with: R/process_WorldBank.R (saves: intermediate/xxx.csv, files)
### Data Downloaded: 7/20/2016
#####################################################################

library(devtools)
devtools::install_github("ohi-science/ohicore@dev") 
#devtools::install_github("ohi-science/ohicore@master")
#install_github('rCharts', 'ramnathv')
library(ohicore)
library(readr)

source('src/R/common.R')

# where to save data:
dir_int <- 'globalprep/supplementary_data/v2016/intermediate'

## describe location of raw data:
dir_wb <- file.path(dir_M, 'git-annex/globalprep/_raw_data/WorldBank/d2016/raw')

## get list of files
wb_file_list <- list.files(path = dir_wb, pattern=glob2rx('*csv'), full.names = TRUE)


source('globalprep/supplementary_data/v2016/process_WorldBank.R', local = TRUE)


#######################################################################################################
###### Population data
###### Get most recent year of population data and supplement with Wikipedia information on population
########################################################################################################

regions <- rgn_master %>%
  filter(rgn_nam_2013 != "DISPUTED") %>%
  filter(rgn_nam_2013 != "Disputed") %>%
  filter(rgn_typ == "eez") %>%
  filter(rgn_id_2013 <= 250) %>%
  filter(rgn_id_2013 != 213) %>%
  select(rgn_id = rgn_id_2013, rgn_nam_2013) %>%
  unique() %>%
  arrange(rgn_id)

pop <- read.csv('globalprep/supplementary_data/v2016/intermediate/wb_rgn_POP.csv') %>%
  filter(year == max(year)) %>%
  full_join(regions) 

write.csv(pop, 'globalprep/supplementary_data/v2016/intermediate/wb_rgn_POP_2015_missing_values.csv', row.names=FALSE)
## fill in missing data by hand and resave!