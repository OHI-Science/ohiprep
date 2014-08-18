## Goal:
##   Compare: 
##     - uniform prior with zeros (last OHI analysis, data is on file.path(dir_neptune_data, "model/GL-NCEAS-FIS_2014a/raw/cmsy.ohi.df_Jul292014.csv")
##     - uniform prior no zeros 
##     - original prior with zeros
##     - original prior no zeros

##  SOP: 1. format each file using the below code, save to the designated file location 
##       2. run OHI2013 and OHI2012 models 
##       3. save files to FIS_B_bmsy/Hex outputs
##       4. compare data files

library(dplyr)
dir_d = '../ohiprep/Global/NCEAS-Fisheries_2014a' # set folder where files are saved

###############################################
## B-Bmsy data----
###############################################
#b_bmsy <- read.csv(file.path(data, "raw/cmsy.ohi.df_Jul292014.csv"), na.strings='')
load("Global/FIS_Bbmsy/Hex outputs/cmsy_ohi_results_table_uniformPrior_no0s.RData")
b_bmsy <- cmsy.ohi.unif.no0.df

b_bmsy_lyr <- b_bmsy %>%
  mutate(fao_id = sapply(strsplit(as.character(stock_id), "_"), function(x)x[2]),
         taxon_name = sapply(strsplit(as.character(stock_id), "_"), function(x)x[1])) %>%
  select(fao_id, taxon_name, year=yr, b_bmsy)

write.csv(b_bmsy_lyr, file.path(dir_d, 'data/fnk_fis_b_bmsy_lyr.csv'), row.names=F, na='')
