# check_NP.r
# investigate NP calculated scores; compare raw values as well

# load libraries
library(gdata)
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall


# get paths.  NOTE: Default path should be ohiprep root directory.
source('src/R/common.R') # set dir_neptune_data
source('src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()
dir_d = 'Global/FAO-Commodities_v2011/data'

# read in input layers for Toolbox
layers_tonnes = read.csv(file.path(dir_d, 'FAO-Commodities_v2011_tonnes_lyr.csv'))
layers_usd    = read.csv(file.path(dir_d, 'FAO-Commodities_v2011_usd_lyr.csv'))