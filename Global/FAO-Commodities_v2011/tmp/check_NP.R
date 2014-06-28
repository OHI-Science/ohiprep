# check_NP.r
# investigate NP calculated scores; compare raw values as well. June 2014

# load libraries
library(gdata)
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

# get paths.
source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()
dir_d = '../ohiprep/Global/FAO-Commodities_v2011'

# read in original input layers from FAO
fao_tonnes = read.csv(file.path(dir_d, 'data', 'FAO-Commodities_v2011_tonnes.csv')); head(fao_tonnes)
fao_usd    = read.csv(file.path(dir_d, 'data', 'FAO-Commodities_v2011_usd.csv')); head(fao_usd)

# read in input layers from layers_global
layers_tonnes = read.csv(file.path(dir_d, 'data', 'FAO-Commodities_v2011_tonnes_lyr.csv')); head(layers_tonnes)
layers_usd    = read.csv(file.path(dir_d, 'data', 'FAO-Commodities_v2011_usd_lyr.csv')); head(layers_usd)

# read in np debug reports
np1 = read.csv('eez2013/reports/debug/eez2013_np_1-harvest_lm-gapfilled_data.csv'); head(np1)
np2 = read.csv('eez2013/reports/debug/eez2013_np_2-rgn-year-product_data.csv'); head(np2) # don't end up comparing this; the problem is earlier

## 1. compare tonnes, usd from layers_global with np1 debug report::: there are differences:: 7834/12011 rows ----
np1_c = np1 %>%
  select(rgn_name, rgn_id, product, year, 
         tonnes_lyr = tonnes, 
         usd_lyr = usd) %>%
  left_join(layers_tonnes, 
            by = c('rgn_id', 'product', 'year')) %>% 
  mutate(tonnes_diff = tonnes_lyr - tonnes) %>%
  left_join(layers_usd, 
            by = c('rgn_id', 'product', 'year')) %>% 
  mutate(usd_diff = usd_lyr - usd) %>%
  select(rgn_name, rgn_id, product, year, 
         tonnes_lyr, tonnes, tonnes_diff, 
         usd_lyr   , usd,    usd_diff); head(np1_c)

np1_diff = np1_c %>% 
  filter(tonnes_diff != 0 | usd_diff != 0) %>%
  filter(!is.na(tonnes_diff) | !is.na(usd_diff))
head(np1_diff) # weird that tonnes and usd have no decimals
#         rgn_name rgn_id     product year tonnes_lyr tonnes tonnes_diff usd_lyr usd usd_diff
# 1  New Caledonia      5 ornamentals 1987       2.75      5       -2.25   32.50  62   -29.50
# 2  New Caledonia      5 ornamentals 1988       2.00      0        2.00   26.75   0    26.75
# 3  New Caledonia      5 ornamentals 1989       2.00      0        2.00   26.75   0    26.75
# 4  New Caledonia      5 ornamentals 1990       1.25      0        1.25   15.50   0    15.50
# 5  New Caledonia      5 ornamentals 1997       1.00      4       -3.00   32.75 131   -98.25
# 6  New Caledonia      5 ornamentals 1998       2.25      5       -2.75   60.50 111   -50.50
summary(np1_diff)
np1_c[30:60,]

## 2.  compare tonnes, usd from layers_global with original FAO data::: there are differences:: 7834/12011 rows ----
fao_c = fao_usd %>%
  left_join(fao_tonnes, 
            by = c('rgn_name', 'rgn_id', 'product', 'year')) %>%
  select(rgn_name, rgn_id, product, year, 
         usd_fao = usd,
         tonnes_fao = tonnes) %>%
  left_join(layers_tonnes, 
            by = c('rgn_id', 'product', 'year')) %>% 
  mutate(tonnes_diff = tonnes_fao - tonnes) %>%
  left_join(layers_usd, 
            by = c('rgn_id', 'product', 'year')) %>% 
  mutate(usd_diff = usd_fao - usd) %>%
  select(rgn_name, rgn_id, product, year, 
         tonnes_fao, tonnes, tonnes_diff, 
         usd_fao   , usd,    usd_diff); head(fao_c)

fao_diff = fao_c %>% 
  filter(tonnes_diff != 0 | usd_diff != 0) %>%
  filter(!is.na(tonnes_diff) | !is.na(usd_diff))
head(fao_diff) # no differences


scores_np = read.csv(file.path(dir_d, 'tmp', 'scores_eez2012-2013_2014-06-27_vs_2013-10-09.csv'))



