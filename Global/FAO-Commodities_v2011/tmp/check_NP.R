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
np1 = read.csv('../ohi-global/eez2013/reports/debug/eez2013_np_1-harvest_lm-gapfilled_data.csv'); head(np1)
np2 = read.csv('../ohi-global/eez2013/reports/debug/eez2013_np_2-rgn-year-product_data.csv'); head(np2) # don't end up comparing this; the problem is earlier

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


## debug July 1 ----

# read in input layers from layers_global
layers_tonnes = read.csv(file.path(dir_d, 'data', 'FAO-Commodities_v2011_tonnes.csv')); head(layers_tonnes)
layers_usd    = read.csv(file.path(dir_d, 'data', 'FAO-Commodities_v2011_usd.csv')); head(layers_usd)

# read in calculated scores np
scores_np = read.csv(file.path(dir_d, 'tmp', 'scores_eez2012-2013_2014-07-01_vs_2013-10-09.csv')); head(scores_np)
prodyr_np = read.csv(file.path(dir_d, 'tmp', 'np_product-year_2014-07-01_vs_2013-10-09.csv')); head(prodyr_np)

# join
np_c = prodyr_np %>%
  left_join(layers_tonnes, 
            by = c('rgn_id', 'rgn_name', 'product', 'year')) %>%
  mutate(tonnes_dif = H_new - tonnes); head(np_c)

np_cdif = np_c %>%
  filter(!is.na(tonnes_dif)) %>%
  group_by(rgn_name, rgn_id, product) %>%
  summarize(count = n()); np_cdif

# join np_c with np_1_lm_debug.csv see which of these are modeled
npl = read.csv('Global/FAO-Commodities_v2011/tmp/np_1_lm_debug.csv', na.strings = ''); head(npl); summary(npl)

np_c_gf = np_c %>%
  select(rgn_name, rgn_id, product, year, 
         H_dif, S_dif, w_dif, tonnes_dif) %>%
  left_join(npl %>%
              select(
                rgn_name,
                rgn_id, 
                product,
                year, 
                tonnes_mdl,
                usd_mdl),
            by = c('rgn_name', 'rgn_id', 'product', 'year')) %>%
  filter(!is.na(tonnes_mdl)) %>%
  group_by(rgn_name, rgn_id, product) %>%
  summarize(count = n()); head(np_c_gf, 30) # don't know if this is actually helpful...

summary(np_c_gf)
# check out 


# check out China
filter(layers_tonnes, rgn_id == 209)




## compare input layers used in Nature2012, Oct2013 and Toolbox2013 to see how raw FAO data changed. (July 2) ----
library(reshape2)
library(stringr)
library(dplyr)
dir_neptune_data = '/Volumes/data_edit'
dir_d = '../ohiprep/Global/FAO-Commodities_v2011'


# raw files for Nature2012
# tonnes_nat = read.csv('')
# usd_nat    = read.csv('')

# raw files for 2013a (October2013)  # see prep_KL/NatProd.R to identify which files were used
dir_oct    = 'model/GL-NCEAS-NaturalProducts_v2013a/raw/prep_KL/NP'

tonnes_oct_tmp = read.csv(file.path(dir_neptune_data, dir_oct, 'GL-FAO-AllCombined_v2009-rgn-allyears-processed.csv'), na.strings=''); head(tonnes_oct_tmp)
names(tonnes_oct_tmp)[5:38]<-c(1976:2009)
tonnes_oct = tonnes_oct_tmp %>%
  melt(id=c('rgn_id', 'rgn_nam', 'commodity', 'layer'), variable='year') %>%
  select(rgn_id, 
         product = layer,
         year,
         tonnes = value) %>%
  mutate(product = str_replace(product, 'Coral' , 'corals'),
         product = str_replace(product, 'FishOil' , 'fish_oil'),
         product = str_replace(product, 'OrnamentalFish' , 'ornamentals'),
         product = str_replace(product, 'Seaweeds' , 'seaweeds'),
         product = str_replace(product, 'Shells' , 'shells'),
         product = str_replace(product, 'Sponges' , 'sponges')) %>%
  group_by(rgn_id, product, year) %>%
  summarize(tonnes = sum(tonnes)); head(tonnes_oct)         
            
tonnes_oct$year = as.numeric(as.character(tonnes_oct$year))
tonnes_oct$tonnes = as.numeric(as.character(tonnes_oct$tonnes)) 
tonnes_oct$tonnes[is.na(tonnes_oct$tonnes)] = 0 # for comparison below; tonnes_tbx all have 0's

dplyr::filter(tonnes_oct, rgn_id == 82) # debug
tonnes_oct[duplicated(tonnes_oct[, c('rgn_id', 'product', 'year')]),] 

weight_oct    = read.csv(file.path(dir_neptune_data, dir_oct, 'stacked_value_data.csv'), na.strings='') %>%
  select(rgn_id  = rgn_id_2013,
         product = Commodity..Commodity.,
         usd     = value); head(weight_oct)
 
# raw files for eez2013 (Toolbox 2013 calcs)
tonnes_tbx = read.csv(file.path(dir_d, 'data', 'FAO-Commodities_v2011_tonnes.csv')); head(tonnes_tbx)
usd_tbx    = read.csv(file.path(dir_d, 'data', 'FAO-Commodities_v2011_usd.csv')); head(usd_tbx)


tonnes_tbx[duplicated(tonnes_tbx[, c('rgn_id', 'product', 'year')]),] 
usd_tbx[duplicated(usd_tbx[, c('rgn_id', 'product', 'year')]),] 

# combine
np_c = usd_tbx %>%
  select(rgn_name, rgn_id, product, year, 
         usd_tbx = usd) %>%
  left_join(tonnes_tbx %>%
              select(rgn_name, rgn_id, product, year, 
                     tonnes_eez2013 = tonnes),
            by = c('rgn_name', 'rgn_id', 'product', 'year')) %>%
  inner_join(tonnes_oct %>%
               select(rgn_id, product, year, 
                      tonnes_2013a = tonnes),
             by = c('rgn_id', 'product', 'year')) %>%
  mutate(tonnes_dif = tonnes_eez2013 - tonnes_2013a) %>%
  arrange(rgn_id, product, year) %>%
  select(-usd_tbx); head(np_c)

np_diffs = np_c %>%
  filter(tonnes_dif != 0,
         year >= 2002, year <= 2009)

np_stats = np_diffs %>%
  group_by(rgn_name, rgn_id) %>%
  summarize(n_diffs_per_rgn = n())

np_stats2 = np_diffs %>%
  group_by(rgn_name, rgn_id, product) %>%
  summarize(n_diffs_per_rgn_product = n())

write.csv(np_diffs,  file.path(dir_d, 'tmp', 'NP_rawFAOharvest_diffs_2013a_eez2013.csv'), row.names=F)
write.csv(np_stats,  file.path(dir_d, 'tmp', 'NP_rawFAOharvest_diffs_2013a_eez2013_n_per_rgn.csv'), row.names=F)
write.csv(np_stats2, file.path(dir_d, 'tmp', 'NP_rawFAOharvest_diffs_2013a_eez2013_n_per_rgn_product.csv'), row.names=F)




