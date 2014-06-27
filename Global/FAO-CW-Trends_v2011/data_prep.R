# data_prep.R

# Prepare FAO fertilizer/pesticides data for CW trends. 
# By JSLowndes Apr2014; File was originally clean_FAOtrends.r:(by JStewart Jul2013)

# search for 'scenario =' and you are able to change it to the year of the global assessment desired.

# for trends in pesticides and fertilizers. Note that for 2014a, only pesticides are updated (fertilizer 2011 data are all 0's)
# Data are in a new format since 2013a. Available from http://faostat3.fao.org/faostat-gateway/go/to/download/R/*/E

#   read in individual files
#   remove Totals row
#   remove/translate FAO data codes (F, ..., -, 0 0)
#   add identifier column
#   concatenate data from each file into a single file
#   run name_to_rgn.r (function by J. Stewart, B. Best)
#   save single file
  

# setup ----

# load libraries
# load libraries
library(gdata)
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

# get paths.  NOTE: Default path should be ohiprep root directory.
source('src/R/common.R') # set dir_neptune_data
source('src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()
dir_d = file.path('../../Global/FAO-CW-Trends_v2011')


## read in and process files ----

for (f in list.files(path = file.path(dir_d, 'raw'), pattern=glob2rx('*csv'), full.names=T)) {  # f = "../../Global/FAO-CW-Trends_v2011/raw/FAO_fertilizers_thru2011.csv"
  
  d.fao = read.csv(f, header=F); head(d.fao)
  
  v = strsplit(as.character(f), '_') 
  names(d.fao) = c(unlist(v)[3], 'country', 'category', 'unit', 'year', 'tonnes'); head(d.fao)
  
  # clean up
  d = d.fao %.%
    select(country, category, year, tonnes) %.%
    group_by(country, year) %.%
    summarise(tonnes = sum(tonnes)); head(d)
  
  ## add rgn_ids with name_to_rgn ----
 m_d = name_to_rgn(gci, fld_name='country', flds_unique=c('country'), fld_value='score', add_rgn_name=T) 

  uifilesave = file.path(dir_d, 'raw', paste('FAO-', unlist(v)[3], '-trends_v2011-cleaned.csv', sep=''))
  add_rgn_id(d, uifilesave)
  
}

## Further processing ----
## Treat Pesticides and Fertilizer differently because Fertilizers have weird 0's and Pesticides don't 

## Pesticides ----
g = "Global/FAO-CW-Trends_v2011/raw/FAO-pesticides-trends_v2011-cleaned.csv"
pest = read.csv(g); head(pest,30)

## clean up data: described in Global SOM 2013: section 5.19 ----

# see if there are a lot of 0's
explore = pest %.%
  filter(tonnes == 0); head(explore,30) # no there aren't

# see if there are countries with only 1 year of data 
explore2 = pest %.%
  group_by(rgn_id, rgn_nam) %.% 
  summarise(count = n()) %.%
  filter(count == 1); explore2 # yes there are

pest2 = pest %.% # remove countries with only 1 year of data 
  filter(!rgn_id %in% explore2$rgn_id)
  
# see if there are countries no data after 2005 
explore3 = pest2 %.%
  group_by(rgn_id, rgn_nam) %.% 
  summarise(max_year = max(year)) %.%
  filter(max_year < 2005); explore3 # yes there are

pest3 = pest2 %.% # remove countries with no data after 2005 
  filter(!rgn_id %in% explore3$rgn_id)

# gapfilling follows, both for pesticides and fertilizers together. 


## Fertilizers ----
g = "Global/FAO-CW-Trends_v2011/raw/FAO-fertilizers-trends_v2011-cleaned.csv"
fert = read.csv(g); head(fert,30)

# clean up data: described in Global SOM 2013: section 5.19
# see if there are a lot of 0's
explore = fert %.%
  filter(tonnes == 0); head(explore,30) # yes they are

fert2 = fert %.% 
  filter(tonnes !=0); head(fert2,30) # remove 0's; don't replace with NA because lm() below will need NAs removed 

# see if there are countries with only 1 year of data 
explore2 = fert2 %.%
  group_by(rgn_id, rgn_nam) %.%
  summarise(count = n()) %.%
  filter(count == 1); explore2 # yes there are 

fert3 = fert2 %.% # remove countries with only 1 year of data 
  filter(!rgn_id %in% explore2$rgn_id)

# see if there are countries no data after 2005 
explore3 = fert3 %.%
  group_by(rgn_id, rgn_nam) %.% 
  summarise(max_year = max(year)) %.%
  filter(max_year < 2005); explore3 # no there aren't




## calculate trend and gapfill for both fertilizers and pesticides:: KLo style. ----
# See readme.md and Global SOM 2013 section 5.19. Approach by Katie Longo, September 2013: github/ohiprep/Global/FAO-CW-Trends_v2011/raw/Fertilizer_Pesticide_trend_KLongo2013.R
# trend years: 2012a (2005:2009) and 2013a (2006:2010). Note:: error in KLo 2013 approach: trend was created through multiplying slope by 4 instead of 5

# set up: be able to specify the scenario. fert's timeseries is one year shorter than

scenario = 2013 # change to 2014, 2013, or 2012

if      (scenario == 2014){
  x1_pest = 2007
  x1_fert = 2006
  scenlab = '2014a'
}else if(scenario == 2013){
  x1_pest = 2006
  x1_fert = 2005
  scenlab = '2013a'
}else if(scenario == 2012){
  x1_pest = 2005
  x1_fert = 2004
  scenlab = '2012a'
}


# 1) calculate fert and pest trend, excluding NAs (done above), for 2014a. fert = 2006:2010 (data not actually updated); pest = 2007:2011
## this is the logic for the calc_trend function below using ddply and summarize. JSL April 2014: couldn't get dplyr to work with lm(). 
# slope = lm(fert$tonnes ~ fert$year)$coefficients[[2]]
# intercept = lm(fert$tonnes ~ fert$year)$coefficients[[1]]
# x1 = 2005
# y1 = x1*slope+intercept
# trend = max(min(slope/(y1) * 5, 1), -1) # normalize slope by y in a given year (we use x1, but could be any year), multiple by 5 for the trend and bound it between -1 and 1. 

# ---------
# function that calculates trend
  calc_trend = function(data, x1) {
    library('plyr')
    trend = plyr::ddply(
      data, .(rgn_id), summarize,
      trend = max(min(lm(value ~ year)$coefficients[[2]] /
                        (lm(value ~ year)$coefficients[[2]]*x1 +
                           lm(value ~ year)$coefficients[[1]]) * 5, 1), -1))
    return(trend)
  }
# --------

## calculate pesticide trends, start whence bookkeeping: 
data = na.omit(pest3)
x1 = x1_pest
names(data) = c('rgn_id', 'rgn_nam', 'value', 'year')
trend_pest = calc_trend(data, x1) %.%
  mutate(whencev01 = 'OD',
         whence_choice = 'OD'); head(trend_pest)

## calculate fertilizer trends: 
data = na.omit(fert3)
names(data) = c('rgn_id', 'rgn_nam', 'value', 'year')
x1 = x1_fert
trend_fert = calc_trend(data, x1) %.%
  mutate(whencev01 = 'OD',
         whence_choice = 'OD'); head(trend_fert) 

detach('package:plyr', unload=T)
library(dplyr)

## 2) calculate pop trend. Example: 2014a for fert (2006:2010) and pest (2007:2011) ----
# no gapfilling of pop required as was done in 2013; the pop file called is complete (missing rgn_ids are unpopulated).
  
pop_file = file.path(dir_neptune_data, 'model/GL-NCEAS-CoastalPopulation_v2013/data/', 'rgn_popsum2005to2015_inland25mi.csv') # dir_neptune_data defined in common.R                  
pop = read.csv(pop_file) %.%
  filter(rgn_id < 255); head(pop)

## calculate pop trends for pest years:
data = pop[pop$year %in% c(x1_pest:(x1_pest+4)),] # 4 here will actually give the 5-year trend
names(data) = c('rgn_id', 'year', 'value')
trend_pop_forpest = calc_trend(data, x1_pest) %.%
  mutate(whencev01 = 'XP',
         whence_choice = 'coastalpop'); head(trend_pop_forpest)

## calculate pop trends for fert years:
data = pop[pop$year %in% c(x1_fert:(x1_fert+4)),] 
names(data) = c('rgn_id', 'year', 'value')
trend_pop_forfert = calc_trend(data, x1_fert)%.%
  mutate(whencev01 = 'XP',
         whence_choice = 'coastalpop'); head(trend_pop_forfert)

## 3) join fert and pest trends with appropriate pop trends, add whence bookkeeping ----
# identify which rgn_ids aren't represented in pest and fert files (using anti_join)

# pesticides
trend_pest_toadd = trend_pop_forpest %.%
  anti_join(trend_pest, by='rgn_id') 

trend_pest2 = rbind(trend_pest, trend_pest_toadd) %.%
  arrange(rgn_id); head(trend_pest2)

# fertilizers
trend_fert_toadd = trend_pop_forfert %.%
  anti_join(trend_fert, by='rgn_id') 

trend_fert2 = rbind(trend_fert, trend_fert_toadd) %.%
  arrange(rgn_id); head(trend_fert2)


## 4) make sure southern islands are trend = 0 and uninhabited islands are trend = NA. 
# any additional missing regions set to NA. 

rgns = read.csv('src/LookupTables/eez_rgn_2013master.csv') %.%
  select(rgn_id = rgn_id_2013) %.%
  mutate(trend = NA,
         whencev01 = NA,         #set up these columns for use later on
         whence_choice = NA) %.%
  filter(rgn_id < 255) %.%
  arrange(rgn_id); head(rgns)

# prep island data
islands = read.csv(file.path('src/LookupTables', 'rgn_southern_uninhabited_islands_2013SOM_tableS6.csv')); islands
trend_island = islands 
trend_island$trend = NA
trend_island$trend[trend_island$Inhabited == 1] = 0
trend_island = trend_island %.%
  mutate(whencev01 = 'XSI', 
         whence_choice = 'southern') %.%
  select(rgn_id, trend, whencev01, whence_choice); head(trend_island)

## pesticides

# identify which islands are already present -- remove them
islands_present = trend_pest2 %.%
  inner_join(islands, by = 'rgn_id')

trend_pest3 = trend_pest2 %.% 
  filter(!rgn_id %in% islands_present$rgn_id)

# rgns2 = rgns %.%
#   anti_join(trend_pest3, by='rgn_id') %.%
#   arrange(rgn_id)

trend_pest4 = rbind(trend_pest3, trend_island) %.% # would have to add rgns2
  arrange(rgn_id); head(trend_pest4)



## fertilizers

# identify which islands are already present -- remove them
islands_present = trend_fert2 %.%
  inner_join(islands, by = 'rgn_id')

trend_fert3 = trend_fert2 %.% 
  filter(!rgn_id %in% islands_present$rgn_id)

# rgns2 = rgns %.%
#   anti_join(trend_fert3, by='rgn_id') %.%
#   arrange(rgn_id)


trend_fert4 = rbind(trend_fert3, trend_island) %.% # would have to add rgns2
  arrange(rgn_id); head(trend_fert4)


## 5) save as poth pressure trend and CW scores

write.csv(trend_pest4, file.path(dir_d, 'data', 
                                 paste('p_pesticides_trends_', scenlab, '.csv', sep='')), row.names = F)
write.csv(trend_fert4, file.path(dir_d, 'data', 
                                 paste('p_fertilizers_trends_', scenlab, '.csv', sep='')), row.names = F)

# calc CW scores and save
# clean waters scores are the 'inverse' of pressures, so the delta in score is the 'inverse' of the pressure trend

# pest
trend_pest_final = trend_pest4 %.%
  mutate(trend = trend*-1) %.%
  arrange(rgn_id)
names(trend_pest_final)[2] = 'trend.score'; head(trend_pest_final)

# fert
trend_fert_final = trend_fert4 %.%
  mutate(trend = trend*-1) %.%
  arrange(rgn_id)
names(trend_fert_final)[2] = 'trend.score'; head(trend_fert_final)

write.csv(trend_pest_final, file.path(dir_d, 'data', 
                                      paste('rgn_cw_pesticides_trends_', scenlab, '.csv', sep='')), row.names = F)
write.csv(trend_fert_final, file.path(dir_d, 'data', 
                                      paste('rgn_cw_fertilizers_trends_', scenlab, '.csv', sep='')), row.names = F)

# --- fin

