# ohi_regional_stats.r

# by JSL July 23 2013. 
# pull statistics for some regional assessments

## setup ----
source('../ohiprep/src/R/common.R') # set dir_neptune_data
library(stringr)
library(tools)
# library(reshape2)

# read in coastal pop from Neptune for 2013
c = read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-CoastalPopulation_v2013/data', 
                       'cntry_popsum2013_inland25mi_complete.csv')); head(c)

# read in total pop from Neptune for 2013
tp = read.csv(file.path(dir_neptune_data, 'model/GL-WorldBank-Statistics_v2012/data', 
                       'rgn_wb_pop_2013a_updated.csv')) %>%
  group_by(rgn_id) %>%
  filter(year == max(year)) %>% # just save the most recent year
  mutate(layer = 'total_pop') %>%
  select(rgn_id, value = count, layer); head(tp)

## read in regional assessments regions ----
nonCountry = c('U.S. West Coast', 'Baltic Sea', 'Hawaii')

RAs = read.csv('../ohiprep/tmp/ohi_regional/rgns_regional.csv') %>%
  select(rgn_nam = region,
         status) %>%
  filter(!rgn_nam %in% nonCountry) %>%
  left_join(read.csv('../ohi-global/eez2013/layers/rgn_global.csv') %>%
              select(rgn_id, 
                     rgn_nam = label),
            by='rgn_nam') %>%
  select(rgn_nam, rgn_id, status) %>%
  arrange(status, rgn_nam); head(RAs)

# read in layers from layers.csv ----
loi = list('area_eez' = 'rgn_area') # layers of interest

layers = read.csv('../ohi-global/eez2013/layers.csv') %>%
  filter(layer %in% loi)

# loop through from layers.csv
if (exists('d')) rm(d)
for (i in 1:length(layers$filename)) { # i=1
  
  l = read.csv(file.path('../ohi-global/eez2013/layers', layers$filename[i])); head(l)
  nam = names(loi)[loi == file_path_sans_ext(layers$filename[i])]
  names(l)[!names(l) %in% c('rgn_id', 'cntry_key', 'sector', 'category')] = 'value'
  
  # sum by rgn_id, add layer indicator
  h = l %>% 
    group_by(rgn_id) %>%
    summarize(value = sum(value, na.rm=T)) %>% # just need to sum by rgn_id
    mutate(layer = nam) %>%
    arrange(rgn_id); head(h); summary(h) 
  
  # rbind
  if (!exists('d')){
    d = h
  } else {
    d = rbind(d, h)
  }
}
head(d)


## read in LE 2014 data ----

dirLE = '../ohiprep/Global/NCEAS-Livelihoods_v2014/data'

# loop through from LE 2014
if (exists('w')) rm(w)
for (j in list.files(dirLE, pattern=glob2rx('*.csv'), full.names=T)){ # j="../ohiprep/Global/NCEAS-Livelihoods_v2014/data/cntry_jobs_2014a.csv"
  
  l = read.csv(j); head(l)
  
  # rename 'value' field
  nam = str_split_fixed(basename(file_path_sans_ext(j)), '_', 2)[2]
#   nam = names(l)[!names(l) %in% c('rgn_id', 'cntry_key', 'sector', 'category', 'year')] 
  names(l)[!names(l) %in% c('rgn_id', 'cntry_key', 'sector', 'category', 'year')] = 'value'
 
  # if cntry_key, translate to rgn_id
  if ('cntry_key' %in% names(l)){
    
    lf = l %>%
      left_join(c,
                by = 'cntry_key') %>% 
      filter(!cntry_key %in% c('ATA')) %>% # Antarctica
      select(rgn_id, sector, value)        
  }
  
  # sum by rgn_id, add layer indicator
  h = lf %>% 
    group_by(rgn_id) %>%
    summarize(value = sum(value, na.rm=T)) %>% # just need to sum by rgn_id
    mutate(layer = nam) %>%
    arrange(rgn_id); head(h); summary(h) 
  
  # rbind
  if (!exists('w')){
    w = h
  } else {
    w = rbind(w, h)
  }
}
head(w)

## coastal pop and coastal inland area from Neptune ----
cp = c %>%
  select(rgn_id, 
         value = cntry_popsum2013_inland25mi) %>%
  mutate(layer = 'coastal_pop')

ca = c %>%
  select(rgn_id, 
         value = area_km2) %>%
  mutate(layer = 'area_inland25km')

# coastline length from Neptune
cl = read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-OceanRegions_v2013a/data', 
                        'rgn_area_inland1km.csv')) %>%
  select(rgn_id, 
         value = area_km2) %>%
  mutate(layer = 'area_inland1km')


## combine all data layers 
h_g = rbind(d, w, ca, cp, cl, tp) %>%
  group_by(rgn_id, layer) %>%
  summarize(value = sum(value, na.rm = T)) %>% # stopifnot(anyDuplicated(h_g[,c('rgn_id', 'layer')]) == 0)
  dcast(rgn_id ~ layer, value.var = 'value') %>%
  select(rgn_id, area_eez, area_inland1km, coastal_pop, total_pop,
         coastal_jobs = jobs_2014a, 
         total_rev    = rev_2014a)

# calculate coastal ratio
h_g$coastal_pop[h_g$total_pop == 0] = 0 # to remove infinities
h_g = h_g %>%  
  mutate(coastal_pop_ratio = coastal_pop/total_pop)
h_g$coastal_pop_ratio[h_g$coastal_pop_ratio > 1] = 1 # cap at 1 for island nations
 head(h_g); summary(h_g)

# calculate coastal rev
h_g = h_g %>%
  mutate(
    coastal_rev  = total_rev  * coastal_pop_ratio)
head(h_g,20); summary(h_g)

## save ----
f_out = '../ohiprep/tmp/ohi_regional/regional_stats'


# save global sum
hb_globalsum <- data.frame(t(colSums(h_g, na.rm=T))); hb_globalsum
write.csv(hb_globalsum, paste(f_out, '_globalsum.csv', sep = ''), row.names = F, na = '')

# save only regional assessments
h_ra = h_g %>%
  filter(rgn_id %in% RAs$rgn_id) %>%
  left_join(RAs, by = 'rgn_id') 

# h_ra$rgn_id %in% above1$rgn_id  # a check from above1

h_ra = h_ra %>%
  select(rgn_nam, status, area_eez, area_inland1km, coastal_pop, coastal_jobs, coastal_rev); head(h_ra)

# add 2 eezs by hand: 
# area_eez from Wikipedia: http://en.wikipedia.org/wiki/Exclusive_economic_zone#United_States
h_ra2 = rbind(h_ra, 
            data.frame(rgn_nam = 'US West Coast', status='current', area_eez=895346+1579538, 
                       area_inland1km=NA, coastal_jobs=NA, coastal_pop=NA, coastal_rev=NA),
            data.frame(rgn_nam = 'Hawaii', status='current', area_eez=825549, 
                       area_inland1km=NA, coastal_jobs=NA, coastal_pop=NA, coastal_rev=NA)) %>%
  arrange(status, rgn_nam); h_ra2

write.csv(h_ra2, paste(f_out, '.csv', sep = ''), row.names = F, na = '')


# save regional assessment sum
h_racurrent_sum <- data.frame(t(colSums(h_ra2 %>% 
                                          filter(status == 'current') %>%
                                          select(-rgn_nam, -status),
                                        na.rm=T))); h_racurrent_sum
write.csv(h_racurrent_sum, paste(f_out, '_currentRAsum.csv', sep = ''), row.names = F, na = '')


# a few calculations:
p = as.numeric(h_racurrent_sum$area_eez/hb_globalsum$area_eez *100 )
cat(sprintf('Currently, OHI covers %f percent of global EEZs, benefits %d people (the population living coastally), accounts for %f coastal jobs and %f dollars of coastal revenue', 
            p, h_racurrent_sum$coastal_pop, h_racurrent_sum$coastal_jobs, h_racurrent_sum$coastal_rev))


# --- fin ---

# Currently, OHI covers 4.949682 percent of global EEZs, benefits 340261751 people, accounts for 2 260 341 088.95 coastal jobs and 6 313 662 315 354.815430
