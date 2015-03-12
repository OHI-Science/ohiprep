## This takes the data produced by the .py script and organizes it for the Toolbox
## Modified MRF: Feb 26 2015
library(foreign) #read dbf files
library(dplyr)
library(tidyr)

#source('src/R/common.R')
dir_prod = 'globalprep/WDPA_MPA/v2015'

# read in tabulated areas and write out LSP layers ----
lyrs = c('rgn_offshore3nm_wdpa_2014des.dbf' = 'lsp_protarea_offshore3nm_2014des.csv',
         'rgn_inland1km_wdpa_2014des.dbf'   = 'lsp_protarea_inland1km_2014des.csv',
         'rgn_offshore3nm_wdpa_2014iucn.dbf' = 'lsp_protarea_offshore3nm_2014iucn.csv',
         'rgn_inland1km_wdpa_2014iucn.dbf'   = 'lsp_protarea_inland1km_2014iucn.csv')

## Compare different datasets:
for (i in 1:length(lyrs)){  
#  i=1
  dbf = names(lyrs)[i]
  csv = lyrs[[i]]

  d = read.dbf(file.path(dir_prod, 'tmp/iucn_dbfs', dbf))

m <- gather(d, year, area_m2, VALUE_0:VALUE_2013)

m <- m %>%
  mutate(year=as.integer(gsub('VALUE_', '', year))) %>%
  mutate(area_km2 = area_m2/(1000*1000)) %>%      
  select(rgn_id=VALUE, year, area_km2) %>%
  arrange(rgn_id, year)

  # fix  for  lsp_protarea_offshore3nm.csv: make any non-represented rgn_id == 0, and 2 special fixes. 
  # see https://github.com/OHI-Science/ohidev/blob/master/report/compare_scores2layers/compare_scores2layers.md#lasting-special-places
length(table(m$rgn_id)) #not all regions had data, need to add those

    rgns = read.csv('../ohi-global/eez2014/layers/rgn_global.csv') %>%
      select(rgn_id)  %>%
      filter(rgn_id < 255) %>%
      arrange(rgn_id)

rgns <- expand.grid(rgn_id=rgns$rgn_id, year=unique(m$year), area_km2=0)

# Here we might want to add in all the year data and not just the max year - use expand.grid for this.
    m <- rbind(m, 
            rgns %>%
                anti_join(m, by = 'rgn_id')) 

     m <- m %>%
      arrange(rgn_id, year)

length(table(m$rgn_id)) #now the regions are there...

# if(i==1){
#   # I am not sure why there are special fixes for these regions...but Julie can probably tell us because she made these additions.
#   # These are only done for the 3nm file...also, this code needs to be redone because I didn't delete the zeros as was done previously.
#   # We will need to look at the 3nm data to see if this generally makes sense
#   
#     m$area_km2[m$rgn_id == 78] = 1.746501543  # special fix for Lebanon[78]
#     m$year[m$rgn_id ==78] = 1972
#     m$area_km2[m$rgn_id == 220] = 0.873250772 # special fix for Sint Maarten[220]
#     m$year[m$rgn_id == 220] = 2006
#} 
   
  # save layer
  write.csv(m, file.path(dir_prod, 'data_iucn', csv), row.names=F, na='')
  
   # get top 10 for sanity check
  cat(sprintf('%s\n',csv))
  s = m %>%
    group_by(rgn_id) %>%
    summarize(
      sum_area_km2 = sum(area_km2)) %>%
    arrange(desc(sum_area_km2)) %>%
    as.data.frame()
  print(head(s, 10), row.names=F)  
}

## Values for 2015 analysis:
# lsp_protarea_offshore3nm.csv
# rgn_id sum_area_km2
# 163    179337.00
# 16    103046.00
# 73     89564.75
# 216     81235.25
# 145     61462.00
# 218     33861.25
# 223     31300.25
# 224     29457.75
# 171     24853.75
# 135     24186.50

# lsp_protarea_inland1km.csv
# rgn_id sum_area_km2
# 163     54926.00
# 224     32964.75
# 73     28656.25
# 16     24052.50
# 218     17682.25
# 145     16940.25
# 171     14081.50
# 216     12391.75
# 210     10413.75
# 180      8464.25

## Values for 2014 analysis:
# lsp_protarea_inland1km.csv
#  rgn_id sum_area_km2
#     163       107914
#      16        98833
#      73        98357
#     216        78680
#     145        61743
#     218        38173
#     224        34266
#     223        32800
#     135        25015
#     171        24478
# lsp_protarea_offshore3nm.csv
#  rgn_id sum_area_km2
#     163        51540
#     224        35412
#      73        30689
#      16        23643
#     218        19547
#     145        16969
#     171        15322
#     216        13750
#     180        10037
#     223         8686

# Previously 2013...Top 10 sum of protected area by region id.
#
# lsp_prot_area_inland1km.csv
#  rgn_id sum_area_km2
#     163    44320.096
#      73    29154.350
#     145    17430.085
#     224    17414.367
#      16    17311.323
#     218    15684.457
#     216    10809.971
#     180     7156.290
#     223     6743.242
#     171     6525.803
# lsp_prot_area_offshore3nm.csv
#  rgn_id sum_area_km2
#     163    106799.44
#      73     94917.12
#      16     70428.55
#     145     60933.69
#     216     59994.07
#     218     35079.36
#     224     30978.57
#     223     29179.67
#     135     23194.41
#     210     23171.71


