library(foreign)
library(reshape2)
library(stringr)
library(dplyr)

source('src/R/common.R')
dir_prod = 'Global/WDPA-MPA_v2014'
dir_tmp  = file.path(dir_neptune_data, sprintf('git-annex/%s/tmp', dir_prod))

dir.create(file.path(dir_prod, 'data'), showWarnings=F)

# read in tabulated areas and write out LSP layers ----
lyrs = c('rgn_offshore3nm_wdpa.dbf' = 'lsp_protarea_offshore3nm.csv',
         'rgn_inland1km_wdpa.dbf'   = 'lsp_protarea_inland1km.csv')

for (i in 1:length(lyrs)){ # i=1
  dbf = names(lyrs)[i]
  csv = lyrs[[i]]
  d = read.dbf(file.path(dir_tmp,dbf))
  m = d %.%
    melt(id.vars='VALUE', variable.name='value_year', value.name='area_m2') %>%
    filter(area_m2 > 0) %>%
    mutate(
      year     = as.integer(sub('VALUE_','', value_year)),
      area_km2 = area_m2 / (1000 * 1000)) %>%
    select(rgn_id=VALUE, year, area_km2) %>%
    arrange(rgn_id, year)

  # fix  for  lsp_protarea_offshore3nm.csv: make any non-represented rgn_id == 0, and 2 special fixes. 
  # see https://github.com/OHI-Science/ohidev/blob/master/report/compare_scores2layers/compare_scores2layers.md#lasting-special-places
  if (i == 1) {  
    rgns = read.csv('../ohi-global/eez2014/layers/rgn_global.csv') %.%
      select(rgn_id)  %.%
      filter(rgn_id < 255) %.%
      arrange(rgn_id)
    
    m = rbind(m, 
              rgns %>%
                anti_join(m, by = 'rgn_id') %>%
                mutate(year = max(m$year, na.rm=T),
                       area_km2 = 0)) %>%
      arrange(rgn_id, year)
    
    m$area_km2[m$rgn_id == 78] = 1.746501543  # special fix for Lebanon[78]
    m$year[m$rgn_id ==78] = 1972
    m$area_km2[m$rgn_id == 220] = 0.873250772 # special fix for Sint Maarten[220]
    m$year[m$rgn_id == 220] = 2006
   
      # str_replace is doing something strange: won't do both at once, only one. 
#       mutate(area_km2 = str_replace(rgn_id, as.character(78), 1.746501543),  # special fix for Lebanon[78]
#              year     = str_replace(rgn_id, as.character(78), 1972)) %>%
#       mutate(area_km2 = str_replace(rgn_id, as.character(220), 0.873250772),  # special fix for Sint Maarten[220]
#              year     = str_replace(rgn_id, as.character(220), 2006)) %>%
#       arrange(rgn_id, year)
  }
  
  # save layer
  write.csv(m, file.path(dir_prod, 'data', csv), row.names=F, na='')
  
   # get top 10 for sanity check
  cat(sprintf('%s\n',csv))
  s = m %.%
    group_by(rgn_id) %>%
    summarize(
      sum_area_km2 = sum(area_km2)) %>%
    arrange(desc(sum_area_km2)) %>%
    as.data.frame()
  print(head(s, 10), row.names=F)  
}

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
