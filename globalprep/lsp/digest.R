## This takes the data produced by the .py script and organizes it for the Toolbox
## Modified MRF: Feb 26 2015

library(foreign) #read dbf files
library(dplyr)
library(tidyr)

#source('src/R/common.R')
dir_prod = 'globalprep/WDPA_MPA/v2015'

# read in tabulated areas and write out LSP layers ----
lyrs = c('rgn_offshore3nm_wdpa.dbf' = 'lsp_protarea_offshore3nm.csv',
         'rgn_inland1km_wdpa.dbf'   = 'lsp_protarea_inland1km.csv')


for (i in 1:length(lyrs)){  
#  i=1
  dbf = names(lyrs)[i]
  csv = lyrs[[i]]

  d = read.dbf(file.path(dir_prod, 'tmp/mang_plan_dbfs', dbf))

m <- gather(d, year, area_m2, VALUE_0:VALUE_2014)

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
   
  # save layer
  write.csv(m, file.path(dir_prod, 'data', csv), row.names=F, na='')
  
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
# 16     98434.50
# 73     84839.75
# 216     77544.25
# 145     61159.25
# 163     45747.75
# 223     29763.50
# 218     28656.00
# 224     24201.50
# 171     23044.50
# 135     22915.25
# lsp_protarea_inland1km.csv
# rgn_id sum_area_km2
# 163     32663.75
# 73     27571.50
# 224     27456.25
# 16     19981.50
# 145     16860.75
# 218     15080.75
# 171     12709.00
# 216      9112.00
# 135      7126.50
# 210      7044.00

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


