wd = 'N:/model/GL-WDPA-MPA_v2013'
setwd(wd)

library(foreign)
library(reshape2)
library(plyr)

# read in tabulated areas and write out LSP layers ----

lyrs = c('rgninland1kmmol_vs_wdpapolydesignated.dbf' = 'lsp_prot_area_inland1km.csv',
         'rgnoffshore3nm_vs_wdpapolydesignated.dbf'  = 'lsp_prot_area_offshore3nm.csv',
         'rgnfao_vs_wdpapolydesignated.dbf'          = 'lsp_prot_area_ocean.csv')

for (i in 1:length(lyrs)){
  dbf = names(lyrs)[i]
  csv = lyrs[[i]]
  d = read.dbf(file.path(wd,'tmp',dbf))
  m = rename(melt(d, id.vars='VALUE', variable.name='value_year', value.name='area_m2'),
             c('VALUE'='rgn_id'))
  m = m[m$area_m2!=0,]
  m$year = as.integer(sub('VALUE_','', m$value_year))
  m$area_km2 = m$area_m2 / (1000 * 1000)
  m = m[order(m$rgn_id, m$year), c('rgn_id','year','area_km2')]
  write.csv(m, file.path(wd, 'data', csv), row.names=F, na='')
  
  # get top 10 for sanity check
  s = ddply(m, .(rgn_id), summarize,
            sum_area_km2 = sum(area_km2))
  cat(sprintf('%s\n',csv))
  print(head(s[order(s$sum_area_km2, decreasing=T),], 10), row.names=F)
}

# Top 10 sum of protected area by region id.
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