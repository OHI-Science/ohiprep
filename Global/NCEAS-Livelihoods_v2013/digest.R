# split livelihoods interpolated prep data product model outputs into layer files digestable by toolbox

library(reshape2)
library(plyr)
library(dplyr)

# paths
source('src/R/common.R') # dir_neptune_data
dir_prod = 'Global/NCEAS-Livelihoods_v2013'
dir.create(file.path(dir_prod, 'data'), showWarnings=F, recursive=T)

# generate layers for toolbox
for (yr in 2012:2013){ # yr=2013
  status_model_curref = read.csv(file.path(dir_neptune_data,  sprintf('model/GL-NCEAS-Livelihoods_2012/data_%d/global_li_status_model_curref.csv', yr)), na.strings='') # head(status_model_curref)
  #paste(names(status_model_curref), collapse="','") # metric,sector,iso3166,cur_year,ref_year,cur_base_value,ref_base_value,cur_adj_value,ref_adj_value
  
  d = status_model_curref %.%
    #melt(id.vars=c('metric','iso3166','sector')) %.%
    rename(c('iso3166'='cntry_key')) %.%
    arrange(metric, cntry_key, sector)
  
  for (m in c('jobs','rev','wage')){
    for (f in c('cur_year','ref_year','cur_base_value','ref_base_value','cur_adj_value','ref_adj_value')){
      x =  filter(d, metric==m)
      write.csv(x[,c('cntry_key','sector',f)], sprintf('%s/data/le_status_model_curref_%s_%s.csv',dir_prod,m,f))
    }
  }    
}
  