# split livelihoods interpolated prep data product model outputs into layer files digestable by toolbox
# TODO: reconsolidate in upgraded toolbox

library(reshape2)
library(plyr)
library(dplyr)

# paths
source('src/R/common.R') # dir_neptune_data
dir_prod = 'Global/NCEAS-Livelihoods_v2013'
dir.create(file.path(dir_prod, 'data'), showWarnings=F, recursive=T)
if (exists('meta')) rm(meta)
  
# generate layers for toolbox
for (yr in 2012:2013){ # yr=2013
  status_model_curref = read.csv(file.path(dir_neptune_data,  sprintf('model/GL-NCEAS-Livelihoods_2012/data_%d/global_li_status_model_curref.csv', yr)), na.strings='')
  
  d = status_model_curref %.%
    rename(c('iso3166'='cntry_key')) %.%
    arrange(metric, cntry_key, sector)
  
  for (m in c('jobs','rev','wage')){
    for (f in c('cur_base_value','ref_base_value','cur_adj_value','ref_adj_value')){  # note: skipping 'cur_year','ref_year'  
      lyr = sprintf('le_%s_%s', m, f)
      
      # write out data file
      x = filter(d, metric==m) %.%
        rename(setNames('value', f)) %.%
        select(cntry_key, sector, value)
      csv = sprintf('%s/data/le_%d_status_model_curref_%s_%s.csv',dir_prod,yr,m,f)
      write.csv(x, csv, row.names=F)
      
      # write metadata
      meta_i = data.frame(
        target         = 'LE',
        layer          = lyr,
        name           = 'Modeled Livelihoods & Economies data',
        description    = 'One of current or reference and base or adjusted value',
        citation_2012n = '6F',
        citation_2013a = NA,
        fld_value      = 'value',
        units          = 'value', 
        ingest         = 'T',
        row.names=lyr)      
      if (!exists('meta')){
        meta = meta_i
      }  
      if (!lyr %in% meta$layer){
        meta = rbind.fill(meta, meta_i)        
      }
      rownames(meta) = meta$layer      
      meta[lyr, sprintf('dir_%da',yr)] = sprintf('ohiprep:%s', dirname(csv))
      meta[lyr, sprintf('fn_%da',yr)]  = basename(csv)        
    }
  }
}

# write metadata
write.csv(meta, sprintf('%s/data/le_layers_metadata.csv', dir_prod), row.names=F, na='')