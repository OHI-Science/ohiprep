# split livelihoods interpolated prep data product model outputs into layer files digestable by toolbox
# TODO: reconsolidate in upgraded toolbox

library(reshape2)
library(plyr)
library(dplyr)
library(RPostgreSQL)

# db
pg = dbConnect(dbDriver("PostgreSQL"), host='neptune.nceas.ucsb.edu', dbname='ohi_global2013', user='bbest') # assumes password in ~/.pgpass
dbSendQuery(pg, 'SET search_path TO global_li, global; SET ROLE TO ohi;')

# paths
source('src/R/common.R') # dir_neptune_data
dir_prod = 'Global/NCEAS-Livelihoods_v2013'
dir.create(file.path(dir_prod, 'data'), showWarnings=F, recursive=T)
dir.create(file.path(dir_prod, 'tmp'), showWarnings=F, recursive=T)
if (exists('meta')) rm(meta)

# gdp per capita ppp
lyr = 'le_gdp_pc_ppp'
csv_in  = file.path(dir_neptune_data, 'model/GL-NCEAS-Livelihoods_v2013a/data/country_gdppcppp_2013a.csv')
csv_out = file.path(dir_prod, 'data/le_cntry_gdp_pc_ppp_2013a.csv')
p = read.csv(csv_in, na.strings='') %.%
  select(
    cntry_key = ISO3166,
    year      = YEAR,
    usd       = VALUE) %.%
  arrange(cntry_key, year) %.%
  write.csv(csv_out, row.names=F, na='')
  
# create metadata
meta = data.frame(
  target         = 'LE',
  layer          = lyr,
  name           = 'Modeled Livelihoods & Economies data',
  description    = 'gross domestic product per person at purchasing power parity',
  citation_2012n = '6F',
  citation_2013a = NA,
  fld_value      = 'usd',
  units          = 'USD', 
  ingest         = 'T',
  dir_2012a      = sprintf('ohiprep:%s', dirname(csv_out)),
  fn_2012a       = basename(csv_out),
  dir_2013a      = sprintf('ohiprep:%s', dirname(csv_out)),
  fn_2013a       = basename(csv_out),  
  row.names=lyr, stringsAsFactors=F)

# aggregation weight layer for livelihoods
lyr = 'le_workforcesize_adj'
csv_out = file.path(dir_prod, 'data/le_workforcesize_adj.csv')
dbGetQuery(pg, "SELECT * FROM global_li.srcdata_adj_workforcesize") %.%
  select(cntry_key=iso3166, year, jobs=value) %.%
  arrange(cntry_key, year) %.%
  write.csv(csv_out, row.names=F, na='')
meta_i = data.frame(
  target         = 'LE',
  layer          = lyr,
  name           = 'Modeled Livelihoods & Economies data',
  description    = 'adjusted workforce size',
  citation_2012n = '6F',
  citation_2013a = NA,
  fld_value      = 'jobs',
  units          = 'jobs', 
  ingest         = 'T',
  dir_2012a      = sprintf('ohiprep:%s', dirname(csv_out)),
  fn_2012a       = basename(csv_out),
  dir_2013a      = sprintf('ohiprep:%s', dirname(csv_out)),
  fn_2013a       = basename(csv_out),  
  row.names=lyr, stringsAsFactors=F)
meta = rbind(meta, meta_i)

# aggregation weight layer for economies
lyr = 'le_revenue_adj'
csv_out = file.path(dir_prod, 'data/le_revenue_adj.csv')
dbGetQuery(pg, "SELECT * FROM global_li.adjustments WHERE whence = 'actual' AND metric = 'rev_adj'") %.%
  select(cntry_key=iso3166, year, usd=value) %.%
  arrange(cntry_key, year) %.%
  write.csv(csv_out, row.names=F, na='')
meta_i = data.frame(
  target         = 'LE',
  layer          = lyr,
  name           = 'Modeled Livelihoods & Economies data',
  description    = 'adjusted revenue',
  citation_2012n = '6F',
  citation_2013a = NA,
  fld_value      = 'usd',
  units          = 'USD', 
  ingest         = 'T',
  dir_2012a      = sprintf('ohiprep:%s', dirname(csv_out)),
  fn_2012a       = basename(csv_out),
  dir_2013a      = sprintf('ohiprep:%s', dirname(csv_out)),
  fn_2013a       = basename(csv_out),  
  row.names=lyr, stringsAsFactors=F)
meta = rbind(meta, meta_i)

# output status_model layers for toolbox
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
        ingest         = 'T')      
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