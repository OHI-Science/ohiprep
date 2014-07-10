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


# trend layers ---
# two tables needed for trend

# table 1. adjustments
adjustments_o = read.csv(
  sprintf('%s/model/GL-NCEAS-Livelihoods_2012/data_%s/global_li_adjustments.csv', dir_neptune_data, yr),
  na.strings='') # %>%

# dlply(.(metric), function(x) summary(x$value))
#
# jobs_adj:
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    62.42   89.73   91.52   91.33   93.64   99.48       6 
# 
# rev_adj:
#       Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#  2.282e+07 4.475e+09 2.309e+10 3.474e+11 1.760e+11 1.468e+13       155 
# 
# wage_adj:
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#        1       1       1       1       1       1

# select(metric) %>% table()
#
# jobs_adj  rev_adj wage_adj 
#     1395     1955      340  

# head()
#
#     metric iso3166 year whence    value
# 1 jobs_adj     ABW 2007 actual 94.29370
# 2 jobs_adj     ALB 2008 actual 87.25179
# 3 jobs_adj     ANT 1998 actual 83.77992
# 4 jobs_adj     ANT 2005 actual 81.83977
# 5 jobs_adj     ANT 2006 actual 85.35421
# 6 jobs_adj     ANT 2007 actual 87.46953

# layer: le_unemployment
# differs slightly from ohiprep:Global/WorldBank-Statistics_v2012/data/rgn_wb_uem_2014a_ratio-gapfilled.csv
#  presumably b/c of different gapfilling technique (see p. 28 of Halpern et al 2012 Nature Supplement)
read.csv(sprintf('%s/stable/GL-NCEAS-Livelihoods/data/global_li_srcdata_adj_worldbank_emp.csv', dir_neptune_data), na.strings='') %>%
  mutate(percent=100-VALUE) %>%
  select(cntry_key=ISO3166, year=YEAR, percent) %>%
  write.csv(sprintf('%s/data/le_unemployment.csv', dir_prod), row.names=F, na='')

for (yr in 2012:2013){ # yr=2013
  dir = sprintf('%s/model/GL-NCEAS-Livelihoods_2012/data_%s', dir_neptune_data, yr)
  
  # table 2. metric_sector_year, limited by metric_sector_refperiod
  d = read.csv(file.path(dir, 'global_li_metric_sector_year.csv'), na.strings='') %>%
    left_join(
      read.csv(file.path(dir, 'global_li_metric_sector_refperiod.csv'), na.strings=''),
      by=c('metric','iso3166','sector')) %>%
    filter(year <= cur_year & year >= ref_year) %>%
    arrange(metric, iso3166, sector, year) %>%
    select(metric, cntry_key=iso3166, sector, year, value)
  
  for (m in c('jobs','rev','wage')){
    d %>%
      filter(metric==m) %>%
      select(cntry_key=iso3166, sector, cur_year) %>%
      write.csv(sprintf('%s/le_%s_sector-year_eez%d.csv', dir_prod, m, yr), row.names=F, na='')
  }
  
  # table 2. metric_sector_refperiod
  d = read.csv(
    sprintf('%s/model/GL-NCEAS-Livelihoods_2012/data_%s/global_li_metric_sector_refperiod.csv', dir_neptune_data, yr),
    na.strings='') # %>%
  
    # head()
    #
    #   metric sector iso3166 cur_year ref_year gap                                                                                                                                                                          years
    # 1   jobs     cf     ABW     2008     2003   5                                                                                                2008 2007 2006 2005 2004 2003 2002 2001 2000 1999 1998 1997 1996 1990 1980 1970
    # 2   jobs     cf     AGO     2002     1995   7                                                                                                                              2002 2001 2000 1995 1990 1980 1973 1972 1971 1970
    # 3   jobs     cf     AIA     2008     2003   5                                                                                                                    2008 2007 2006 2005 2004 2003 2002 2001 1995 1990 1982 1981
    # 4   jobs     cf     ALB     2008     2003   5                          2008 2007 2006 2005 2004 2003 2002 2001 2000 1999 1998 1997 1996 1995 1994 1993 1992 1991 1990 1989 1988 1987 1986 1985 1984 1983 1982 1981 1980 1970
    # 5   jobs     cf     ARE     2008     2003   5 2008 2007 2005 2004 2003 2002 2001 2000 1999 1998 1997 1996 1995 1994 1993 1992 1991 1990 1989 1988 1987 1986 1985 1984 1983 1982 1981 1980 1979 1978 1977 1976 1975 1972 1970
    # 6   jobs     cf     ARG     2008     2003   5                                                             2008 2005 2004 2003 1996 1990 1989 1988 1987 1986 1985 1984 1983 1982 1980 1977 1976 1975 1974 1973 1972 1971 1970    
  
  for (m in c('jobs','rev','wage')){
    # cur_year
    d %>%
      filter(metric==m) %>%
      select(cntry_key=iso3166, sector, cur_year) %>%
      write.csv(sprintf('%s/le_%s_year_cur_eez%d.csv', dir_prod, m, yr),row.names=F, na='')
  
    # ref_year
    d %>%
      filter(metric==m) %>%
      select(cntry_key=iso3166, sector, ref_year) %>%
      write.csv(sprintf('%s/le_%s_year_ref_eez%d.csv', dir_prod, m, yr),row.names=F, na='')
  }

}  



  
  # status layers with years
  
   # 
  # wages: 1
  
  read.csv('~/github/ohi-global/eez2013/layers/le_revenue_adj.csv') %>%
    summary()
    head()
  
  # get adjustments
  adjustments = workforce_adj %>%
    mutate(metric='jobs_adj') %>%
    select(metric, cntry_key, year, value=jobs) %>%
    rbind(
      rev_adj %>%
        mutate(metric='rev_adj') %>%
        select(metric, cntry_key, year, value=usd)) %>%
    arrange(metric, cntry_key, year) %>%
    
    #dlply(.(metric), function(x) summary(x$value))  
    #
    # $jobs_adj
    #      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    #     29450    796300   2615000  13760000   7619000 783200000 
    # 
    # $rev_adj
    #      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    # 2.282e+07 3.812e+09 2.250e+10 3.522e+11 1.730e+11 1.468e+13 
  
  #
  dbGetQuery(pg, "SELECT * FROM global_li.adjustments") %>%
    filter(metric=='jobs_adj') %>% 
    write.csv('')
    head
    #     metric iso3166 year whence    value
    # 1 jobs_adj     ABW 2007 actual 94.29370
    # 2 jobs_adj     ALB 2008 actual 87.25179
    # 3 jobs_adj     ANT 1998 actual 83.77992
    # 4 jobs_adj     ANT 2005 actual 81.83977
    # 5 jobs_adj     ANT 2006 actual 85.35421
    # 6 jobs_adj     ANT 2007 actual 87.46953
  
    #select(value) %>% summary()
    #  Min.   :62.42  
    #  1st Qu.:89.73  
    #  Median :91.52  
    #  Mean   :91.33  
    #  3rd Qu.:93.64  
    #  Max.   :99.48  
    #  NA's   :6
    write.csv()
  
  dbGetQuery(pg, "SELECT * FROM global_li.srcdata_adj_worldbank_emp") %>%
    arrange(iso3166)
    head()
    #   iso3166 year    value
    # 1     AFG 2005 91.53225
    # 2     ALB 2001 77.32426
    # 3     ALB 2008 87.25179
    # 4     DZA 1989 83.10000
    # 5     DZA 1990 80.20000
    # 6     DZA 1991 79.40000
  

      
  # /var/data/ohi/stable/GL-NCEAS-Livelihoods/data/global_li_srcdata_adj_worldbank_emp.csv
  ---
  read.csv('layers/tr_unemployment.csv') %>%
    left_join(
      read.csv('layers/rgn_labels.csv') %>%
        select(rgn_id, rgn_name=label),
      by='rgn_id') %>%
    filter(rgn_name=='Peru') %>%
    mutate(
      pct_emp=100-percent) %>%
    merge(
      dbGetQuery(pg, "SELECT * FROM global_li.srcdata_adj_worldbank_emp") %>%
        select(iso3166, year, pct_li=value) %>%
        filter(iso3166=='PER'),
      by='year') %>%
    mutate(
      pct_dif=pct_emp-pct_li) %>%
    arrange(desc(abs(pct_dif)))
    #select(rgn_name) %>% table()
    #    year rgn_id percent rgn_name pct_emp iso3166   pct_li       pct_dif
    # 1  2004    138     5.2     Peru    94.8     PER 90.97399  3.826008e+00
    # 2  2005    138     5.2     Peru    94.8     PER 91.31178  3.488224e+00
    # 3  2006    138     4.6     Peru    95.4     PER 92.57402  2.825976e+00
    # 4  2008    138     4.5     Peru    95.5     PER 93.23761  2.262394e+00
    # 5  2007    138     4.5     Peru    95.5     PER 93.27703  2.222972e+00
    # 6  2002    138     9.7     Peru    90.3     PER 92.30000 -1.999999e+00
    # 7  2003    138    10.3     Peru    89.7     PER 91.56728 -1.867278e+00
    # 8  2001    138     8.8     Peru    91.2     PER 92.12343 -9.234294e-01
    # 9  2000    138     7.8     Peru    92.2     PER 92.63795 -4.379496e-01
    # 10 1990    138     8.6     Peru    91.4     PER 91.44860 -4.859642e-02
    # 11 1998    138     7.8     Peru    92.2     PER 92.24547 -4.547504e-02
    # 12 1993    138     9.9     Peru    90.1     PER 90.06356  3.643989e-02
    # 13 1996    138     7.0     Peru    93.0     PER 92.96449  3.550859e-02
    # 14 1992    138     9.4     Peru    90.6     PER 90.56840  3.160161e-02
    # 15 1994    138     8.9     Peru    91.1     PER 91.06992  3.008461e-02
    # 16 1995    138     7.1     Peru    92.9     PER 92.92819 -2.819255e-02
    # 17 1997    138     7.7     Peru    92.3     PER 92.27291  2.709401e-02
    # 18 1999    138     8.0     Peru    92.0     PER 92.02546 -2.546135e-02
    # 19 1991    138     5.8     Peru    94.2     PER 94.20965 -9.653021e-03
    # 20 1987    138     4.8     Peru    95.2     PER 95.20000 -1.907349e-07
    # 21 1986    138     5.3     Peru    94.7     PER 94.70000 -7.348575e-10
    # Why differences?
  
  
  
    head()
  
  # adjustments
  status/digest.sql
  status/model.sql
  trend/ingest.sql
  trend/digest.sq
  
  # adjustments:
  #   rev: adj_worldbank_gdp
  #      tmp/cntry_wb_gdp_2012a.csv
  #      tmp/cntry_wb_gdp_2013a.csv
  #   jobs:  adj_worldbank_emp # (100-%unemployed)*total labor force
  #      tmp/cntry_wb_emp_2012a.csv
  #      tmp/cntry_wb_emp_2013a.csv
  #   wage: dropping wage ILO adjustment by faking with ones  
  #     tmp/rgn_nber_oww_2013a_nofill.csv
  #     ilo = read.csv('raw/n12_le_email/ForDarren/ILO_Wages_Data.csv', na.strings=''); head(ilo)
  /Volumes/data_edit/model/GL-NCEAS-Livelihoods_2012/data_2013/global_li_adjustments.csv

  # bbest@neptune:/var/data/ohi/model/GL-NCEAS-Livelihoods_2012$ grep -ri adjustments *
  
  ingest2.sql:\echo "load JOB adjustments"
  ingest2.sql:DROP TABLE IF EXISTS global_li.adjustments;
  ingest2.sql:CREATE TABLE adjustments AS
  ingest2.sql:ALTER TABLE adjustments
  ingest2.sql:-- \echo "load WAGE adjustments"
  ingest2.sql:-- INSERT INTO adjustments
  ingest2.sql:-- \echo "load REV adjustments"
  ingest2.sql:-- INSERT INTO adjustments
  ingest2.sql:\echo "load WAGE adjustments 2013a: faked with ones to drop ILO adjustment"
  ingest2.sql:INSERT INTO adjustments
  ingest2.sql:\echo "load REV adjustments 2013a"
  ingest2.sql:INSERT INTO adjustments
  ingest2.sql:-- \echo "load POPULATION adjustments"
  ingest2.sql:-- INSERT INTO adjustments
  ingest2.sql:  JOIN    adjustments a USING (iso3166)
  
  model3.sh:csv2psql --schema=global_li --key=metric,r2_name,year _model2.csv global_li_adjustments_model | psql -q
  model3.sql:\echo "clear all adjustments"
  model3.sql:UPDATE  adjustments
  model3.sql:FROM    adjustments d
  model3.sql:  FROM    adjustments_model m
  model3.sql:  FROM    adjustments_model m
  
  model5.sh:csv2psql --schema=global_li --key=metric,iso3166,year _model4.csv global_li_adjustments_model_gaps | psql -q
  
  digest.sql:UPDATE  adjustments d
  digest.sql:  FROM    adjustments d
  digest.sql:  JOIN    adjustments_model_gaps g USING (metric, iso3166, year)
  digest.sql:UPDATE  adjustments d
  digest.sql:  FROM    adjustments d
  digest.sql:  JOIN    adjustments_model m ON (m.r2_name = n.name AND m.metric = d.metric AND m.year = d.year)
  digest.sql:\echo "Transfering modeled adjustments data into status model"
  digest.sql:  FROM    adjustments
  digest.sql:  FROM    adjustments a
  
  export.sql:\o data/global_li_adjustments_model.csv
  export.sql:  SELECT * FROM adjustments_model
  export.sql:\o data/global_li_adjustments_model_gaps.csv
  export.sql:  SELECT * FROM adjustments_model_gaps
  export.sql:\o data/global_li_adjustments.csv
  export.sql:  SELECT * FROM adjustments

  report.sql:FROM        adjustments
  
  
  
}

read.csv(file.path(dir_neptune_data,  sprintf('model/GL-NCEAS-Livelihoods_2012/data_%d/global_li_status_model_curref.csv', yr)), na.strings='')
