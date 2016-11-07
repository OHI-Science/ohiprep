# for ohi-global 2013-10-09, used this script:
#   /Volumes/data_edit/model/GL-NCEAS-Livelihoods_v2013a/model.R

# setup ----
library(plyr)

# working directory
root.data = '/Volumes/data_edit'
wd = file.path(root.data, 'model/GL-NCEAS-Livelihoods_v2013a')
setwd(wd)

# plan 2013-09-16: Replace all wages with [2013 OWW NBER wages new column] / [CPI conversion factor] / [PPP]. Replace wages_adj with ones instead of ILO.
# CPI for 2010 USD conversion factor from OSU:
#   /Volumes/data_edit/model/GL-OSU-CPI_v2012/raw/conversionfactor2010_osu.csv
# PPP data 
#   /Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/cntry_wb_ppp_2013a.csv
# GDP / CPI

# connect to postgres db ----
require('RPostgreSQL')
dbi.pg = dbDriver("PostgreSQL") 
pg = dbConnect(dbi.pg, host='neptune.nceas.ucsb.edu', dbname='ohi_global2013', user='bbest') # assumes password in ~/.pgpass
dbSendQuery(pg, 'SET search_path TO global_li, global; SET ROLE TO ohi;')

cntry_georegions = read.csv('tmp/cntry_georegions_wide_2013b.csv', na.strings=''); head(cntry_georegions)

cntry2013_country2012 = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/cntry2013_country2012.csv', na.strings=''); head(cntry2013_country2012) # cntry_key_2013,  country_id_2012

# load CPI, PPP, GDP ----
cpi = read.csv('tmp/conversionfactor2010_osu.csv', na.strings=''); head(cpi) # year, cf2010
cpi[duplicated(cpi[,'year']),] # removed NAs at end in original file

# PPP: wage adjustment in LE/status/model.sql ----
ppp = rename(read.csv('/Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/country_wb_gdppcppp_2013a.csv', na.strings=''),
                  c('LCUpUSD'='VALUE','country_id'='ISO3166','year'='YEAR')); head(ppp)
ppp$ID = NA
table(ppp[duplicated(ppp[,c('ISO3166','YEAR')]), 'ISO3166'])
write.csv(ppp, 'data/country_gdppcppp_2013a.csv', row.names=F, na='')

# revenue adjustment: gdp ----
gdp = rename(read.csv('/Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/country_wb_gdp_2013a.csv', na.strings=''),
             c('country_id'='iso3166','USD'='value')); head(gdp)
table(gdp[duplicated(gdp[,c('iso3166','year')]), c('year','iso3166')])
subset(gdp, value < 0) # from temporal gapfilling
#  iso3166 year      value
#      NCL 2009 -6.895e+08
gdp = subset(gdp, value > 0) # remove NCL

# debug compare with Nature 2012 GDP
gdp.n = rename(dbGetQuery(pg, "SELECT * FROM srcdata_adj_worldbank_gdp"),
               c('value'='value.n')); head(gdp.n)
gdp = merge(gdp, gdp.n, all.x=T)
gdp = within(gdp, {
  value2012 = value / cpi[cpi$year==2012,'cf2010']
  value2013 = value / cpi[cpi$year==2013,'cf2010']
  value.dif = value - value.n
  value2012.dif = value2012 - value.n
  value2013.dif = value2013 - value.n
  })
gdp = arrange(gdp, iso3166, year)
write.csv(gdp, 'tmp/gdp_cpi_debug.csv', row.names=F, na='')
summary(gdp[,c('value','value2012','value2013','value.n','value.dif','value2012.dif','value2013.dif')])
print(subset(gdp, iso3166=='BRA' & year > 2000, c(iso3166,year,value,value2012,value2013,value.n,value.dif,value2012.dif,value2013.dif)), row.names=F)
table(gdp[,c('iso3166','year')])

dbRemoveTable(pg, 'srcdata_adj_worldbank_gdp_2013a')
dbWriteTable(pg, 'srcdata_adj_worldbank_gdp_2013a', gdp[,c('iso3166','year','value')], row.names=F)
table(dbGetQuery(pg, 'SELECT iso3166 FROM srcdata_adj_worldbank_gdp_2013a'))

# wage: apply CPI conversion factor, upload ----
wage = rename(read.csv('tmp/country_wagesOWW_2013a.csv', na.strings=''),
              c('country_id'='iso3166')); head(wage)
wage[duplicated(wage[,c('iso3166','sector','year')]),]

# divide by CPI to convert to 2010 US Dollars
wage = merge(wage, cpi, by='year', all.x=T); head(wage)
wage[is.na(wage$cf2010), ] # check for missing CPI
wage = within(wage, {value = USD / cf2010})

# methods_SL.docx: "used CPI to convert current dollars to 2010 dollars"
cf.2012.to.2010 = cpi[cpi$year==2012,'cf2010'] # 1.053
wage = within(wage, {value2010 = USD / cf.2012.to.2010}) # so wag

# check with Nature 2012 wages
liv_wages = read.csv('/Volumes/local_edit/src/toolbox/data/global_2012_nature/layers/liv_wages.csv', na.strings=''); head(liv_wages)
x = merge(rename(wage, c('iso3166'='country_id')), 
          rename(liv_wages, c('usd'='value.2012n')), by=c('country_id','year','sector'), all.x=T); head(x)
x$value.dif = x$value - x$value.2012n
x$value2010.dif = x$value2010 - x$value.2012n
x$USD.dif   = x$USD - x$value.2012n
summary(x[,6:ncol(x)])
subset(x, country_id=='BRA')
write.csv(x, 'wages_cpi_debug.csv', row.names=F, na='')

# upload to db
wage = rename(arrange(wage, iso3166, year, sector)[,c('iso3166','year','sector','value2010')], c('value2010'='value'))
wage$metric = 'wage'
dbRemoveTable(pg, 'srcdata_wage_2013a')
dbWriteTable(pg,  'srcdata_wage_2013a', wage, row.names=F)

# dropping wage ILO adjustment by faking with ones ----
ilo = dbGetQuery(pg, "SELECT * FROM srcdata_adj_ilo_wages"); head(ilo)
ilo.wages.ones = ddply(wage, .(iso3166, year), summarize, value=1); head(ilo.wages.ones); summary(ilo.wages.ones)

dbRemoveTable(pg, 'srcdata_adj_ilo_wages_2013a')
dbWriteTable(pg,  'srcdata_adj_ilo_wages_2013a', ilo.wages.ones, row.names=F)

# revenue: upload ----
# note: only new data, not old
rev = rename(read.csv('data/country_revenue_2013a.csv', na.strings=''),
             c('country_id'='iso3166','USD'='value')); head(rev)
rev$metric = 'rev'
table(subset(rev, value ==0, c(iso3166, sector)))
#           sector
# iso3166    aqf tour
#   Trindade  34   25
rev = subset(rev, value > 0)
table(rev[,c('iso3166', 'sector')])

dbRemoveTable(pg, 'srcdata_rev_2013a')
dbWriteTable(pg, 'srcdata_rev_2013a', rev)


# running LE models on neptune ----

# files with modifications for 2012a,2013a: /var/data/ohi/model/GL-NCEAS-Livelihoods_2012/{import,ingest2}.sql

# 2013a:
export OHI_PARAMDIR=/var/data/ohi/usr/local/src/param
export PGDATABASE=ohi_global2013
cd /var/data/ohi/model/GL-NCEAS-Livelihoods_2012/
cp import_2013.sql import.sql
flow
cd /usr/local/ohi/src/model/global2013/livelihoods/status
flow
cd /usr/local/ohi/src/model/global2013/livelihoods/trend
flow
cd /usr/local/ohi/src/model/global2013/livelihoods/status
flow
cd /usr/local/ohi/src/model/global2013/livelihoods/trend
flow

# copy interim files specific to scenario
cd /usr/local/ohi/src/model/global2013/livelihoods/status
rm -rf data_2013; cp -r data data_2013
for x in _output*.csv;do cp $x ${x/.csv/_2013.csv};done
#for x in _output*_2013_2012.csv;do rm -f $x ;done
rm -rf *201*_*201*.csv
cd /var/data/ohi/model/GL-NCEAS-Livelihoods_2012
rm -rf data_2013; cp -r data data_2013
for x in _*.csv;do cp $x ${x/.csv/_2013.csv};done
#for x in _*_2013_2012.csv;do rm -f $x ;done
rm -rf *201*_*201*.csv

# copy final status and region files
cd /usr/local/ohi/src/model/global2013/livelihoods
cp trend/data/global_li_trend_region.csv   trend/data/global_li_trend_region_2013a.csv
cp status/data/global_li_status_region.csv status/data/global_li_status_region_2013a.csv


d = read.csv('/Volumes/local_edit/src/model/global2013/livelihoods/status/data/global_li_status_region_2013a.csv', na.strings='')
print(subset(d, component=='economy'), row.names=F)

# TODO CHECK:
# Running psql script digest.sql
# "following query MUST HAVE ZERO ROWS"
#   id | count 
#  ----+-------
#   85 |     2
#   84 |     2
# (2 rows)

# 2012a

# do 2012: remove last year from srcdata and rerun. Strategy: a) remove the most
# recent year for just the sectors of revenue that have changed (mar, aqf, tour) and all wages
# (and not any jobs) so that the only elements varying between 2012 and 2013 are
# those where data has been added since Nature 2012.

# Check if 2012 data loaded
#m = dbGetQuery(pg, "SELECT * FROM srcdata_metric_sector_year_2012a"); head(m); dim(m) # 20034 rows
#addmargins(table(m[,c('metric','sector')], useNA='ifany')) Sum: 
#       sector
# metric   aqf    cf   mar   mmw    og    ph    sb  tour  tran   wte   Sum
#   jobs     0  3219   842   454     0     0     0  3312     0    25  7852
#   rev   2518   455   684   460  2670     0     0  3652     0    14 10453
#   wage     0   188     0     0     0   309   246   666   320     0  1729
#   Sum   2518  3862  1526   914  2670   309   246  7630   320    39 20034

m = dbGetQuery(pg, "SELECT * FROM metric_sector_year"); head(m); dim(m) # 20914 rows
addmargins(table(m[,c('metric','sector')], useNA='ifany'))
#       sector
# metric   aqf    cf   mar   mmw    og    ph    sb  tour  tran   wte   Sum
#   jobs     0  3219   842   454     0     0     0  3312     0    25  7852
#   rev   2687   455   811   460  2670     0     0  3805     0    14 10902
#   wage     0   237     0     0     0   410   323   789   401     0  2160
#   Sum   2687  3911  1653   914  2670   410   323  7906   401    39 20914
m = ddply(m, .(metric, sector, iso3166), mutate, year.max = max(year)); head(m); dim(m)
idx = m$metric=='wage' | (m$metric=='rev' & m$sector %in% c('aqf','mar','tour'))
table(m[idx,c('metric','sector')])
#       sector
# metric  aqf   cf  mar   ph   sb tour tran
#   rev  2687    0  811    0    0 3805    0
#   wage    0  237    0  410  323  789  401
m$year.max[idx] = m$year.max[idx] - 1
m = m[m$year <= m$year.max, ]; dim(m)                             
addmargins(table(m[,c('metric','sector')], useNA='ifany'))
#       sector
# metric   aqf    cf   mar   mmw    og    ph    sb  tour  tran   wte   Sum
#   jobs     0  3219   842   454     0     0     0  3312     0    25  7852
#   rev   2532   455   697   460  2670     0     0  3652     0    14 10480
#   wage     0   209     0     0     0   369   293   729   360     0  1960
#   Sum   2532  3883  1539   914  2670   369   293  7693   360    39 20292

dbRemoveTable(pg, 'srcdata_metric_sector_year_2012a')
dbWriteTable(pg, 'srcdata_metric_sector_year_2012a', m[,c('metric','sector','iso3166','year','value')])

# update /var/data/ohi/model/GL-NCEAS-Livelihoods_2012/import.sql
export PGDATABASE=ohi_global2013
OHI_PARAMDIR=/var/data/ohi/usr/local/src/param
cd /var/data/ohi/model/GL-NCEAS-Livelihoods_2012/
cp import_2012.sql import.sql
flow
cd /usr/local/ohi/src/model/global2013/livelihoods/status
flow
cd /usr/local/ohi/src/model/global2013/livelihoods/trend
flow
cd /usr/local/ohi/src/model/global2013/livelihoods/status
flow
cd /usr/local/ohi/src/model/global2013/livelihoods/trend
flow

# copy interim files specific to scenario
cd /usr/local/ohi/src/model/global2013/livelihoods/status
rm -rf data_2012; cp -r data data_2012
for x in _output*.csv;do cp $x ${x/.csv/_2012.csv};done
for x in _output*_2013_2012.csv;do rm -f $x ;done; ls
cd /var/data/ohi/model/GL-NCEAS-Livelihoods_2012
rm -rf data_2012; cp -r data data_2012
for x in _*.csv;do cp $x ${x/.csv/_2012.csv};done
for x in _*_2013_2012.csv;do rm -f $x ;done

# copy final status and region files
cd /usr/local/ohi/src/model/global2013/livelihoods
cp trend/data/global_li_trend_region.csv   trend/data/global_li_trend_region_2012a.csv
cp status/data/global_li_status_region.csv status/data/global_li_status_region_2012a.csv

# TODO CHECK:
# Running psql script digest.sql
# "following query MUST HAVE ZERO ROWS"
#   id | count 
#  ----+-------
#   85 |     2
#   84 |     2
# (2 rows)

# then run disaggregation from 2012 country_id to 2013 rgn_id. 
/Volumes/data_edit/model/GL-NCEAS-LayersDisaggregated_v2013a/digest_disaggregate.R

# then run layers_2013.R with calc.LIV.ECO=T

# investigate Bulgaria (BGR) with -1.9 delta trend ----
setwd('/Volumes/local_edit/src/model/global2013/LE/trend')

i.12 = read.csv('_outputs_2012a/_ingest.csv', na.strings=''); head(i.12)
i.13 = read.csv('_outputs_2013a/_ingest.csv', na.strings=''); head(i.12)
m.12 = read.csv('_outputs_2012a/_model.csv', na.strings=''); head(m.12)
m.13 = read.csv('_outputs_2013a/_model.csv', na.strings=''); head(m.13)

print(subset(i.12, iso3166=='BGR' & metric=='rev', c(sector,year,value,base_value,adj_value)), row.names=F)
print(subset(i.13, iso3166=='BGR' & metric=='rev', c(sector,year,value,base_value,adj_value)), row.names=F)
print(subset(m.12, iso3166=='BGR' & metric=='rev', c(sector,value,r2, n, pvalue, x1, x2)), row.names=F)
print(subset(m.13, iso3166=='BGR' & metric=='rev', c(sector,value,r2, n, pvalue, x1, x2)), row.names=F)


"-- weighted average here using total jobs per sector as weight for trend
INSERT INTO trend_score
SELECT  metric, iso3166, SUM(total_sector.value * d.value)/total.value AS value
FROM    trend_metric_sector d
JOIN    (
  SELECT  d.metric, d.sector, d.iso3166, SUM(t.value) AS value
  FROM    trend_metric_sector d 
  JOIN    trend_data t USING (metric, sector, iso3166)
  WHERE   metric IN ('jobs', 'rev') AND t.value IS NOT NULL
  GROUP BY d.metric, d.sector, d.iso3166
  ) total_sector USING (metric, sector, iso3166)
JOIN    (
  SELECT  d.metric, d.iso3166, SUM(t.value) AS value
  FROM    trend_metric_sector d 
  JOIN    trend_data t USING (metric, sector, iso3166)
  WHERE   metric IN ('jobs', 'rev') AND t.value IS NOT NULL
  GROUP BY d.metric, d.iso3166
  HAVING SUM(t.value) > 0
) total USING (metric, iso3166)
WHERE   metric IN ('jobs', 'rev')
GROUP BY metric, iso3166, total.value
ORDER BY metric, iso3166"

jobs.BGR = dbGetQuery(pg, "SELECT    metric, sector, iso3166, year, value
FROM (
  SELECT    iso3166, 'cf' as sector, 'jobs' as metric, year, value
  FROM      srcdata_jobs_cf
  UNION
  SELECT    iso3166, 'mar' as sector, 'jobs' as metric, year, value
  FROM      srcdata_jobs_mar
  UNION
  SELECT    iso3166, 'mmw' as sector, 'jobs' as metric, year, value
  FROM      srcdata_jobs_mmw
  UNION
  SELECT    iso3166, 'tour' as sector, 'jobs' as metric, year, value
  FROM      srcdata_jobs_tour
  UNION
  SELECT    iso3166, 'wte' as sector, 'jobs' as metric, year, value
  FROM      srcdata_jobs_wte
) d
WHERE     value IS NOT NULL AND iso3166 = 'BGR'
ORDER BY  metric, sector, iso3166, year")
print(jobs.BGR[,c('sector','year','value')], row.names=F) #  AND year > 1998


# OLD: histogram status and trend ----
trend_region = dbGetQuery(pg, "SELECT * FROM trend_region")
hist(trend_region$value)
summary(trend_region)

status_region = dbGetQuery(pg, "SELECT * FROM status_region")
hist(status_region$value)
summary(status_region)

status_rgn = read.csv('/Volumes/data_edit/model/GL-NCEAS-LayersDisaggregated_v2013a/data/rgn_liv.eco_status_2013a.csv', na.strings='')
hist(status_rgn$value)
summary(status_rgn)

# OLD: adjustments comparison ----

adjustments = read.csv('tmp/le_adj.n12.csv', na.strings='')
# jobs:  adj_worldbank_emp # (100-%unemployed)*total labor force
#   tmp/cntry_wb_emp_2012a.csv
#   tmp/cntry_wb_emp_2013a.csv
# wages: adj_ilo_wages
#   tmp/rgn_nber_oww_2013a_nofill.csv
#   ilo = read.csv('raw/n12_le_email/ForDarren/ILO_Wages_Data.csv', na.strings=''); head(ilo)
# rev:   adj_worldbank_gdp
#   tmp/cntry_wb_gdp_2012a.csv
#   tmp/cntry_wb_gdp_2013a.csv

# OLD: ppp error check ----
ppp     = rename(read.csv('tmp/country_wb_ppp_2013a.csv', na.strings=''),
                 c('LCUpUSD'='ppp')); head(ppp) # country_id, year, LCUpUSD
subset(ppp, country_id=='BRA')
ppp.n12 = rename(read.csv('tmp/global_af_ppp.csv', na.strings=''),
                 c('ISO3166'='country_id','YEAR'='year','VALUE'='ppp.12n')); head(ppp.n12)
ppp = merge(ppp, ppp.n12, all=T)
gdppcppp = rename(read.csv('/Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/country_wb_gdppcppp_2013a.csv', na.strings=''),
                  c('LCUpUSD'='gdppcppp')); head(gdppcppp)
ppp = merge(ppp, gdppcppp, all=T)
print(subset(ppp, country_id %in% c('Alaska','Hawaii','USA','BRA') & year > 2000), row.names=F)

# sql:import.sql:138: ERROR:  duplicate key value violates unique constraint "metric_sector_year_pkey"
#PRIMARY KEY (metric, sector, iso3166, year),
#FOREIGN KEY (iso3166) REFERENCES country,
#FOREIGN KEY (sector) REFERENCES sectors

rev = read.csv('data/eco_rev_2013a.csv', na.strings='')
table(rev[duplicated(rev[,c('sector','cntry_key','year')]), c('sector','cntry_key')])
sector
# cntry_key aqf cf mar mmw tour
#       CHN  68  3   0   4   50
#       GLP  34  0   0   0   25
#       GUM   0  0   1   0    0
#       MNP   0  0   1   0    0
#       MTQ  34  0   0   0   25
#       PRI   0  0   0   0   25
#       SCG   0  0   6   0    0
#       VIR   0  0   0   0   25
table(rev[duplicated(rev[,c('sector','cntry_key','year')]), c('year','cntry_key')])
#        year
# iso3166 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012
#     CHN    2    2    2    2    2    2    2    2    2    2    2    2    4    4    4    5    4    4    5    4    4    5    5    4    4    4    5    4    4    4    4    5    5    4    2    2    2
#     GLP    1    1    1    1    1    1    1    1    1    1    1    1    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    1    1    1
#     GUM    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    1    0    0    0    0    0
#     MNP    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    1    0    0    0    0    0
#     MTQ    1    1    1    1    1    1    1    1    1    1    1    1    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    2    1    1    1
#     PRI    0    0    0    0    0    0    0    0    0    0    0    0    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1
#     SCG    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    1    1    1    1    1    1    0
#     VIR    0    0    0    0    0    0    0    0    0    0    0    0    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1





#   cntry_key sector year             USD
# 1     Aruba     cf 1997     872022.4637
# 2     Aruba     cf 2002     619597.5755
# 3     Aruba     cf 2007     589854.8896
# 4       AGO     cf 2002  644037706.7000
# 5       AGO     cf 2007 1097712983.0000
# 6       ALB     cf 1997    1705302.2610
table(rev$sector)
#  aqf   cf  mar  mmw tour  wte 
# 5372  449  852  444 4000   14
print(ddply(rev, .(sector), summarize, max.year = max(year)), row.names=F)
#  sector max.year
#     aqf     2009*
#      cf     2007
#     mar     2011*
#     mmw     2008
#    tour     2012*
#     wte     2008


#   cntry_key year           USD
# 1       CCK 1960 12034001424.5
# 2       CCK 1961 12659200524.0
# 3       CCK 1962 12976031144.5
# 4       CCK 1963 14067032758.5
# 5       CCK 1964 15515222024.5
# 6       CCK 1965 15793409416.5


rev.a = dbGetQuery(pg, "SELECT * FROM global_li.metric_sector_year WHERE metric='rev' ORDER BY metric, sector, iso3166, year"); head(rev.a) # AND sector<>'og' 
table(rev.a$sector)
#  aqf   cf  mar  mmw   og tour  wte 
# 1503  455  279  460 2670 3335   14 
print(ddply(rev.a, .(sector), summarize, max.year = max(year)), row.names=F)
#  sector max.year
#     aqf     2008
#      cf     2007
#     mar     2007
#     mmw     2008
#      og     2009
#    tour     2010
#     wte     2008

table(rev$cntry_key[!rev$cntry_key %in% rev.a$iso3166])
#                Alaska                 Aruba               Bonaire                Brunei               Curacao         Easter Island     Galapagos Islands                Hawaii              Malaysia                   MNP 
#                    59                    29                    27                    66                    25                    59                    59                    59                    59                     6 
#                  Saba        Sint Eustatius Southern Saint-Martin              Trindade                   TUV 
#                    27                    27                    25                    59                    42

table(rev.a$iso3166[!rev.a$iso3166 %in% rev$cntry_key])
#         ABW         ANT         ATA         BDI         BLM         BOL         BWA         CHE         ESH         HKG         LUX         MAC         MKD         MNE         MSR         MWI         MYS         PRY         PSE         SVK         SWZ         TKM         UGA 
#          41          49          19          29           4           8           8          15          15          49           7          20          38          38          15          31          52          20          15           9           5           3           5 
# Wake Island         ZMB         ZWE 
#          15          14           2



# for liv_jobs.csv and liv_wages.csv, these didn't match (NAs):
#   ABW -- Aruba
#   BLM --?
#   BRN -- Brunei
#   HKG -- Hong Kong
#   MNE --?
# from:  '/Volumes/data_edit/model/GL-NCEAS-Countries-v2/data/by_country_iso3166.csv':
#   BLM  Saint Barthélemy  Saint-Barthélemy
#   MNE	Montenegro	Republic of Montenegro 


# evaluating results ----

# Solomon Islands
#   ECO_i_n12    ECO.i.12 ECO.i.13
#         100 7.116590988      100




# jobs ----
jobs = read.csv('tmp/liv_jobs_2012a_tocomplete2.csv'); head(jobs)

metric = dbGetQuery(pg, "SELECT * FROM global_li.metric_sector_year ORDER BY metric, sector, iso3166, year"); head(jobs.a)

head(ddply(jobs  , .(country_id), summarize, year.max=max(year, na.rm=T)))
head(ddply(jobs.a, .(metric, iso3166)   , summarize, year.max=max(year, na.rm=T)))

cntry_rgn_2013        = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/cntry_rgn_2013.csv', na.strings=''); head(cntry_rgn_2013)
cntry2013_country2012 = rename(read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/cntry2013_country2012.csv', na.strings=''),
                               c('cntry_key_2013'='cntry_key','country_id_2012'='country_id')); head(cntry2013_country2012)
d = merge(cntry_rgn_2013, cntry2013_country2012, all=T); head(d)
d[d$rgn_id %in% d$rgn_id[duplicated(d$rgn_id)],'duplicated_rgn'] = 1
d[d$cntry_key != d$country_id,'country_not_cntry'] = 1
d = d[order(d$rgn_id, d$country_id, d$cntry_key), c('rgn_id','country_id','cntry_key','duplicated_rgn','country_not_cntry')]
write.csv(d,'/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn2013_to_country2012.csv', row.names=F, na='')

d = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn2013_to_country2012.csv', na.strings='')
print(d[!is.na(d$duplicated_rgn),1:3], row.names=F)
#  rgn_id        country_id         cntry_key
#      13               GUM               GUM
#      13               MNP               MNP
#     116               PRI               PRI
#     116               VIR               VIR
#     137               ECU               ECU
#     137 Galapagos Islands Galapagos Islands
#     140               GLP               GLP
#     140               MTQ               MTQ
#     163            Alaska            Alaska
#     163            Hawaii            Hawaii
#     163               USA               USA
#     171               BRA               BRA
#     171          Trindade          Trindade
#     224               CHL               CHL
#     224     Easter Island     Easter Island
print(d[!is.na(d$country_not_cntry),1:3], row.names=F)

# old wage ----

wage.rgn = rename(read.csv('tmp/rgn_nber_oww_2013a_nofill.csv'),
                  c('rgn_id_2013'='rgn_id')); head(wage.rgn); dim(wage.rgn)
cntry_rgn = read.csv(file.path(root.data,'model/GL-NCEAS-OceanRegions_v2013a/manual_output/cntry_rgn_2013.csv'), na.strings=''); head(cntry_rgn)
wage = merge(wage.rgn, cntry_rgn[!cntry_rgn$cntry_key %in% c('Alaska','Hawaii','VIR'),], all.x=T); head(wage)
# missing
wage[is.na(wage$cntry_key),]
# duplicates
print(wage[wage$rgn_id %in% wage[duplicated(wage[,c('rgn_id','year')]),'rgn_id'],])
wage = ddply(wage, .(cntry_key, year), summarize, USD = sum(USD)) # CHN = China + Hong Kong
write.csv(wage, 'data/cntry_wage_2013a.csv')

w = rename(read.csv('tmp/liv_wages_2012a_completed.csv', na.strings=''),
           c('cntry_key_2013'='cntry_key')); head(w)
w.s = ddply(w, .(cntry_key, sector), summarize, max.year = max(year))
w.s$source = 'file'
w.a = dbGetQuery(pg, "SELECT iso3166 AS cntry_key, sector, year, value AS usd FROM global_li.metric_sector_year WHERE metric='wage' AND sector<>'og' ORDER BY metric, sector, iso3166, year"); head(w.a)
w.a$source = 'db'
w.a.s = ddply(w.a, .(cntry_key, sector), summarize, max.year = max(year))

m = merge(w.s, w.a.s, all=T); m
print(subset(m, is.na(source)), row.names=F)
#  cntry_key sector max.year source
#        AUT     sb     2002   <NA>
#        AUT   tour     2002   <NA>
#        BFA   tour     2000   <NA>
#        CZE   tour     2003   <NA>
#        HKG   tour     2003   <NA>
#        HUN     sb     2003   <NA>
#        HUN   tour     2003   <NA>
#        KAZ   tour     2003   <NA>
#        MDA   tour     2003   <NA>
#        MWI   tour     2002   <NA>
