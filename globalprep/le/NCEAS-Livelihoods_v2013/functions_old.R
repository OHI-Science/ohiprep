LIV.ECO.2012n = function(){
  
  #   ld.csv=layers_data.csv, 
  #   LIV.status.csv = file.path(dir.results, sprintf('LIV_status_%s.csv', sfx.scenario)),
  #   LIV.trend.csv  = file.path(dir.results, sprintf('LIV_trend_%s.csv' , sfx.scenario)),
  #   ECO.status.csv = file.path(dir.results, sprintf('ECO_status_%s.csv', sfx.scenario)),
  #   ECO.trend.csv  = file.path(dir.results, sprintf('ECO_trend_%s.csv' , sfx.scenario)),
  #   liv_adj_year=2009, eco_adj_min_year=2000
  
  library(sqldf)
  
  # database connection
  pg = dbConnect(dbDriver("PostgreSQL"), host='neptune.nceas.ucsb.edu', dbname='ohi_nature2012', user='bbest') # assumes password in ~/.pgpass
  dbSendQuery(pg, 'SET search_path TO global_li, global; SET ROLE TO ohi;')
  
  # TODO: remove Brazil entries
  # TODO: i_eco_rev_adj_gdp -> cnky_eco_rev_whence in functions.R
  # i_eco_rev_adj_gdp     country_id,year,value_num            Economies model variable  revenue adjustment (GDP)                     6F	  USD	    ECO	6F_eco_rev_adj_gdp.csv
  
  # since i_eco_rev_adj_gdp missing sector og for generating cny_eco_rev_adj_gdp, creating from filesystem
  #ohi.load('global_li_adjustments', dir='/var/data/ohi/model/GL-NCEAS-Livelihoods/data')
  global_li_adjustments = read.csv('/var/data/ohi/model/GL-NCEAS-Livelihoods/data/global_li_adjustments.csv', na.strings='')
  cnky_eco_rev_whence = sqldf(
    "SELECT iso3166 AS country_id, whence, year, value AS USD 
    FROM global_li_adjustments
    WHERE metric='rev_adj'")
  # TODO: i_eco_rev_adj_gdp -> cnky_eco_rev_whence in functions.R
  # TODO: consider splitting into two layers, since currently funkily incorporating [whence] (actual or model) as [category] but category isn't sector like other layers.
  
  
  # TODO: cny_liv_jobs_adj_unemp -> i_liv_jobs_adj_unemp in functions.R
  # TODO: ?cny_eco_rev_adj_gdp
  # TODO: cny_ao_ppp -> cny_le_ppp -> cny_ao_need (or ppppcgdp layer_id from Nature 2012?)
  # x = contrast(x=head(b), by.x=c('metric','sector','country_id','year'), on.x=c('value','base_value','base_whence','adj_value'),
  #              y=head(a), by.y=c('metric','sector','iso3166'   ,'year'), skip.y.na=F)
  
  # DEBUG isolated run
  #liv_adj_year=2009
  #eco_adj_min_year=2000
  #layers_data.csv = '/Volumes/local_edit/src/toolbox/data/global_2012_nature/layers_data.csv'  
  #source('/Volumes/local_edit/src/R/ohi/R/ohi.R')
  
  library(plyr)
  library(reshape2)
  library(sqldf)
  
  # cast data ----  
  
  # layers
  lyrs = list('cky' = c('cnky_eco_rev'             = 'rev',
                        'cnky_liv_jobs'            = 'jobs',
                        'cnky_liv_wages'           = 'wage'),
              'cy'  = c('cny_ao_need'              = 'ppp',
                        'cny_liv_pct_unemp'        = 'jobs',
                        'cny_le_workforcesize_adj' = 'workforce',
                        'cnky_eco_rev_whence'      = 'rev',
                        'cnky_liv_wages_adj_lilo'  = 'wage'))
  # NOTE: cnky_eco_rev_whence & cnky_liv_wages_adj_lilo are wierd ones unique by country & year with additional category column of whence
  layers = sub('(cky|cy)\\.','', names(unlist(lyrs)))
  
  # get layer data
  d = subset(read.csv(ld.csv, na.strings=''), layer %in% layers)
  
  # check for missing layers
  layers.missing = layers[!layers %in% d$layer]
  if (length(layers.missing)>0) stop(sprintf('Missing layer(s): %s', paste(layers.missing, collapse=', ')))
  # missing: cny_liv_pct_unemp
  
  # HACK: rbind unemp
  # adj_unemp  National unemployment statistics		7.48	percent	48	7_48_adj_unemp.csv																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																									
  
  # unemployment
  #   /Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_uem_2012a.csv
  #   /Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_uem_2013a.csv
  #     
  # total labor force
  #   /Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_lab_2012a.csv
  #   /Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_lab_2013a.csv
  
  x = read.csv('/Volumes/local_edit/src/toolbox/data/global_2012_nature_ftp/layers/7_48_adj_unemp.csv', na.strings=''); head(x)
  # TODO: disaggregate this layer too for 2013
  x = rename(x, c('country_id' = 'id_chr',
                  'percent'    = 'value_num'))
  x$layer = 'cny_liv_pct_unemp'
  names(d)[!names(d) %in% names(x)]
  d = rbind.fill(d, x)
  
  # check for duplicates
  for (lyr in lyrs[['cky']]){
    x = subset(d, layer==lyr)
    print(x[duplicated(x[,c('id_chr','category','year')]),])
  }
  for (lyr in lyrs[['cy']]){ # lyr='cny_le_workforcesize_adj'
    x = subset(d, layer==lyr)    
    print(x[duplicated(x[,c('id_chr','year')]),])
  }
  
  # cast per country, sector, year
  cky = rename(dcast(d, id_chr + category + year ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['cky']]))),
               c('id_chr'='country_id', 'category'='sector', lyrs[['cky']])) #; table(rk$habitat); print(head(subset(rk, habitat=='mangrove')), row.names=F); head(rk)
  #print(cky[duplicated(cky[,c('country_id','sector','year')]),])
  
  # cast per country, year
  cy = rename(dcast(d, id_chr + year ~ layer, value.var='value_num', subset = .(layer %in% names(lyrs[['cy']]))),
              c('id_chr'='country_id', lyrs[['cy']])) #; table(rk$habitat); print(head(subset(rk, habitat=='mangrove')), row.names=F); head(rk)
  #print(head(cy[duplicated(cy[,c('country_id','year')]),]))
  
  # add whence 
  cy.t = rename(dcast(d, id_chr + year ~ layer, value.var='category', subset = .(layer %in% c('cnky_eco_rev_whence','cnky_liv_wages_adj_lilo'))),
                c('id_chr'='country_id', 'cnky_eco_rev_whence'='rev_whence', 'cnky_liv_wages_adj_lilo'='wage_whence')); head(cy); head(cy.t); table(cy.t$rev_whence); table(cy.t$wage_whence)
  cy = merge(cy, cy.t, all.x=T); head(cy); summary(cy)
  #print(cy[duplicated(cy[,c('country_id','year')]),])
  
  # calculate status ----
  
  # get most recent (cur) and reference (ref) year, optimally going back 5 years
  jobs = ddply(subset(cky, !is.na(jobs), c('country_id','sector','year','jobs')), .(country_id, sector), summarize,
               metric = 'jobs',
               year_cur = max(year),
               year_ref = year[na.omit(match(c(5, 6, 4, 7, 3, 8, 2, 9, 1, 10), max(year) - year))[1]])
  rev = ddply(subset(cky, !is.na(rev), c('country_id','sector','year','rev')), .(country_id, sector), summarize,
              metric = 'rev',
              year_cur = max(year),
              year_ref = year[na.omit(match(c(5, 6, 4, 7, 3, 8, 2, 9, 1, 10), max(year) - year))[1]])
  wage = ddply(subset(cky, !is.na(wage), c('country_id','sector','year','wage')), .(country_id, sector), summarize,
               metric = 'wage',
               year_cur = max(year),
               year_ref = year[na.omit(match(c(5, 6, 4, 7, 3, 8, 2, 9, 1, 10), max(year) - year))[1]])
  
  # append jobs/rev/wage to base_cur and base_ref based on country_id, sector
  jobs  = sqldf("SELECT j.*, d.jobs  AS base_cur FROM jobs  AS j JOIN cky AS d ON (j.year_cur=d.year AND j.country_id=d.country_id AND j.sector=d.sector)")
  jobs  = sqldf("SELECT j.*, d.jobs  AS base_ref FROM jobs  AS j JOIN cky AS d ON (j.year_ref=d.year AND j.country_id=d.country_id AND j.sector=d.sector)")
  rev   = sqldf("SELECT j.*, d.rev   AS base_cur FROM rev   AS j JOIN cky AS d ON (j.year_cur=d.year AND j.country_id=d.country_id AND j.sector=d.sector)")
  rev   = sqldf("SELECT j.*, d.rev   AS base_ref FROM rev   AS j JOIN cky AS d ON (j.year_ref=d.year AND j.country_id=d.country_id AND j.sector=d.sector)")
  wage  = sqldf("SELECT j.*, d.wage AS base_cur FROM wage AS j JOIN cky AS d ON (j.year_cur=d.year AND j.country_id=d.country_id AND j.sector=d.sector)")
  wage  = sqldf("SELECT j.*, d.wage AS base_ref FROM wage AS j JOIN cky AS d ON (j.year_ref=d.year AND j.country_id=d.country_id AND j.sector=d.sector)")
  
  # append jobsrev/wage to adj_cur and adj_ref based on just country_id
  jobs  = sqldf("SELECT j.*, d.jobs  AS adj_cur FROM jobs  AS j JOIN cy AS d ON (j.year_cur=d.year AND j.country_id=d.country_id)")
  jobs  = sqldf("SELECT j.*, d.jobs  AS adj_ref FROM jobs  AS j JOIN cy AS d ON (j.year_ref=d.year AND j.country_id=d.country_id)")
  rev   = sqldf("SELECT j.*, d.rev   AS adj_cur FROM rev   AS j JOIN cy AS d ON (j.year_cur=d.year AND j.country_id=d.country_id)")
  rev   = sqldf("SELECT j.*, d.rev   AS adj_ref FROM rev   AS j JOIN cy AS d ON (j.year_ref=d.year AND j.country_id=d.country_id)")
  wage  = sqldf("SELECT j.*, d.wage AS adj_cur FROM wage AS j JOIN cy AS d ON (j.year_cur=d.year AND j.country_id=d.country_id)")
  wage  = sqldf("SELECT j.*, d.wage AS adj_ref FROM wage AS j JOIN cy AS d ON (j.year_ref=d.year AND j.country_id=d.country_id)")
  
  # combine jobs, rev, wage into single metrics table
  jrw = rbind(jobs, rev, wage)
  
  # quality enforce minimum data standards for countries to include: sum of sector years must be non-zero, 
  #   and at least 2 sectors are required across revenue and jobs, but any ok for wage
  jrw$base_sum = with(jrw, base_ref + base_cur)
  q = ddply(ddply(jrw, 
                  .(country_id, metric), summarize,
                  n_nonzero_sectors = length(sector) - sum(base_sum==0)), 
            .(country_id), summarize,
            n_nonzero_metric_sectors = sum(n_nonzero_sectors))
  mq = merge(jrw, q, all=T)
  m = subset(mq, (base_sum != 0) & (n_nonzero_metric_sectors > 1 | metric=='wage'))
  #m0 = subset(mq, n_nonzero_metric_sectors > 1 | (metric=='wage' & base_sum != 0))
  #   sqldf("SELECT m.* FROM m LEFT JOIN n USING (country_id,sector,metric) WHERE n.country_id IS NULL")
  #   x = merge(m[,1:3],m0[,1:3], all=T)
  # TODO: should this quality control really be for nonzero sectors per country,metric and not just country?
  #  FIX:
  #   q = ddply(jrw, .(country_id, metric), summarize,
  #             n_nonzero_sectors = length(sector) - sum(base_sum==0)))
  #   mq = merge(jrw, q, all=T)
  #   m = subset(mq, base_sum != 0 & (n_nonzero_sectors > 1 | metric=='wage')))
  
  # # b = sqldf("SELECT * FROM m WHERE base_ref <> 0 AND adj_ref <> 0")
  # # dim(b)
  # status_model_curref = dbGetQuery(pg, "SELECT * FROM global_li.status_model_curref")
  # subset(status_model_curref, iso3166=='BRA' & metric=='wage')
  # subset(m, country_id=='BRA' & metric=='wage')  
  # # status_model_curref_lim = dbGetQuery(pg, "SELECT * FROM global_li.status_model_curref WHERE ref_base_value <> 0 AND ref_adj_value <> 0")
  # # dim(status_model_curref_lim)
  
  # calculate jobs (LIV) and revenue (ECO) scores
  s_jr = sqldf(
    "SELECT  metric, country_id, COUNT(*) AS n_sector,
    (SUM(base_cur) / SUM(base_ref)) / (AVG(adj_cur) / AVG(adj_ref)) AS score
    FROM     m
    WHERE    base_ref <> 0 AND adj_ref <> 0 AND metric IN ('jobs', 'rev')
    GROUP BY metric, country_id
    ORDER BY country_id, metric")
  
  # for wage (LIV), compute the corrected relative value per metric per country:
  #   0. w'_i = (w_c/w_r)/(W_c/W_r) for each sector i per country
  #   1. let w' = unweighted mean(w'_i) across all sector i per country
  #   2. multiply w' by the purchasing power parity (PPP) value for the country
  s_w = sqldf(
    "SELECT  metric, country_id, (w_prime * ppp) AS score, n_sector
    FROM     (
    SELECT  metric, country_id, AVG(w_prime_i) AS w_prime, COUNT(*) AS n_sector
    FROM    (
    SELECT  metric, country_id, sector, 
    (base_cur / base_ref) / (adj_cur / adj_ref) AS w_prime_i
    FROM    m
    WHERE   metric = 'wage' AND base_ref <> 0 AND adj_ref <> 0
    ) t0
    GROUP BY  metric, country_id
    ) t1
    JOIN    ( -- find current ppp value per country
    SELECT  country_id, year, ppp
    FROM    cy
    JOIN    ( -- find most recent ppp year
    SELECT    country_id, MAX(year) AS year 
    FROM      cy
    WHERE     ppp IS NOT NULL
    GROUP BY  country_id
    ) max USING (country_id, year)
    ) p USING (country_id)
    ORDER BY metric, country_id")
  
  # 3. set the best country (PPP-adjusted average wage) equal to 1.0 and then rescale all countries to that max  
  s_w$score = s_w$score / max(s_w$score)
  
  # combine the corrected relative values into a single status score for LIV (jobs & wage) and ECO (revenue)
  s = sqldf(
    "SELECT *
    FROM (
    SELECT d.country_id, 
    cast('livelihood' AS varchar) AS component,   
    CASE WHEN j.score IS NOT NULL AND w.score IS NOT NULL 
    THEN (0.5 * j.score + 0.5 * w.score) 
    ELSE COALESCE(j.score, w.score) 
    END AS value
    FROM      (SELECT DISTINCT country_id FROM m) d    
    LEFT JOIN (SELECT * FROM s_jr WHERE metric = 'jobs') j USING (country_id)
    LEFT JOIN (SELECT * FROM s_w WHERE metric = 'wage') w USING (country_id)
    UNION        
    SELECT    d.country_id, 'economy', e.score
    FROM      (SELECT DISTINCT country_id FROM m) d    
    LEFT JOIN (SELECT * FROM s_jr WHERE metric = 'rev') e USING (country_id)        
    ) t
    WHERE value IS NOT NULL
    ORDER BY country_id")
  
  # assign status as value clamped 0 to 1, and multiply by 100
  s$score = pmin(s$value, 1) * 100
  
  # aggregate ----
  
  # aggregate countries to regions by country workforce size for livelihood
  w_liv = subset(cy, year==liv_adj_year & !is.na(workforce), c(country_id,workforce))
  
  s_liv = aggregate_by_country_weighted(df=subset(s, component=='livelihood'), w=w_liv,
                                        col.value='score', col.country='country_id', col.weight='workforce') # ABW workforce==NA  # summary(s_liv)
  # TODO: look for difference in LIV between ftp Nature2012 and global2012
  # s_liv.a = dbGetQuery(pg, "SELECT id, value*100 AS status FROM global_li.status_region WHERE component = 'livelihood'"); head(s_liv.a)
  #ck.LIV = contrast(s_liv, s_liv.a, by.x='region_id', by.y='id', on.x='score', on.y='status', precision=4)  
  # all y in x success: 171 
  # all x in y success: 171 
  # score.equ success
  #ck.LIV = contrast(x=s_liv, by.x='region_id', by.y='id', on.x='score', on.y='status',
  #                     y=subset(results_global_data, goal.subgoal=='LIV'), precision=2)
  # dropping mutual NAs: 1 / 172 
  # all y in x success: 172 
  # all x in y FAIL!: 3 / 171 
  #     region_id            score
  # 79         79 93.6637698071856
  # 110       110 93.6637698071856
  # 114       114 93.6637698071856
  # score.equ  FAIL! on 3 / 171 
  #  region_id            score  score.y          score.dif
  #        121 68.3349668096027 66.75735  1.577616809602731
  #        122 58.4077369325363 55.57214  2.835596932536262
  #        171 82.5785097996376 83.16348 -0.584970200362378
  
  # aggregate countries to regions by country revenue for economies
  w_eco = w = ddply(subset(cy, !is.na(rev) & year >= eco_adj_min_year & rev_whence=='actual'), .(country_id), summarize,
                    rev_adj = rev[which.max(year)])  
  s_eco = aggregate_by_country_weighted(df=subset(s, component=='economy'), w=w_eco,
                                        col.value='score', col.country='country_id', col.weight='rev_adj') # ABW workforce==NA  
  # TODO: look for difference in ECO between ftp Nature2012 and global2012
  # s_eco.a = dbGetQuery(pg, "SELECT id, value*100 AS status FROM global_li.status_region WHERE component = 'economy'"); head(s_eco.a)
  #ck.ECO = contrast(s_eco, s_eco.a, by.x='region_id', by.y='id', on.x='score', on.y='status', precision=4)
  # all y in x success: 171 
  # all x in y success: 171 
  # score.equ success  
  # ck.ECO = contrast(x=s_eco, by.x='region_id', by.y='id', on.x='score', on.y='status',
  #                    y=subset(results_global_data, goal.subgoal=='ECO'), precision=2)
  # dropping mutual NAs: 1 / 172 
  # all y in x success: 172 
  # all x in y FAIL!: 3 / 171 
  #     region_id            score
  # 79         79 75.0066169564487
  # 110       110 75.0066169564487
  # 114       114 75.0066169564487
  # score.equ  FAIL! on 73 / 171 
  #  region_id            score  score.y          score.dif
  #          1 74.2692832666138 60.35782 13.911463266613758
  #          2 74.2692832666138 60.35782 13.911463266613758
  #         10 76.9955069354751 73.61761  3.377896935475107
  #         12 77.2720562615789 62.23359 15.038466261578854
  #         15 77.3206334298282 77.91991 -0.599276570171767
  #         16 99.8922517164328 94.90722  4.985031716432843
  # ...  
  
  # gather status
  d.status = merge(setNames(s_liv[,c('region_id','score')], c('region_id','LIV_status')),
                   setNames(s_eco[,c('region_id','score')], c('region_id','ECO_status')), all=T)
  
  print('LE.browser')
  browser()
  LIV.status.csv = file.path(dir.results, sprintf('LIV_status_%s.csv', sfx.scenario))
  LIV.trend.csv  = file.path(dir.results, sprintf('LIV_trend_%s.csv' , sfx.scenario))
  ECO.status.csv = file.path(dir.results, sprintf('ECO_status_%s.csv', sfx.scenario))
  
  
  #  head(d.status)
  
  # calculate trend ----
  # TODO: get whence for jobs (have for rev & wage)  
  
  #   adjustments = dbGetQuery(pg, "SELECT * FROM global_li.adjustments"); head(adjustments); dim(adjustments); head(metric_sector_refperiod)
  #   b = sqldf("SELECT 'jobs_adj'  AS metric, country_id, year, NULL  AS whence, jobs  AS value FROM cy WHERE jobs  IS NOT NULL
  #         UNION
  #         SELECT 'rev_adj'   AS metric, country_id, year, rev_whence AS whence, rev   AS value FROM cy WHERE rev   IS NOT NULL
  #         UNION
  #         SELECT 'wage_adj' AS metric, country_id, year, NULL       AS whence, wage AS value FROM cy WHERE wage IS NOT NULL" ); head(b)
  #   x = contrast(x=b,           by.x=c('metric','country_id','year'), on.x='value',
  #                y=adjustments, by.y=c('metric','iso3166'   ,'year'), on.y='value', skip.y.na=F)
  #  all good
  
  #   head(jrw)
  #   head(cky)
  #   status_model_curref = dbGetQuery(pg, "SELECT * FROM global_li.status_model_curref"); head(a); dim(a)
  #   adjustments = dbGetQuery(pg, "SELECT * FROM global_li.adjustments"); head(adjustments); dim(adjustments); head(metric_sector_refperiod)
  #   dlply(adjustments, .(metric), function(x) table(x$whence))
  
  #   a = dbGetQuery(pg, "    -- grab adjustment data per metric-sector-year in sector timeframe
  #     SELECT  d.metric, d.sector, d.iso3166,
  #             a.year, a.whence AS adj_whence, a.value AS adj_value
  #     FROM    global_li.metric_sector_refperiod d
  #     JOIN    global_li.adjustments a USING (iso3166)
  #     WHERE   a.metric = d.metric || '_adj' AND 
  #             a.year <= d.cur_year AND
  #             a.year >= d.ref_year"); head(a)  
  b = sqldf(
    "-- SELECT d.metric, d.sector, d.country_id, 
    --   a.year, a.
    -- grab adjustment data per metric-sector-year in sector timeframe
    SELECT  d.metric, d.sector, d.country_id,
    a.year, a.whence AS adj_whence, a.value AS adj_value
    FROM    jrw AS d
    JOIN (
    SELECT 'jobs'  AS metric, country_id, year, NULL       AS whence, jobs  AS value FROM cy WHERE jobs  IS NOT NULL
    UNION
    SELECT 'rev'   AS metric, country_id, year, rev_whence AS whence, rev   AS value FROM cy WHERE rev   IS NOT NULL
    UNION
    SELECT 'wage' AS metric, country_id, year, NULL       AS whence, wage AS value FROM cy WHERE wage IS NOT NULL
    ) AS a USING (country_id, metric)
    WHERE   a.year <= d.year_cur AND
    a.year >= d.year_ref"); head(b)
  
  
  #   a = dbGetQuery(pg, "SELECT  d.metric, d.sector, d.iso3166, d.year,
  #           cast(NULL as double precision)AS value,
  #           m.value                       AS base_value, 
  #           cast('actual' as varchar(80)) AS base_whence,
  #           d.adj_value,
  #           d.adj_whence
  #   FROM    ( 
  #     -- grab adjustment data per metric-sector-year in sector timeframe
  #     SELECT  d.metric, d.sector, d.iso3166,
  #             a.year, a.whence AS adj_whence, a.value AS adj_value
  #     FROM    global_li.metric_sector_refperiod d
  #     JOIN    global_li.adjustments a USING (iso3166)
  #     WHERE   a.metric = d.metric || '_adj' AND 
  #             a.year <= d.cur_year AND
  #             a.year >= d.ref_year
  #     ) d
  #   JOIN    global_li.metric_sector_year m USING (metric, sector, iso3166, year)
  #   WHERE   m.value IS NOT NULL -- double-check we're getting an actual base
  #   ORDER BY d.metric, d.sector, d.iso3166, d.year"); head(a)
  
  b = sqldf(
    "SELECT metric, sector, country_id, year,
    CAST(NULL as REAL) AS value,
    m.value                       AS base_value, 
    cast('actual' as varchar(80)) AS base_whence,
    adj_value,
    adj_whence
    FROM  (
    -- grab adjustment data per metric-sector-year in sector timeframe
    SELECT  d.metric, d.sector, d.country_id,
    a.year, a.whence AS adj_whence, a.value AS adj_value
    FROM    jrw AS d
    JOIN (
    SELECT 'jobs'  AS metric, country_id, year, wage_whence AS whence, jobs  AS value FROM cy WHERE jobs  IS NOT NULL
    UNION
    SELECT 'rev'   AS metric, country_id, year, rev_whence   AS whence, rev  AS value FROM cy WHERE rev   IS NOT NULL
    UNION
    SELECT 'wage' AS metric, country_id, year, NULL          AS whence, wage AS value FROM cy WHERE wage IS NOT NULL
    ) AS a USING (country_id, metric)
    WHERE   a.year <= d.year_cur AND
    a.year >= d.year_ref) AS d
    JOIN  (
    SELECT 'jobs' AS metric, sector, country_id, year, jobs AS value FROM cky WHERE jobs  IS NOT NULL
    UNION
    SELECT 'rev'  AS metric, sector, country_id, year, rev  AS value FROM cky WHERE rev   IS NOT NULL
    UNION
    SELECT 'wage' AS metric, sector, country_id, year, wage AS value FROM cky WHERE wage IS NOT NULL
    ) AS m USING (metric, sector, country_id, year)
    WHERE   m.value IS NOT NULL -- double-check we're getting an actual base
    ORDER BY d.metric, d.sector, d.country_id, d.year"); head(b)  
  #x = contrast(x=b, by.x=c('metric','sector','country_id','year'), on.x=c('value','base_value','base_whence','adj_value'),
  #             y=a, by.y=c('metric','sector','iso3166'   ,'year'), skip.y.na=F)
  # all y in x FAIL!: 28 / 5933 
  #      metric sector     country_id year value.y base_value.y base_whence.y adj_value.y
  # 1085   jobs    mmw            ATA 1998      NA            5        actual          NA
  # 1086   jobs    mmw            ATA 2008      NA           84        actual          NA
  # 1089   jobs    mmw         Azores 1998      NA           11        actual          NA
  # 1090   jobs    mmw         Azores 2008      NA           46        actual          NA
  # 1107   jobs    mmw Canary Islands 1998      NA          943        actual          NA
  # 1108   jobs    mmw Canary Islands 2008      NA          576        actual          NA"  
  #table(subset(x, is.na(base_value), c(metric, sector)))
  #        sector
  # metric mmw og tour
  #   jobs   6  0    0
  #   rev    6 12    0
  #   wage   0  2    2
  #table(subset(x, is.na(base_value), c(metric, country_id)))
  #          country_id
  #   metric ATA Azores Canary Islands KAZ Wake Island
  #   jobs   2      2              2   0           0
  #   rev    8      2              2   0           6
  #   wage   0      0              0   4           0
  
  # QUICK HACK to consume Nature 2012 results as a placeholder
  d = read.csv(file.path(root.data ,'model/GL-NCEAS-LayersDisaggregated_v2013a/data/rgn_results_2012n_long.csv'), na.strings=''); head(d)
  
  
  
}

LIV.ECO.2013 = function(layers){
  
  # DEBUG: initialize -----
  
  # arguments
  yr = 2013
  
  library(devtools)
  library(RPostgreSQL)
  
  #wd = '~/Code/ohicore'
  #setwd(wd)
  load_all()
  
  #library(RCurl)
  library(dplyr)
  
  # paths
  dir_data  = '/Volumes/data_edit'
  dir_local = '/Volumes/local_edit'
  
  # db for comparison
  pg = dbConnect(dbDriver("PostgreSQL"), host='neptune.nceas.ucsb.edu', dbname='ohi_global2013', user='bbest') # assumes password in ~/.pgpass
  dbSendQuery(pg, 'SET search_path TO global_li, global; SET ROLE TO ohi;')
  
  
  # END ----
  # inputs
  status_model_curref = read.csv(file.path(dir_data,  sprintf('model/GL-NCEAS-Livelihoods_2012/data_%d/global_li_status_model_curref.csv', yr)), na.strings='') # head(status_model_curref)
  paste(names(status_model_curref), collapse=', ') # metric,sector,iso3166,cur_year,ref_year,cur_base_value,ref_base_value,cur_adj_value,ref_adj_value
  
  # generate layers for toolbox
  for (yr in 2012:2013){
    status_model_curref = read.csv(file.path(dir_data,  sprintf('model/GL-NCEAS-Livelihoods_2012/data_%d/global_li_status_model_curref.csv', yr)), na.strings='') # head(status_model_curref)
    
    d = status_model_curref %.%
      melt(id.vars=c('metric','iso3166','sector')) %.%
      select(metric, cntry_key=iso3166, sector, variable, value) %.%
      arrange(metric, cntry_key, sector, variable)
    write.csv()
    
    for (m in c('jobs','rev','wage')){
      x = d %.%
        filter(metric==m) %.%
        select(cntry_key, sector, variable, value)
      write.csv()
      
      jobs = d %.%
        filter(metric=='jobs') %.%
        select(cntry_key, sector, variable, value)
      
      jobs = d %.%
        filter(metric=='jobs') %.%
        select(cntry_key, sector, variable, value)
      
      rev = status_model_curref %.%
        filter(metric=='rev') %.%
        select(
          cntry_key=iso3166, 
          sector, cur_year, ref_year, cur_base_value, ref_base_value, cur_adj_value, ref_adj_value) %.%
        melt(id.vars=c('cntry_key','sector')) %.%
        arrange(cntry_key, sector, variable)
      
    }
    status_model_curref
    
    
    wage
    
    #  use fields: metric, iso3166, cur_base_value, ref_base_value, cur_adj_value, ref_adj_value 
    ppp                 = read.csv(file.path(dir_data, 'model/GL-NCEAS-Livelihoods_v2013a/data/country_gdppcppp_2013a.csv'), na.strings='') # head(ppp); dim(ppp); summary(ppp)
    
    
    
    # model... ----
    
    # compute the corrected relative value per metric per country, for JOBS
    status_jobs_rev = status_model_curref %.%
      filter(ref_base_value != 0 & ref_adj_value != 0 & metric %in% c('jobs', 'rev')) %.%
      group_by(metric, iso3166) %.%
      summarise(
        score    = (sum(cur_base_value, na.rm=T) / sum(ref_base_value, na.rm=T)) / (mean(cur_adj_value, na.rm=T) / mean(ref_adj_value, na.rm=T)),
        n_sector = n()) %.%
      arrange(metric, iso3166)
    
    # compute the corrected relative value per metric per country, for WAGE
    # 0. extract w'_i = (w_c/w_r)/(W_c/W_r) for each sector i per country
    t0 = status_model_curref %.%
      filter(metric=='wage' & ref_base_value != 0 & ref_adj_value != 0) %.%
      mutate(w_prime_i = (cur_base_value / ref_base_value) / (cur_adj_value / ref_adj_value)) %.%
      select(metric, iso3166, sector, w_prime_i) %.%
      group_by(metric, iso3166) %.%
      summarise(w_prime  = mean(w_prime_i, na.rm=T),
                n_sector = n()) %.%
      arrange(metric, iso3166)
    
    # 1. let w' = unweighted mean(w'_i) across all sector i per country
    # 2. multiple w' by the most recent purchasing power parity (PPP) value for the country  
    p = ppp %.%
      rename(c(ISO3166='iso3166',YEAR='year',VALUE='value')) %.%
      arrange(iso3166, year) %.%
      group_by(iso3166) %.%
      summarise(year      = last(year),
                ppp_value = last(value)) %.%
      filter(!is.na(ppp_value)) %.%
      arrange(iso3166)
    t2 = t0 %.%
      merge(p, by='iso3166') %.%
      mutate(score = w_prime * ppp_value) %.%
      select(metric, iso3166, score, n_sector) %.%
      arrange(metric, iso3166)
    
    # 3. set the best country (PPP-adjusted average wage) equal to 1.0 and then rescale all countries to that max
    max_wage_score = max(t2$score, na.rm=T)
    status_wage = t2 %.%
      mutate(score = score / max_wage_score)
    
    # combine the corrected relative values into a single status score
    status_model_combined = ungroup(status_jobs_rev) %.%
      rbind(status_wage)
    status_score = status_model_combined %.%
      # liv
      dcast(iso3166 ~ metric, value.var='score') %.%
      group_by(iso3166) %.%
      mutate(
        value     = mean(c(jobs, wage), na.rm=T),
        component = 'livelihood') %.%
      select(iso3166, component, value) %.%
      ungroup() %.% 
      arrange(iso3166, component, value) %.%
      # eco
      rbind(status_model_combined %.%
              filter(metric=='rev') %.%
              mutate(
                value     = score,
                component = 'economy') %.%
              select(iso3166, component, value)) %.%
      # order
      filter(!is.na(value)) %.%
      arrange(iso3166, component) %.%
      # clamp
      mutate(score = pmin(value, 1))
    
    # DEBUG: status_score compare ---
    status_score_pg = dbGetQuery(pg, "SELECT * FROM status_score ORDER BY iso3166, component")
    status_score_vs = status_score %.%
      merge(
        status_score_pg %.%
          select(iso3166, component, value, score_pg = score), 
        by=c('iso3166','component','value')) %.%
      mutate(
        score_dif         = score - score_pg,
        score_na_mismatch = ifelse(is.na(score)==is.na(score_pg), T, F))
    summary(abs(status_score_vs$score_dif))
    sum(status_score_vs$score_na_mismatch)
    
    # TODO NEXT: 
    #  * aggregate country_2012 to region_2012
    #    - original: /Volumes/local_edit/src/model/global2013/livelihoods/status
    #    - new: aggregate_by_country_weighted(), eg above
    #  * disaggregate region_2012 to region_2013
    #    - /Volumes/data_edit/model/GL-NCEAS-LayersDisaggregated_v2013a/digest_disaggregate.R
    
  }
  
  LIV = function(layers){
    
    # scores
    scores = rename(subset(SelectLayersData(layers, layers=c('rn_liveco_status'='status','rn_liveco_trend'='trend'), narrow=T),
                           category=='livelihood'),
                    c(id_num='region_id', category='goal', layer='dimension', val_num='score'))
    scores = mutate(scores, 
                    score = ifelse(dimension=='status', score * 100, score),
                    goal  = 'LIV')
    return(scores)  
  }
  
  
  ECO = function(layers){
    
    # scores
    scores = rename(subset(SelectLayersData(layers, layers=c('rn_liveco_status'='status','rn_liveco_trend'='trend'), narrow=T),
                           category=='economy'),
                    c(id_num='region_id', category='goal', layer='dimension', val_num='score'))
    scores = mutate(scores, 
                    score = ifelse(dimension=='status', score * 100, score),
                    goal  = 'ECO')
    return(scores)
  }
  
  LE = function(scores, layers){
    
    # calculate LE scores
    scores.LE = scores %.% 
      filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %.%
      dcast(region_id + dimension ~ goal, value.var='score') %.%
      mutate(score = rowMeans(cbind(ECO, LIV), na.rm=T)) %.%
      select(region_id, dimension, score) %.%
      mutate(goal  = 'LE')
    
    # rbind to all scores
    scores = scores %.%
      rbind(scores.LE)  
    
    # LIV, ECO and LE: nullify unpopulated regions and those of the Southern Ocean Islands
    r_s_islands   = subset(SelectLayersData(layers, layers='rnk_rgn_georegions', narrow=T), 
                           category=='r2' & val_num==999, id_num, drop=T)
    r_unpopulated = subset(ddply(SelectLayersData(layers, layers='rny_le_popn', narrow=T), .(id_num), summarize, 
                                 count = val_num[which.max(year)]),
                           is.na(count) | count==0, id_num, drop=T)
    scores[with(scores, 
                goal %in% c('LIV','ECO','LE') & 
                  !dimension %in% c('pressures','resilience') & 
                  region_id %in% union(r_s_islands, r_unpopulated)),
           'score'] = NA
    
    # return scores
    return(scores)  
  }
  