\set search_path global_af,global_srcdata,global
SET search_path TO :search_path;

DROP TABLE IF EXISTS global.resilience_region_mora_s4;
CREATE TABLE global.resilience_region_mora_s4 (id int PRIMARY KEY, value double precision);

\set aggregator_input global_af.oaf_mora
\set aggregator_output global.resilience_region_mora_s4
\i '/usr/local/ohi/src/model/lib/aggregator.sql'

\echo "S4: Regions with nodata, filled with georegion average"
SELECT id, label
FROM  t_scores
JOIN regions_global USING (id)
WHERE source = 'georegion'
ORDER BY id
;

\echo "Regions with resilience=0"
INSERT INTO resilience_region_mora_s4
  SELECT  id, 0.0
  FROM    regions_global
  WHERE   id NOT IN (SELECT id FROM resilience_region_mora_s4)
RETURNING *
;


DROP TABLE IF EXISTS global.resilience_region_mora2009;
CREATE TABLE global.resilience_region_mora2009 (id int PRIMARY KEY, value double precision);


CREATE TEMPORARY TABLE t_input AS
SELECT  iso3166, 1 - AVG(value) AS value
FROM    (
  SELECT  iso3166, component, AVG(value) AS value
  FROM    mora2009
  GROUP BY iso3166, component
) t
WHERE   component IN (
    'fishing-effort',
    'foreign-fishing',
    'implementation-capability',
    'policy-transparency',
    'scientific-robustness',
    'subsidies'
    )
GROUP BY iso3166
HAVING COUNT(*) = 6 -- we take the simple mean, across all categories and all regions
ORDER BY iso3166
;

select * from t_input order by iso3166;

\set aggregator_input t_input
\set aggregator_output global.resilience_region_mora2009
\i '/usr/local/ohi/src/model/lib/aggregator.sql'

\echo "Mora2009: Regions with nodata, filled with georegion average"
SELECT id, label
FROM  t_scores
JOIN regions_global USING (id)
WHERE source = 'georegion'
ORDER BY id
;

\echo "Regions with resilience=0"
INSERT INTO global.resilience_region_mora2009
  SELECT  id, 0.0
  FROM    regions_global
  WHERE   id NOT IN (SELECT id FROM global.resilience_region_mora2009)
RETURNING *
;

\o data/global_resilience_region_mora_s4.csv
COPY (SELECT id, value FROM resilience_region_mora_s4 ORDER BY id) TO STDOUT WITH CSV HEADER
;

\o data/global_resilience_region_mora2009.csv
COPY (SELECT id, value FROM resilience_region_mora2009 ORDER BY id) TO STDOUT WITH CSV HEADER
;
