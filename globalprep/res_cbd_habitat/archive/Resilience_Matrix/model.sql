SET search_path TO global;

\echo "Regulations resilience is weighted average"
CREATE TEMPORARY TABLE t_step1 AS
SELECT  l.id, m.goal, m.component, l.type, l.category, 
        SUM(l.weight * l.value)/SUM(l.weight) AS value, COUNT(*)
FROM    resilience_matrix m 
JOIN    v_resilience_layers l USING (layer)
GROUP BY l.id, m.goal, m.component, l.type, l.category
ORDER BY l.id, m.goal, m.component, l.type, l.category
;
\echo "verify integrity"
ALTER TABLE t_step1 ALTER COLUMN "value" SET NOT NULL;

-- select * from t_step1;

\echo "Ecological resilience is mean of integrity and regulations"
CREATE TEMPORARY TABLE t_step2 AS
SELECT  id, goal, component, type, AVG(value) AS value, COUNT(*)
FROM    t_step1 l
GROUP BY id, goal, component, type
ORDER BY id, goal, component, type
;

\echo "verify integrity"
ALTER TABLE t_step2 ALTER COLUMN "value" SET NOT NULL;

-- select * from t_step2;

\echo "The following query should have ZERO rows"
SELECT  id, goal, component
FROM    t_step2 l
GROUP BY id, goal, component
HAVING COUNT(*) > 2
;

\echo "Using gamma=0.5 to combine ecological and social"
DROP TABLE IF EXISTS resilience CASCADE;
CREATE TABLE resilience AS
SELECT  goal, component, id, AVG(value)::double precision AS value
FROM    t_step2 l
GROUP BY goal, component, id
ORDER BY goal, component, id
;

-- select * from resilience;

ALTER TABLE resilience 
  ADD PRIMARY KEY (goal, component, id),
  ALTER COLUMN "value" SET NOT NULL
;

CREATE OR REPLACE VIEW resilience_region AS
  SELECT * FROM resilience
;

CREATE OR REPLACE VIEW v_resilience_goal_layers AS
select m.goal, m.component, d.layer, d.type, d.category, d.id, d.value, d.weight 
from resilience_matrix m join v_resilience_layers d using (layer) 
order by m.goal, m.component, d.layer, d.type, d.category, d.id
;

\o data/global_resilience.csv
COPY (
  SELECT * FROM resilience
) TO STDOUT WITH CSV HEADER
;

\o _step1.csv
COPY (
  SELECT * FROM t_step1 ORDER BY goal, component, id
) TO STDOUT WITH CSV HEADER
;

\o _step2.csv
COPY (
  SELECT * FROM t_step2 ORDER BY goal, component, id
) TO STDOUT WITH CSV HEADER
;
