set search_path to global;

\o README_REPORT.html
\H

\C "Resilience matrix, by goal formatted like SOM"
select goal, component, name, layer AS layer_code
from resilience_matrix m join resilience_layers l using (layer)
order by goal, component, name
;

\C "Resilience matrix, by layer"
select *
from resilience_matrix m join resilience_layers l using (layer)
order by layer, goal, component
;

\C "Resilience matrix, by goal"
select *
from resilience_matrix m join resilience_layers l using (layer)
order by goal, component, layer
;


\C "Mismatches -- should have ZERO rows"
select m.*, l.*
from (select distinct layer from resilience_matrix) m 
left join resilience_layers l using (layer)
where l.layer is null
;

\C "Mismatches -- should have ZERO rows"
select m.*, l.*
from (select distinct layer from resilience_layers) l
left join resilience_matrix m using (layer)
where m.layer is null
;
