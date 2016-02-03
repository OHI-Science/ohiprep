SET search_path TO global;

CREATE OR REPLACE VIEW v_resilience_layers AS
  SELECT  type, category, layer, l.value AS weight, id, d.value
  FROM    resilience_layers l
  JOIN    resilience_layer_data d USING (layer)
  WHERE   d.value IS NOT NULL
  ORDER BY type, category, layer, id
;

\o data/global_v_resilience_layers.csv
COPY (
  SELECT * FROM v_resilience_layers
) TO STDOUT WITH CSV HEADER
;
