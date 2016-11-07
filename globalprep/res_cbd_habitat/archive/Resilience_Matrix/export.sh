rm finish.sql 2>/dev/null

psql -Xq -A -t -c 'select distinct layer from global.resilience_layer_data order by layer' | \
while read f; do
    dst="r_$(echo $f | tr '-' _ )"
    cat >> finish.sql <<EOM
\echo copying layer $f to data/$dst.csv...
\o data/$dst.csv
copy (
  select id, value from global.resilience_layer_data where layer = '$f' order by id
) to stdout with csv header
;
\o
EOM
done
