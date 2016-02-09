test -d data || mkdir data

env PGDATABASE=ohi_nature2012 ohi-db-export-table global_bd resilience_combos > data/global_resilience_combos.csv
