for fn in manual_output/global_resilience_*.csv; do
    csv2psql --schema=global --cascade $fn | psql -Xq
    rsync $fn data/
done
