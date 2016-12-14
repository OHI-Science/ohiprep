SpatialFishCatch folder



"data" folder: includes data used in model. Both raw and created


The file "FAO_raw.csv" is the FAO data exported from FishStatJ from 1950-2011.

Jamie used “clean_FAOfisheries_Jamie.R” which modified the script “clean_FAOfisheries.R” from JStewart and BBest to clean up “FAO_raw.csv” and create “FAO_raw_clean.csv”

Script "FAO_species.R" was used to parse out the unique species reported as catch to FAO. This was grouped into the varying taxonomic levels FAO uses including species, genus, family and order+. Each list was saved in a corresponding .csv file with the name "FAO Family.csv", "FAO Genus.csv" and so on.
