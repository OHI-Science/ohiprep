\echo copying layer af-oaf-mora to data/r_af_oaf_mora.csv...
\o data/r_af_oaf_mora.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'af-oaf-mora' order by id
) to stdout with csv header
;
\o
\echo copying layer alien-species to data/r_alien_species.csv...
\o data/r_alien_species.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'alien-species' order by id
) to stdout with csv header
;
\o
\echo copying layer bd-wgi-government-effectiveness to data/r_bd_wgi_government_effectiveness.csv...
\o data/r_bd_wgi_government_effectiveness.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'bd-wgi-government-effectiveness' order by id
) to stdout with csv header
;
\o
\echo copying layer bd-wgi-political-stability to data/r_bd_wgi_political_stability.csv...
\o data/r_bd_wgi_political_stability.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'bd-wgi-political-stability' order by id
) to stdout with csv header
;
\o
\echo copying layer cbd-alien-species to data/r_cbd_alien_species.csv...
\o data/r_cbd_alien_species.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'cbd-alien-species' order by id
) to stdout with csv header
;
\o
\echo copying layer cbd-habitat to data/r_cbd_habitat.csv...
\o data/r_cbd_habitat.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'cbd-habitat' order by id
) to stdout with csv header
;
\o
\echo copying layer cbd-mariculture to data/r_cbd_mariculture.csv...
\o data/r_cbd_mariculture.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'cbd-mariculture' order by id
) to stdout with csv header
;
\o
\echo copying layer cbd-signatories to data/r_cbd_signatories.csv...
\o data/r_cbd_signatories.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'cbd-signatories' order by id
) to stdout with csv header
;
\o
\echo copying layer cbd-tourism to data/r_cbd_tourism.csv...
\o data/r_cbd_tourism.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'cbd-tourism' order by id
) to stdout with csv header
;
\o
\echo copying layer cbd-water to data/r_cbd_water.csv...
\o data/r_cbd_water.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'cbd-water' order by id
) to stdout with csv header
;
\o
\echo copying layer cites to data/r_cites.csv...
\o data/r_cites.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'cites' order by id
) to stdout with csv header
;
\o
\echo copying layer fishing to data/r_fishing.csv...
\o data/r_fishing.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'fishing' order by id
) to stdout with csv header
;
\o
\echo copying layer fishing-v1 to data/r_fishing_v1.csv...
\o data/r_fishing_v1.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'fishing-v1' order by id
) to stdout with csv header
;
\o
\echo copying layer fishing-v1-eez to data/r_fishing_v1_eez.csv...
\o data/r_fishing_v1_eez.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'fishing-v1-eez' order by id
) to stdout with csv header
;
\o
\echo copying layer fishing-v2 to data/r_fishing_v2.csv...
\o data/r_fishing_v2.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'fishing-v2' order by id
) to stdout with csv header
;
\o
\echo copying layer fishing-v2-eez to data/r_fishing_v2_eez.csv...
\o data/r_fishing_v2_eez.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'fishing-v2-eez' order by id
) to stdout with csv header
;
\o
\echo copying layer fishing-v3 to data/r_fishing_v3.csv...
\o data/r_fishing_v3.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'fishing-v3' order by id
) to stdout with csv header
;
\o
\echo copying layer fishing-v3-eez to data/r_fishing_v3_eez.csv...
\o data/r_fishing_v3_eez.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'fishing-v3-eez' order by id
) to stdout with csv header
;
\o
\echo copying layer fishing-v4 to data/r_fishing_v4.csv...
\o data/r_fishing_v4.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'fishing-v4' order by id
) to stdout with csv header
;
\o
\echo copying layer fishing-v4-eez to data/r_fishing_v4_eez.csv...
\o data/r_fishing_v4_eez.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'fishing-v4-eez' order by id
) to stdout with csv header
;
\o
\echo copying layer habitat to data/r_habitat.csv...
\o data/r_habitat.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'habitat' order by id
) to stdout with csv header
;
\o
\echo copying layer habitat-combo to data/r_habitat_combo.csv...
\o data/r_habitat_combo.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'habitat-combo' order by id
) to stdout with csv header
;
\o
\echo copying layer habitat-combo-eez to data/r_habitat_combo_eez.csv...
\o data/r_habitat_combo_eez.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'habitat-combo-eez' order by id
) to stdout with csv header
;
\o
\echo copying layer li-gci to data/r_li_gci.csv...
\o data/r_li_gci.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'li-gci' order by id
) to stdout with csv header
;
\o
\echo copying layer li-sector-evenness to data/r_li_sector_evenness.csv...
\o data/r_li_sector_evenness.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'li-sector-evenness' order by id
) to stdout with csv header
;
\o
\echo copying layer li-wgi-regulatory-quality to data/r_li_wgi_regulatory_quality.csv...
\o data/r_li_wgi_regulatory_quality.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'li-wgi-regulatory-quality' order by id
) to stdout with csv header
;
\o
\echo copying layer mariculture to data/r_mariculture.csv...
\o data/r_mariculture.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'mariculture' order by id
) to stdout with csv header
;
\o
\echo copying layer mora to data/r_mora.csv...
\o data/r_mora.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'mora' order by id
) to stdout with csv header
;
\o
\echo copying layer mora-et-al-2009 to data/r_mora_et_al_2009.csv...
\o data/r_mora_et_al_2009.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'mora-et-al-2009' order by id
) to stdout with csv header
;
\o
\echo copying layer mora-s4 to data/r_mora_s4.csv...
\o data/r_mora_s4.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'mora-s4' order by id
) to stdout with csv header
;
\o
\echo copying layer mpa-percent to data/r_mpa_percent.csv...
\o data/r_mpa_percent.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'mpa-percent' order by id
) to stdout with csv header
;
\o
\echo copying layer mpa-percent-eez to data/r_mpa_percent_eez.csv...
\o data/r_mpa_percent_eez.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'mpa-percent-eez' order by id
) to stdout with csv header
;
\o
\echo copying layer msi-gov to data/r_msi_gov.csv...
\o data/r_msi_gov.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'msi-gov' order by id
) to stdout with csv header
;
\o
\echo copying layer msi-gov-code-of-conduct to data/r_msi_gov_code_of_conduct.csv...
\o data/r_msi_gov_code_of_conduct.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'msi-gov-code-of-conduct' order by id
) to stdout with csv header
;
\o
\echo copying layer msi-gov-traceability to data/r_msi_gov_traceability.csv...
\o data/r_msi_gov_traceability.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'msi-gov-traceability' order by id
) to stdout with csv header
;
\o
\echo copying layer species-diversity to data/r_species_diversity.csv...
\o data/r_species_diversity.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'species-diversity' order by id
) to stdout with csv header
;
\o
\echo copying layer species-diversity-3nm to data/r_species_diversity_3nm.csv...
\o data/r_species_diversity_3nm.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'species-diversity-3nm' order by id
) to stdout with csv header
;
\o
\echo copying layer tourism to data/r_tourism.csv...
\o data/r_tourism.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'tourism' order by id
) to stdout with csv header
;
\o
\echo copying layer water to data/r_water.csv...
\o data/r_water.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'water' order by id
) to stdout with csv header
;
\o
\echo copying layer wgi-all to data/r_wgi_all.csv...
\o data/r_wgi_all.csv
copy (
  select id, value from global.resilience_layer_data where layer = 'wgi-all' order by id
) to stdout with csv header
;
\o
