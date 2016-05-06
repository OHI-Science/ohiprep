### Summarize mean category and trend per region -----
##############################################################################=
### create final outputs for global analysis

sum_by_loiczid  <- read_csv(sum_by_loiczid_file,
                            col_types = 'ddddd')  ### file from previous step
### This returns dataframe with variables:
### loiczid | weighted_mean_cat | weighted_mean_trend | n_cat_spp | n_tr_spp

rgn_cell_lookup <- extract_cell_id_per_region(reload = FALSE)
### | rgn_id | rgn_name | loiczid | proportionArea | csq | cell_area

### saves lookup table to git-annex/globalprep/spp_ico/rgns/cellID_region_gcs_global.csv
### To use a different region shapefile, add an argument of: rgn_layer = '<rgn file here, without extension>'
### To use a shape file from a different directory (other than git-annex/globalprep/spatial/v2015/data), add an ogr_location argument.
### To run a different type of analysis, add an ohi_type argument with global (default), HS, or AQ

sum_by_rgn <- get_means_per_rgn(sum_by_loiczid, rgn_cell_lookup, rgn_note = 'gl')
### This returns dataframe with variables:
### rgn_id | rgn_mean_cat | rgn_mean_trend | status


spp_status <- sum_by_rgn %>%
  dplyr::select(rgn_id, score = status)
spp_trend <- sum_by_rgn %>%
  dplyr::select(rgn_id, score = rgn_mean_trend)
write_csv(spp_status, file.path(dir_git, scenario, 'output/spp_status_global.csv'))
write_csv(spp_trend,  file.path(dir_git, scenario, 'output/spp_trend_global.csv'))