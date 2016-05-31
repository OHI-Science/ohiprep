### Summarize mean category and trend per region -----
##############################################################################=
### create final outputs for global analysis

sum_by_loiczid_nobirds  <- read_csv(sum_by_loiczid_file_nobirds,
                            col_types = 'ddddd')  ### file from previous step
### This returns dataframe with variables:
### loiczid | weighted_mean_cat | weighted_mean_trend | n_cat_spp | n_tr_spp

rgn_cell_lookup <- extract_cell_id_per_region(reload = FALSE)
### | rgn_id | rgn_name | loiczid | proportionArea | csq | cell_area

### saves lookup table to git-annex/globalprep/spp_ico/rgns/cellID_region_gcs_global.csv
### To use a different region shapefile, add an argument of: rgn_layer = '<rgn file here, without extension>'
### To use a shape file from a different directory (other than git-annex/globalprep/spatial/v2015/data), add an ogr_location argument.
### To run a different type of analysis, add an ohi_type argument with global (default), HS, or AQ

sum_by_rgn_nobirds <- get_means_per_rgn(sum_by_loiczid_nobirds, rgn_cell_lookup, rgn_note = 'gl_nobirds')
### This returns dataframe with variables:
### rgn_id | rgn_mean_cat | rgn_mean_trend | status


spp_status_nobirds <- sum_by_rgn_nobirds %>%
  dplyr::select(rgn_id, score = status)
spp_trend_nobirds <- sum_by_rgn_nobirds %>%
  dplyr::select(rgn_id, score = rgn_mean_trend)
write_csv(spp_status_nobirds, file.path(dir_git, scenario, 'output/spp_status_global_nobirds.csv'))
write_csv(spp_trend_nobirds,  file.path(dir_git, scenario, 'output/spp_trend_global_nobirds.csv'))

### Summarize mean category and trend per region within 3 nm of shore -----
### create final outputs for 3nm zone:
### This version is for the 3 nm coastal zone cells...
rgn_cell_lookup_3nm <- extract_cell_id_per_region(reload = FALSE, 
                                                  ogr_location = file.path(dir_M, 'git-annex/Global/NCEAS-Regions_v2014/data'),
                                                  rgn_layer = 'rgn_offshore3nm_gcs')
### | sp_id | loiczid | proportionArea | csq | cell_area
### saves lookup table to git-annex/globalprep/spp_ico/rgns/cellID_rgn_offshore3nm_gcs_global.csv

sum_by_rgn_3nm_nobirds <- get_means_per_rgn(sum_by_loiczid_nobirds, rgn_cell_lookup_3nm, rgn_note = '3nm_nobirds')

spp_status_3nm_nobirds <- sum_by_rgn_3nm_nobirds %>%
  dplyr::select(rgn_id, score = status)
spp_trend_3nm_nobirds <- sum_by_rgn_3nm_nobirds %>%
  dplyr::select(rgn_id, score = rgn_mean_trend)
write_csv(spp_status_3nm_nobirds, file.path(dir_git, scenario, 'output/spp_status_3nm_nobirds.csv'))
write_csv(spp_trend_3nm_nobirds,  file.path(dir_git, scenario, 'output/spp_trend_3nm_nobirds.csv'))