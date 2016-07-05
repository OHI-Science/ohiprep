### Summarize mean category and trend per region within 3 nm of shore -----
### create final outputs for 3nm zone:
### This version is for the 3 nm coastal zone cells...
rgn_cell_lookup_3nm <- extract_cell_id_per_region(reload = FALSE, 
                                                  ogr_location = file.path(dir_M, 'git-annex/Global/NCEAS-Regions_v2014/data'),
                                                  rgn_layer = 'rgn_offshore3nm_gcs')
### | sp_id | loiczid | proportionArea | csq | cell_area
### saves lookup table to git-annex/globalprep/spp_ico/rgns/cellID_rgn_offshore3nm_gcs_global.csv

sum_by_loiczid  <- read_csv(sum_by_loiczid_file,
                            col_types = 'ddddd') ### filename from top-level script
### This returns dataframe with variables:
### loiczid | weighted_mean_cat | weighted_mean_trend | n_cat_spp | n_tr_spp

sum_by_rgn_3nm <- get_means_per_rgn(sum_by_loiczid, rgn_cell_lookup_3nm, rgn_note = '3nm')

if(!exists('sum_by_rgn_3nm')) 
  sum_by_rgn_3nm <- read.csv(file.path(dir_git, scenario, 'summary/rgn_summary_3nm.csv'))
spp_status_3nm <- sum_by_rgn_3nm %>%
  dplyr::select(rgn_id, score = status)
spp_trend_3nm <- sum_by_rgn_3nm %>%
  dplyr::select(rgn_id, score = rgn_mean_trend)
write_csv(spp_status_3nm, file.path(dir_git, scenario, 'output/spp_status_3nm.csv'))
write_csv(spp_trend_3nm,  file.path(dir_git, scenario, 'output/spp_trend_3nm.csv'))