### Summarize mean category and trend per region -----
##############################################################################=
### create final outputs for HS zone:
### This version is for the high seas cells...
rgn_cell_lookup_hs <- extract_cell_id_per_region(reload = FALSE, rgn_layer = 'regions_gcs', ohi_type = 'hs')
### | sp_id | loiczid | proportionArea | csq | cell_area
### saves lookup table to git-annex/globalprep/spp_ico/rgns/cellID_region_gcs_HS.csv

sum_by_loiczid  <- read_csv(sum_by_loiczid_file,
                            col_types = 'ddddd')  ### filename from top-level script
### This returns dataframe with variables:
### loiczid | weighted_mean_cat | weighted_mean_trend | n_cat_spp | n_tr_spp

sum_by_rgn_hs <- get_means_per_rgn(sum_by_loiczid, rgn_cell_lookup_hs, rgn_note = 'hs')

if(!exists('sum_by_rgn_hs')) 
  sum_by_rgn_hs <- read.csv(file.path(dir_goal, 'summary/rgn_summary_hs.csv'))
spp_status_hs <- sum_by_rgn_hs %>%
  dplyr::select(rgn_id, score = status)
spp_trend_hs <- sum_by_rgn_hs %>%
  dplyr::select(rgn_id, score = rgn_mean_trend)
write_csv(spp_status_hs, file.path(dir_goal, 'output/spp_status_hs.csv'))
write_csv(spp_trend_hs,  file.path(dir_goal, 'output/spp_trend_hs.csv'))


### create final outputs for AQ zone:
### This version is for the Antarctic cells...
rgn_cell_lookup_aq <- extract_cell_id_per_region(reload = FALSE, rgn_layer = 'regions_gcs', ohi_type = 'aq')
### | sp_id | loiczid | proportionArea | csq | cell_area
### saves lookup table to git-annex/globalprep/spp_ico/rgns/cellID_region_gcs_AQ.csv

sum_by_rgn_aq <- get_means_per_rgn(sum_by_loiczid, rgn_cell_lookup_aq, rgn_note = 'aq')

if(!exists('sum_by_rgn_aq')) 
  sum_by_rgn_aq <- read.csv(file.path(dir_goal, 'summary/rgn_summary_aq.csv'))
spp_status_aq <- sum_by_rgn_aq %>%
  dplyr::select(rgn_id, score = status)
spp_trend_aq <- sum_by_rgn_aq %>%
  dplyr::select(rgn_id, score = rgn_mean_trend)
write_csv(spp_status_aq, file.path(dir_goal, 'output/spp_status_aq.csv'))
write_csv(spp_trend_aq,  file.path(dir_goal, 'output/spp_trend_aq.csv'))
