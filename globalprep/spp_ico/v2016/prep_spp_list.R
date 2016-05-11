### Gather species information from IUCN (non-spatial) -----
##############################################################################=
### create species list for global analysis

### set reload, source_pref, fn_tag in parent script

if(reload) {
  ### NOTE: Manually set up the subpopulations for 2016: see subpops_list.csv.
  ### For each of these species, the shapefile was modified so that the id_no
  ### field related to the subpop iucn_sid instead of the parent iucn_sid.
  
  spp_all_file <- create_spp_master_lookup(source_pref = source_pref, fn_tag = fn_tag, reload = TRUE)
  spp_all <- read_csv(spp_all_file)
  ### | am_sid | sciname | am_cat | iucn_sid | iucn_cat | pop_trend | pop_cat | 
  ### | info_source | spp_group | id_no | objectid | spatial_source | cat_score | trend_score |
  
  ### check subpops: all spp with an IUCN subpop code
  spp_all_subpops <- spp_all %>%
    filter(!is.na(iucn_subpop)) %>%
    unique()
  
  ### if there is any cleaning to be done, this is where it would go.
  
  write_csv(spp_all, file.path(dir_anx, scenario, 'int/spp_all_cleaned.csv'))
}
