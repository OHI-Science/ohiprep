### plot scores for spp

spp_all_d14v16 <- read_csv(file.path(dir_anx, 'v2015', 'int/spp_all.csv')) %>%
  rename(cat_score = category_score)

### AquaMaps: take original cells-to-species file (fields: SpeciesID, 
###   CsquareCode, probability), merge with csquarecode-to-loiczid using
###   data.table x[y] merging; then ditch csquarecode.  Resulting
###   fields: am_sid, loiczid, probability
am_cells <- read_csv(file.path(dir_data_am, '../d2014/tables/hcaf.csv')) %>%
  setnames(tolower(names(.))) %>%
  select(csquarecode, loiczid) %>%
  data.table(key = 'csquarecode')
acs_d14_raw <- read_csv(file.path(dir_data_am, '../d2014/tables/ohi_hcaf_species_native.csv'),
                        col_types = '_ccd__') %>%
  setnames(tolower(names(.))) %>%
  rename(am_sid = speciesid)
acs_d14 <- data.table(am_cells, key = 'csquarecode')[data.table(acs_d14_raw, key = 'csquarecode')] %>%
  select(-csquarecode) %>%
  as.data.frame()

rm('am_cells', 'acs_d14_raw') ### clean house a bit

### IUCN: v2015/int/iucn_cells_spp.csv is the bound species-to-cells, with 
###   id_no NA for many species (no id_no from polygons).
###   Bind this with iucn_sid-to-sciname matches from the species info list.
###   Resulting fields: iucn_sid, sciname, loiczid, presence, propArea
ics_d14_raw <- read_csv(file.path(dir_anx, 'v2015', 'int/iucn_cells_spp.csv'), col_types = 'cdddd') %>%
  rename(iucn_sid = id_no)
### the raw data contains many species with no id_no value from the polygons.  Fix that!: 
ics_d14 <- ics_d14_raw %>%
  # select(iucn_sid, sciname) %>%
  filter(!is.na(iucn_sid)) %>%      ### This first subset contains id_no (now iucn_sid) from polygons
  bind_rows(ics_d14_raw %>%         ### This next subset is missing id_no (iucn_sid); join to iucn_sid by sciname
              filter(is.na(iucn_sid)) %>%
              select(-iucn_sid) %>%
              left_join(spp_all_d14v16 %>%
                          select(iucn_sid, sciname),
                        by = 'sciname')) %>%
  unique()
rm('ics_d14_raw') ### cleaning house


acs_sum_d14v16 <- process_am_summary_per_cell(spp_all_d14v16,
                                              spp_cells = acs_d14,
                                              fn_tag = '_d14v16', 
                                              prob_filter = 0.4, reload = FALSE) %>%
  read_csv(col_types = 'dddddc')

ics_sum_d14v16 <- process_iucn_summary_per_cell(spp_all_d14v16, 
                                                spp_cells = ics_d14,
                                                fn_tag = '_d14v16', reload = FALSE) %>%
  read_csv(col_types = 'dddddc')

sum_cell_d14v16 <- process_means_per_cell(acs_sum_d14v16, ics_sum_d14v16, fn_tag = '_d14v16') %>%
  read_csv(col_types = 'ddddd')

sum_rgn_d14v16  <- get_means_per_rgn(sum_cell_d14v16, rgn_cell_lookup, rgn_note = 'gl_d14v16')

### Create final outputs:
spp_status_d14v16 <- sum_rgn_d14v16 %>%
  dplyr::select(rgn_id, score = status)
spp_trend_d14v16 <- sum_rgn_d14v16 %>%
  dplyr::select(rgn_id, score = rgn_mean_trend)
write_csv(spp_status_d14v16, file.path(dir_git, scenario, 'output/spp_status_global_d14v16.csv'))
write_csv(spp_trend_d14v16,  file.path(dir_git, scenario, 'output/spp_trend_global_d14v16.csv'))


library(ggplot2)
status_2016   <- read_csv(file.path(dir_git, 'v2016', 'output/spp_status_global.csv'))
trend_2016    <- read_csv(file.path(dir_git, 'v2016', 'output/spp_trend_global.csv'))
status_2015   <- read_csv(file.path(dir_git, 'v2015', 'data/spp_status_global.csv'))
trend_2015    <- read_csv(file.path(dir_git, 'v2015', 'data/spp_trend_global.csv'))
status_d14v16 <- read_csv(file.path(dir_git, 'v2016', 'output/spp_status_global_d14v16.csv'))
trend_d14v16  <- read_csv(file.path(dir_git, 'v2016', 'output/spp_trend_global_d14v16.csv'))

rgn_cell_lookup <- extract_cell_id_per_region(reload = FALSE)

status_df <- status_2016 %>% rename(st_2016 = score) %>%
  full_join(status_2015 %>% rename(st_2015 = score), by = 'rgn_id') %>%
  full_join(trend_2016 %>% rename(tr_2016 = score), by = 'rgn_id') %>%
  full_join(trend_2015 %>% rename(tr_2015 = score), by = 'rgn_id') %>%
  full_join(status_d14v16 %>% rename(st_d14v16 = score), by = 'rgn_id') %>%
  full_join(trend_d14v16 %>% rename(tr_d14v16 = score), by = 'rgn_id') %>%
  full_join(rgn_cell_lookup %>% select(rgn_id, rgn_name) %>% unique(), by = 'rgn_id')

st_2016_2015 <- ggplot(status_df %>%
                         select(rgn_id, rgn_name, st_2016, v2015 = st_2015, v2016 = st_d14v16) %>%
                         gather(method, st_2015, c(v2016, v2015)), 
                       aes(x = st_2015, y = st_2016, color = method, key = rgn_name)) + 
  geom_point(alpha = .5) +
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  scale_x_continuous(limits = c(.5, 1)) +
  scale_y_continuous(limits = c(.5, 1)) +
  labs(x = 'Status: old data (d2014)',
       y = 'Status: new data (d2015)',
       title = 'SPP Status: v2015 vs v2016',
       color = 'Method for old data')

# library(plotly)
# print(ggplotly(st_2016_2015))
# print(st_2016_2015)
ggsave(file.path(dir_git, scenario, 'Figs/scatterplot_spp_status_global_d15v16_d14v15.png'),
       plot = st_2016_2015)

z <- read_csv(file.path(dir_git, scenario, 'output/rgn_spp_gl.csv')) %>%
  filter(pop_cat != 'DD' & !is.na(spatial_source)) %>%
  select(-sciname, -pop_cat, -spatial_source) %>%
  distinct() %>%
  group_by(rgn_id) %>%
  mutate(n_spp_rgn_valid = n())
zz <- z %>% 
  select(rgn_id, n_spp = n_spp_rgn_valid) %>%
  distinct()

st_d14v15_d14v16 <- ggplot(status_df %>%
                             left_join(zz, by = 'rgn_id'), 
                           aes(x = st_d14v16, y = st_2015, key = rgn_name, color = n_spp)) + 
  geom_point(alpha = .5) +
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  scale_x_continuous(limits = c(.5, 1)) +
  scale_y_continuous(limits = c(.5, 1)) +
  scale_color_gradient2(low = 'red', mid = 'blue', high = 'blue', midpoint = 2500) +
  labs(x = 'Status: old data, new method',
       y = 'Status: old data, old method',
       title = 'Status: old (d2014) data by new (v2016) and old (v2015) methods')
# library(plotly)
# print(ggplotly(st_d14v15_d14v16))
# print(st_2016_d14v16)
ggsave(file.path(dir_git, scenario, 'Figs/scatterplot_spp_status_global_d14v15_d14v16.png'),
       plot = st_d14v15_d14v16)

# tr_2016v2015 <- ggplot(status_df, aes(x = tr_2015, y = tr_2016)) + 
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0, color = 'red') +
#   scale_x_continuous(limits = c(-1, 1)) +
#   scale_y_continuous(limits = c(-1, 1)) +
#   labs(title = 'Trend: 2016 vs 2015')

# print(tr_2016v2015)

# x <- read_csv(file.path(dir_git, scenario, 'debug/spp_gp_ranges.csv')) %>%
#   mutate(label = ifelse(spp_group == 'BOTW', 'BIRDS!', 'not birds'))
# 
# y <- ggplot(x, aes(x = total_cells, y = gp_area_cat, color = label)) + 
#   geom_point(size = 3, alpha = .8) + 
#   labs(x = 'total area (# of cells)', 
#        y = 'mean IUCN risk (0 = LC, 100 = EX)', 
#        color = 'SPP group')
# y
# 
# ggsave(file.path(dir_git, scenario, 'debug/birds_health.png'))