source('~/github/ohiprep/src/R/common.R')

library(ggplot2)
library(data.table)

goal     <- 'globalprep/spp_ico'
scenario <- 'v2016'
dir_anx  <- file.path(dir_M, 'git-annex', goal) 
dir_data_am    <- file.path(dir_M, 'git-annex/globalprep/_raw_data', 'aquamaps/d2015') 
dir_git  <- file.path('~/github/ohiprep', goal)

source(file.path(dir_git, scenario, 'spp_fxn.R'))

status_2016 <- read_csv(file.path(dir_git, scenario, 'output/spp_status_global.csv'))
status_2015 <- read_csv(file.path(dir_git, 'v2015', 'data/spp_status_global.csv'))
status_2015_from_layers <- read_csv(file.path(dir_git, scenario, 'spp_status_global_2015_from_layers.csv'))
setdiff(status_2015, status_2015_from_layers)

status_df <- status_2016 %>% rename(st_2016 = score) %>%
  full_join(status_2015 %>% rename(st_2015 = score), by = 'rgn_id') %>%
  mutate(diff = st_2016 - st_2015) %>%
  arrange(desc(diff))

st_diff_hi <- status_df[1:20, ]
st_diff_lo <- status_df %>%
  arrange(diff) %>%
  .[1:20, ]

ggplot(st_diff_hi, aes(x = st_2015, y = st_2016)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = 'red') +
  scale_x_continuous(limits = c(0, 1)) + 
  scale_y_continuous(limits = c(0, 1))

### get species lists for these top 20 score shifts (for 2016 only) and see how the mean scores add up
### get cell IDs for each of these top 20 countries; then recreate species/cell lists for each country
### for 2015 species, use 2014 Aquamaps and IUCN intersections_old data to approximate country scores

rgn_spp_gl <- read_csv(file.path(dir_git, scenario, 'output/rgn_spp_gl.csv'))
pop_cat    <- data.frame(pop_cat   = c("LC", "NT", "VU", "EN", "CR", "EX"), 
                         cat_score = c(   0,  0.2,  0.4,  0.6,  0.8,   1))
diff_hi_spp <- st_diff_hi %>% 
  left_join(rgn_spp_gl, by = 'rgn_id') %>%
  left_join(pop_cat,    by = 'pop_cat')

# diff_lo_spp <- st_diff_lo %>% 
#   left_join(rgn_spp_gl, by = 'rgn_id') %>%
#   left_join(pop_cat,    by = 'pop_cat')

unique(diff_hi_spp$rgn_name) ### mostly islands - all top 12
# unique(diff_lo_spp$rgn_name) ### mostly middle east/mediterranean
### so seems to be consistent shifts by region... OK

st_diff_sum <- diff_hi_spp %>%
  group_by(rgn_id, st_2015, st_2016) %>% 
  summarize(mean_cat_2016 = sum(n_cells*cat_score)/sum(n_cells),
            st_2016a = (.75 - mean_cat_2016)/.75)

### 2015 IUCN cell file from 2014 IUCN data
am_cs15_file <- file.path(dir_data_am, 'csv/hcaf_sp_native_trunc_2014.csv')
am_cs16_file <- file.path(dir_data_am, 'csv/hcaf_sp_native_trunc.csv')

rgn_cells <- extract_cell_id_per_region(reload = FALSE) # %>%
# filter(rgn_id %in% st_diff_hi$rgn_id)

am_cs15 <- read_csv(am_cs15_file, col_types = 'ccd') %>%
  #   filter(loiczid %in% rgn_cells$loiczid) %>%
  rename(am_sid = speciesid)

am_cs16 <- read_csv(am_cs16_file, col_types = 'cdd') %>%
  #   filter(loiczid %in% rgn_cells$loiczid) %>%
  rename(am_sid = speciesid)

spp_all <- read_csv(file.path(dir_anx, scenario, 'int/spp_all_cleaned.csv'))

# am_cs15a <- am_cs15 %>%
#   left_join(spp_all %>%
#               filter(str_detect(spatial_source, 'am')),
#             by = 'am_sid') %>%
#   select(am_sid, loiczid, pop_cat, cat_score, trend_score) %>%
#   filter(!is.na(cat_score)) %>%
#   unique()
# 
# am_cs15aa <- am_cs15a %>%
#   group_by(loiczid) %>%
#   summarize(weighted_mean_cat = mean(cat_score),
#             weighted_mean_trend = mean(trend_score, na.rm = TRUE),
#             n_spp = n())
# 
# am_cs16a <- am_cs16 %>%
#   left_join(spp_all %>%
#               filter(str_detect(spatial_source, 'am')),
#             by = 'am_sid') %>%
#   select(am_sid, loiczid, pop_cat, cat_score, trend_score) %>%
#   filter(!is.na(cat_score)) %>%
#   unique()
# 
# am_cs16aa <- am_cs16a %>%
#   group_by(loiczid) %>%
#   summarize(weighted_mean_cat = mean(cat_score),
#             weighted_mean_trend = mean(trend_score, na.rm = TRUE),
#             n_spp = n())

am_cs15b <- process_am_summary_per_cell(spp_all, spp_cells = am_cs15, prob_filter = .40, fn_tag = 'am_testing15_40')
am_cs15b0 <- process_am_summary_per_cell(spp_all, spp_cells = am_cs15, prob_filter = 0,  fn_tag = 'am_testing15_0')

am_cs16b <- process_am_summary_per_cell(spp_all, spp_cells = am_cs16, prob_filter = 0, fn_tag = 'am_testing16')

am_2015 <- process_means_per_rgn(am_cs15b %>%
                                   rename(weighted_mean_cat   = mean_cat_score,
                                          weighted_mean_trend = mean_pop_trend_score), 
                                 rgn_cells, rgn_note = 'am_testing15_40') 
am_2015_0 <- process_means_per_rgn(am_cs15b0 %>%
                                   rename(weighted_mean_cat   = mean_cat_score,
                                          weighted_mean_trend = mean_pop_trend_score), 
                                 rgn_cells, rgn_note = 'am_testing15_0') 
am_2016 <- process_means_per_rgn(am_cs16b %>%
                                   rename(weighted_mean_cat   = mean_cat_score,
                                          weighted_mean_trend = mean_pop_trend_score),
                                 rgn_cells, rgn_note = 'am_testing16') 

am_1516 <- am_2015 %>%
  dplyr::select(rgn_id, st_2015 = status) %>%
  left_join(am_2016 %>%
              dplyr::select(rgn_id, st_2016 = status),
            by = 'rgn_id') %>%
  left_join(am_2015_0 %>%
              dplyr::select(rgn_id, st_2015_0 = status),
            by = 'rgn_id') %>%
  mutate(diff = st_2016 - st_2015) %>%
  left_join(rgn_spp_gl %>% 
              dplyr::select(rgn_id, rgn_name) %>% 
              unique(), 
            by = 'rgn_id')

am <- ggplot(am_1516) +
  geom_point(aes(x = st_2015, y = st_2016),
             col = 'blue', alpha = .8) +
  geom_abline(slope = 1, intercept = 0, col = 'red', alpha = .5) +
  scale_x_continuous(limits = c(.5, 1)) +  
  scale_y_continuous(limits = c(.5, 1))

am_th <- am +
  geom_point(data = am_1516, 
             aes(x = st_2015_0, y = st_2016), 
             col = 'green', alpha = .4) +
  labs(title = 'AM 2015 vs 2016, 0% vs 40%',
       x = 'SPP status v2015 (2014 data)',
       y = 'SPP status v2016 (2015 data)')



x

# ggsave(file.path(dir_git, scenario, 'Figs/both15v15_update.png'), height = 3, width = 5)
