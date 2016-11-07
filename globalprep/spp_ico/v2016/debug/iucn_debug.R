source('~/github/ohiprep/src/R/common.R')

library(ggplot2)
library(data.table)

goal     <- 'globalprep/spp_ico'
scenario <- 'v2016'
dir_anx  <- file.path(dir_M, 'git-annex', goal) 
dir_data_am    <- file.path(dir_M, 'git-annex/globalprep/_raw_data', 'aquamaps/d2015') 
dir_data_iucn  <- file.path(dir_M, 'git-annex/globalprep/_raw_data', 'iucn_spp') 
dir_data_bird  <- file.path(dir_M, 'git-annex/globalprep/_raw_data', 'birdlife_intl') 
dir_git  <- file.path('~/github/ohiprep', goal)

source(file.path(dir_git, scenario, 'spp_fxn.R'))

status_2016 <- read_csv(file.path(dir_git, scenario, 'output/spp_status_global.csv'))
trend_2016  <- read_csv(file.path(dir_git, scenario, 'output/spp_trend_global.csv'))
# status_2016 <- read_csv(file.path(dir_git, scenario, 'output/spp_status_global_40.csv'))
# trend_2016  <- read_csv(file.path(dir_git, scenario, 'output/spp_trend_global_40.csv'))
# status_2016 <- read_csv(file.path(dir_git, scenario, 'output/spp_status_global_40_nobirds.csv'))
# trend_2016  <- read_csv(file.path(dir_git, scenario, 'output/spp_trend_global_40_nobirds.csv'))
status_2015 <- read_csv(file.path(dir_git, 'v2015', 'data/spp_status_global.csv'))
trend_2015  <- read_csv(file.path(dir_git, 'v2015', 'data/spp_trend_global.csv'))

status_df <- status_2016 %>% rename(st_2016 = score) %>%
  full_join(status_2015 %>% rename(st_2015 = score), by = 'rgn_id') %>%
  full_join(trend_2016 %>% rename(tr_2016 = score), by = 'rgn_id') %>%
  full_join(trend_2015 %>% rename(tr_2015 = score), by = 'rgn_id') %>%
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
iucn_cs15_file <- file.path(dir_data_iucn, 'iucn_cells_d2015.csv')
iucn_cs16_file <- file.path(dir_data_iucn, 'iucn_cells_d2016.csv')

rgn_cells_hi <- extract_cell_id_per_region(reload = FALSE) # %>%
#   filter(rgn_id %in% st_diff_hi$rgn_id)

iucn_cs15_all <- read_csv(iucn_cs15_file, col_types = 'cdddd') 
iucn_cs16_all <- read_csv(iucn_cs16_file, col_types = 'cddddc') 

iucn_cs15 <- iucn_cs15_all %>%
#  filter(loiczid %in% rgn_cells_hi$loiczid) %>%
  rename(iucn_sid = id_no)

iucn_cs16 <- iucn_cs16_all # %>%
#   filter(loiczid %in% rgn_cells_hi$loiczid)

spp_all <- read_csv(file.path(dir_anx, scenario, 'int/spp_all_cleaned.csv'))

iucn_cs15b <- process_iucn_summary_per_cell(spp_all, spp_cells = iucn_cs15, fn_tag = 'iucn_testing15') %>%
  rename(weighted_mean_cat   = mean_cat_score,
         weighted_mean_trend = mean_pop_trend_score)

iucn_cs16b <- process_iucn_summary_per_cell(spp_all, spp_cells = iucn_cs16, fn_tag = 'iucn_testing16')

iucn2015 <- process_means_per_rgn(iucn_cs15b %>%
                                    rename(weighted_mean_cat   = mean_cat_score,
                                           weighted_mean_trend = mean_pop_trend_score),
                                  rgn_cells_hi, rgn_note = 'iucn_testing15') 
iucn2016 <- process_means_per_rgn(iucn_cs16b %>%
                                    rename(weighted_mean_cat   = mean_cat_score,
                                           weighted_mean_trend = mean_pop_trend_score),
                                  rgn_cells_hi, rgn_note = 'iucn_testing16') 


iucn_1516 <- iucn2015c %>%
  dplyr::select(rgn_id, st_2015 = status) %>%
  left_join(iucn2016c %>%
              dplyr::select(rgn_id, st_2016 = status),
            by = 'rgn_id') %>%
  mutate(diff = st_2016 - st_2015) %>%
  left_join(rgn_spp_gl %>% 
              dplyr::select(rgn_id, rgn_name) %>% 
              unique(), 
            by = 'rgn_id')
  

ggplot(iucn_1516, aes(x = st_2015, y = st_2016)) +
  geom_point(col = 'blue') +
  geom_abline(slope = 1, intercept = 0, col = 'red') +
  scale_x_continuous(limits = c(0, 1)) +  
  scale_y_continuous(limits = c(0, 1))

