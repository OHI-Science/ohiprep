source('~/github/ohiprep/src/R/common.R')

library(ggplot2)
library(readr)
library(data.table)
library(plotly) # install.packages('plotly')


goal     <- 'globalprep/spp_ico'
scenario <- 'v2016'
dir_anx  <- file.path(dir_M, 'git-annex', goal) 
dir_data_am    <- file.path(dir_M, 'git-annex/globalprep/_raw_data', 'aquamaps/d2015') 
dir_data_iucn  <- file.path(dir_M, 'git-annex/globalprep/_raw_data', 'iucn_spp') 
dir_data_bird  <- file.path(dir_M, 'git-annex/globalprep/_raw_data', 'birdlife_intl') 
dir_git  <- file.path('~/github/ohiprep', goal)

source(file.path(dir_git, scenario, 'spp_fxn.R'))

status_2016 <- read_csv(file.path(dir_git, scenario, 'output/spp_status_global.csv'))
status_2015 <- read_csv(file.path(dir_git, 'v2015', 'data/spp_status_global.csv'))
status_2015_from_layers <- read_csv(file.path(dir_git, scenario, 'debug/spp_status_global_2015_from_layers.csv'))
setdiff(status_2015, status_2015_from_layers)

status_df <- status_2016 %>% rename(st_2016 = score) %>%
  full_join(status_2015 %>% rename(st_2015 = score), by = 'rgn_id') %>%
  mutate(diff = st_2016 - st_2015) %>%
  arrange(desc(diff))

ggplot(status_df, aes(x = st_2015, y = st_2016)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = 'red') +
  scale_x_continuous(limits = c(0, 1)) + 
  scale_y_continuous(limits = c(0, 1))


##### AquaMaps #####

rgn_spp_gl <- read_csv(file.path(dir_git, scenario, 'output/rgn_spp_gl.csv'))

rgn_cells <- extract_cell_id_per_region(reload = FALSE) # %>%
# filter(rgn_id %in% st_diff_hi$rgn_id)

spp_all <- read_csv(file.path(dir_anx, scenario, 'int/spp_all_cleaned.csv'))

### 2015 IUCN cell file from 2014 IUCN data
am_cs15_file <- file.path(dir_data_am, 'csv/hcaf_sp_native_trunc_2014.csv')
am_cs16_file <- file.path(dir_data_am, 'csv/hcaf_sp_native_trunc.csv')

am_cs15 <- read_csv(am_cs15_file, col_types = 'ccd') %>%
  #   filter(loiczid %in% rgn_cells$loiczid) %>%
  rename(am_sid = speciesid)

am_cs16 <- read_csv(am_cs16_file, col_types = 'cdd') %>%
  #   filter(loiczid %in% rgn_cells$loiczid) %>%
  rename(am_sid = speciesid)

am_cs15b  <- process_am_summary_per_cell(spp_all, 
                                         #spp_cells = am_cs15,  
                                         prob_filter = .40, 
                                         fn_tag = 'am_posttest15_40')
am_cs15b0 <- process_am_summary_per_cell(spp_all, 
                                         #spp_cells = am_cs15,  
                                         prob_filter = 0,   
                                         fn_tag = 'am_posttest15_0')
am_cs16b  <- process_am_summary_per_cell(spp_all, 
                                         #spp_cells = am_cs16,  
                                         prob_filter = 0,   
                                         fn_tag = 'am_posttest16')


##### IUCN #####

iucn_cs15_file <- file.path(dir_anx, 'v2015', 'int/iucn_cells_spp.csv')
iucn_cs16_file <- file.path(dir_anx, scenario, 'int/iucn_cells_spp.csv')

iucn_cs15 <- read_csv(iucn_cs15_file, col_types = 'cdddd') %>%
  rename(iucn_sid = id_no)
iucn_cs16 <- read_csv(iucn_cs16_file, col_types = 'cddddc') 

iucn_cs15b <- process_iucn_summary_per_cell(spp_all, 
                                            spp_cells = iucn_cs15, 
                                            fn_tag = 'iucn_posttest15')
iucn_cs16b <- process_iucn_summary_per_cell(spp_all, 
                                            spp_cells = iucn_cs16, 
                                            fn_tag = 'iucn_posttest16')


##### BOTH #####

both_cs15   <- process_means_per_cell(am_cs15b,  iucn_cs15b, 
                                      fn_tag = 'both_posttest15')
both_cs15_0 <- process_means_per_cell(am_cs15b0, iucn_cs15b,
                                      fn_tag = 'both_posttest15_0')
both_cs16   <- process_means_per_cell(am_cs16b,  iucn_cs16b, 
                                      fn_tag = 'both_posttest16')

both_2015   <- process_means_per_rgn(both_cs15,   rgn_cells,
                                       rgn_note = 'both_posttest15')
both_2015_0 <- process_means_per_rgn(both_cs15_0, rgn_cells,
                                       rgn_note = 'both_posttest15_0')
both_2016   <- process_means_per_rgn(both_cs16,   rgn_cells,
                                       rgn_note = 'both_posttest16')


##### CONSOLIDATE for plotting #####

both_1516 <- both_2015 %>%
  dplyr::select(rgn_id, st_2015 = status) %>%
  left_join(both_2016 %>%
              dplyr::select(rgn_id, st_2016 = status),
            by = 'rgn_id') %>%
  left_join(both_2015_0 %>%
              dplyr::select(rgn_id, st_2015_0 = status),
            by = 'rgn_id') %>%
  mutate(diff = st_2016 - st_2015,
         diff15 = st_2015 - st_2015_0) %>%
  left_join(rgn_spp_gl %>% 
              dplyr::select(rgn_id, rgn_name, n_spp_rgn) %>% 
              unique(), 
            by = 'rgn_id') %>%
  left_join(status_df %>%
              dplyr::select(rgn_id, st_2015_old = st_2015),
            by = 'rgn_id')


both <- ggplot(both_1516) +
  geom_point(aes(x = st_2015_old, y = st_2016, key = 'rgn_name'),
             col = 'grey30', alpha = .5) +
  geom_point(aes(x = st_2015, y = st_2016),
             col = 'blue', alpha = .8) +
  geom_point(aes(x = st_2015_0, y = st_2016),
             col = 'green', alpha = .5) +
  geom_abline(slope = 1, intercept = 0, col = 'red', alpha = .5) +
  scale_x_continuous(limits = c(.5, 1)) +  
  scale_y_continuous(limits = c(.5, 1)) +
  labs(title = 'IUCN + AquaMaps 2015 vs 2016',
       x = 'SPP status v2015 (2014 data)',
       y = 'SPP status v2016 (2015 data)')

ggplotly(both)

both_15only <- ggplot(both_1516) +
  geom_point(aes(x = st_2015_old, y = st_2015, 
                 col = n_spp_rgn,
                 key = rgn_name),
             alpha = .5) +
  geom_abline(slope = 1, intercept = 0, col = 'red', alpha = .5) +
  scale_x_continuous(limits = c(.5, 1)) +  
  scale_y_continuous(limits = c(.5, 1)) +
  scale_colour_gradient(low = "green", high = "red") +
  labs(title = 'IUCN + AquaMaps 2015 old vs new',
       x = 'SPP status v2015 (2014 data)',
       y = 'SPP status v2015 updated (2014 data)')


ggplotly(both_15only)

# ggsave(file.path(dir_git, scenario, 'Figs/both15v15_update.png'), height = 3, width = 5)


##### Pick out odd regions #####

### Finland: 85 spp; old: 87 -> 79.
### Bouvet Island: 137 spp; 81 -> 100!
x <- rgn_spp_gl %>%
  filter(rgn_name %in% c('Bouvet Island', 'Finland')) %>%
  dplyr::select(rgn_name, rgn_id, am_sid, iucn_sid, sciname, 
                pop_cat, n_cells, spatial_source)

y <- spp_all %>%
  filter((!is.na(iucn_sid) & iucn_sid %in% x$iucn_sid) | 
           (!is.na(am_sid)   & am_sid %in% x$am_sid))
### Very few IUCN species mapped here.  That is unlikely?

z <- read_csv(file.path(dir_anx, scenario, 'int/spp_iucn_maps_all.csv')) %>%
  filter(id_no %in% x$iucn_sid)
### None of these id_no match up with IUCN ids that are listed as found in these two regions.  Unlikely?


am_match   <- read_csv(file.path(dir_anx, scenario, 'int/namecheck_am.csv')) %>%
  filter((!is.na(am_sid)   & am_sid %in% x$am_sid))
iucn_match <- read_csv(file.path(dir_anx, scenario, 'int/namecheck_iucn.csv')) %>%
  filter((!is.na(iucn_sid) & iucn_sid %in% x$iucn_sid))

##### spp cell to region, without filtering - which species exist in these two regions? #####

rgn_cells_test <- rgn_cells %>%
  filter(rgn_id %in% x$rgn_id)
iucn_cells_test <- iucn_cs16 %>%
  filter(loiczid %in% rgn_cells_test$loiczid)
am_cells_test <- am_cs16 %>%
  filter(loiczid %in% rgn_cells_test$loiczid)
iucn_spp_test <- iucn_cells_test %>%
  dplyr::select(sciname, iucn_sid) %>%
  unique()
am_spp_test <- am_cells_test %>%
  dplyr::select(am_sid) %>%
  unique()

yy <- spp_all %>%
  filter(iucn_sid %in% iucn_spp_test$iucn_sid | 
           am_sid   %in% am_spp_test$am_sid)
yyy <- yy %>%
  dplyr::select(am_sid, am_cat, sciname, iucn_sid, pop_trend, pop_cat, 
                spp_group, iucn_subpop, id_no, spatial_source) %>%
  mutate(iucn_dropped = ifelse(str_detect(spatial_source, 'iucn') & !iucn_sid %in% x$iucn_sid, TRUE, FALSE),
         am_dropped   = ifelse(str_detect(spatial_source, 'am')   & !am_sid   %in% x$am_sid,   TRUE, FALSE))

am_dropped <- yyy %>%
  filter(am_dropped == TRUE & str_detect(spatial_source, 'am')) %>%
  filter(!is.na(pop_cat) & pop_cat != 'DD') %>%
  dplyr::select(-spp_group, -id_no, -iucn_subpop)
### NONE - AquaMaps is good?


iucn_dropped <- yyy %>%
  filter(iucn_dropped == TRUE & str_detect(spatial_source, 'iucn')) %>%
  filter(!is.na(pop_cat) & pop_cat != 'DD')

write_csv(iucn_dropped, file.path(dir_anx, scenario, 'int/dropped_spp_iucn.csv'))




##### testing the bug #####
library(dplyr)
library(tidyr)
library(readr)
library(data.table)

dir_debug <- '~/github/ohiprep/globalprep/spp_ico/v2016/debug'

# test_df <- iucn_cells_spp %>% sample_frac(.05)
# write_csv(test_df, file.path(dir_debug, 'test_df.csv'))

test_df  <- read_csv(file.path(dir_debug, 'test_df.csv'), col_types = 'cdd')
# test_df <- test_df %>% sample_frac(.01)
dt_keyed <- data.table(test_df, key = "loiczid") 

# rgn_df   <- rgn_cell_lookup %>%
#   filter(loiczid %in% test_df$loiczid)
# write_csv(rgn_df, file.path(dir_debug, 'rgn_df.csv'))
rgn_df <- read_csv(file.path(dir_debug, 'rgn_df.csv'), col_types = 'dcd')
# rgn_df <- rgn_df %>% filter(loiczid %in% test_df$loiczid)
rgn_dt_keyed <- data.table(rgn_df, key = "loiczid") 

### create merged data table using x[y] function
result_dt <- dt_keyed[rgn_dt_keyed] 
length(unique(result_dt$iucn_sid))
# [1] 1853
class(result_dt)

### using unique()

result_dt1 <- result_dt %>%
  unique()

length(unique(result_dt1$iucn_sid))
# [1] 1769

dropped <- result_dt %>%
  filter(!iucn_sid %in% result_dt1$iucn_sid)

### using distinct()
result_dt2 <- result_dt %>% 
  distinct()

length(unique(result_dt2$iucn_sid))
# [1] 1769

### converting data.table back to data.frame
result_df3 <- result_dt %>% 
  as.data.frame() %>%
  unique()
length(unique(result_df3$iucn_sid))
# [1] 1853

### using unique( , by = 'X') seems to work...
result_dt4 <- result_dt %>% 
  unique(by = names(result_dt))

length(unique(result_dt4$iucn_sid))
# [1] 1853

### was the old method simply using the first column to find unique values?
length(unique(result_dt1$loiczid))
length(unique(result_dt2$loiczid))
length(unique(result_df3$loiczid))
length(unique(result_dt4$loiczid))
### yep.

key(result_df)
# [1] "loiczid"
### makes sense I suppose.

### repeat original, but remove key from data table first.
result_dt5 <- result_dt %>%
  setkey(NULL) %>%
  unique()
length(unique(result_dt5$iucn_sid))
# [1] 1853
