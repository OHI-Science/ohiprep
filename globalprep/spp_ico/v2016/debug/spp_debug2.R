source('~/github/ohiprep/src/R/common.R')

library(ggplot2)
library(readr)
library(data.table)
library(plotly) # install.packages('plotly')


goal     <- 'globalprep/spp_ico'
scenario <- 'v2016/debug'
dir_anx  <- file.path(dir_M, 'git-annex', goal) 
dir_data_am    <- file.path(dir_M, 'git-annex/globalprep/_raw_data', 'aquamaps/d2015') 
dir_data_iucn  <- file.path(dir_M, 'git-annex/globalprep/_raw_data', 'iucn_spp/d2015') 
dir_data_bird  <- file.path(dir_M, 'git-annex/globalprep/_raw_data', 'birdlife_intl') 
dir_git  <- file.path('~/github/ohiprep', goal)


rgn_spp_gl <- read_csv(file.path(dir_git, 'v2016', 'output/rgn_spp_gl.csv'))


spp_all_15 <- read_csv(file.path(dir_anx, 'v2015', 'int/spp_all.csv'))
spp_all_16 <- read_csv(file.path(dir_anx, 'v2016', 'int/spp_all_cleaned.csv'))

### Possible causes for differences:
### * difference in how species maps are attached to species info
###   * id_no join vs name join
###   * 
### * difference in species risk categories from one year to next?
### * 
### for two large-difference regions
### * isolate the cells and the species-cell lists
### * use the v2015 process and the v2016 process to compare:
###   * which species are included
###   * 


##### IUCN #####

ics15_file <- file.path(dir_anx, 'v2015', 'int/iucn_cells_spp.csv')
ics16_file <- file.path(dir_anx, 'v2016', 'int/iucn_cells_spp.csv')

ics15_raw <- read_csv(ics15_file, col_types = 'cdddd') %>%
  rename(iucn_sid = id_no) 

ics15_all <- ics15_raw %>%
  filter(!is.na(iucn_sid)) %>%      ### This first subset contains id_no (now iucn_sid) from polygons
  bind_rows(x <- ics15_all %>%         ### This next subset is missing id_no (iucn_sid); join to iucn_sid by sciname
              filter(is.na(iucn_sid)) %>%
              select(-iucn_sid) %>%
              left_join(spp_all_15 %>%
                          filter(!is.na(category_score) & !(category_score == 'DD')) %>%
                          dplyr::select(iucn_sid, sciname, category_score, trend_score),
                        by = 'sciname')) %>%
  unique()

ics16_all <- read_csv(ics16_file, col_types = 'cddddc') 

##### Examine the raw cell files #####
# nrow(ics15_all %>% select(iucn_sid, sciname) %>% unique()) 
# # 3463 spp
# nrow(ics16_all %>% select(iucn_sid, sciname) %>% unique()) 
# # 4154 spp
# 
# spp_valid_15 <- ics15_all %>% 
#   select(iucn_sid, sciname) %>%
#   filter(!is.na(iucn_sid)) %>% 
#   filter(iucn_sid %in% spp_all_15$iucn_sid) %>% ### these are ones with ID and valid category
#   bind_rows(ics15_all %>%
#               filter(is.na(iucn_sid)) %>%
#               select(sciname) %>%
#               left_join(spp_all_15 %>% 
#                           select(iucn_sid, sciname), 
#                         by = 'sciname')) %>% ### these are ones without ID; attach with sciname
#   unique()
# # 3381 spp  
# 
# spp_valid_16 <- ics16_all %>%
#   select(iucn_sid, sciname) %>%
#   filter(iucn_sid %in% (spp_all_16 %>%
#            filter(pop_cat != 'DD' & !is.na(pop_cat)) %>%
#            .$iucn_sid)) %>% ### these are ones with ID and valid category
#   unique()
# # 4073 spp

### species per cell are increasing

spp_all_16_x <- spp_all_16 %>%
  filter(pop_cat != 'DD' & !is.na(pop_cat)) %>%
  filter(!is.na(spatial_source))
# 15567-80 loiczid with more sciname_dupes than species?
# ics15_x <- ics15_all %>%
#   filter(loiczid %in% c(15567)) #:15580))

ics15_duped_spp1 <-  ics15_all %>%
  select(-presence, -prop_area) %>%
  distinct() 
ics15_duped_spp2 <- ics15_duped_spp1 %>%
  group_by(loiczid, iucn_sid) %>%
  mutate(n_dupe_ids_15 = n() - 1)
ics15_duped_spp3 <- ics15_duped_spp2 %>%
  select(iucn_sid, loiczid, n_dupe_ids_15) %>%
  ungroup() %>%
  distinct()
ics15_duped_spp4 <- ics15_duped_spp3 %>%
  group_by(loiczid) %>%
  summarize(n_spp_15 = n(),
            n_dupe_ids_15 = sum(n_dupe_ids_15))
ics16_duped_spp1 <- ics16_all %>%
  select(-presence, -prop_area, -subpop) %>%
  filter(iucn_sid %in% spp_all_16_x$iucn_sid) %>%
  distinct() 
ics16_duped_spp2 <- ics16_duped_spp1 %>%
  group_by(loiczid, iucn_sid) %>%
  mutate(n_dupe_ids_16 = n() - 1)
ics16_duped_spp3 <- ics16_duped_spp2 %>%
  select(iucn_sid, loiczid, n_dupe_ids_16) %>%
  ungroup() %>%
  distinct()
ics16_duped_spp4 <- ics16_duped_spp3 %>%
  group_by(loiczid) %>%
  summarize(n_spp_16 = n(),
            n_dupe_ids_16 = sum(n_dupe_ids_16)) 

cell_n_compare <- ics15_duped_spp4 %>%
  left_join(ics16_duped_spp4, by = 'loiczid') %>%
  mutate(n_spp_15_adj = n_spp_15 - n_dupe_ids_15,
         n_spp_16_adj = n_spp_16 - n_dupe_ids_16)

write_csv(cell_n_compare, file.path(dir_git, 'v2016/debug/cell_compare.csv'))

cell_n_plot <- ggplot(cell_n_compare %>%
                        sample_frac(.25), 
                      aes(x = n_spp_15, y = n_spp_16, key = loiczid)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  labs(x = 'Species per cell, IUCN 2014',
       y = 'Species per cell, IUCN 2015')

cell_n_plot
ggsave(file.path(dir_git, 'v2016/debug', 'scatter_spp_per_cell.png'))

plotly::ggplotly(cell_n_plot)

### species shapefiles aren't changing.
spp_cell_compare <- ics15_all %>%
  filter(iucn_sid %in% (spp_all_16 %>%
                          filter(pop_cat != 'DD' & !is.na(pop_cat)) %>%
                          .$iucn_sid)) %>%
  group_by(iucn_sid) %>%
  summarize(n_cell_15 = n()) %>%
  inner_join(ics16_all %>%
               filter(iucn_sid %in% (spp_all_16 %>%
                                       filter(pop_cat != 'DD' & !is.na(pop_cat)) %>%
                                       .$iucn_sid)) %>%
               group_by(iucn_sid) %>%
               summarize(n_cell_16 = n()),
             by = 'iucn_sid')

spp_cell_plot <- ggplot(spp_cell_compare, 
                        aes(x = n_cell_15, y = n_cell_16, key = 'iucn_sid')) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  labs(x = 'cells per species, IUCN 2014',
       y = 'cells per species, IUCN 2015')

spp_cell_plot
ggsave(file.path(dir_git, 'v2016/debug', 'scatter_cell_per_spp.png'))

### so changes in species per cell must be due to new polygons, or
### changes in id numbers.

### New check: for each cell, how many of the species are cross-listed, e.g.
### (number of scinames) - (number of unique scinames) or same with (iucn_sid)



##### check that rgn cells are identical #####
# source(file.path(dir_git, scenario, 'spp_fxn_v2015.R'))
# rgn_cells_15 <- extract_cell_id_per_region(reload = FALSE)
# source(file.path(dir_git, scenario, '../spp_fxn.R'))
# rgn_cells_16 <- extract_cell_id_per_region(reload = FALSE)
# x <- setdiff(rgn_cells_15, rgn_cells_16)  
# x is zero-length vector

##### process using 2015 scripts and 2015 spp info table #####
source(file.path(dir_git, scenario, 'spp_fxn_v2015.R'))
rgn_cells <- extract_cell_id_per_region(reload = FALSE) %>%
  filter(rgn_id %in% c(105, 174)) ### Finland and Bouvet Island

ics15 <- ics15_all %>% filter(loiczid %in% rgn_cells$loiczid)
ics16 <- ics16_all %>% filter(loiczid %in% rgn_cells$loiczid)

ics15_15a <- process_iucn_summary_per_cell(spp_all_15, 
                                           spp_cells = ics15, 
                                           fn_tag = 'debug15_15')
ics16_15a <- process_iucn_summary_per_cell(spp_all_15, 
                                           spp_cells = ics16, 
                                           fn_tag = 'debug16_15')

head(ics15_15a)
# loiczid mean_cat_score mean_popn_trend_score n_cat_species n_trend_species source
#   34969      0.1142857            -0.2500000             7               4   iucn
#   34970      0.1142857            -0.2500000             7               4   iucn
#   34971      0.1142857            -0.2500000             7               4   iucn
#   35688      0.1000000            -0.1666667             6               3   iucn
#   35689      0.1000000            -0.1666667             6               3   iucn
#   35690      0.1000000            -0.1666667             6               3   iucn
head(ics16_15a)
# loiczid mean_cat_score mean_popn_trend_score n_cat_species n_trend_species source
#   34969     0.10000000            -0.2500000             8               4   iucn
#   34970     0.10000000            -0.2500000             8               4   iucn
#   34971     0.10000000            -0.2500000             8               4   iucn
#   35688     0.08571429            -0.1666667             7               3   iucn
#   35689     0.08571429            -0.1666667             7               3   iucn
#   35690     0.08571429            -0.1666667             7               3   iucn

# ics16_15aa <- process_iucn_summary_per_cell(spp_all_16 %>%
#                                               rename(category_score = cat_score), 
#                                            spp_cells = ics16, 
#                                            fn_tag = 'debug16_15proc_16sppall')
### 2015 process with 2016 species list - this makes a small increase in cat spp etc;
### but not likely the cause for concern...


##### process using 2016 scripts and 2016 spp info table #####

source(file.path(dir_git, 'v2016', 'spp_fxn.R'))
ics15_16a <- process_iucn_summary_per_cell(spp_all_16, 
                                           spp_cells = ics15, 
                                           fn_tag = 'debug15_16')
ics16_16a <- process_iucn_summary_per_cell(spp_all_16, 
                                           spp_cells = ics16, 
                                           fn_tag = 'debug16_16')
head(ics15_16a)
# loiczid mean_cat_score mean_pop_trend_score n_cat_species n_trend_species source
#   34969            0.3                 -0.5             2               1   iucn
#   34970            0.3                 -0.5             2               1   iucn
#   34971            0.3                 -0.5             2               1   iucn
#   35688            0.3                 -0.5             2               1   iucn
#   35689            0.3                 -0.5             2               1   iucn
#   35690            0.3                 -0.5             2               1   iucn
head(ics16_16a)
# loiczid mean_cat_score mean_pop_trend_score n_cat_species n_trend_species source
#   34969     0.06000000          -0.20491803            70              61   iucn
#   34970     0.06027397          -0.20312500            73              64   iucn
#   34971     0.05405405          -0.20769231            74              65   iucn
#   35688     0.08888889           0.07142857            18              14   iucn
#   35689     0.08888889           0.07142857            18              14   iucn
#   35690     0.04545455          -0.16666667            66              57   iucn

### Why such a big difference in n_cat_species and n_trend_species?
### * why does the n_cat_species for 2015 dataset drop so much? 
###   * not joining by name, probably: this validates that
###   * x <- process_iucn_summary_per_cell(spp_all_16, 
###                                    spp_cells = ics15 %>%
###                                      left_join(spp_valid_15 %>%
###                                                  rename(iucn_sid1 = iucn_sid),
###                                                by = 'sciname') %>%
###                                      mutate(iucn_sid = ifelse(is.na(iucn_sid), iucn_sid1, iucn_sid)) %>%
###                                      unique(),
###                                    fn_tag = 'debug15_16_noNAs')
### * why the huge numbers for n_cat_species and n_trend_species? 
###   * are the DD/NA species filtered out first?! See if this makes a difference:
###   * x <- process_iucn_summary_per_cell(spp_all_16,
###             spp_cells = ics16 %>%
###               filter(iucn_sid %in% spp_valid_16$iucn_sid),
###             fn_tag = 'debug16_16_na_rm')
###     head(x) ### NOPE! no change... WTF

### * 380 cells in 2015; 378 and 379 for 2016.
###   * 2015 process creates an NA cell - ??? why?:
###     * 2015 process in left join (by sciname) joins cells to species info list; 
###       therefore species info spp with no cells get NA loiczid. Really want an inner_join, but
###       the data.table join works more like a left_join.
###   * 2016 process drops that loiczid = NA "cell"; and since drops some 2016 spp, might drop a cell to zero
###     * x <- ics15 %>% filter(!loiczid %in% ics15_16a$loiczid)
###     * verified! one species with no id_no field from shapefile

### Could it just be that there are so many more species in these cells? wtf.
### nrow(ics15) is 4835 spp-cell pairs, while ics16 is 19482.
# checkcell <- 34969
# checkcell <- 212772

cell_15 <- ics15 %>% filter(loiczid == checkcell) %>% # n = 7 species
  select(-iucn_sid) %>%
  left_join(spp_all_15, by = 'sciname') %>%
  filter(is.na(parent_sid)) %>%
  select(sciname, iucn_sid, loiczid, spp_group, cat_score = category_score, trend_score)
cell_16 <- ics16 %>% filter(loiczid == checkcell) %>% # n = 75 species.
  select(-sciname) %>%
  left_join(spp_all_16, by = 'iucn_sid') %>%
  select(sciname, iucn_sid, loiczid, spp_group, cat_score, trend_score) %>%
  group_by(iucn_sid) %>%
  mutate(sciname = paste(sciname, collapse = ', ')) %>%
  ungroup() %>%
  unique()

write_csv(cell_15, file.path(dir_git, sprintf('v2016/debug/iucn_15_in_cell%s.csv', checkcell)))
write_csv(cell_16, file.path(dir_git, sprintf('v2016/debug/iucn_16_in_cell%s.csv', checkcell)))

### are bird ranges larger or healthier than other ranges, on average?
spp_ranges <- ics16_all %>% 
  group_by(iucn_sid) %>%
  summarize(n_cells = n()) %>%
  left_join(spp_all_16 %>%
              select(iucn_sid, spp_group, cat_score) %>%
              unique(),
            by = 'iucn_sid') %>%
  filter(!is.na(cat_score))
spp_gp_ranges <- spp_ranges %>%
  mutate(area_cat = n_cells * cat_score) %>%
  group_by(spp_group) %>%
  summarize(gp_area_cat = sum(area_cat)/sum(n_cells) * 100,
            n_spp = n(),
            total_cells = sum(n_cells)) %>%
  arrange(desc(gp_area_cat))
write_csv(spp_gp_ranges, file.path(dir_git, 'v2016/debug/spp_gp_ranges.csv'))

DT::datatable(spp_gp_ranges)

# for checkcell = 34969
# cell_lost <- cell_15 %>% filter(!iucn_sid %in% cell_16$iucn_sid)
# #           sciname iucn_sid loiczid  spp_group cat_score trend_score
# # Phocoena phocoena    17027   34969 MAMMMARINE         0          NA
# 
# cell_gain <- cell_16 %>% 
#   filter(!iucn_sid %in% cell_15$iucn_sid)
# head(cell_gain)
# #            sciname iucn_sid loiczid spp_group cat_score trend_score
# #    Alauda arvensis 22717415   34969      BOTW       0.0        -0.5
# #         Alca torda 22694852   34969      BOTW       0.2         0.5
# #         Anas acuta 22680301   34969      BOTW       0.0        -0.5
# #   Anthus pratensis 22718556   34969      BOTW       0.2        -0.5
# # Arenaria interpres 22693336   34969      BOTW       0.0        -0.5


### OK, now to plot 2015 vs 2016 scores, and color code by % of bird species...?


# ics16_rgn <- ics16_all %>%
#   select(-presence, -sciname, -prop_area, -subpop) %>%
#   left_join(rgn_cells_16, by = 'loiczid') %>%
#   unique()
# spp_ct_rgn <- ics16_rgn %>%
#   group_by(rgn_id) %>%
#   summarize(n_cells = n())

rgn_spp_gp <- rgn_spp_gl %>%
  filter(!is.na(pop_cat) & pop_cat != 'DD') %>%
  select(iucn_sid, sciname, pop_cat, spatial_source, rgn_id, rgn_name, n_cells, n_spp_rgn) 

rgn_spp_gp1 <- rgn_spp_gp %>%
  left_join(x <- spp_all_16 %>%
              filter(!is.na(pop_cat) & pop_cat != 'DD') %>%
              select(iucn_sid, spp_group) %>%
              unique(),
            by = 'iucn_sid') %>%
  mutate(spp_group = ifelse(spatial_source == 'am', 'aquamaps', spp_group)) %>%
  filter(!is.na(spp_group))

rgn_spp_gp2 <- rgn_spp_gp1 %>%
  left_join(data.frame(pop_cat  = c("LC", "NT", "VU", "EN", "CR", "EX"), 
                       cat_score = c(   0,  0.2,  0.4,  0.6,  0.8,   1))) %>%
  mutate(cat_wt = cat_score * n_cells)

rgn_spp_gp3 <- rgn_spp_gp2 %>%
  group_by(rgn_name, rgn_id, spp_group) %>%
  summarize(gp_cat   = sum(cat_wt)/sum(n_cells),
            gp_cells = sum(n_cells),
            n_spp_gp = n()) %>%
  mutate(birds = ifelse(spp_group == 'BOTW', TRUE, FALSE))

rgn_spp_gp4 <- rgn_spp_gp3 %>%
  group_by(rgn_name, rgn_id, birds) %>%
  summarize(rgn_cat = sum(gp_cat * gp_cells) / sum(gp_cells),
            rgn_cells = sum(gp_cells),
            rgn_spp   = sum(n_spp_gp)) %>%
  group_by(rgn_name, rgn_id) %>%
  mutate(n_birds = ifelse(birds, rgn_spp, 0),
         tot_spp = sum(rgn_spp),
         cat_bird = ifelse(birds, rgn_cat, 0),
         cells_bird = ifelse(birds, rgn_cells, 0),
         cat_tot = sum(rgn_cells * rgn_cat)/sum(rgn_cells),
         cells_tot = sum(rgn_cells)) %>%
  filter(birds) %>% 
  select(-birds)

rgn_spp_gp5 <- rgn_spp_gp4 %>%
  mutate(pct_n_bird = n_birds/tot_spp,
         pct_area_bird = cells_bird/cells_tot) %>%
  select(rgn_name, rgn_id, 
         n_birds, n_spp = tot_spp,
         pct_n_bird,
         pct_area_bird,
         cat_bird, cat_tot) %>%
  mutate(birds_high = (cat_bird > cat_tot))


status_2016 <- read_csv(file.path(dir_git, 'v2016', 'output/spp_status_global.csv'))
status_2015 <- read_csv(file.path(dir_git, 'v2015', 'data/spp_status_global.csv'))

status_df <- status_2016 %>% rename(st_2016 = score) %>%
  full_join(status_2015 %>% rename(st_2015 = score), by = 'rgn_id') %>%
  mutate(diff = st_2016 - st_2015) %>%
  arrange(desc(diff)) %>%
  left_join(rgn_spp_gp5, by = 'rgn_id') %>%
  mutate(st_recalc = (.75 - cat_tot)/.75)

status_plot <- ggplot(status_df, aes(x = st_2015, y = st_2016)) +
  geom_point(aes(color = n_birds)) +
  #  geom_point(aes(x = st_recalc, y = st_2016), color = 'yellow') +
  geom_abline(slope = 1, intercept = 0, col = 'red') +
  scale_x_continuous(limits = c(0.7, 1)) + 
  scale_y_continuous(limits = c(0.7, 1)) +
  scale_color_continuous(low = 'darkred', high = 'yellow') +
  labs(x = 'Status by 2015 methods/data',
       y = 'Status by 2016 methods/data')
status_plot

diff_plot <- ggplot(status_df, aes(x = pct_n_bird, y = diff)) +
  geom_point(aes(color = n_birds))

# geom_point(aes(x = st_recalc, y = st_2016), color = 'yellow') +
# geom_abline(slope = 1, intercept = 0, col = 'red') +
# scale_x_continuous(limits = c(0.7, 1)) + 
# scale_y_continuous(limits = c(0.7, 1))

diff_plot

diff_lm <- lm(diff ~ pct_n_bird, data = status_df)
