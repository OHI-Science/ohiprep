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

ics15_all <- read_csv(ics15_file, col_types = 'cdddd') %>%
  rename(iucn_sid = id_no)
ics16_all <- read_csv(ics16_file, col_types = 'cddddc') 

##### Examine the raw cell files #####
# nrow(ics15_all %>% select(iucn_sid, sciname) %>% unique()) 
# # 3371 spp
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

