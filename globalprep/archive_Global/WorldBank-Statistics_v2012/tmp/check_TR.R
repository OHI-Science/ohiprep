# check_TR.r
# investigate TR calculated scores; compare raw values as well. June 2014

# load libraries
library(gdata)
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

# get paths.
source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()
dir_d =      '../ohiprep/Global/WorldBank-Statistics_v2012'
dir_lookup = '../ohiprep/src/LookupTables'
dir_anx_scores = file.path(dir_neptune_data, 'git-annex/Global/NCEAS-OHI-Scores-Archive/scores')

# investigate the 'Vanavatu Problem' (= JSL's gapfilling script temporal.gapfill.r add_gapfill.r didn't gapfill if there was only 1 year of data, so this was gapfilled by hand by BH. 

# read in for rgn names
rgns = read.csv(file.path(dir_lookup, 'eez_rgn_2013master.csv'), na.strings = ''); head(rgns)

# read in JSL's add gapfill files

# u_o = read.csv(file.path(dir_d, 'tmp', 'rgn_wb_uem_2014awith_add_gapfill.csv'), na.strings = '') %>% # unemployment original
u_o = read.csv(file.path(dir_neptune_data, 'model/GL-WorldBank-Statistics_v2012/data/rgn_wb_uem_2013a.csv'),na.strings = '') %>%
  select(rgn_id, percent, year) %>%
  left_join(rgns %.%
              select(rgn_id = rgn_id_2013,
                     rgn_name = rgn_nam_2013),
            by = 'rgn_id'); head(u_o)
u_o[duplicated(u_o[, c('rgn_id', 'year', 'rgn_name')]),] 

u_o1 = u_o %.%
  group_by(rgn_id, rgn_name) %>%
  summarize(count = n()) %>%
  filter(count == 1); u_o1


# scores original v new (s_on)
# dir_anx_scores = file.path(dir_neptune_data, 'git-annex/Global/NCEAS-OHI-Scores-Archive/scores')
# s_on = read.csv(file.path(dir_anx_scores, 'scores_eez2012-2013_2014-07-01_vs_2013-10-09.csv'), na.strings=''); head(s_on)

s_on = read.csv(file.path(dir_d, 'tmp/scores_eez2012-2013_2014-07-01_vs_2013-10-09_unfiltered.csv'), na.strings='')

# check length of scores reporting
s_onx = s_on %>%
  filter(year == 2013, 
         dimension == 'status') %>% 
  select(goal, 
         rgn_id = region_id) %>%
  group_by(goal) %>%
  summarize(n_rgns_per_goal = n()); s_onx

tr_on = s_on %>%
  filter(year == 2013,
         #          score_old == 100,
         goal == 'TR', 
         dimension == 'status') %>%
  select(rgn_id = region_id,
         rgn_name = region_name,
         score_new,
         score_old,
         score_dif) %>%
  arrange(desc(abs(score_dif))); head(tr_on, 30)


# compare 'Vanavatu Problem' (u_o1) with score_difs

u_o1c = u_o1 %>%   # u_o1c: u_o1 compare
  left_join(tr_on,          
            by = c('rgn_id', 'rgn_name')) %>%
  arrange(desc(abs(score_dif))); u_o1c


filter(tr_on, rgn_id == 21)





