# check_laborforce.r
# compare gapfilled labor force values to total population values; other approach of gapfilling may be necessary.

source('src/R/common.R') # set dir_neptune_data; load reshape2, plyr, dplyr
source('src/R/ohi_clean_fxns.R') # get functions
dir_d = 'Global/WorldBank-Statistics_v2012'

# load libraries
library(dplyr) 
# devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
# library(ohicore) 

# explore ----
# read in gapfilled labor file
l = read.csv(file.path(dir_d, 'data', 'rgn_wb_tlf_2014a.csv'), na.strings='') %>%
               select(rgn_id, year, 
                      labforce = count); head(l)

# read in total population
p = read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-CoastalPopulation_v2013/data', 'rgn_popsum2005to2015_inland25mi.csv'), na.strings=''); head(p)

# join l and p

vs = l %>%
  left_join(p, 
            by=c('rgn_id', 'year')) %>%
  mutate(pop_dif = popsum - labforce,
         pop_prob = pop_dif < 0) %>%
  filter(year >=2005,
         pop_prob != F); head(vs, 30)

vs_grp = vs %>%
  group_by(rgn_id) %>%
  summarize(n_prob = n())

# try gapfilling labor force differently ----
# first tried laborforce/totalpop, but this was L/P >> 1 in many cases. Not shown. 
# below, see (L-(L*U) / P as a ratio for gapfilling: this too was >> 1 in 1/3 of cases, so not a good approach for gapfilling. 

# library(dplyr)
l_gg = read.csv(file.path(dir_d, 'data', 'rgn_wb_tlf_2014a.csv'), na.strings=''); head(l_gg) # georegional_gapfilling as a reminder
l_attr_csv = read.csv(file.path(dir_d, 'data', 'rgn_wb_tlf_2014a_attr.csv'), na.strings=''); head(l_attr_csv)
u_attr_csv = read.csv(file.path(dir_d, 'data', 'rgn_wb_uem_2014a_attr.csv'), na.strings=''); head(u_attr_csv)

# see which z_levels are represented
z_levels = l_attr_csv %>%
  group_by(z_level) %>%
  summarize(n_z_level = n()); z_levels
#   z_level n_z_level
# 1      r0       161 ## r0 are the uninhabiteds that are NA'd here: head(filter(l_attr, z_level == 'r0'))
# 2      r2      1659
# 3       v      3240

z_levels = u_attr_csv %>%
  group_by(z_level) %>%
  summarize(n_z_level = n()); z_levels
#   z_level n_z_level
# 1      r0       231
# 2      r1       837 ## laborforce didn't have any r1 gapfilling
# 3      r2      3849
# 4       v      2343

# narrow selected columns; rename a few
l_attr = l_attr_csv %>%
  select( 
         r2_label, 
         v_label,
         year     = yr, 
         rgn_id   = id,
         r2l      = r2, 
         L_geomean    = r2_v, 
         zl_level = z_level,
         L = z);   head(l_attr)


# join with percent unemployment
lu_attr = l_attr %>%
  left_join(u_attr_csv %>%
              select(
                rgn_id   = id, 
                r2_label, 
                v_label,
                year     = yr, 
                rgn_id   = id,
                r2u      = r2, 
                U_geomean    = r2_v, 
                zu_level = z_level,
                U    = z),  
            by = c('r2_label', 'v_label', 'year', 'rgn_id')); head(lu_attr)

lu_attr$U = lu_attr$U / 100 ## divide by 100 for calculations below


# separate original v. spatial gapfilled data

lu_v = lu_attr %>%
  filter(zl_level == 'v' & zu_level == 'v'); head(lu_v)
# lu_r2 = lu_attr %>%
#   filter(zl_level != 'v' & zu_level != 'v'); head(lu_v) # would be messy because of r1 values in unemployment

# read in population
p = read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-CoastalPopulation_v2013/data', 'rgn_popsum2005to2015_inland25mi.csv'), na.strings=''); head(p)


# join lu and p

lu_p = lu_v %>%
  left_join(p %>%
              select(rgn_id, year, 
                     P = popsum),
            by=c('rgn_id', 'year')) %>%
  mutate(
    Lgeomean_over_P = L_geomean / P,
    L_over_P = L / P,
    L_over_P_flag = L_over_P >= 1,
    employed = L - (L * U),
    employed_over_P  = employed / P, 
    employed_over_P_flag = employed_over_P >= 1) %>%
  filter(P != 0)          # Howland and Baker Island[158] and Jarvis Island[149]
#   filter(p_minus_l < 0)

head(lu_p)
summary(lu_p)

filter(lu_p, employed < 0)

lu_summary = lu_p %>%
  select(rgn_id,
         rgn_name = v_label, 
         year,
         L,
         L_geomean,
         U,
         U_geomean, 
         Pop = P, 
         L_over_P, 
         L_over_P_exceeds1 = L_over_P_flag, 
         employed, 
         employed_over_P, 
         employed_over_P_exceeds1 = employed_over_P_flag)
summary(lu_summary)
         
         
         
         
    
       
       
    
    
    
    
    
    
    
