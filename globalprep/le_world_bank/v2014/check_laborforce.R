# check_laborforce.r

# try gapfilling labor force differently ----
# 1. tried laborforce/totalpop, but this was L/P >> 1 in many cases. Not shown. 
# 2. tried with a (L-(L*U)) combobelow, see (L-(L*U) / P as a ratio for gapfilling: this too was >> 1 in 1/3 of cases, so not a good approach for gapfilling. 

source('src/R/common.R') # set dir_neptune_data; load reshape2, plyr, dplyr
source('src/R/ohi_clean_fxns.R') # get functions
library(dplyr) 
dir_d = 'Global/WorldBank-Statistics_v2012'

# read in total population and labor force gapfilled attributes
p = read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-CoastalPopulation_v2013/data', 'rgn_popsum2005to2015_inland25mi.csv'), na.strings=''); head(p)
l_attr_csv = read.csv(file.path(dir_d, 'data', 'rgn_wb_tlf_2014a_attr.csv'), na.strings=''); head(l_attr_csv) # L 

## 1. gapfill L with georegional L/ population ratio

# a) see which z_levels are represented
z_levels = l_attr_csv %>%
  group_by(z_level) %>%
  summarize(n_z_level = n()); z_levels
#   z_level n_z_level
# 1      r0       161 ## r0 are the uninhabiteds that are NA'd here: head(filter(l_attr, z_level == 'r0'))
# 2      r2      1659
# 3       v      3240


# b) narrow selected columns; rename a few
l_attr = l_attr_csv %>%
  select( 
         r2_label, 
         v_label,
         year     = yr, 
         rgn_id   = id,
         r2, 
         Lgeomean = r2_v, 
         z_level,
         L = z);  head(l_attr)


# c) join l and p
l_p = l_attr %>%
  left_join(p %>%
      select(rgn_id, year, 
             P = popsum),
      by=c('rgn_id', 'year')) %>%
  filter(L > P) %>%      # just look at cases where L > P
  mutate(
    L_over_P             = L / P,
    L_over_P_flag        = L_over_P >= 1,
    Lgeomean_over_P      = Lgeomean / P,
    Lgeomean_over_P_flag = Lgeomean_over_P >= 1)
head(filter(l_p, !is.na(P)))
summary(l_p)

# look at fewer variables (L == Lgeomean)
l_p_summary = l_p %.%
  select(r2_label, v_label, year, rgn_id, r2, L, P, L_over_P, L_over_P_flag)
head(filter(l_p_summary, !is.na(P)))
#       r2_label   v_label year rgn_id r2       L      P L_over_P L_over_P_flag
# 1 Eastern Africa Mayotte 2005     29 14 5905971 196420 30.06807          TRUE
# 2 Eastern Africa Mayotte 2006     29 14 6083524 207571 29.30816          TRUE
# 3 Eastern Africa Mayotte 2007     29 14 6267187 218722 28.65367          TRUE
# 4 Eastern Africa Mayotte 2008     29 14 6455776 229873 28.08410          TRUE
# 5 Eastern Africa Mayotte 2009     29 14 6648908 241024 27.58608          TRUE
# 6 Eastern Africa Mayotte 2010     29 14 6848570 252176 27.15790          TRUE
summary(l_p_summary)

#       r2_label                           v_label                         year          rgn_id              r2       
# Caribbean       :414   American Samoa                        :  23   Min.   :1990   Min.   :  1.0   Min.   :  5.0  
# Micronesia      :276   Amsterdam Island and Saint Paul Island:  23   1st Qu.:1995   1st Qu.: 36.0   1st Qu.: 29.0  
# Northern Europe :207   Andaman and Nicobar                   :  23   Median :2001   Median :111.0   Median : 53.0  
# Eastern Africa  :184   Anguilla                              :  23   Mean   :2001   Mean   :110.7   Mean   :137.7  
# Polynesia       :161   Antigua and Barbuda                   :  23   3rd Qu.:2007   3rd Qu.:156.0   3rd Qu.: 61.0  
# Southern Islands:161   Aruba                                 :  23   Max.   :2012   Max.   :250.0   Max.   :999.0  
# (Other)         :417   (Other)                               :1682                                                 
#        L                   P             L_over_P     L_over_P_flag  
# Min.   :    55799   Min.   :       0   Min.   :  0    Mode :logical  
# 1st Qu.:    73676   1st Qu.:    4568   1st Qu.: 12    FALSE:32       
# Median :  1781284   Median :   37050   Median : 56    TRUE :448      
# Mean   :  9102582   Mean   :  461938   Mean   :Inf    NA's :1340     
#  3rd Qu.:  5984075   3rd Qu.:  120625   3rd Qu.:462                   
#  Max.   :224461759   Max.   :21159500   Max.   :Inf                   
#  NA's   :161         NA's   :1340       NA's   :1340   


## 2. Try by adding U
u_attr_csv = read.csv(file.path(dir_d, 'data', 'rgn_wb_uem_2014a_attr.csv'), na.strings=''); head(u_attr_csv) # U

# a) see which z_levels are represented
z_levels = u_attr_csv %>%
  group_by(z_level) %>%
  summarize(n_z_level = n()); z_levels
#   z_level n_z_level
# 1      r0       231
# 2      r1       837 ## laborforce didn't have any r1 gapfilling
# 3      r2      3849
# 4       v      2343

# join with percent unemployment,  rename a few
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


# c) join lu and p
lu_p = lu_attr %>%
  left_join(p %>%
              select(rgn_id, year, 
                     P = popsum),
            by=c('rgn_id', 'year')) %>%
  filter(L > P) %>%
  mutate(
    Lgeomean_over_P = Lgeomean / P,
    L_over_P = L / P,
    L_over_P_flag = L_over_P >= 1,
    employed = L - (L * U),
    employed_over_P  = employed / P, 
    employed_over_P_flag = employed_over_P >= 1) 

head(lu_p)
summary(lu_p)

# clean this up for summary
lu_summary = lu_p %>%
  select(rgn_id,
         rgn_name = v_label, 
         year,
         L,
         Lgeomean,
         U,
         U_geomean, 
         Pop = P, 
         L_over_P, 
         L_over_P_exceeds1 = L_over_P_flag, 
         employed, 
         employed_over_P, 
         employed_over_P_exceeds1 = employed_over_P_flag)
head(lu_summary)
#    rgn_id rgn_name year      L Lgeomean      U U_geomean     Pop L_over_P L_over_P_exceeds1 employed employed_over_P
# 1     45  Eritrea 2005 2292004  5905971 0.0590      5.90  924400 2.479451              TRUE  2156776        2.333163
# 2     45  Eritrea 2006 2395623  6083524 0.0670      6.70  948053 2.526888              TRUE  2235117        2.357586
# 3     45  Eritrea 2007 2491630  6267187 0.0525      5.25  971707 2.564178              TRUE  2360819        2.429559
# 4     45  Eritrea 2008 2585821  6455776 0.0720      7.20  995360 2.597875              TRUE  2399642        2.410828
# 5     45  Eritrea 2009 2673501  6648908 0.0490      4.90 1019010 2.623626              TRUE  2542499        2.495068
# 6     45  Eritrea 2010 2764038  6848570 0.0770      7.70 1042670 2.650923              TRUE  2551207        2.446802
# employed_over_P_exceeds1
# 1                     TRUE
# 2                     TRUE
# 3                     TRUE
# 4                     TRUE
# 5                     TRUE
# 6                     TRUE
> 
summary(lu_summary)
         
# rgn_id                     rgn_name        year            L                Lgeomean               U          
# Min.   :  1.0   Andaman and Nicobar:  8   Min.   :2005   Min.   :    66052   Min.   :    66052   Min.   :0.00100  
# 1st Qu.: 72.0   Angola             :  8   1st Qu.:2007   1st Qu.:  1718130   1st Qu.:  1802588   1st Qu.:0.05700  
# Median :136.0   Anguilla           :  8   Median :2009   Median :  5514754   Median :  6455776   Median :0.08200  
# Mean   :130.9   Antigua and Barbuda:  8   Mean   :2009   Mean   : 24962001   Mean   : 17255719   Mean   :0.09015  
# 3rd Qu.:191.0   Argentina          :  8   3rd Qu.:2011   3rd Qu.: 17677939   3rd Qu.: 17765461   3rd Qu.:0.11375  
# Max.   :250.0   Aruba              :  8   Max.   :2012   Max.   :791711771   Max.   :224461759   Max.   :0.37600  
# (Other)            :914                                                                           
#    U_geomean           Pop            L_over_P   L_over_P_exceeds1       employed      employed_over_P
# Min.   : 1.200   Min.   :        0   Min.   :  1   Mode:logical      Min.   :    59133   Min.   :  1    
# 1st Qu.: 5.900   1st Qu.:    37420   1st Qu.:  2   TRUE:962          1st Qu.:  1542999   1st Qu.:  2    
# Median : 8.150   Median :   626414   Median :  6   NA's:0            Median :  4950561   Median :  5    
#  Mean   : 8.735   Mean   :  7595342   Mean   :Inf                     Mean   : 23117275   Mean   :Inf    
#  3rd Qu.:10.500   3rd Qu.:  5156568   3rd Qu.: 57                     3rd Qu.: 15240429   3rd Qu.: 52    
#  Max.   :31.200   Max.   :217414000   Max.   :Inf                     Max.   :765585283   Max.   :Inf    
#  NA's   :86                                                                                              
# employed_over_P_exceeds1
# Mode :logical           
# FALSE:37                
# TRUE :925               
# NA's :0                          
         
         
    
       
       
    
    
    
    
    
    
    
