# data_prep.R: reformat and add rgn_ids to World Economic Forum (WEF) data 
#
# by JStewartLowndes Mar2014; updated from 'clean_WEF.R' by JStewart in May 2013
#   Data: 
#       Global Competitiveness Index (GCI)
#       Travel and Tourist Competitiveness Index (TTCI)
#   read in individual files
#   call add_rgn_id.r to add OHI region_ids
#   georegional gapfilling with gapfill_georegions.r 
#   final processing by hand: see end of script


# setup ----

# load libraries
library(gdata)
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall


# get paths.  NOTE: Default path should be ohiprep root directory.
source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()
dir_d = '../ohiprep/Global/WEF-Economics_v2014'


# read in files ----
d.gci = read.csv(file.path(dir_d, 'raw', 'WEF_GCI_2013-2014_Table3_reformatted.csv')); head(d.gci)

# clean up 
gci = d.gci %.%
  select(country = Country, 
         score = Score_1_to_7) %.%
  mutate(country = str_replace(country, 'Korea', 'South Korea')); head(gci)

# and rescale (this makes export_rescaled_layer.R from GL-WEF-Economics_v2013 obsolete)
gci$score = gci$score/7; head(gci)

# (could also do this as):
# rng = c(min(gci$score), 7)
# d.m2 = within(d.m2,{
#   score = (score - rng[1]) / (rng[2] - rng[1])}); head(d.m2); summary(d.m2)


## add rgn_ids with name_to_rgn ---- 
# source('../ohiprep/src/R/ohi_clean_fxns.R')
m_d = name_to_rgn(gci, fld_name='country', flds_unique=c('country'), fld_value='score', collapse_fxn = mean, add_rgn_name=T) 

stopifnot(max(m_d$score) < 1)

## georegional gapfilling with gapfill_georegions.r ----
georegions = read.csv('../ohi-global/eez2013/layers/rgn_georegions.csv', na.strings='') %.%
  dcast(rgn_id ~ level, value.var='georgn_id')

georegion_labels = read.csv('../ohi-global/eez2013/layers/rgn_georegion_labels.csv') %.%    
  mutate(level_label = sprintf('%s_label', level)) %.%
  dcast(rgn_id ~ level_label, value.var='label') %.%
  left_join(
    read.csv('../ohi-global/eez2013/layers/rgn_labels.csv') %.%
      select(rgn_id, v_label=label),
    by='rgn_id') %.%
  arrange(r0_label, r1_label, r2_label, v_label); head(georegion_labels)


layersave = file.path(dir_d, 'data', 'rgn_wef_gci_2014a.csv')
attrsave  = file.path(dir_d, 'data', 'rgn_wef_gci_2014a_attr.csv')

# library(devtools); load_all('../ohicore')
# source('../ohicore/R/gapfill_georegions.R')
d_g_a = gapfill_georegions(
  data = m_d %.%
    filter(!rgn_id %in% c(213,255)) %.%
    select(rgn_id, score),
  fld_id = 'rgn_id',
  georegions = georegions,
  georegion_labels = georegion_labels,
  r0_to_NA = TRUE, 
  attributes_csv = attrsave) # don't chain gapfill_georegions or will lose head(attr(d_g_a, 'gapfill_georegions')) ability

# investigate attribute tables
head(attr(d_g_a, 'gapfill_georegions'))  # or to open in excel: system(sprintf('open %s', attrsave))


## last step: give North Korea the minimum value and save ----
d_g = d_g_a %.%
  select(rgn_id, score) %.%
  arrange(rgn_id); head(d_g)

# find minimum  
s_min = min(d_g %.%
              select(score) %.%
              filter(!is.na(score)))

# replace North Korea (rgn_id == 21) in gapfilled_data
d_g$score[d_g$rgn_id == 21] = s_min 

# save
stopifnot(anyDuplicated(d_g[,c('rgn_id')]) == 0)
write.csv(d_g, layersave, na = '', row.names=FALSE)


## also change attributes table ---- 
d_attr = read.csv(attrsave) %.%
  filter(id != 21)

d_nk = d_attr %.%
  filter(id == 21) %.%
  mutate(
    z_level = 'XH',
    
    r2_v = s_min, 
    r1_v = s_min, 
    r0_v = s_min, 
    z    = s_min, 
    
    r2         = NA,
    r1         = NA,
    r0         = NA,
    r2_n_notna = NA,
    r1_n_notna = NA,
    r0_n_notna = NA,
    z_ids      = NA,
    r2_n       = NA,
    r1_n       = NA,
    r0_n       = NA,
    z_n        = NA,
    z_n_pct    = NA,
    z_g_score  = NA); d_nk

d_attr_fin = rbind(d_attr, d_nk) %.%
  arrange(r0_label, r1_label, r2_label, v_label) 
write.csv(d_attr_fin, attrsave, na = '', row.names=F)


# --- fin





# ## 2013 stuff to clean TTCI data. Not done in 2014 so would have to be updated a bit for nextime data are updated
# 
# d.ttci = read.csv('WEF_TTCI_2012-2013_Table1_reformatted.csv')
# d.ttci2 = cbind(d.ttci,rep('ttci',length(d.ttci[,1])))
# names(d.ttci2) = c('Region','Rank2013','IndexScore2013','Rank2011','layer')
# 
# # concatenate f files
# d.all = rbind(d.gci2[c(1,3,5)], d.ttci2[c(1,3,5)])
# 
# 
# ## run add_rgn_id and save
# uifilesave = paste(dir1, 'data/', 'GL-WEF-Economics_v2013-cleaned.csv', sep='')
# add_rgn_id(d.all, uifilesave)
# 
# 
# ## georegional gapfilling with add_gapfill.r 
# 
# cleaned_data1 = read.csv(uifilesave)
# 
# layer_uni = unique(cleaned_data1$layer)
# layernames = sprintf('rgn_wef_%s_tmp.csv', tolower(layer_uni))
# s_island_val = NA # assign what southern islands will get. 
# 
# for(i in 1:length(layer_uni)) {
#   cleaned_layer = cleaned_data1[cleaned_data1$layer == layer_uni[i],]
#   cleaned_layer$layer = NULL
#   
#   layersave = paste(dir1, 'raw/', layernames[i], sep='')    
#   add_gapfill_singleyear(cleaned_layer, layersave, s_island_val)
# }
# 