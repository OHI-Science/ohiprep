# check_whence.r 
# by JSLowndes, May 2014. 

# combine certain data layers from global OHI, then make a heatmap displaying 
# whether a layer was gapfilled or not for each country. see gapfilling code
# categories rules: https://docs.google.com/a/nceas.ucsb.edu/spreadsheet/ccc?key=0AjLReAQzT2SVdGNZcDNYRjlfSEYteEFyQnotZ0ZjNmc&usp=drive_web#gid=9

# original data is represented with 0, gapfilled with 1. Individual layers are summed and divided by the amount of total layers. Example: if in a single region, 2 layers are original and 2 are gapfilled and there are 4 total, the score would be (0+0+1+1)/4 = .5

# create whence files TUES:::::
track alien invasives # in neptune_data:model/GL-NCEAS-Resilience_v2013a/data disaggreate
pathogens?
msi #in neptune_data:model/GL-NCEAS-Resilience_v2013a/data disaggreate

check:
make sure CW adds right
split into georegions
remove TR as a placeholder


## setup ---
# load libraries
source('src/R/common.R') # set dir_neptune_data; load reshape2, plyr, dplyr
# devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
library(stringr)
library(ggplot2)
library(ohicore)  # for github/ohicore/R/gapfill_georegions.R

# read google spreadsheep for layers
g.url = 'https://docs.google.com/spreadsheet/pub?key=0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE&single=true&gid=0&output=csv'
l = read.csv(textConnection(RCurl::getURL(g.url, ssl.verifypeer = FALSE)), skip=1, na.strings=''); head(l); names(l)

# select only layers 
l_whence_tmp = l %.%
  filter(  !is.na(whence_2013a) & 
           !targets %in% c('NP', 'LIV', 'ECO')) %.% ##TODO add 'TR' back in after testing)
  mutate(root_whence = str_split_fixed(as.character(dir_whence_2013a), ':', 2)[,1],
         dir_whence = str_split_fixed(as.character(dir_whence_2013a), ':', 2)[,2]) %.%
  select(tar = targets,
         root_whence,
         dir_whence,
         whence_2013a)

# sort out different directories and combine
l_nep = l_whence_tmp %.%
  filter(root_whence == 'neptune_data') %.%
  mutate(fp = file.path(dir_neptune_data, dir_whence, whence_2013a))
l_git = l_whence_tmp %.%
  filter(root_whence == 'ohiprep') %.%
  mutate(fp = file.path(dir_whence, whence_2013a))

l_whence = rbind(l_nep, l_git) %.%
  select(tar, root_whence, fp) %.%
  arrange(tar)

l_whence$tar = gsub('_.*', '', l_whence$tar) # for name tracking purposes below
l_whence$tar = gsub(' .*', '', l_whence$tar) # for name tracking purposes below

## read in all files, summarize and combine ----
d_f =  matrix(nrow=0, ncol=0) # data from files

for (i in 1:length(l_whence$fp)){ # i = 1
                                  # i = 6
  
  f = l_whence$fp[i]
  d = read.csv(f); tail(d)
  fname = strsplit(f, '/')
  lis = lapply(fname, lapply, length) # roundabout way of finding the last list element in the path
  names(lis) = lapply(fname, length)
  fname2 = strsplit(unlist(fname)[as.numeric(names(lis))], '_')
   
  if('whencev01' %in% names(d)){
    
    # summary statistics
    dstats = d %.%
      filter(rgn_id != 213) %.% # remove Antarctica
      group_by(rgn_id) %.% 
      summarise(count_rgn_id = n(), 
                count_SG     = sum(whencev01 != 'OD'), # 'SG' means 'spatial gapfilling'; 'OD' means 'original data'
                perc_SG      = count_SG / count_rgn_id) %.%
      arrange(rgn_id) %.%
      select(rgn_id, 
             perc_SG); head(dstats)
    
  }else if ('z_level' %in% names(d)) {
    
    # summary statistics
    dstats = d %.%
      filter(id != 213) %.% # remove Antarctica
      group_by(id) %.% 
      summarise(count_rgn_id = n(), 
                count_SG     = sum(z_level != 'v'), # 'v' means 'original value'
                perc_SG      = count_SG / count_rgn_id) %.%
      arrange(id) %.%
      select(rgn_id = id, 
             perc_SG); head(dstats)
    
  }
  
  #rename and reformat
 names(dstats) = c('rgn_id', paste(l_whence$tar[i], '_', unlist(fname2)[2], '_', unlist(fname2)[3], sep='')) 
  
  if(i == 1){
    d_f = rbind(d_f, dstats)
  } else {
    dstats$rgn_id = NULL
    d_f = cbind(d_f, dstats)
  }
  
}

head(d_f) # write.csv(d_f, 'whence_2013afiles.csv', row.names = F)


## add non-gapfilled elements ----
## add non-gapfilled layers                          # TODO: add a check from the whence_2013a column to track
d_fl = d_f %.%           # data from files and layers
  mutate(CW_ciesin_cpop  = 0,
         P_sp_genetic    = 0,
         P_targetharvest = 0, 
         P_fishing       = 0,
         AO_sust         = 0, 
         TR_wttc_empd    = 0); head(d_fl)
  
## add non-gapfilled goals
d_flg = d_fl %.% # data from files, layers, and goals
  mutate(BD = 0,
         CP = 0,
         CS = 0,
         FP = 0,
         LE = 1,
         NP = 0,
         SP = 0); head(d_flg)

# number of layers per goal
n_lpg = list(
  AO = 3,
  BD = 1,
  CP = 1,
  CS = 1,
  CW = 4,
  FP = 1, 
  LE = 1,
  NP = 1, 
  SP = 1,
  TR = 4,    ## TODO: TR is a placeholder
  pr = 7, ## TODO: placeholder--need to finalize
  re = 7) ## TODO: placeholder--need to finalize

## collapse by goal ----
nam = str_split_fixed(as.character(names(d_flg)), '_', 2)[,1] # identify goal prefixes

# CW, TR, resil, press
nam_cw = names(d_flg)[nam == 'CW']
nam_tr = names(d_flg)[nam == 'TR']
nam_ao = names(d_flg)[nam == 'AO']
nam_pr = names(d_flg)[nam == 'pressures'  | nam == 'P']
nam_re = names(d_flg)[nam == 'resilience' | nam == 'R']

d_flg$cw_sum = rowSums(d_flg[,nam_cw], na.rm=T)
d_flg$tr_sum = rowSums(d_flg[,nam_tr], na.rm=T)
d_flg$ao_sum = rowSums(d_flg[,nam_ao], na.rm=T)
d_flg$pr_sum = rowSums(d_flg[,nam_pr], na.rm=T)
d_flg$re_sum = rowSums(d_flg[,nam_re], na.rm=T); head(d_flg) 

d_g = d_flg %.% 
  mutate(CW = ( cw_sum / as.numeric(n_lpg[['CW']]) ), 
         TR = ( tr_sum / as.numeric(n_lpg[['TR']]) ),
         AO = ( ao_sum / as.numeric(n_lpg[['AO']]) ),
         pr = ( pr_sum / as.numeric(n_lpg[['pr']]) ),
         re = ( re_sum / as.numeric(n_lpg[['re']]) ))
d_g = d_g[,(!names(d_g) %in% c(nam_cw, 'cw_sum',
                               nam_tr, 'tr_sum',
                               nam_ao, 'ao_sum',
                               nam_pr, 'pr_sum',
                               nam_re, 're_sum'))]; head(d_g) 

           

## prepare for heatmap by georegion ----

# link to georegions for display purposes 
georegions = read.csv('../ohicore/inst/extdata/layers.Global2013.www2013/rgn_georegions_long_2013b.csv', na.strings='') %.%
  dcast(rgn_id ~ level, value.var='georgn_id'); head(georegions)

georegion_labels = read.csv('../ohicore/inst/extdata/layers.Global2013.www2013/rgn_georegions_labels_long_2013b.csv') %.%    
  mutate(level_label = sprintf('%s_label', level)) %.%
  dcast(rgn_id ~ level_label, value.var='label') %.%
  left_join(
    read.csv('../ohicore/inst/extdata/layers.Global2013.www2013/rgn_labels.csv') %.%
      select(rgn_id, v_label=label),
    by='rgn_id') %.%
  arrange(r0_label, r1_label, r2_label, v_label); head(georegion_labels)

# for labeling
d_g_lab = d_g %.%
  left_join(georegion_labels %.%
              select(rgn_id, r2_label, v_label),
            by = 'rgn_id'); head(d_g_lab)


# georegions list--for splitting the figures into manageable portions
georegions_list = list(
    grp1=c(
      'Australia and New Zealand',
      'Eastern Asia',
      'Melanesia',
      'Micronesia',
      'Polynesia',
      'South-Eastern Asia',
      'Southern Asia',
      'Western Asia'
    ),
    grp2=c(
      'Caribbean',
      'Central America',
      'Northern America',
      'South America',
      'Southern Islands'
    ),
    grp3=c(
      'Eastern Africa',
      'Eastern Europe',
      'Middle Africa',
      'Northern Africa',          
      'Northern Europe',
      'Southern Africa',
      'Southern Europe',
      'Western Africa',
      'Western Europe'
    ))


## prepare data for heatmap plotting ----

data <- d_g_lab %.%
  filter(!is.na(r2_label)) %.%
  select(rgn_id,
         AO, BD, CP, CS, CW, FP, LE, NP, SP, TR, pr, re, # order goals alphabetically
         r2_label, v_label); head(data)

data_melt <- melt(data, id.vars = c('rgn_id', 'r2_label', 'v_label'), value.name = 'prop_gf', id="rgn_id") # melt
data_melt$prop_gf[data_melt$prop_gf > 0 & data_melt$prop_gf < 1] = 0.5 # for heatmapping: change value for 3 discrete colors


## heatmap plotting! ----

# PLOTTING OPTIONS: 
# 1. single figure with all rgn_ids together, ordered by georegion 
# 2. separate figure for each georegion

# prepare for the groupings
grps = 3
# data_melt1 = data_melt %.%
#   filter(r2_label %in% georegions_list[['grp1']] ) ; head(data_melt1)
# data_melt2 = data_melt %.%
#   filter(r2_label %in% georegions_list[['grp2']] ) ; head(data_melt2)
# data_melt3 = data_melt %.%
#   filter(r2_label %in% georegions_list[['grp3']] ) ; head(data_melt3)

## different heatmaps, by group ----
# grp 1 

for (j in 1:grps){ # j=1
  if        (j==1){
    data_meltj = data_melt %.%
  filter(r2_label %in% georegions_list[['grp1']] ) 
  } else if (j==2){
     data_meltj = data_melt %.%
  filter(r2_label %in% georegions_list[['grp2']] ) 
  } else if (j==3){
     data_meltj = data_melt %.%
  filter(r2_label %in% georegions_list[['grp3']] ) 
  }

data_m = data_meltj %.%
  arrange(variable, r2_label) %.%
  select(r2_label,v_label, variable, prop_gf); head(data_m)

ggplot(data_m, aes(x=variable, y=as.factor(v_label), fill=as.factor(prop_gf))) +
  geom_raster() +
  labs(y = '', x = "") + 
  theme(axis.text.y = element_text(size=10),
        axis.text.x = element_text(angle=90, vjust=1)) +
  scale_fill_brewer(name  = '', type = "seq", palette = (1),labels=c('original', 'partially gapfilled', 'fully gapfilled'))
ggsave(file.path('tmp/whence_figures', paste('OHI_2013_heatmap', j, '.png', sep='')), width=10, height=15)
}



# grp 2
data_m = data_melt2 %.%
  arrange(variable, r2_label) %.%
  select(r2_label,v_label, variable, prop_gf); head(data_m,20)

ggplot(data_m, aes(x=variable, y=as.factor(v_label), fill=as.factor(prop_gf))) +
  geom_raster() +
  labs(y = '', x = "") + 
  theme(axis.text.y = element_text(size=10),
        axis.text.x = element_text(angle=90, vjust=1)) +
  scale_fill_discrete(name  = '', breaks=c(1,2), labels=c('original', 'gapfilled'))

ggsave(file.path('tmp/whence_figures', paste('whence_2.png', sep='')), width=10, height=15)

# grp 3
data_m = data_melt3 %.%
  arrange(variable, r2_label) %.%
  select(r2_label,v_label, variable, prop_gf); head(data_m,20)

ggplot(data_m, aes(x=variable, y=as.factor(v_label), fill=as.factor(prop_gf))) +
  geom_raster() +
  labs(y = '', x = "") + 
  theme(axis.text.y = element_text(size=10),
        axis.text.x = element_text(angle=90, vjust=1)) +
  scale_fill_discrete(name  = '', breaks=c(1,2), labels=c('original', 'gapfilled'))

ggsave(file.path('tmp/whence_figures', paste('whence_3.png', sep='')), width=5, height=15)


# 2. for each georegion, create a heatmap with existing data 
for(g in unique(data$r2_label)) { # g="Australia and New Zealand"
  
  data_m = data_melt %.%
    filter(r2_label == g) %.%
    select(v_label, variable, prop_gf); head(data_m)
  
  # arrange by target
  #   nam1 = str_split_fixed(as.character(data_m$variable), '_', 2); head(nam1)
  #   target = nam1[,1]; head(target)
  #   data_m$variable = nam1[,2]; head(data_m)
  #   data_m$target = target; head(data_m)
  #   data_m = data_m %.% #--arrange by target!!!!!!!!!
  #     arrange(target)
  
  
  ggplot(data_m, aes(x=variable, y=as.factor(v_label), fill=as.factor(prop_gf))) +
    geom_raster() +
    labs(y = g, x = "") + 
    theme(axis.text.y = element_text(size=10),
          axis.text.x = element_text(angle=90, vjust=1)) +
    scale_fill_discrete(name  = '', breaks=c(1,2), labels=c('original', 'gapfilled'))
  
  ggsave(file.path('tmp/whence_figures', paste('whence_', g, '.png', sep='')), width=5, height=15)
  
}


## Mel's original heat map of whence data ----
# by MFrazier May 2014
library(ggplot2)

data <- d_f # read.csv("whence_2013afiles.csv")
data_melt <- melt(data, id="rgn_id")
data_melt$prop_gf[data_melt$prop_gf == 0] <- NA # head(data_melt)

ggplot(data_melt, aes(x=variable, y=as.factor(rgn_id), fill=as.factor(prop_gf))) +
  geom_raster() +
  labs(y="region ID", x="") + 
  theme(axis.text.y = element_text(size=6),
        axis.text.x = element_text(angle=90, vjust=1)) +
  scale_fill_discrete(name  ="Whence", breaks=c(1), labels="")
ggsave("whence_example.png", width=5, height=15)


## check gapfilling techniques ----
# 
# library(dplyr)
# georegions = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn_georegions_long_2013b.csv', na.strings='') %.%
#   dcast(rgn_id ~ level, value.var='georgn_id')
# 
# gl = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn_georegions_labels_long_2013b.csv', na.strings='')
# georegion_labels = gl  %.%    
#   mutate(level_label = sprintf('%s_label', level)) %.%
#   dcast(rgn_id ~ level_label, value.var='label') %.%
#   left_join(
#     gl %.%
#       select(rgn_id, v_label=label),
#     by='rgn_id')
# 
#   # setup data for georegional gapfilling (remove Antarctica rgn_id=213)
#   d_g = gapfill_georegions(
#     data = d %.%
#       filter(rgn_id!=213) %.%
#       select(rgn_id, year, Xtr),
#     georegions = georegions,
#     georegion_labels = georegion_labels)
# 
# 
# #----fin
