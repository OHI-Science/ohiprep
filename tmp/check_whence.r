# check_whence.r 
# by JSLowndes, May 2014. 

# combine certain data layers from global OHI, then make a heatmap displaying 
# whether a layer was gapfilled or not for each country. see gapfilling code
# categories rules: https://docs.google.com/a/nceas.ucsb.edu/spreadsheet/ccc?key=0AjLReAQzT2SVdGNZcDNYRjlfSEYteEFyQnotZ0ZjNmc&usp=drive_web#gid=9

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
           !targets %in% c('NP', 'LIV', 'ECO')) %.% ## add 'TR' back in after testing)
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
                count_OD     = sum(whencev01 == 'OD'), # 'OD' means 'original data'
                perc_OD      = count_OD / count_rgn_id) %.%
      arrange(rgn_id) %.%
      select(rgn_id, 
             perc_OD); head(dstats)
    
  }else if ('z_level' %in% names(d)) {
    
    # summary statistics
    dstats = d %.%
      filter(id != 213) %.% # remove Antarctica
      group_by(id) %.% 
      summarise(count_rgn_id = n(), 
                count_OD     = sum(z_level == 'v'), # 'v' means 'original value'
                perc_OD      = count_OD / count_rgn_id) %.%
      arrange(id) %.%
      select(rgn_id = id, 
             perc_OD); head(dstats)
    
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
## add non-gapfilled layers
d_fl = d_f %.%           # data from files and layers
  mutate(CW_ciesin_cpop = 0); head(d_fl)
  
## add non-gapfilled goals
d_flg = d_fl %.% # data from files, layers, and goals
  mutate(BD = 1,
         CP = 1,
         CS = 1,
         FP = 1,
         LE = 0,
         NP = 1,
         SP = 1); head(d_flg)

# number of layers per goal
n_lpg = list(
  AO = 2,
  BD = 1,
  CP = 1,
  CS = 1,
  CW = 4,
  FP = 1, 
  LE = 1,
  NP = 1, 
  SP = 1,
  TR = 4) ## TODO: TR is a placeholder

## collapse by goal ---- this 
glog = str_split_fixed(as.character(names(d_flg)), '_', 2)[,1] # identify goal prefixes

d_g = d_flg %.% 
  mutate(CW = (sum(glog == 'CW')) / as.numeric(n_lpg[['CW']])) %.% # TODO: check this math is correct (that as.numeric worked,etc)
  select(-CW_jmp_san, -CW_cw_fertilizers, -CW_cw_pesticides, -CW_ciesin_cpop) %.%
  mutate(TR = (sum(glog == 'TR')) / as.numeric(n_lpg[['TR']])) %.% # TODO: check this math is correct (that as.numeric worked,etc)
  select(-TR_wb_tlf, -TR_wb_uem); head(d_g) 
  # select(-(unlist(names(d_flg)[which(glog == 'CW')]))); head(d_g)   # == 'CW' TODO: make this work!!
           
# nam1 = str_split_fixed(as.character(names(d_g)), '_', 2); (nam1)
#   target = nam1[,1]; head(target)
#   d_g$variable = nam1[,2]; head(d_g)
#   data_m$target = target; head(data_m)
#   data_m = data_m %.% 
#     arrange(target)

## heatmap by georegion ----

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

# prepare data for plotting 
data <- d_g_lab %.%
  filter(!is.na(r2_label)); head(data)

data_melt <- melt(data, id.vars = c('rgn_id', 'r2_label', 'v_label'), id="rgn_id")
data_melt$value[data_melt$value == 0] <- NA; head(data_melt) 


## combine goals by data layer proportions ----

# PLOTTING OPTIONS: 
# 1. single figure with all rgn_ids together, ordered by georegion 
# 2. separate figure for each georegion


# 1. one big figure --actually has to be split into to

# TODO: break into grps
# logic for combining by layer proportion

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

# prepare for the groupings

data_melt1 = data_melt %.%
  filter(r2_label %in% georegions_list[['grp1']] ) ; head(data_melt1)
data_melt2 = data_melt %.%
  filter(r2_label %in% georegions_list[['grp2']] ) ; head(data_melt2)
data_melt3 = data_melt %.%
  filter(r2_label %in% georegions_list[['grp3']] ) ; head(data_melt3)

## 3 different heatmaps, by group ----
# grp 1 
data_m = data_melt1 %.%
  arrange(variable, r2_label) %.%
  select(r2_label,v_label, variable, value); head(data_m,20)

ggplot(data_m, aes(x=variable, y=as.factor(v_label), fill=as.factor(value))) +
  geom_raster() +
  labs(y = '', x = "") + 
  theme(axis.text.y = element_text(size=10),
        axis.text.x = element_text(angle=90, vjust=1)) +
  scale_fill_discrete(name  = '', breaks=c(1,2), labels=c('original', 'gapfilled'))

ggsave(file.path('tmp/whence_figures', paste('whence_1.png', sep='')), width=10, height=15)

# grp 2
data_m = data_melt2 %.%
  arrange(variable, r2_label) %.%
  select(r2_label,v_label, variable, value); head(data_m,20)

ggplot(data_m, aes(x=variable, y=as.factor(v_label), fill=as.factor(value))) +
  geom_raster() +
  labs(y = '', x = "") + 
  theme(axis.text.y = element_text(size=10),
        axis.text.x = element_text(angle=90, vjust=1)) +
  scale_fill_discrete(name  = '', breaks=c(1,2), labels=c('original', 'gapfilled'))

ggsave(file.path('tmp/whence_figures', paste('whence_2.png', sep='')), width=10, height=15)

# grp 3
data_m = data_melt3 %.%
  arrange(variable, r2_label) %.%
  select(r2_label,v_label, variable, value); head(data_m,20)

ggplot(data_m, aes(x=variable, y=as.factor(v_label), fill=as.factor(value))) +
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
    select(v_label, variable, value); head(data_m)
  
  # arrange by target
  #   nam1 = str_split_fixed(as.character(data_m$variable), '_', 2); head(nam1)
  #   target = nam1[,1]; head(target)
  #   data_m$variable = nam1[,2]; head(data_m)
  #   data_m$target = target; head(data_m)
  #   data_m = data_m %.% #--arrange by target!!!!!!!!!
  #     arrange(target)
  
  
  ggplot(data_m, aes(x=variable, y=as.factor(v_label), fill=as.factor(value))) +
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
data_melt$value[data_melt$value == 0] <- NA # head(data_melt)

ggplot(data_melt, aes(x=variable, y=as.factor(rgn_id), fill=as.factor(value))) +
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
