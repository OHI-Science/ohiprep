# check_whence.r 
# by JSLowndes, May 2014. 

# combine certain data layers from global OHI, then make a heatmap displaying 
# whether a layer was gapfilled or not for each country. see gapfilling code
# categories rules: https://docs.google.com/a/nceas.ucsb.edu/spreadsheet/ccc?key=0AjLReAQzT2SVdGNZcDNYRjlfSEYteEFyQnotZ0ZjNmc&usp=drive_web#gid=9

# original data is represented with 0, gapfilled with 1. Individual layers are summed and divided by the amount of total layers. Example: if in a single region, 2 layers are original and 2 are gapfilled and there are 4 total, the score would be (0+0+1+1)/4 = .5

# create whence files TUES:::::
# track alien invasives # in neptune_data:model/GL-NCEAS-Resilience_v2013a/data disaggreate
# pathogens?
# msi #in neptune_data:model/GL-NCEAS-Resilience_v2013a/data disaggreate


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
anx = read.csv('tmp/layers_global2013_annex.csv'); anx  # note TRstatus is operated on differently than TR

## track total number of layers per goal and pressures/resilience, with a few manual overrides ----
# see decisions in https://docs.google.com/a/nceas.ucsb.edu/spreadsheet/ccc?key=0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE&usp=drive_web&pli=1#gid=7
layers_tmp = l %.%
  select(targets, layer, dir_whence_2013a, whence_2013a) %.%
  filter(!targets %in% c('spatial',                       # not included in the total at all       
                         'LE', 'ECO', 'LIV', 'LIV ECO', 'REV',   # all others: not included since handled separately
                         'FIS', 'MAR', 'FIS NP',
                         'HAB',              
                         'ICO', 'LSP'))

# save file so we can inspect pressures, resilience used
layers_tmpsave = layers_tmp %.%
  filter(targets %in% c('pressures', 'pressures CW', 'resilience'))
write.csv(layers_tmpsave, file.path('tmp/included_press_resil_2013a_whence.csv'), na = '', row.names=FALSE)

# continue with filtering and the final count
# this is a ridiculously laborious way to just exclude the 'exclude' rows but keep the NAs! But was losing the NAs in other approaches
idx1 = as.data.frame(which(layers_tmp$dir_whence_2013a == 'exclude')); names(idx1) = 'idx'
idx2 = as.data.frame(1:length(layers_tmp$dir_whence_2013a)); names(idx2) = 'idx'
idx_keep = idx2 %.%
  anti_join(idx1, by='idx')

# layers to include
layers_include = layers_tmp[idx_keep$idx,] 
stopifnot(sum(is.na(layers_include$targets)) == 0) # was getting weird additions of NAs

layers_include_anx = rbind(layers_include, anx) # combine with annex files

# select only layers with gapfilling
l_whence_tmp = layers_include_anx %.%
  filter(!is.na(dir_whence_2013a)) %.%
  mutate(root_whence = str_split_fixed(as.character(dir_whence_2013a), ':', 2)[,1],
         dir_whence  = str_split_fixed(as.character(dir_whence_2013a), ':', 2)[,2]) %.%
  select(tar = targets,
         root_whence,
         dir_whence,
         whence_2013a)

# point to the different directories and combine
l_nep = l_whence_tmp %.%
  filter(root_whence == 'neptune_data') %.%
  mutate(fp = file.path(dir_neptune_data, dir_whence, whence_2013a))
l_git = l_whence_tmp %.%
  filter(root_whence == 'ohiprep') %.%
  mutate(fp = file.path(dir_whence, whence_2013a))
l_glo = l_whence_tmp %.%
  filter(root_whence == 'ohi-global') %.%
  mutate(fp = file.path('../ohi-global', dir_whence, whence_2013a))

l_whence = rbind(l_nep, l_git, l_glo) %.%
  filter(tar != 'HAB CS CP') %.%
  select(tar, root_whence, fp) %.%
  arrange(tar)

# for name tracking purposes below
l_whence$tar = gsub('_.*', '', l_whence$tar) 
l_whence$tar = gsub(' .*', '', l_whence$tar) 
layers_include_anx$targets = gsub('_.*', '', layers_include_anx$targets) 
layers_include_anx$targets = gsub(' .*', '', layers_include_anx$targets) 


# produce final count of number layers per goal/press/resil (n_lpg)
n_lpg_tmp = layers_include_anx %.%
  filter(targets != 'HAB CS CP') %.% # we need to keep the NAs
  group_by(targets) %.%
  summarize(layers_tot_n = n())

n_lpg = rbind(n_lpg_tmp, data.frame(targets = c('HAB', 'CS', 'CP'), layers_tot_n = 3 ))
n_lpg$layers_tot_n[n_lpg$targets == 'AO']  = 2     # see below in ## add non-gapfilled layers 
n_lpg$layers_tot_n[n_lpg$targets == 'TR']  = 4 
n_lpg$layers_tot_n[n_lpg$targets == 'FP']  = 1 
n_lpg$layers_tot_n[n_lpg$targets == 'NP']  = 1 
n_lpg$layers_tot_n[n_lpg$targets == 'SPP'] = 1 
n_lpg


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
head(d_f)


## deal with habitats and combine to d_f ----

l_hab = l_whence_tmp %.%
  filter(tar == 'HAB CS CP')

d_h =  matrix(nrow=0, ncol=0) 

for (f in list.files(path = file.path(dir_neptune_data, l_hab$dir_whence[1]), pattern=glob2rx('*gapfill.csv'), full.names=T)) {  
  # f = "/Volumes/data_edit/model/GL-NCEAS_Habitat_health-v2013a/tmp/Habitat_whence/HAB_gapfill.csv" 
  # note: this assumes that all layers are in the same path as indicated in layers_global (l). 

  # for labeling
  fname = strsplit(f, '/')
  lis = lapply(fname, lapply, length) # roundabout way of finding the last list element in the path
  names(lis) = lapply(fname, length)
  fname2 = strsplit(unlist(fname)[as.numeric(names(lis))], '_')
  goal_id = unlist(fname2)[1]

  
  # now work with the files
  d = read.csv(f); tail(d)
  dnam = names(d)
  names(d)[4] = 'whencev01'
  d$whencev01[is.na(d$whencev01)] = 0 # even though there is no habitat there, for display purposes, there is no gapfilling that occurs there
  
  c = d %.%
    filter(metric == 'condition') %.%
    select(rgn_id = rgn_id_2013,
           whencev01)
  names(c)[names(c) == 'whencev01'] = paste(goal_id, '_hab_condit', sep='')
  
  t = d %.%
    filter(metric == 'trend') %.%
    select(rgn_id = rgn_id_2013,
           whencev01)
  names(t)[names(t) == 'whencev01'] = paste(goal_id, '_hab_trend', sep='')
  
  d_h_tmp = c %.%
    left_join(t, by = 'rgn_id') %.%
    mutate(extent = 0) %.%
    arrange(rgn_id); head(d_h_tmp)
  names(d_h_tmp)[names(d_h_tmp) == 'extent'] = paste(goal_id, '_hab_extent', sep='')
  
  if(dim(d_h)[1] == 0){
    d_h = rbind(d_h, d_h_tmp)
  } else {
    d_h_tmp$rgn_id = NULL
    d_h = cbind(d_h, d_h_tmp)
  }
  
}

# combine habitat layers with the rest of the layers
d_fh = d_f %.%
  left_join(d_h, by='rgn_id'); head(d_fh)


## add non-gapfilled elements ----
## add non-gapfilled layers                         
# d_fl = d_fh %.%           # data from files and layers ####TODO: can get rid of this: don't actually need these as placeholders as long as only remaining list is included from n_lpg
#   mutate(CW_ciesin_cpop  = 0,
#          TR_wttc_empd    = 0,
#          pressures_sp_genetic    = 0,
#          pressures_targetharvest = 0, 
#          pressures_fishing       = 0,
#          resilience_msi = 0); head(d_fl)

## add non-gapfilled goals
d_flg = d_fh %.% # data from files, layers, and goals
  mutate(FP  = 0,
         NP  = 0,
         SP  = 0,
         SPP = 0); head(d_flg)

# num layers per goal; see https://docs.google.com/a/nceas.ucsb.edu/spreadsheet/ccc?key=0At9FvPajGTwJdEJBeXlFU2ladkR6RHNvbldKQjhiRlE&usp=drive_web&pli=1#gid=7
# n_lpg = list( # TODO: delete all of this and just use above with l_prop (rename to n_lpg)
#   AO = 2,
#   BD = 1,
#   CP = 3,
#   CS = 3,
#   CW = 4,
#   FP = 1, 
#   LE = 1,
#   NP = 1, 
#   SP = 1,
#   TR = 4,    ## TODO: TR is a placeholder
#   pr = 7, ## TODO: placeholder--need to finalize # ALL LAYERS: MAKE THIS  the count from the pressures matrix
#   re = 7) ## TODO: placeholder--need to finalize

## collapse by goal ----
nam = str_split_fixed(as.character(names(d_flg)), '_', 2)[,1] # identify goal prefixes

# CW, TR, resil, press
nam_cp = names(d_flg)[nam == 'CP']
nam_cs = names(d_flg)[nam == 'CS']
nam_ha = names(d_flg)[nam == 'HAB']
nam_cw = names(d_flg)[nam == 'CW']
nam_tr = names(d_flg)[nam == 'TR'] 
nam_ao = names(d_flg)[nam == 'AO']
nam_le = names(d_flg)[nam == 'ECO' | nam == 'LIV']
nam_pr = names(d_flg)[nam == 'pressures']
nam_re = names(d_flg)[nam == 'resilience']

d_flg$cp_sum = rowSums(d_flg[,nam_cp], na.rm=T)
d_flg$cs_sum = rowSums(d_flg[,nam_cs], na.rm=T)
d_flg$ha_sum = rowSums(d_flg[,nam_ha], na.rm=T)
d_flg$cw_sum = rowSums(d_flg[,nam_cw], na.rm=T)
d_flg$tr_sum = rowSums(d_flg[,nam_tr], na.rm=T)
d_flg$ao_sum = rowSums(d_flg[,nam_ao], na.rm=T)
d_flg$le_sum = rowSums(d_flg[,nam_le], na.rm=T)
d_flg$pr_sum = rowSums(d_flg[,nam_pr], na.rm=T)
d_flg$re_sum = rowSums(d_flg[,nam_re], na.rm=T); head(d_flg) 

d_g = d_flg %.% 
  mutate(CP  = ( cp_sum / n_lpg$layers_tot_n[ n_lpg$targets == 'CP'  ] ), 
         CS  = ( cs_sum / n_lpg$layers_tot_n[ n_lpg$targets == 'CS'  ] ), 
         HAB = ( ha_sum / n_lpg$layers_tot_n[ n_lpg$targets == 'HAB' ] ), 
         CW  = ( cw_sum / n_lpg$layers_tot_n[ n_lpg$targets == 'CW'  ] ), 
         TR  = ( tr_sum / n_lpg$layers_tot_n[ n_lpg$targets == 'TR'  ] ), 
         AO  = ( ao_sum / n_lpg$layers_tot_n[ n_lpg$targets == 'AO'  ] ), 
         LE  = ( le_sum / sum(n_lpg$layers_tot_n[ n_lpg$targets == 'ECO' | n_lpg$targets == 'LIV' ]) ), 
         pr  = ( pr_sum / n_lpg$layers_tot_n[ n_lpg$targets == 'pressures'  ] ), 
         re  = ( re_sum / n_lpg$layers_tot_n[ n_lpg$targets == 'resilience' ] )) 
d_g = d_g[,(!names(d_g) %in% c(nam_cp, 'cp_sum',
                               nam_cs, 'cs_sum',
                               nam_ha, 'ha_sum',
                               nam_cw, 'cw_sum',
                               nam_tr, 'tr_sum',
                               nam_ao, 'ao_sum',
                               nam_le, 'le_sum',
                               nam_pr, 'pr_sum',
                               nam_re, 're_sum'))]; head(d_g) 

# further collapses necessary for TR and BD 
nm = str_split_fixed(as.character(names(d_g)), '_', 2)[,1] # identify goal prefixes

nm_bd = names(d_g)[nm == 'HAB'      | nm == 'SPP']
d_g$bd_sum = rowSums(d_g[,nm_bd], na.rm=T)

nm_tr = names(d_g)[nm == 'TRstatus' | nm == 'TR']
d_g$tr_sum = rowSums(d_g[,nm_tr], na.rm=T) 

d_g2 = d_g %.%
  mutate(BD = ( bd_sum / 2 ), 
         TR2 = ( pmin(tr_sum, 1) ) ) # need to cap this at 1 so that gapfilling at the status level overrules
d_g2 = d_g2[,(!names(d_g2) %in% c(nm_bd, 'bd_sum',
                                  nm_tr, 'tr_sum'))]; head(d_g2) 
names(d_g2)[names(d_g2) == 'TR2'] = 'TR'


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
d_g_lab = d_g2 %.%
  left_join(georegion_labels %.%
              select(rgn_id, r2_label, v_label),
            by = 'rgn_id'); head(d_g_lab)


# georegions lists for figures ----
#for splitting the figures into manageable portions
grps = 3
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

## different heatmaps, by group
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
  arrange(variable, desc(r2_label)) %.%
  select(r2_label,v_label, variable, prop_gf); head(data_m)

ggplot(data_m, aes(x=variable, y=factor(v_label, levels=unique(v_label)), fill=as.factor(prop_gf))) +   # levels=unique(v_label) to override alphabetical order
  geom_raster() +
  labs(y = '', x = "") + 
  theme(axis.text.y = element_text(size=10),
        axis.text.x = element_text(angle=90, vjust=1)) +
  scale_fill_brewer(name  = '', type = "seq", palette = (1),labels=c('original', 'partially gapfilled', 'fully gapfilled'))

ggsave(file.path('tmp/whence_figures', paste('OHI_2013_heatmap', j, '.png', sep='')), width=10, height=15)

}




## Mel's original heat map of whence data ----
# by MFrazier May 2014
# library(ggplot2)
# 
# data <- d_f # read.csv("whence_2013afiles.csv")
# data_melt <- melt(data, id="rgn_id")
# data_melt$prop_gf[data_melt$prop_gf == 0] <- NA # head(data_melt)
# 
# ggplot(data_melt, aes(x=variable, y=as.factor(rgn_id), fill=as.factor(prop_gf))) +
#   geom_raster() +
#   labs(y="region ID", x="") + 
#   theme(axis.text.y = element_text(size=6),
#         axis.text.x = element_text(angle=90, vjust=1)) +
#   scale_fill_discrete(name  ="Whence", breaks=c(1), labels="")
# ggsave("whence_example.png", width=5, height=15)


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
