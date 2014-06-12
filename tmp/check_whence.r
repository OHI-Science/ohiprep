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


l = read.csv('tmp/layers_2014 - datalayers.csv', skip=1, na=''); head(l); names(l)

l_whence = l %.%
  mutate(whence2013 = file.path(dir_neptune_data, dir_whence_2013a, whence_2013a)) %.%
  filter(!is.na(whence_2013a)) %.%
  select(tar = target, 
         whence2013) %.%
  arrange(tar)

l_whence$tar = gsub('_.*', '', l_whence$tar) # for name tracking purposes below

# read in all files, summarize and combine
d.all =  matrix(nrow=0, ncol=0)
count = 0 # this facilitates the cbinding below

for (f in l_whence$whence2013){ # f = '/Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_gdppcppp_2013a_whence.csv'
  
  count = count + 1
  fname = strsplit(f, '/')
  fname2 = strsplit(unlist(fname)[7], '_')
  d = read.csv(f); tail(d)
  
#   # make sure all rgn_ids are present (since CW trends are gapfilled a bit later too)
#   rgns = read.csv('/Users/julialowndes/github/ohiprep/src/LookupTables/eez_rgn_2013master.csv'); head(rgns)
#   tp = rgns %.% 
#     select(rgn_id = rgn_id_2013) %.%
#     anti_join(d,
#               by='rgn_id') %.%
#     arrange(rgn_id); tp
#   tp$rgn_id = tp$rgn_id < 255; tp
  
  # summary statistics
  dstats = d %.%
    group_by(rgn_id) %.% 
    summarise(count_rgn_id = n(), 
              count_OD     = sum(whencev01 == 'OD'), # 'OD' means 'original data'
              perc_OD      = count_OD / count_rgn_id) %.%
    arrange(rgn_id) %.%
    select(rgn_id, perc_OD); head(dstats)
  
  #rename and reformat
  names(dstats) = c('rgn_id', paste(l_whence$tar[count], '_', unlist(fname2)[2], '_', unlist(fname2)[3], sep='')) 
  
  if(count == 1){
    d.all = rbind(d.all, dstats)
  } else {
    dstats$rgn_id = NULL
    d.all = cbind(d.all, dstats)
  }
}
head(d.all) # write.csv(d.all, 'whence_2013afiles.csv', row.names = F)


## collapse by goal ---- this 
# d.all2 = d.all 
# nam1 = str_split_fixed(as.character(names(d.all2)), '_', 2); (nam1)
#   target = nam1[,1]; head(target)
#   data_m$variable = nam1[,2]; head(data_m)
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
d.all_lab = d.all %.%
  left_join(georegion_labels %.%
              select(rgn_id, r2_label, v_label),
            by = 'rgn_id'); head(d.all_lab)

# prepare data for melting
data <- d.all_lab %.%
  filter(!is.na(r2_label)); head(data)

data_melt <- melt(data, id.vars = c('rgn_id', 'r2_label', 'v_label'), id="rgn_id")
data_melt$value[data_melt$value == 0] <- NA; head(data_melt)

# for each georegion, create a heatmap with existing data 
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
    theme(axis.text.y = element_text(size=6),
          axis.text.x = element_text(angle=90, vjust=1)) +
    scale_fill_discrete(name  = '', breaks=c(1,2), labels=c('original', 'gapfilled'))
  
  ggsave(file.path('tmp/whence_figures', paste('whence_', g, '.png', sep='')), width=5, height=15)
  
}


## Mel's original heat map of whence data ----
# by MFrazier May 2014
library(ggplot2)

data <- d.all # read.csv("whence_2013afiles.csv")
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

library(dplyr)
georegions = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn_georegions_long_2013b.csv', na.strings='') %.%
  dcast(rgn_id ~ level, value.var='georgn_id')

gl = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn_georegions_labels_long_2013b.csv', na.strings='')
georegion_labels = gl  %.%    
  mutate(level_label = sprintf('%s_label', level)) %.%
  dcast(rgn_id ~ level_label, value.var='label') %.%
  left_join(
    gl %.%
      select(rgn_id, v_label=label),
    by='rgn_id')

  # setup data for georegional gapfilling (remove Antarctica rgn_id=213)
  d_g = gapfill_georegions(
    data = d %.%
      filter(rgn_id!=213) %.%
      select(rgn_id, year, Xtr),
    georegions = georegions,
    georegion_labels = georegion_labels)


#----fin
