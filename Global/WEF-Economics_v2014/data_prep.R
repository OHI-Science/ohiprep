# data_prep.R: reformat and add rgn_ids to World Economic Forum (WEF) data 
#
# by JStewartLowndes Mar2014; updated from 'clean_WEF.R' by JStewart in May 2013
#   Data: 
#       Global Competitiveness Index (GCI)
#       Travel and Tourist Competitiveness Index (TTCI)
#   read in individual files
#   call add_rgn_id.r to add OHI region_ids
#   georegional gapfilling with add_gapfill.r 
#   final processing by hand: see end of script


# setup ----

# load libraries
library(reshape2)
library(gdata)
library(dplyr)

# from get paths configuration based on host machine name
source('src/R/common.R') # set dir_neptune_data
# Otherwise, presume that scripts are always working from your default ohiprep folder
dir_d = 'Global/WEF-Economics_v2014'

# get functions
source('src/R/ohi_clean_fxns.R')

# read in files ----
d.gci = read.csv(file.path(dir_d, 'raw', 'WEF_GCI_2013-2014_Table3_reformatted.csv')); head(d.gci)

# clean up
gci = d.gci %.%
  select(country = Country, 
         score = Score_1_to_7); head(gci)
gci[,'country'] = gsub('Korea', 'South Korea', gci[,'country']) 

# rescale -- this makes export_rescaled_layer.R from GL-WEF-Economics_v2013 obsolete
rng = c(1, 7)
gci = within(gci,{
    score = (score - rng[1]) / (rng[2] - rng[1])}); head(gci)

## run add_rgn_id and save ----
uifilesave = file.path(dir_d, 'raw', 'WEF_GCI_rescaled-cleaned.csv')
add_rgn_id(gci, uifilesave)

## georegional gapfilling with add_gapfill.r ----
cleaned_data1 = read.csv(uifilesave)
s_island_val = NA # assign what southern islands will get. 
dirsave = file.path(dir_d, 'data')
layersave = 'rgn_wef_gci_2014a_rescaled'
cleaned_data1 = mutate(cleaned_data1, year=rep(2014)) # required for running add_gapfill
add_gapfill(cleaned_data1, dirsave, layersave, s_island_val)



# Final steps:
## Layernames files were combined into rgn_wef_BHgapfilling.csv; BH then gapfilled things that would have needed an r0 value by hand. [all Pacific Islands]
##this file was then saved as separate two separate files with the correct columns (year = 2013 all the way was added, and label was changed from 'IndexScore2013' to simply 'score'
## North Korea was also given the minimum value (see BH's gapfilling notes in rgn_wef_BHgapfilling.csv)
# FINAL files: rgn_wef_gci_2013a.csv and rgn_wef_ttci_2013a.csv



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
# # Final steps:
# ## Layernames files were combined into rgn_wef_BHgapfilling.csv; BH then gapfilled things that would have needed an r0 value by hand. [all Pacific Islands]
# ##this file was then saved as separate two separate files with the correct columns (year = 2013 all the way was added, and label was changed from 'IndexScore2013' to simply 'score'
# ## North Korea was also given the minimum value (see BH's gapfilling notes in rgn_wef_BHgapfilling.csv)
# # FINAL files: rgn_wef_gci_2013a.csv and rgn_wef_ttci_2013a.csv
# 
# 
# 
# 
# 
# 
# 
# 
# 
