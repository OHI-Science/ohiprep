# data_prep.R: reformat and add rgn_ids to World Economic Forum (WEF) data 
# by JStewartLowndes Mar2014; updated from 'clean_WEF.R' by JStewart in May 2013)
#   Data: 
#       Global Competitiveness Index (GCI)
#       Travel and Tourist Competitiveness Index (TTCI)
#   read in individual files
#   call add_rgn_id.r to add OHI region_ids
#   georegional gapfilling with add_gapfill.r 
#   final processing by hand: see end of script

# TODO: make this work across machines
# configuration based on machine name
# conf = list(
#   'AMPHITRITE'=list(  # BB's Windows 8 on MacBook Pro VMWare
#     dir_git = 'G:/ohigit',
#     dir_big = 'N:/'       )
#   'gore.nceas.ucsb.edu'=list(  # JSL's iMac
#     dir_git = 'G:/ohigit',
#     dir_big = 'N:/'       ))[[Sys.info()['nodename']]]

dir1 = ('/Users/jstewart/github/ohiprep/Global/WEF-Economics_v2014/') # for now...
setwd(file.path(dir1))

library(reshape2)
library(gdata)
options(max.print=5E6)
source('/Volumes/local_edit/src/R/jstewart/ohi_clean_fxns.r') 


# read in files

d.gci = read.csv('WEF_GCI_2013-2014_Table3_reformatted.csv')
d.gci2 = cbind(d.gci[,c(1,2,3,5)],rep('gci',length(d.gci[,1])))

d.ttci = read.csv('WEF_TTCI_2012-2013_Table1_reformatted.csv')
d.ttci2 = cbind(d.ttci,rep('ttci',length(d.ttci[,1])))

names(d.gci2) = c('Country','Rank2013','IndexScore2013','Rank2011','layer')
names(d.ttci2) = c('Country','Rank2013','IndexScore2013','Rank2011','layer')

# concatenate f files
d.all = rbind(d.gci2[c(1,3,5)], d.ttci2[c(1,3,5)])


## run add_rgn_id and save
uifilesave = paste(dir1, 'data/', 'GL-WEF-Economics_v2013-cleaned.csv', sep='')
add_rgn_id(d.all, uifilesave)


## georegional gapfilling with add_gapfill.r 

cleaned_data1 = read.csv(uifilesave)

layer_uni = unique(cleaned_data1$layer)
layernames = sprintf('rgn_wef_%s_tmp.csv', tolower(layer_uni))
s_island_val = NA # assign what southern islands will get. 

for(i in 1:length(layer_uni)) {
  cleaned_layer = cleaned_data1[cleaned_data1$layer == layer_uni[i],]
  cleaned_layer$layer = NULL
  
  layersave = paste(dir1, 'raw/', layernames[i], sep='')    
  add_gapfill_singleyear(cleaned_layer, layersave, s_island_val)
}

# Final steps:
## Layernames files were combined into rgn_wef_BHgapfilling.csv; BH then gapfilled things that would have needed an r0 value by hand. [all Pacific Islands]
##this file was then saved as separate two separate files with the correct columns (year = 2013 all the way was added, and label was changed from 'IndexScore2013' to simply 'score'
## North Korea was also given the minimum value (see BH's gapfilling notes in rgn_wef_BHgapfilling.csv)
# FINAL files: rgn_wef_gci_2013a.csv and rgn_wef_ttci_2013a.csv









