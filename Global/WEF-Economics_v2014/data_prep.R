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

# TODO: make this work across machines
# configuration based on machine name
# conf = list(
#   'AMPHITRITE'=list(  # BB's Windows 8 on MacBook Pro VMWare
#     dir_git = 'G:/ohigit',
#     dir_big = 'N:/'       )
#   'gore.nceas.ucsb.edu'=list(  # JSL's iMac
#     dir_git = 'G:/ohigit',
#     dir_big = 'N:/'       ))[[Sys.info()['nodename']]]

# setup
source('/Users/jstewart/github/ohiprep/src/R/ohi_clean_fxns.R') # also fix this directory
dir1 = ('/Users/jstewart/github/ohiprep/Global/WEF-Economics_v2014') # also fix this directory
wd = file.path(dir1, 'raw')
setwd(wd)

library(reshape2)
library(gdata)
options(max.print=5E6)

# read in files
d.gci = read.csv('WEF_GCI_2013-2014_Table3_reformatted.csv'); head(d.gci)

# clean up
gci = d.gci[c(1,3)]; names(gci) = c('country','score'); head(gci) # only keep relevant columns
gci[,'country'] = gsub('Korea', 'South Korea', gci[,'country']) 

# rescale -- this makes export_rescaled_layer.R from GL-WEF-Economics_v2013 obsolete
rng = c(1, 7)
gci = within(gci,{
    score = (score - rng[1]) / (rng[2] - rng[1])
}); head(gci)

## run add_rgn_id and save
uifilesave = file.path(wd, 'WEF_GCI_rescaled-cleaned.csv')
add_rgn_id(gci, uifilesave)

## georegional gapfilling with add_gapfill.r 
cleaned_data1 = read.csv(uifilesave)
s_island_val = NA # assign what southern islands will get. 
layersave = file.path(wd, 'rgn_wef_gci_2014a_rescaled_tmp.csv')    
add_gapfill_singleyear(cleaned_data1, layersave, s_island_val)

# have to do it here since add_gapfill_singleyear can't handle a year column
gcitmp = read.csv(layersave)
gcitmp = cbind(gcitmp,rep(2014,length(gcitmp[,1]))) #add year
names(gcitmp) = c('country','score', 'year');
layersave2 = file.path(dir1, 'data', 'rgn_wef_gci_2014a_rescaled.csv')    
write.csv(gcitmp, layersave2, na = '', row.names=FALSE)





# Final steps:
## Layernames files were combined into rgn_wef_BHgapfilling.csv; BH then gapfilled things that would have needed an r0 value by hand. [all Pacific Islands]
##this file was then saved as separate two separate files with the correct columns (year = 2013 all the way was added, and label was changed from 'IndexScore2013' to simply 'score'
## North Korea was also given the minimum value (see BH's gapfilling notes in rgn_wef_BHgapfilling.csv)
# FINAL files: rgn_wef_gci_2013a.csv and rgn_wef_ttci_2013a.csv





d.ttci = read.csv('WEF_TTCI_2012-2013_Table1_reformatted.csv')
d.ttci2 = cbind(d.ttci,rep('ttci',length(d.ttci[,1])))
names(d.ttci2) = c('Region','Rank2013','IndexScore2013','Rank2011','layer')

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









