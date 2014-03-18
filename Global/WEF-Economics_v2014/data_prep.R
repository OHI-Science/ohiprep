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
cleaned_data1 =  read.csv(uifilesave)
s_island_val = NA # assign what southern islands will get. 
dirsave = file.path(dir_d, 'data')
layersave = 'rgn_wef_gci_2014a' # don't name it rescaled--it just is. 
cleaned_data1 = mutate(cleaned_data1, year=rep(2014)) # required for running add_gapfill (add_gapfill_oneyear.r is deleted)
add_gapfill(cleaned_data1, dirsave, layersave, s_island_val)


## last step: give North Korea the minimum value ----

gapfilled_data = read.csv(file.path(dirsave, paste(layersave, '.csv', sep=''))); head(gapfilled_data)
whence_data = read.csv(file.path(dirsave, paste(layersave, '_whencev01.csv', sep=''))); head(whence_data)

# find minimum
gapfilled_data_noNA = gapfilled_data %.%
  filter(!is.na(score))

s_min = min(gapfilled_data_noNA$score)

# replace North Korea (rgn_id == 21) in gapfilled_data
gapfilled_data$score[gapfilled_data$rgn_id == 21] = s_min 

gapfilled_data$whencev01 = as.character(gapfilled_data$whencev01)
gapfilled_data$whencev01[gapfilled_data$rgn_id == 21] = 'XH'

gapfilled_data$whence_choice = as.character(gapfilled_data$whence_choice)
gapfilled_data$whence_choice[gapfilled_data$rgn_id == 21] = 'XH'


# replace North Korea (rgn_id == 21) in whence_data
whence_data$score[whence_data$rgn_id == 21] = s_min 

whence_data$whence_choice = as.character(whence_data$whence_choice)
whence_data$whence_choice[whence_data$rgn_id == 21] = 'XH' 

whence_data$rgn_id_whence[whence_data$rgn_id == 21] = 'XH' 
whence_data$rgn_nam_whence = as.character(whence_data$rgn_nam_whence)
whence_data$rgn_nam_whence[whence_data$rgn_id == 21] = 'XH' 
whence_data$score_whence[whence_data$rgn_id == 21] = 'XH' 

# whence_data[whence_data$rgn_id == 21,]

# save
write.csv(gapfilled_data, file.path(dirsave, paste(layersave, '.csv', sep='')), na = '', row.names=FALSE)   
write.csv(whence_data, file.path(dirsave, paste(layersave, '_whencev01.csv', sep='')), na = '', row.names=FALSE)   




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