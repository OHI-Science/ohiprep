# data_prep.R

# Prepare FAO commodities data for Natural Products goal. 
# By JSLowndes Jun2014; File was originally clean_FAOcommodities.r:(by JStewart Apr2013)

#   read in individual files
#   remove Totals row
#   remove/translate FAO data codes (F, ..., -, 0 0)
#   add identifier column
#   concatenate data from each file into a single file
#   run add_rgn_id.r (function by J. Stewart, B. Best)
#   gafilling: rules like for Mariculture: gapfilling category = TP.
#   save single files for each commodity 
  

# setup ----

# load libraries
library(reshape2)
library(gdata)
library(dplyr)

source('/Volumes/local_edit/src/R/jstewart/ohi_clean_fxns.R') 
dir1 = ('/Volumes/data_edit/model/GL-FAO-Commodities_v2009/')
setwd(paste(dir1,'raw', sep=''))

# from get paths configuration based on host machine name
source('src/R/common.R') # set dir_neptune_data
# Otherwise, presume that scripts are always working from your default ohiprep folder
dir_d = 'Global/FAO-Commodities_v2011'

# get functions
source('src/R/ohi_clean_fxns.R')


## read in and process files ----
# rewrite this as only 1 file with all commodities
fao_comm = matrix(nrow=0, ncol=0)
for (f in list.files(pattern=glob2rx('*.csv'))){
  
  d = read.csv(f)
  n = names(d)
  d.1 = d
  d.1 <- d.1[d.1[,1] != "Totals",] # remove Totals line
  d.1 <- d.1[d.1[,1] != "Yugoslavia SFR",] 
  
  d.m = melt(data=d.1, id.vars=names(d)[1:3], variable.name='year')
  names(d.m) = c('country','commodity','trade','year','value')
  
  # remove FAO indicators
  d.m$value = trim(d.m$value)
  d.m$value = sub(' F', '', d.m$value, fixed=T) # FAO denotes with F when they have estimated the value using best available data
  d.m$value = sub('0 0', '0.1', d.m$value, fixed=T)# FAO denotes something as '0 0' when it is > 0 but < 1/2 of a unit. 
  d.m$value = sub('-', '0', d.m$value, fixed=T) # FAO's 0.
  d.m$value = sub('...', 'NA', d.m$value, fixed=T)
  d.m$value = as.numeric(d.m$value)
  
  # add identifier column
  a = strsplit(f, '-', fixed=FALSE)
  a.1 = strsplit(unlist(a)[3], '_')
  d.m$layer = rep.int(unlist(a.1)[1], length(d.m$value))  
  
  # miscellaneous corals and shells are double-counted: they are in both shells and in corals. Remove from one. 
  if(d.m$layer[1] == 'Coral'){
    d.m <- d.m[(d.m$commodity != 'Miscellaneous corals and shells'),]
  }
  
  #recast
  d.tgf = dcast(d.m, country + commodity + trade + layer ~ year) 
  
  
  # concatenate and save as a huge file so can make some decisions by hand
  fao_comm = rbind(fao_comm, d.tgf)
}

# sort by country
fao_comm2 = fao_comm[order(fao_comm$country, fao_comm$layer, fao_comm$commodity),]
faofilesave = paste(dir1, 'data/', 'GL-FAO-AllCombined_v2009-rgn.csv', sep='')
add_rgn_id(fao_comm2, faofilesave)
# process by hand: assign all Netherlands Antilles children 1/6 of total and save as GL-FAO-AllCombined_v2009-rgn-processed.csv


## gapfilling, category TP, as was done in Mariculture. read it in, melt, recast and aggregate by sum, and then remelt and save as individual commodities. 

# read in file that had a few modifications by hand (see the README.md)
p = read.csv(paste(dir1, 'data/', 'GL-FAO-AllCombined_v2009-rgn-processed.csv', sep=''))
p.1 = p

yrt_nadex = which(is.na(p.1$X2009)) # ID where most recent year t are NAs
yrtminus1_nadex = which(is.na(p.1$X2008)) # ID where year t-1 are NAs
p.1$X2008[yrtminus1_nadex] = 0 # first fill most recent year NAs with 0 temporarily
p.1$X2009[yrt_nadex] = p.1$X2008[yrt_nadex] # then replace t NAs with the t-1 values
p.1$X2008[yrtminus1_nadex] = NA # then change the 2010 value back to NA (probably won't matter for status, but just in case. 
p.1$gapfilled = rep(0, dim(p.1)[1])
p.1$gapfilled[yrt_nadex] = 1

p.1end = dim(p.1)[2]
p.m = melt(data=p.1, id.vars=names(p.1)[c(1:4,p.1end)], variable.name='year')
p.m$year = sub('X', '', p.m$year, fixed=F) # fix years: remove R's X
p.m$year = as.numeric(as.character(p.m$year))

# get ready to cast, melt and save as data layer for each layer
p.m$layer = gsub('OrnamentalFish', 'orn', p.m$layer) 
p.m$layer = gsub('Seaweeds', 'swd', p.m$layer) 
p.m$layer = gsub('Shells', 'shl', p.m$layer) 
p.m$layer = gsub('Sponges', 'spg', p.m$layer) 
p.m$layer = gsub('Coral', 'crl', p.m$layer) 
p.m$layer = gsub('FishOil', 'oil', p.m$layer) 

layer_uni = unique(p.m$layer)
layernames = sprintf('rgn_fao_%s.csv', tolower(layer_uni))

for(i in 1:length(layer_uni)) {
  p.mi = p.m[p.m$layer == layer_uni[i],]
  
  # transpose with sum aggregate function to sum over the commodity subcategory
  p.ti = dcast(p.mi, rgn_id + rgn_nam + layer + gapfilled ~ year, fun.aggregate = sum) 
  
  # remelt
  p.mi2 = melt(data=p.ti, id.vars=names(p.ti)[1:4], variable.name='year')
  
  # prep and save
  p.mi3 = p.mi2[c(1,6,5,4)]
  p.mi3 = p.mi3[order(p.mi3$rgn_id, p.mi3$year),]

  layersave = paste(dir1, 'data/', layernames[i], sep='') 
  write.csv(p.mi3, layersave, na = '', row.names=FALSE)
}
  

