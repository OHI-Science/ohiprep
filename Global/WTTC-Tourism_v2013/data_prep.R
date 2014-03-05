# data_prep.R. 
# Add rgn_ids for World Travel and Tourism Council (WTTC)
# Previously had been named clean_WTTC.r (by JStewart May2013). This script created by JStewartLowndes Mar2014.
#
#   read in .xls files from the WTTC raw folder downloaded from www.wttc.org/research/economic-data-search-tool
#   data_prep.R. will reformat so that data aren't on every other line like the .xls table
#   files must be accessed to the same year (eg end year 2013, assuming all after that is projected)
#   adds identifier and units columns, and then runs add_rgn_id.r. 
#   no georegional gapfilling -- but save as separate files  

# setup
source('/Users/jstewart/github/ohiprep/src/R/ohi_clean_fxns.R') # also fix this directory
dir1 = ('/Users/jstewart/github/ohiprep/Global/WTTC-Tourism_v2013') # also fix this directory
wd = file.path(dir1, 'raw')
setwd(wd)

library(mgcv) # for troubleshooting below
library(reshape2)
library(gdata) # to enable read.xls
options(max.print=5E6)

# read in files
d.all =  matrix(nrow=0, ncol=0)
for (f in list.files(pattern=glob2rx('*.xls'))){ 
  
  d = read.xls(f, sheet=1, blank.lines.skip=F, skip=7, header=T, check.names=F); head(d)
  
  # get the rows to line up properly and then remove every other line (weird spacing issue in .xls)
  metric = as.character(d[2,1])
  col1 = d[1:dim(d)[1]-1,1]
  col1 = c(metric, as.character(col1))
  d[,1] = col1
  d.1 = d[d[,1] != metric,]; head(d.1) # remove all the spacer lines 
  
  # some name cleaning. # not the most elegant way, but it works!
  d.1 <- d.1[d.1[,1] != "Other Oceania",] # remove
  d.1[,1] = gsub('St Kitts', 'Saint Kitts and Nevis', d.1[,1]) 
  d.1 = rbind(d.1, d.1[d.1[,1] == 'Former Netherlands Antilles',]) # FNA must be split into 'Sint Maarten', 'Curacao', 'Bonaire','Saba', 'Sint Eustasius' (Aruba reported separately)
  d.1[length(d.1[,1]),1] = 'Sint Maarten' 
  d.1 = rbind(d.1, d.1[d.1[,1] == 'Former Netherlands Antilles',])
  d.1[length(d.1[,1]),1] = 'Curacao' 
  d.1 = rbind(d.1, d.1[d.1[,1] == 'Former Netherlands Antilles',])
  d.1[length(d.1[,1]),1] = 'Bonaire' 
  d.1 = rbind(d.1, d.1[d.1[,1] == 'Former Netherlands Antilles',])
  d.1[length(d.1[,1]),1] = 'Saba' 
  d.1 = rbind(d.1, d.1[d.1[,1] == 'Former Netherlands Antilles',])
  d.1[length(d.1[,1]),1] = 'Sint Eustasius' 
  # now delete FNA 


  
  # add data indicator 
  v = strsplit(as.character(f), '\\_') 
  v.1= strsplit(unlist(v)[2], '\\.') 
  Identifier = rep.int(unlist(v.1)[1], dim(d.1)[1])
  Units = rep.int(metric, dim(d.1)[1])
  d.2 = cbind(d.1[,1], Identifier, Units, d.1[,2:dim(d.1)[2]])
  names(d.2)[1] = 'Country'
  
  # concatenate f files
  d.all = rbind(d.all, d.2)
  
}

# melt together
d.allm = melt(data=d.all, id.vars=names(d.2)[1:3], variable.name='year')
d.allm$year = as.numeric(as.character(d.allm$year)) 

# Print out all the unique indicators
print('these are all the variables that are included in the cleaned file: ')
print(data.frame(unique(d.all$Identifier)))

# work out the units
d.allm$value = as.numeric(as.character(factor(d.allm$value)))
d.allm[d.allm[,3] == '\'000',5] = d.allm[d.allm[,3] == '\'000',5]*1000
d.allm$Units = gsub('\'000', 'count', d.allm$Units) 
d.allm$value = as.numeric(as.character(factor(d.allm$value)))
d.allm[d.allm[,3] == '2011 US$ bn',5] = d.allm[d.allm[,3] == '2011 US$ bn',5] * 1000000000
d.allm$Units = gsub('2011 US\\$ bn', 'USD', d.allm$Units) 
 
# rearrange as add_rgn_id expects
d.all2 = d.allm[,c(1,5,4,2,3)] 
d.all2[,2] = as.numeric(as.character(factor(d.all2[,2])))
names(d.all2)[c(1,2,4,5)] = c('country', 'value_num', 'layer', 'units')
d.all3 = d.all2[order(d.all2$layer, d.all2$country, d.all2$year),]

## run add_rgn_id and save
uifilesave = file.path(dir1, 'data', 'GL-WTTC-Tourism_v2013-cleaned.csv')
add_rgn_id(d.all3, uifilesave)

## no georegional gapfilling--but do save as separate files

cleaned_data1 = read.csv(uifilesave)
cleaned_data1$layer = gsub('TotalContributionToEmployment', 'empt', cleaned_data1$layer) 
cleaned_data1$layer = gsub('TotalContributionToGDP', 'gdpt', cleaned_data1$layer)   

layer_uni = unique(cleaned_data1$layer)
layernames = sprintf('rgn_wttc_%s_2013a.csv', tolower(layer_uni))

for(i in 1:length(layer_uni)) { #i=1
  cleaned_layer = cleaned_data1[cleaned_data1$layer == layer_uni[i],]
  cleaned_layer$layer = NULL
  cleaned_layer$rgn_nam = NULL
  names(cleaned_layer)[2] = as.character(cleaned_layer$units[2])
  cleaned_layer$units = NULL
  
  layersave = paste(dir1, 'data/', layernames[i], sep='') 
  write.csv(cleaned_layer, layersave, na = '', row.names=FALSE)
  print('WTTC non-gapilled data layer saved: ')
  print(layersave)
}



