# data_prep.R. 

# Add rgn_ids for World Travel and Tourism Council (WTTC)
# Previously had been named clean_WTTC.r (by JStewart May2013). This script created by JStewartLowndes Mar2014.
#
#   read in .xls files from the WTTC raw folder downloaded from www.wttc.org/research/economic-data-search-tool
#   data_prep.R. will reformat so that data aren't on every other line like the .xls table
#   files must be accessed to the same year (eg end year 2013, assuming all after that is projected)
#   adds identifier and units columns, and then runs add_rgn_id.r. 
#   no georegional gapfilling -- but save as separate files  

# setup ----

# load libraries
library(mgcv) # for troubleshooting below
library(reshape2)
library(gdata)
library(dplyr)

# from get paths configuration based on host machine name
source('src/R/ohi_clean_fxns.R') # get functions
source('src/R/common.R') # set dir_neptune_data
# Otherwise, presume that scripts are always working from your default ohiprep folder
dir_d = 'Global/WTTC-Tourism_v2013'


# read in and process files ----
d.all =  matrix(nrow=0, ncol=0)
for (f in list.files(path = file.path(dir_d, 'raw'), pattern=glob2rx('*xls'), full.names=T)){ # f='Global/WTTC-Tourism_v2013/raw/WTTC_DirectContributionToEmployment_2013.xls'
  
  d = read.xls(f, sheet=1, blank.lines.skip=F, skip=7, header=T, check.names=F); head(d)
  
  # get the rows to line up properly and then remove every other line (weird spacing issue in .xls)
  metric = as.character(d[2,1])
  col1 = d[1:dim(d)[1]-1,1]
  col1 = c(metric, as.character(col1))
  d[,1] = col1
  d.1 = d[d[,1] != metric,]; head(d.1) # remove all the spacer lines 
  
  # some name cleaning
  names(d.1)[1] = 'rgn_nam'
  d.1 <- d.1[d.1[,1] != "Other Oceania",] # remove
  d.1[,1] = gsub('St Kitts', 'Saint Kitts and Nevis', d.1[,1]) # rename
  
  # add data indicator 
  v = strsplit(as.character(f), '\\/') 
  v.1 = strsplit(unlist(v)[4], '\\_') 
  v.2= strsplit(unlist(v.1)[2], '\\.') 
  Identifier = rep.int(unlist(v.2)[1], dim(d.1)[1])
  Units = rep.int(metric, dim(d.1)[1])
  d.2 = cbind(d.1[,1], Identifier, Units, d.1[,2:dim(d.1)[2]])
  names(d.2)[1] = 'Country'
  
  # concatenate f files
  d.all = rbind(d.all, d.2)
  
}

# melt together
d.allm = melt(data=d.all, id.vars=names(d.2)[1:3], variable.name='year')
d.allm$year = as.numeric(as.character(d.allm$year)); head(d.allm) 

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
d.all3 = d.all2[order(d.all2$layer, d.all2$country, d.all2$year),]; head(d.all3)

# partition Netherland Antilles
ind = d.all3$country %in% c('Former Netherlands Antilles') 
d.all4 = rbind(d.all3[!ind,],
             data.frame(country=c('Sint Maarten', 'Curacao', 'Bonaire', 'Saba', 'Sint Eustasius'), # Aruba reported separately
                        value_num=rep(d.all3$value_num[ind], 5),
                        year=rep(d.all3$year[ind], 5),
                        layer=rep(d.all3$layer[ind], 5),
                        units=rep(d.all3$units[ind], 5)))

## run add_rgn_id and save ----
uifilesave = file.path(dir_d, 'raw', 'WTTC-Tourism_v2013-cleaned.csv')
add_rgn_id(d.all4, uifilesave)


## check for duplicate regions, sum them ----

# explore; identify dups
dclean = read.csv(uifilesave); head(dclean)
d.dup = dclean[duplicated(dclean[,c('rgn_id', 'year', 'layer', 'units')]),]; head(d.dup)
dup_ids = unique(d.dup$rgn_id) # 116, 140, 209
filter(dclean, rgn_id == 116, year == 2013)
filter(dclean, rgn_id == 140, year == 2013)
filter(dclean, rgn_id == 209, year == 2013)

# remove duplicates with sum_duplicates.r
d_fix = sum_duplicates(dclean, dup_ids, fld.nam = 'value_num'); head(d_fix)

filter(d_fix, rgn_id == 116, year == 2013)
filter(d_fix, rgn_id == 140, year == 2013)
filter(d_fix, rgn_id == 209, year == 2013)


## no georegional gapfilling--but do save as separate files
d_fix$layer = gsub('DirectContributionToEmployment', 'empd', d_fix$layer) 
d_fix$layer = gsub('TotalContributionToEmployment', 'empt', d_fix$layer) 
d_fix$layer = gsub('TotalContributionToGDP', 'gdpt', d_fix$layer)   

layer_uni = unique(d_fix$layer)
layernames = sprintf('rgn_wttc_%s_2014a.csv', tolower(layer_uni))

for(i in 1:length(layer_uni)) { #i=1
  cleaned_layer = d_fix[d_fix$layer == layer_uni[i],]
  cleaned_layer$layer = NULL
  cleaned_layer$rgn_nam = NULL
  names(cleaned_layer)[4] = as.character(cleaned_layer$units[2])
  cleaned_layer$units = NULL
  
  layersave = file.path(dir_d, 'data', layernames[i]) 
  write.csv(cleaned_layer, layersave, na = '', row.names=FALSE)
  print('WTTC non-gapilled data layer saved: ')
  print(layersave)
}



