# clean_FAO.r: FAO data (by BBest, JStewart Apr2013)
# modified and used by Jafflerbach Feb 2014
#   read in individual files
#   remove Totals row
#   remove/translate FAO data codes (F, ..., -, 0 0)
#   add identifier column
#   concatenate data from each file into a single file
#   run add_ISO.r (function by J. Stewart, B. Best)
#   save single file


dir1 = ('~/Documents/OHI')
setwd(dir1)

library(reshape2)
library(gdata)
options(max.print=5E6)
options(gsubfn.engine = "R") # otherwise, get X11 launching for sqldf package
require(sqldf)
  

# read in files

d = read.csv('FAO_raw.csv', check.names=F) # this will keep R from adding the stupid X in front of the numeric column names
d1 = d
unique(d1[,1])
names(d1) = c('Country', 'Common_Name', 'Scientific_Name', 'Taxon_ID','Family','Order', 'Group', 'Fishing_area','Measure', 1950:2011)

# remove totals line
d1[,1] = gsub('Totals.+', 'Totals', d1[,1]) 
d1 <- d1[d1[,1] != "Totals",] # remove Totals line

# remove any Inland waters
d1[,8] = gsub('\\w+ - Inland \\w+', 'Inland', d1[,8]) 
d1[,8] = gsub('\\w+, Inland', 'Inland', d1[,8]) 
d1 <- d1[d1[,8] != "Inland",] # remove Inland lines

# remove accents or weirdnesses
d1[,1] = gsub('.+voire', 'Ivory Coast', d1[,1]) # Ivory Coast
d1[,1] = gsub('.+union', 'Reunion', d1[,1]) # Reunion
d1[,1] = gsub('.+publique du', 'Republic of', d1[,1]) # Congo
d1[,1] = gsub('Cura.+', 'Curacao', d1[,1]) # Curacao 
d1[,1] = gsub('Saint Barth.+', 'Saint Barthelemy', d1[,1]) # Saint Barthelemy 
 
# melt dataframe into long format
dm = melt(data=d1, id.vars=names(d1)[1:9], variable.name='Year')
names(dm)[11] = c('Catch')

# manipulate
dm$Catch = trim(dm$Catch)
dm$Catch = sub(' F', '', dm$Catch, fixed=T) # FAO denotes a Catch with F when they have estimated the Catch using best available data
dm$Catch = sub(' 0', '', dm$Catch, fixed=T)
dm$Catch = sub('-', '0', dm$Catch, fixed=T) # FAO denotes something as '-' when it is > 0 but < 1/2 of a unit. So here we made it 0.
dm$Catch = sub('...', 'NA', dm$Catch, fixed=T)
dm$Catch = as.numeric(dm$Catch)

# remove NAs so they are not counted each year
dm = dm[!is.na(dm$Catch),]

## This works, but we went with reading the ISCAAP taxon ID file from FAO below
# # read in ISCAAP taxon key file
# tk = read.xls(paste(dir1, 'CatchEEZ_TaxonNames.xls', sep=''), fileEncoding='latin1')
#  
# # join FAO data to taxon key 
# dj = sqldf("SELECT b.Taxonkey, a.*
#                  FROM dm AS a
#                  LEFT OUTER JOIN (
#                      SELECT DISTINCT Taxonkey, CommonName  
#                      FROM tk 
#                      ) AS b ON b.CommonName = a.Species") 
# 
# # look deeper at mismatches
# dj_na = dj[is.na(dj[,1]),]
# dj_na[,3] = gsub('.+nei', 'nei', dj_na[,3])
# dj_na2 = dj_na2[dj_na2$Species != "nei",] # remove nei line

