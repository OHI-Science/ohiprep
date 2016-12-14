# clean_FAO.r: FAO data (by BBest, JStewart Apr2013)
# modified and used by Jafflerbach Feb 2014
#   read in individual files
#   remove Totals row
#   remove/translate FAO data codes (F, ..., -, 0 0)
#   add identifier column
#   concatenate data from each file into a single file
#   run add_ISO.r (function by J. Stewart, B. Best)
#   save single file


dir1 = ('~/Spatial Proj')
setwd(dir1)

library(reshape2)
library(gdata)
options(max.print=5E6)
options(gsubfn.engine = "R") # otherwise, get X11 launching for sqldf package
require(sqldf)
  

# read in files

d = read.csv('FAO_marinefisheries_tonnes_1950_2011.csv', check.names=F) # this will keep R from adding the stupid X in front of the numeric column names
d1 = d
unique(d1[,1])
names(d1) = c('Country', 'Species', 'FishingArea', 'Measure', 1950:2011)

# remove totals line
d1[,1] = gsub('Totals.+', 'Totals', d1[,1]) 
d1 <- d1[d1[,1] != "Totals",] # remove Totals line

# remove any Inland waters
d1[,3] = gsub('\\w+ - Inland \\w+', 'Inland', d1[,3]) 
d1[,3] = gsub('\\w+, Inland', 'Inland', d1[,3]) 
d1 <- d1[d1[,3] != "Inland",] # remove Inland lines

# remove accents or weirdnesses
d1[,1] = gsub('.+voire', 'Ivory Coast', d1[,1]) # Ivory Coast
d1[,1] = gsub('.+union', 'Reunion', d1[,1]) # Reunion
d1[,1] = gsub('.+publique du', 'Republic of', d1[,1]) # Congo
d1[,1] = gsub('Cura.+', 'Curacao', d1[,1]) # Curacao 
d1[,1] = gsub('Saint Barth.+', 'Saint Barthelemy', d1[,1]) # Saint Barthelemy 
 
# melt dataframe into long format
dm = melt(data=d1, id.vars=names(d1)[1:4], variable.name='Year')
names(dm)[6] = c('Catch')

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


# read in ISCAAP taxon ID file from FAO
ti = read.csv(paste(dir1, 'ASFIS_2013_KLJS.csv', sep=''))

# remove non-comformitive codes
ti[,2] = as.character(ti[,2])
ti2 = ti[nchar(ti[,2]) == 10,]

# remove any without genus
ti3 = ti2[ti2$speciesIDpresent != "XX",] # remove XX line

# join FAO data to taxon key 
dti = sqldf("SELECT b.speciesIDpresent, a.*
                 FROM dm AS a
                 LEFT OUTER JOIN (
                     SELECT DISTINCT speciesIDpresent, English_name  
                     FROM ti3 
                     ) AS b ON b.English_name = a.Species") 

# remove FAO low resolution
dti2 = dti[!is.na(dti$speciesIDpresent),] # remove NA lines

# cast data back but count its length (number of species) for each year
dc = dcast(dti2, Country ~ Year, fun.aggregate = length)

# poke around a bit
dc$slope2000_2011 = dc[,dim(dc)[2]] - dc[,dim(dc)[2]-11]

dc2 = dc[, c(1, 50:64)]

write.table(dc2, 'FAO_fisheriesCleanCount.csv', sep=',', row.names=FALSE)
  
