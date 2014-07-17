#  data_prep.r

#  formerly clean.r: reformat Ocean Conservancy trash data (by JStewart Apr2013)
#  read in identified trash file
#  name_to_rgn.r
#  read in newly accessed 2012 file, but also older 2010 and 2011 files and concatenate them all. 
#  calculate lbs_per_mile
#  georegional gapfilling with gapfill_georegions.r

# Note: Islands are reported separately and are identified in the Island column


# the 2014 report has data from 2013. This is a fourth year of data available.  NOTE: data are available for 3 separate years. Things are confusing because the year identified in the report and in the data are different: So the 2012 report (pdf) has csv files named 2011. But the xlsx files used in OHI2012 are labeled 2010 and 2011. JS has checked and made sure that the most recent available data (2011 csv) is different from 2011 xlsx and 2010 xlsx. So in the coding and beyond, the data from the 2011 csv file is renamed and treated as 2012. 

# Reports are all in neptune_data:git-annex/Global/OceanConserv-Trash_v2013/reports
# Data are all in    neptune_data:git-annex/Global/OceanConserv-Trash_v2013/raw [some are .xls; some are .csv)
# 2013 data: 
#         2014 report has 2013 data: icc-data-2014.pdf has the same data as '!!Copy of 2013-14_US-Global_Summary_PPM.xlsx'
# 2012 data: 
#       2013 report 2013-trash-free-seas-report.pdf has the same data as '2013 ICC Data Release/International-PPM_OpenAccess.csv'
#       report from: http://www.oceanconservancy.org/our-work/international-coastal-cleanup/2013-trash-free-seas-report.pdf (googled) 
#       data from: http://www.oceanconservancy.org/our-work/international-coastal-cleanup/2012-ocean-trash-index.html
# 2011 data: 
#       2012 report has 2011 data: 2012-icc-data-pdf.pdf has the same data as 'ICC 2011_OTI-PPM_FINAL.xlsx'
# 2010 data:
#       2011 report has presumably 2010 data: Marine_Debris_2011_Report_OC.pdf has the same data as OC2011_total.xlsx * mislabeling 2011 instead of 2010?

# libraries
library(gdata)
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

# get paths
source('src/R/common.R') # set dir_neptune_data
source('src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()
dir_d = 'Global/OceanConserv-Trash_v2013' 

## identify files ----
files = list(  
  f2013 = file.path(dir_d, 'raw', '!!Copy of 2013-14_US-Global_Summary_PPM.xlsx'),
  f2012 = file.path(dir_d, 'raw', 'raw/2013 ICC Data Release', 'International-PPM_OpenAccess.csv'),
  f2011 = file.path(dir_d, 'raw', 'ICC 2011_OTI-PPM_FINAL.xlsx'),
  f2010 = file.path(dir_d, 'raw', 'GlobalTrashCleanup_OC2010.xls'))

## read in 2013 data ----
f = read.xls(files$f2013, sheet = 2, header=T); head(f); 
f = f[1:3,] # just keep People, Pounds, Miles
rownames(f) = f$Country
country = names(f)[2:length(names(f))]
ft = as.data.frame(t(f[,-1]), row.names = NULL) # still need to get rid of row.names...
ft2 = cbind(country, ft) %>%
  filter(country != 'Totals') 

## read in 2012 data ----

## read in 2011 data ----
d = read.xls(files$f2011, skip=1); head(d)
d = d %>%
  select(country = X, 
         island  = X.1,
         people  = People.2,
         pounds  = Pounds.2,
         miles   = Miles.2,
         bags    = Bags.2) %>%
  filter(country != 'Country or Location'); head(d)

...continue here

d1 = d
d1 <- d1[d1[,1] != "Total",] # remove Totals line
d1 <- d1[d1[,1] != "Channel Islands",] # remove Channel Islands

d.m = melt(data=d1 , id.vars=names(d)[1:2])
d.m[,4] = gsub('-', 'NA', d.m[,4]) 
d.m[,4] = gsub(',', '', d.m[,4]) 
names(d.m) = c('country', 'island','metric','value')

# add identifier column
metrcC = as.character(d.m[,3])
d.m$identifier = rep.int(NA, length(metrcC))
d.m$identifier[grep('\\w+.1', metrcC)] = 'Sea'
d.m$identifier[grep('\\w+.2', metrcC)] = 'Total'
d.m$identifier[is.na(d.m$identifier)] = 'Land'

# redo metric column
d.m$metric[grep('People\\w*', metrcC)] = 'People'
d.m$metric[grep('Pounds\\w*', metrcC)] = 'Pounds'
d.m$metric[grep('Miles\\w*', metrcC)] = 'Miles'
d.m$metric[grep('Bags\\w*', metrcC)] = 'Bags'

# transpose
d.all = dcast(d.m, country + island + identifier ~ metric) 

# for OHI, just report Total trash (sum of Land and Sea trash)
d.all2 = d.all[d.all$identifier == 'Total',]
d.all3 = d.all2[d.all2[,1] != '',]# remove all individual islands since reported as a total
d.x = d.all3[,c(1,5,6)] # report just country, lbs, miles
d.x$year = rep.int(2012, dim(d.x)[1])  # replicate year
names(d.x) = c('country_id', 'pounds', 'miles', 'year')
d.x$pounds = as.numeric(d.x$pounds)
d.x$miles = as.numeric(d.x$miles)

# sum the subparts (reporting regions) of the UK: (wish I could have done this once it was concatenated but...)
idx = d.x$country_id %in% c('United Kingdom', 'Northern Ireland','Scotland', 'Wales')
d.x2 = rbind(d.x[!idx,],
     data.frame(country_id='United Kingdom',
            pounds=sum(d.x$pounds[idx]),
            miles=sum(d.x$miles[idx]),
            year=2012))

# partition Netherland Antilles
ind = d.x2$country_id %in% c('Netherland Antilles', 'Netherlands Antilles')
d.x3 = rbind(d.x2[!ind,],
  data.frame(country_id=c('Sint Maarten', 'Curacao', 'Bonaire','Saba', 'Sint Eustasius', 'Aruba'),
            pounds=rep(d.x2$pounds[ind]),
            miles=rep(d.x2$miles[ind]),
            year=rep(2012)))

## format 2011 data
f = read.xls('OC2011_total.xlsx')
f.x = f[,c(1,3,4)]
f.x$year = rep.int(2011, dim(f.x)[1])  # replicate year
names(f.x) = c('country_id', 'pounds', 'miles', 'year')

# sum the subparts (reporting regions) of the UK:
idx = f.x$country_id %in% c('United Kingdom', 'Channel Islands', 'Northern Ireland','Scotland', 'Wales')
f.x2 = rbind(f.x[!idx,],
     data.frame(country_id='United Kingdom',
            pounds=sum(f.x$pounds[idx]),
            miles=sum(f.x$miles[idx]),
            year=2011))

# partition Netherland Antilles
ind = f.x2$country_id %in% c('Netherland Antilles', 'Netherlands Antilles')
f.x3 = rbind(f.x2[!ind,],
  data.frame(country_id=c('Sint Maarten', 'Curacao', 'Bonaire', 'Saba', 'Sint Eustasius', 'Aruba'),
            pounds=rep(f.x2$pounds[ind]),
            miles=rep(f.x2$miles[ind]),
            year=rep(2011))) 

## concatenate all files
tr = rbind(d.x3, f.x3) #, g.x3)
tr$pounds = as.numeric(tr$pounds); tr$miles = as.numeric(tr$miles)

# calculate trash density: pounds/miles
tr$pounds_per_mile = as.numeric(tr$pounds)/as.numeric(tr$miles)
# tr$pounds_per_mile[is.infinite(tr$pounds_per_mile)] = NA
  
tr2 = tr[,c(1,5,4)]
tr2[,3] = as.numeric(as.character(factor(tr2[,3])))
tr3 = tr2[order(tr2$country_id, tr2$year),]
tr3 = na.omit(tr3) # any NAs will have to be gapfilled

## run add_rgn_id and save
uifilesave = file.path(dir1, 'data', 'GL-OceanConserv-Trash_v2012-cleaned.csv')
add_rgn_id(tr3, uifilesave)



## georegional gapfilling-- save as separate files

cleaned_data1 = read.csv(uifilesave)

year_uni = unique(cleaned_data1$year)
layernames = sprintf('rgn_oc_trash_%sa.csv', year_uni+1) # because 2013a uses 2012 data, 2012a data uses 2011 data. 

for(i in 1:length(year_uni)) { # i=1
  cleaned_layer = cleaned_data1[cleaned_data1$year == year_uni[i],]
  cleaned_layer$year = NULL
  cleaned_layer$rgn_nam = NULL
  
  layersave = file.path(dir1, 'data', layernames[i]) 
  
  add_gapfill_singleyear(cleaned_layer, layersave, s_island_val=0)
}

##
##
## whence tracking; May/June 2014 ---- 
dir_neptune_data = c('Windows' = '//neptune/data_edit',
                     'Darwin'  = '/Volumes/data_edit',
                     'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]
dir_root = file.path('/Users', Sys.info()[['user']]) # or dri_root = path.expand("~/")
source(file.path(dir_root, 'github/ohiprep/src/R/ohi_clean_fxns.R'))
library(dplyr)


d = read.csv(uifilesave); head(d)

year_uni = unique(d$year)
layernames = sprintf('rgn_oc_trash_%sa_whence', year_uni+1) # because 2013a uses 2012 data, 2012a data uses 2011 data. 
dirsave = file.path(dir1, 'data') 

for(i in 1:length(year_uni)) { # i=1
  cleaned_layer = d[d$year == year_uni[i],] 
  cleaned_layer$rgn_nam = NULL # keep year column so that add_gapfill will work; remove afterwards. 
  
  layersave = layernames[i]

  add_gapfill(cleaned_layer, dirsave, layersave, s_island_val=0, dpath = file.path(dir_root, 'github/ohiprep/src/LookupTables'))

  f = read.csv(file.path(dirsave, paste(layersave, '.csv', sep=''))) 
  f = f %.%
    select(-year) #remove year column as a fix for now
  write.csv(f, file.path(dirsave, paste(layersave, '.csv', sep=''))) 
}




