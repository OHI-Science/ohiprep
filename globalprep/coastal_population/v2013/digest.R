library(foreign)
library(plyr)

#wd = 'N:/model/GL-NCEAS-CoastalPopulation_v2013/data'
wd = '/Volumes/data_edit/model/GL-NCEAS-CoastalPopulation_v2013/data'
setwd(wd)

#for (dbf in list.files(pattern=glob2rx('rgn_popsum*_inland25mi.dbf'))){ 
for (yr in 2005:2015){
  dbf = sprintf('rgn_popsum%d_inland25mi.dbf', yr)
  fld = sprintf('rgn_popsum%d_inland25mi', yr)
  csv = sprintf('rgn_popsum%d_inland25mi.csv', yr)
  d = rename(read.dbf(dbf), c('VALUE'='rgn_id','SUM'=fld))
  d$area_km2 = d$AREA / (1000*1000)
  write.csv(d[,c('rgn_id',fld,'area_km2')], csv, row.names=F, na='')
}

# get population and density across all years
d = rbind(ldply(2005:2015, function(yr) {
  x = rename(read.csv(sprintf('rgn_popsum%d_inland25mi.csv', yr)), 
         setNames('popsum', sprintf('rgn_popsum%d_inland25mi',yr)))
  within(x, { year = yr
              pop_per_km2 = popsum / area_km2}) })); head(d)
write.csv(d, 'rgn_popsum_area_density_2005to2015_inland25mi.csv', row.names=F, na='')
write.csv(d[,c('rgn_id','year','popsum')], 'rgn_popsum2005to2015_inland25mi.csv', row.names=F, na='')

# check to see which regions are missing
rgns = read.csv('N:/model/GL-NCEAS-OceanRegions_v2013a/data/rgn_details.csv'); head(rgns)
pop  = read.csv('rgn_popsum2013_inland25mi.csv'); head(pop)
rgns.missing = subset(rgns, !rgn_id %in% pop$rgn_id & rgn_typ=='eez', c(rgn_id, rgn_key, rgn_nam))
write.csv(rgns.missing, 'rgn_missing_popsum2013_inland25mi.csv', row.names=F, na='')