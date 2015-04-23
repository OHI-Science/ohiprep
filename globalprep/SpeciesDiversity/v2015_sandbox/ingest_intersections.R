# "C:\Program Files\R\R-3.0.1\bin\x64\Rscript.exe" N:\model\GL-NCEAS-SpeciesDiversity_v2013a\ingest_intersections.R

library(foreign)
library(plyr)

wd = 'N:/model/GL-NCEAS-SpeciesDiversity_v2013a'
td = file.path(wd,'tmp')
cd = file.path(wd,'cache')
xd = file.path(cd,'iucn_intersections')
setwd(wd)

# read in spp
spp = read.csv('tmp/spp_iucn_marine_global.csv')


# show extinction risk category and population trend
spp$category = factor(as.character(spp$category),
                      levels=c('DD','LC','NT','VU','EN','CR','EX'), ordered=T)
table(spp$category)
#   DD   LC   NT   VU   EN   CR   EX 
# 2138 4512  530  653  200  122   18
table(spp$popn_trend)
#            Decreasing Increasing     Stable    Unknown 
#        250       1261        164        983       5515


# get list of dbf files
setwd(xd)
dbfs = list.files('.', glob2rx('range_*_mar_sid*_pts.dbf')) # 2553 before corals

# extract sid and group from dbf path, and assign info
sids = as.integer(gsub('range_([a-z]+)_mar_sid([0-9]+)_pts.dbf','\\2', dbfs))
grps =            gsub('range_([a-z]+)_mar_sid([0-9]+)_pts.dbf','\\1', dbfs)
spp$shp_dbf = NA # path to dbf
spp$shp_grp = NA # group like birds or seacucumbers
spp$shp_cnt = NA # number of points counted with intersection of rgn_fao_am_cells_pts_gcs
idx = match(sids, spp$sid)
spp$shp_dbf[idx] = dbfs
spp$shp_grp[idx] = grps

# compile monster table of species per cell
cells_spp = data.frame(cid=integer(0), sid=integer(0))
for (i in 1:length(dbfs)){ # i=1
  
  # read data
  d = foreign::read.dbf(dbfs[i])
  if ('ORIG_FID' %in% names(d)){
    d = rename(d, c('ORIG_FID'='cid'))
  }
  
  # log dbf path, group, and count of points to species record
  spp$shp_cnt[spp$sid==sids[i]] = nrow(d) # spp[spp$sid==sids[i], ]
  
  # merge with cells_spp
  cells_spp = rbind(cells_spp, d[,c('cid','sid')])
}

write.csv(cells_spp, file.path(td,'cells_spp_iucn.csv'), row.names=F, na='')
write.csv(spp, file.path(td,'spp_iucn.csv'), row.names=F, na='')