
# status field --
# using:
#   CR = Critically Endangered
#   EN = Endangered
#   VU = Vulnerable
#   NT = Near Threatened
#   LC = Least Concern
# including:
#   DD = Data deficient
#   NE = Not evaluated
# missing:
#   EX = Extinct
#   EW = Extinct in the wild
#   PE = Probably extinct (informal)
#   PEW = Probably extinct in the wild (informal)
#iucn.cats.ok = c('CR','EN','VU','NT','LC')

# read in IUCN spp 
spp = read.csv('C:/Users/best/Documents/data/model/GL-IUCN-RedList/data/spp.csv', na.strings='')

# exclude fields
spp = spp[,!names(spp) %in% c('oid')]
# rename fields
names.spp.0 = c('scientific','rl_category','popn_trend','src_xls'     ,'kingdom','phylum','class','order','family','sid_shp','src_shp')
names(spp)  = c('scientific','status'     ,'popn_trend','iucn_src_xls','kingdom','phylum','class','order','family','sid','iucn_src_shp')
idx = which(!is.na(spp$iucn_src_shp))
spp[idx,'src_distn'] = rep('IUCN',length(idx))

tocapital = function(strings){
  s = as.character(strings) # ensure as character
  return(sprintf('%s%s', substr(s,1,1), tolower(substr(s,2,nchar(s)))))
}

for (f in c('kingdom','phylum','class','order','family')){ # f = 'kingdom'
  levels(spp[[f]]) = tocapital(levels(spp[[f]]))
}

# somehow NA (maybe a subspecies) got introduced in scientific names
idx = grep('.* NA$', levels(spp[['scientific']]))
str = levels(spp[['scientific']])[idx]
levels(spp[['scientific']])[idx] = substr(str, 1, nchar(str)-3)

# read in SAUP list of species with AquaMaps distributions
saup = read.csv('C:/Users/best/Documents/data/ingest/GL-AquaMaps/data/tbl_fish_species.csv',na.strings='\\N')
saup['scientific'] = sprintf('%s %s', saup$genus_name, saup$species_name)
# summary(saup)
# names(saup)
# head(saup)
saup = saup[,!names(saup) %in% c('id','genus_name','species_name','common_name')]
names.saup.0 = c('species_id','kingdom_name','phylum_name','class_name','order_name','family_name','iucn'    ,'scientific')
names(saup)  = c('saup_sid'  ,'s.kingdom'   ,'s.phylum'   ,'s.class'   ,'s.order'   ,'s.family'   ,'s.status','scientific')

# supplemental info for merging with IUCN  
saup[['s.src_distn']] = rep('SAUP',nrow(saup))
saup = saup[order(saup$saup_sid),]
saup[['s.sid']] = seq(max(spp$sid, na.rm=T) + 1, length.out=nrow(saup)) # max(spp$sid, na.rm=T) is 2286
  
# convert SAUP IUCN status from IUCN Red List Categories & Criteria v2.3 (1994-2000) to v3.1 (since 2001)
# http://en.wikipedia.org/wiki/Wikipedia:Conservation_status
# LR/cd: Least Risk / Conservation Dependent -> NT
# LR/lc: Least Risk / Least Concern -> LC
# LR/cd: Least Risk / Conservation Dependent -> NT
# convert other SAUP IUCN oddities, presumably: NL = not listed = NT; \\N  or N.E. = not evaluated = NE
table(saup$s.status)
lut = list('\\N'='NE','CR'='CR','DD'='DD','EN'='EN','LC'='LC','LR/cd'='NT','LR/lc'='LC','LR/nt'='NT',
           'N.E.'='NE','NL'='NE','NT'='NT','VU'='VU')
levels(saup$s.status) = unlist(lut[levels(saup$s.status)])
table(saup$s.status)
  
# dim(saup)
spp = merge(spp, saup, by='scientific', all.x=T, all.y=T)
# dim(spp)
# names(spp)
# idx = which(!is.na(spp$saup_sid) & is.na(spp$iucn_sid))
# length(idx)
# 11,549 saup + 3,453 iucn (1,167 without shp but having pop_trend) - 13,361 spp merged = 1,641 saup matching iucn

#paste(levels(spp$status), collapse="','")
# levels.spp.status.0 = c('CR','DD','EN','LC','NA','NE','NT','VU')
# spp$status = factor(spp$status, levels=c('CR','EN','VU','NT','LC','DD','NE','NA'),
#                     labels=c('Critically Endangered (CR)','Endangered (EN)','Vulnerable (VU)',
#                        'Near Threatened (NT)','Least Concern (LC)','Data Deficient (DD)',
#                        'Not Evaluated (NE)','Not Available (NA)'))
  
  
# merge with trophic data
troph = read.csv('C:/Users/best/Documents/data/ingest/GL-SeaLifeBase-SpeciesTrophicLevels/img/trophs.csv')
troph = troph[,c('scientific','EstimateTroph','seEstimateTroph','source')]
names(troph) = c('scientific','troph','troph_se','troph_src')

spp = merge(spp, troph, by='scientific', all.x=T)

for (f in c('kingdom','phylum','class','order','family','status','sid','src_distn')){ # f='sid'
  v = spp[[f]]
  s.f = sprintf('s.%s',f)
  s.v = spp[[s.f]]
  idx = which(is.na(v) & !is.na(s.v))
  if (class(v)=='factor'){
    spp[[f]] = factor(as.character(v), levels = c(levels(v), levels(s.v)[!levels(s.v) %in% levels(v)]))
    spp[idx,f] = as.character(spp[idx, s.f])
  } else if (class(v) %in% c('integer','numeric')){
    spp[idx,f] = spp[idx, s.f]
  } else {
    spp[idx,f] = as.character(spp[idx, s.f])
  } 
}

# get rid of spp without a distribution    
spp = spp[!is.na(spp$src_distn),]

# trim and align fields
spp = spp[,c('sid','scientific','src_distn','status','popn_trend','troph',
             'kingdom','phylum','class','order','family',
             'iucn_src_shp','iucn_src_xls','saup_sid','troph_se','troph_src')]
class(spp$sid) = 'integer'
write.csv(spp, 'C:/Users/best/Documents/data/ingest/GL-AquaMaps/data/spp_iucn_saup.csv', row.names=F, na='')

# write out only valid spp used for extinction risk and population trend
names(spp)
write.csv(spp, 'C:/Users/best/Documents/data/ingest/GL-AquaMaps/data/spp_iucn_saup.csv', row.names=F, na='')
spp.used = spp[spp$popn_trend %in% c('decreasing','increasing','stable') | spp$status %in% c('CR','EN','VU','NT','LC') & !is.na(spp$src_distn),]
write.csv(spp.used, 'C:/Users/best/Documents/data/ingest/GL-AquaMaps/data/spp_used_OhiStatusOrTrend.csv', row.names=F, na='')

# clarifications with Liz
write.csv(spp[which(is.na(spp$status) & spp$src_distn=='IUCN'),], 
          'C:/Users/best/Documents/data/ingest/GL-AquaMaps/data/spp_IUCNshp_NoStatus.csv', row.names=F, na='')

spp_SAUPshp_SAUPstatus = spp[spp$src_distn=='SAUP' & is.na(spp$iucn_src_xls) & (spp$status %in% c('CR','EN','VU','NT','LC')),]
write.csv(spp_SAUPshp_SAUPstatus,  
          'C:/Users/best/Documents/data/ingest/GL-AquaMaps/data/spp_SAUPshp_SAUPstatus.csv', row.names=F, na='')
table(spp_SAUPshp_SAUPstatus[,c('class','status')])
spp_IUCNshp_SAUPstatus = spp[spp$src_distn=='IUCN' & is.na(spp$iucn_src_xls) & (spp$status %in% c('CR','EN','VU','NT','LC')),]
nrow(spp_IUCNshp_SAUPstatus)
write.csv(spp_SAUPshp_SAUPstatus,  
          'C:/Users/best/Documents/data/ingest/GL-AquaMaps/data/spp_IUCNshp_SAUPstatus.csv', row.names=F, na='')
  
# had to use 64-bit R with RStudio and setup_psqlODBC_x64_8.3.4.0.exe from http://code.google.com/p/visionmap/downloads/list
require('RODBC')
con = odbcConnect('best_neptune')

# upload spp to postgres
##DEBUG
sqlDrop(con,'spp')
#sqlSave(con, spp[1:5,], 'spp', rownames=F)
sqlSave(con, spp, 'spp', rownames=F)
# sqlQuery(con, 'CREATE UNIQUE INDEX idx_spp_sid ON spp(sid);')
# sqlQuery(con, 'CREATE UNIQUE INDEX idx_cells_cid ON cells(cid);')
# sqlQuery(con, 'CREATE UNIQUE INDEX idx_cells_csquarecod ON cells(csquarecod);')
# sqlQuery(con, 'CREATE INDEX idx_am_spp_cells_speciesid ON am_spp_cells(speciesid);')

# upload weights
status_wts = read.csv('C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/data/status_wts.csv')
sqlDrop(con,'spp_status_weights')
sqlSave(con, status_wts, 'spp_status_weights', rownames=F, safer=F)
sqlQuery(con, 'CREATE UNIQUE INDEX idx_spp_status_weights_scheme_status ON spp_status_weights(scheme,status);')

# NEW: get species list per OHI region for iconic species

# upload regions
regions = read.csv('C:/Users/best/Documents/data/model/GL-NCEAS-OceanRegions-v3/data/regions_details.csv')
names(regions) = tolower(names(regions))
names(regions)[1] = 'rid' # head(regions)
sqlDrop(con,'regions')
sqlSave(con, regions, 'regions', rownames=F)
sqlQuery(con, 'CREATE UNIQUE INDEX idx_regions_rid ON regions(rid);')

# upload summed cells_regions
cr = read.csv('C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/data/q_cells_regions.csv')
cr2 = aggregate(cr$rgn_cell_area_m2, by=list(rid=cr$rid,cid=cr$cid), sum)
names(cr2) = c('rid','cid','area_m2')
sqlDrop(con,'cells_regions')
sqlSave(con, cr2, 'cells_regions', rownames=F)
sqlQuery(con, 'CREATE UNIQUE INDEX idx_cells_regions_rid_cid ON cells_regions(rid,cid);')


sqlQuery(con, 'CREATE UNIQUE INDEX idx_cells_spp_cid_sid ON cells_spp(cid,sid);')
sqlQuery(con, 'CREATE UNIQUE INDEX idx_spp_sid ON spp(sid);')

# get (r)egions and their (s)pecies with area
t0 = Sys.time()
rs = sqlQuery(con, 'SELECT cr.rid AS region, s.scientific, Sum(cr.area_m2) AS area_m2
  FROM (cells_spp AS cs 
    INNER JOIN cells_regions AS cr ON cs.cid = cr.cid) 
    INNER JOIN spp AS s ON cs.sid = s.sid
GROUP BY region, s.scientific 
ORDER BY region, s.scientific;')
head(rs)
Sys.time() - t0 # 15.55403 mins (pre-indexes: idx_cells_spp_cid_sid, idx_spp_sid)
write.csv(rs, file='C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/data/regions_species.csv', row.names=F)

rs = read.csv(file='C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/data/regions_species.csv')


# calculate population Trend per region
# not factoring in species extinction risk
# avg popn trend _ region = sum(A_sp * T_sp) / sum(A_sp)
spp = sqlQuery(con, 'SELECT * FROM spp') # head(spp)
levels(spp$popn_trend) = c(-0.5,0.5,0,NA) # "decreasing" "increasing" "stable"     "unknown"

# merge region species area with population trend, removing all rows for species without popn_trend
rst = merge(rs, spp[!is.na(spp$popn_trend),c('scientific','popn_trend')], by='scientific') 
rst$sp_trend_x_area = rst$area_m2 * as.numeric(as.character(rst$popn_trend))
rst$spp_cnt = 1
write.csv(rst[order(rst$region, rst$scientific),c('region','scientific','area_m2','popn_trend','sp_trend_x_area')], file='C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/data/region_species_trend_popn_data.csv', row.names=F)
summary(rst)
names(rst)
rsts = aggregate(rst[,c('sp_trend_x_area','area_m2','spp_cnt')], by=list(region=rst$region), FUN=sum) # head(rsts)
rsts$trend_popn = rsts$sp_trend_x_area / rsts$area_m2 # area-weighted population trend
write.csv(rsts[,c('region','trend_popn','spp_cnt')], file='C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/data/region_species_trend_popn.csv', row.names=F)


# merge (e)xtinction (r)isk to species, yielding only (s)pecies that have been (e)valuated
er = status_wts[status_wts$scheme=='OHI v1',c('status','weight')]
names(er) = c('status','er_v1')
er = merge(er, status_wts[status_wts$scheme=='OHI v2',c('status','weight')])
names(er) = c('status','er_v1','er_v2') # er
se = merge(spp, er) # nrow(spp); nrow(se); head(se) # head(rs)

# merge region species area with extinction risk, removing all rows for species without extinction risk assessed
rse = merge(rs, se[!is.na(spp$popn_trend),c('scientific','er_v1','er_v2')], by='scientific') # head(rse)
rse$sp_erv1_x_area = rse$area_m2 * as.numeric(as.character(rse$er_v1))
rse$sp_erv2_x_area = rse$area_m2 * as.numeric(as.character(rse$er_v2))
head(rse[rse$region==1,])
rses = aggregate(rse[,c('sp_erv1_x_area','sp_erv2_x_area','area_m2')], by=list(region=rse$region), FUN=sum) # head(rsts)
rses$status_extrisk_v1 = 1 - rses$sp_erv1_x_area / rses$area_m2 # area-weighted extinction risk
rses$status_extrisk_v2 = 1 - rses$sp_erv2_x_area / rses$area_m2 # area-weighted extinction risk

head(rses)
# merge species status and 
regions_species = merge(rsts[,c('region','trend_popn')], rses[,c('region','status_extrisk_v1','status_extrisk_v2')])
write.csv(region_species, file='C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/data/region_species_extriskstatus_popntrend.csv', row.names=F)


# plot
require(maptools)
require(fields)
require(lattice)

setwd('C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/data')
rs = read.csv('region_species_trend_popn.csv') # names(rs)
rgns = readShapePoly('C:/Users/best/Documents/data/model/GL-NCEAS-OceanRegions-v3/data/ocean_regions_shp/ocean_regions_poly_simplified10km.shp')
names(rgns)[2] = 'rid'

# compare with old
rs = merge(rs, read.csv('C:/Users/best/Desktop/spp_extrisk_v2/spp_status_regions_score.csv', col.names=c('region','erv02')))
rs = merge(rs, read.csv('C:/Users/best/Desktop/spp_extrisk_v1/spp_status_regions_score.csv', col.names=c('region','erv01')))
rs$dv1 = rs$status_extrisk_v1 - rs$erv01
rs$dv2 = rs$status_extrisk_v2 - rs$erv02
summary(rs)

idx = match(rgns@data$rid, rs$region)
rgns@data$trend_popn = rs$trend_popn[idx]
rgns@data$spp_cnt = rs$spp_cnt[idx]
rgns@data$status_extrisk_v1 = rs$status_extrisk_v1[idx]
rgns@data$dv1 = rs$dv1[idx]
rgns@data$dv2 = rs$dv2[idx]

summary(rs[,c('dv1','dv2')])

png('species_trend_popn.png', width=1200, height=800)  
sp.theme(set=T, regions=list(col=tim.colors(256)))
spplot(rgns, 'trend_popn', main='Population Trend')
dev.off()

png('species_trend_popn_spp_cnt.png', width=1200, height=800)  
sp.theme(set=T, regions=list(col=tim.colors(256)))
spplot(rgns, 'spp_cnt', main='Count of Species used in Population Trend')
dev.off()


png('species_status_extrisk_v1.png', width=1200, height=800)  
sp.theme(set=T, regions=list(col=tim.colors(256)))
spplot(rgns, 'status_extrisk_v1', main='Status of Extinction Risk (v1)')
dev.off()
png('species_status_extrisk_v2.png', width=1200, height=800)  
sp.theme(set=T, regions=list(col=tim.colors(256)))
spplot(rgns, 'status_extrisk_v2', main='Status of Extinction Risk (v2)')
dev.off()



png('species_status_extrisk_v1_dif.png', width=1200, height=800)  
sp.theme(set=T, regions=list(col=tim.colors(256)))
spplot(rgns, 'dv1', main='Difference in Calc of Status of Extinction Risk (v1)')
dev.off()
png('species_status_extrisk_v2_dif.png', width=1200, height=800)  
sp.theme(set=T, regions=list(col=tim.colors(256)))
spplot(rgns, 'dv2', main='Difference in Calc of Status of Extinction Risk (v2)')
dev.off()



# output table comparing assessed extinction risk categories and population trends
t = table(spp[,c('status','popn_trend')], useNA='ifany')
t = addmargins(t)
write.csv(t, file='species_counts_extrisk_vs_popntrend.csv')




# test for iconic species in former Soviet Union Baltic regions
spp_iconic = data.frame(
  common=c('Blue Shark','Fin Whale','Porbeagle','Basking Shark','Humpback Whale'),
  scientific=c('Prionace glauca','Balaenoptera physalus','Lamna nasus','Cetorhinus maximus','Megaptera novaeangliae')
  )
rgn_spp[rgn_spp$scientific %in% spp_iconic$scientific & rid %in% c(52,53,125,129,140),]


# Baltics 

head(rgn_spp)




# get habitat-forming spp
spp = read.csv('C:/Users/best/Documents/data/ingest/GL-AquaMaps/data/spp_iucn_saup.csv', na.strings='')
names(spp)
levels(spp$iucn_src_shp)
unique(spp$class[spp$iucn_src_shp %in% c('coral1.shp','coral2.shp','coral3.shp','mangroves.shp','seagrasses.shp')])
c('Anthozoa','Magnoliopsida','Polypodiopsida','Liliopsida','Hydrozoa')

# run query in Postgres, like:
# ALTER TABLE cells ADD COLUMN spp_risk_cnt integer;
# ALTER TABLE cells ADD COLUMN spp_risk_ohi1_sum real;
# ALTER TABLE cells ADD COLUMN spp_risk_ohi1_avg real;
# UPDATE cells SET spp_risk_ohi1_sum=cw.wt, spp_risk_cnt=cw.cnt FROM (
#     SELECT cs.cid, SUM(weight) AS wt, COUNT(s.sid) AS cnt
#      FROM cells_spp AS cs
#       JOIN spp AS s ON cs.sid=s.sid 
#       JOIN (SELECT status, weight FROM spp_status_weights WHERE scheme='OHI v1') AS w ON s.status=w.status
#     GROUP BY cs.cid) cw WHERE cells.cid=cw.cid;
# UPDATE cells SET spp_risk_ohi1_sum=0,spp_risk_cnt=0 WHERE spp_risk_ohi1_sum IS NULL;
# UPDATE cells SET spp_risk_ohi1_avg=spp_risk_ohi1_sum/spp_risk_cnt WHERE spp_risk_cnt NOT IN (NULL,0);

# generate scores
require('rgdal')
require('fields')
require('raster')
require('RODBC')

con = odbcConnect('best_neptune')

d = sqlQuery(con, 'SELECT cid, spp_risk_cnt, spp_norisk_cnt, spp_risk_bd_sum, spp_risk_bi_sum, spp_risk_ohi1_sum, spp_risk_ohi2_sum,
             spp_hab_risk_cnt, spp_hab_risk_ohi1_sum,  
             shape_area, spp_troph_sum, spp_troph_cnt, spp_popn_sum, spp_popn_cnt FROM cells;')
head(d)
d['spp_risk_bd_avg']       = d$spp_risk_bd_sum       / d$spp_risk_cnt
d['spp_risk_bi_avg']       = d$spp_risk_bi_sum       / d$spp_risk_cnt
d['spp_risk_ohi1_avg']     = d$spp_risk_ohi1_sum     / d$spp_risk_cnt
d['spp_risk_ohi2_avg']     = d$spp_risk_ohi2_sum     / d$spp_risk_cnt
d['spp_hab_risk_ohi1_avg'] = d$spp_hab_risk_ohi1_sum / d$spp_hab_risk_cnt
d['spp_troph_avg']         = d$spp_troph_sum         / d$spp_troph_cnt
d['spp_popn_avg']          = d$spp_popn_sum          / d$spp_popn_cnt
d['spp_assessed_ratio']    = d$spp_risk_cnt          / (d$spp_norisk_cnt+d$spp_risk_cnt)
d['spp_risk_status']       = 1 - d$spp_risk_ohi2_avg
d['spp_risk_status_ratio'] = 1 - d$spp_risk_ohi2_avg * d$spp_assessed_ratio
d['spp_hab_risk_status']   = 1 - d$spp_hab_risk_ohi1_avg

write.csv(d, 'C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/data/cell_scores.csv', row.names=F)

#C:\Users\best\Documents\data\model\GL-NCEAS-SpeciesDiversity\data

# output GeoTIFF rasters
setwd('C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity')
r = raster('C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/data/spp_cid_gcs.tif')
idx = match(values(r), d$cid)
for (v in names(d)[2:ncol(d)]){ # v = names(d)[2]
  q = r; values(q) = NA
  values(q) = d[idx,v]
  plot(q)
  writeRaster(q, sprintf('rasters/%s.tif',v), overwrite=T)
}

# TODO: redo now that using raster instead of readGDAL for r cid
r@data = cbind(r@data, d[match(r@data$cid,d$cid),-1])
s = stack(r)

s.n = list(
     'spp_risk_cnt'='Species Count for Extinction Risk Assessed', 
     'spp_norisk_cnt'='Species Count for Extinction Risk Not Assessed',
     'spp_assessed_ratio'='Ratio of Assessed Species Counts: Assessed / Total',           
     'spp_risk_bd_avg'='Average Extinction Risk (Butchart 2004 Decimal)',
     'spp_risk_bi_avg'='Average Extinction Risk (Butchart 2004 Integer)',
     'spp_risk_ohi1_avg'='Average Extinction Risk (OHI v1)',
     'spp_risk_ohi2_avg'='Average Extinction Risk (OHI v2)',
     #'spp_hab_risk_cnt'='Habitat-Forming Species Count for Extinction Risk Assessed',
     #'spp_hab_risk_ohi1_avg'='Habitat-Forming Average Extinction Risk (OHI v1)',
     'spp_troph_cnt'='Species Count for Trophic Estimate',
     'spp_troph_avg'='Average Trophic Estimate',
     'spp_popn_cnt'='Species Count for Population Trend',
     'spp_popn_avg'='Average Population Trend',     
     'spp_risk_status'='Species Status Score: 1 - Avg Ext Risk (OHI)',
     'spp_risk_status_ratio'='Species Status Score * Ratio: 1 - [ Avg Ext Risk (OHI) * Ratio ]'
     #'spp_hab_risk_status'='Habitat-Forming Species Status Score (OHI v1)'
     )

setwd('C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/img')
for (i in 1:length(s.n)){ # i=1
  f = names(s.n[i])
  h = s.n[[i]]
  png(sprintf('%02d_%s.png',i,f), height=1000,width=1200, pointsize=18)
  plot(raster(s,f),main=h, col=tim.colors(64), maxpixels=5E5)
  #map('world',col='grey',fill=T,add=T)
  dev.off()
}

r = readGDAL('C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/data/spp_cid_gcs.tif')
names(r) = 'cid'
r@data = cbind(r@data, d[match(r@data$cid,d$cid), names(s.n)])[,-1]
names(r@data) = as.character(unlist(s.n[names(s.n)]))
s = stack(r)

pdf('rasters.pdf', width=11, height=8.5, paper='special', pointsize=8)
plot(s, col=tim.colors(64),maxpixels=5E5)
dev.off()

# output for GRASS recode
#   r.recode input=oldmap output=newmap << EOF
#     1:1:1.1:1.1
#     2:2:7.5:7.5
#     3:3:0.4:0.4
setwd('C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/data')
spp_risk_status.recode = with(d, sprintf('%d:%d:%g:%g',cid,cid,spp_risk_status,spp_risk_status))
write.table(spp_risk_status.recode, 'spp_cid2status_recode.txt', row.names=F, quote=F, col.names=F)

setwd('C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/data')
spp_hab_status.recode = with(d, sprintf('%d:%d:%g:%g',cid,cid,spp_hab_risk_status,spp_hab_risk_status))
write.table(spp_hab_status.recode, 'spp_cid2habstatus_recode.txt', row.names=F, quote=F, col.names=F)

# output as geotiff in Mollweide: too SLOW / memory problems... skipping.
r = readGDAL('C:/Users/best/Documents/data/model/GL-NCEAS-SpeciesDiversity/data/spp_cid_mol.tif')
names(r) = 'cid'
r@data = cbind(r@data, d[match(r@data$cid,d$cid), names(s.n)])[,-1]

writeGDAL(dataset, fname, drivername = "GTiff", type = "Float32",
 mvFlag = NA, options=NULL, copy_drivername = "GTiff", setStatistics=FALSE)


#plot(s,c(3,4,5,6), col=tim.colors(64))
  
  
# # look for relationships, plot residuals on map  
# plot(d$spp_risk_ohi1_avg, d$spp_popn_avg)
# m = lm(spp_risk_ohi1_avg~spp_popn_avg, data=d)
# summary(m)
# 
# plot(d$spp_troph_avg, d$spp_popn_avg)
# m = lm(spp_troph_avg~spp_popn_avg, data=d)
# summary(m)
# 
# 
# #pairs(d) # SLOW


