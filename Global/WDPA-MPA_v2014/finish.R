# generate cumulative sum of protected areas by domain (inland1km or offshore3nm)
# borrowing from calc.LSP() in functions.R'; used for CBD resilience(?)

wd =  'N:/model/GL-WDPA-MPA_v2013' # '/Volumes/data_edit/model/GL-WDPA-MPA_v2013'
setwd(wd)

library(foreign)
library(reshape2)
library(plyr)

# read in files of protected area added by year, and seperately total area in domain per region
rny_protarea_inland1km   = read.csv(file.path(wd,'data','lsp_prot_area_inland1km.csv'  ), na.strings=''); head(rny_protarea_inland1km)
rny_protarea_offshore3nm = read.csv(file.path(wd,'data','lsp_prot_area_offshore3nm.csv'), na.strings=''); head(rny_protarea_offshore3nm)
rny_protarea_ocean       = read.csv(file.path(wd,'data','lsp_prot_area_ocean.csv'      ), na.strings=''); head(rny_protarea_ocean)

rn_totarea_inland1km   = read.csv(file.path(wd,'tmp','rgn_area_inland1km.csv'  ), na.strings=''); head(rn_totarea_inland1km)
rn_totarea_offshore3nm = read.csv(file.path(wd,'tmp','rgn_area_offshore3nm.csv'), na.strings=''); head(rn_totarea_offshore3nm)
rn_totarea_ocean       = read.csv(file.path(wd,'tmp','rgn_fao_mol_area.csv'    ), na.strings=''); head(rn_totarea_offshore3nm)

# merge and cast data
ry = merge(merge(rename(rny_protarea_inland1km  , c('area_km2'='new_protarea_inland1km')),
                 rename(rny_protarea_offshore3nm, c('area_km2'='new_protarea_offshore3nm')),
                 all=T, by=c('rgn_id','year')),
           rename(rny_protarea_ocean            , c('area_km2'='new_protarea_ocean')), 
           all=T, by=c('rgn_id','year')); head(ry)

r  = merge(merge(rename(rn_totarea_inland1km,   c('area_km2'='totarea_inland1km')),
                 rename(rn_totarea_offshore3nm, c('area_km2'='totarea_offshore3nm')),
                 all=T, by=c('rgn_id')),
           rename(rn_totarea_ocean,             c('area_km2'='totarea_ocean')),
           all=T, by=c('rgn_id')); head(r)

# fill in time series from first year specific region_id up to max year for all regions and generate cumulative sum
yr.max = max(ry$year)
r.yrs = ddply(ry, .(rgn_id), function(x){
  data.frame(rgn_id=x$rgn_id[1],
             year=min(x$year):yr.max)
})
r.yrs = merge(r.yrs, ry, all.x=T)
r.yrs$new_protarea_inland1km[is.na(r.yrs$new_protarea_inland1km)]     = 0
r.yrs$new_protarea_offshore3nm[is.na(r.yrs$new_protarea_offshore3nm)] = 0
r.yrs$new_protarea_ocean[is.na(r.yrs$new_protarea_ocean)]             = 0
r.yrs = within(r.yrs, {
  cumsum_protarea_inland1km   = ave(new_protarea_inland1km  , rgn_id, FUN=cumsum)
  cumsum_protarea_offshore3nm = ave(new_protarea_offshore3nm, rgn_id, FUN=cumsum)
  cumsum_protarea_ocean       = ave(new_protarea_ocean      , rgn_id, FUN=cumsum)
  cumsum_protarea_coastal     = cumsum_protarea_inland1km + cumsum_protarea_offshore3nm
})

# get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
r.yrs = merge(r.yrs, r, all.x=T); head(r.yrs)
r.yrs = within(r.yrs,{
  pct_protarea_inland1km   = pmin(cumsum_protarea_inland1km   / totarea_inland1km   * 100, 100)
  pct_protarea_offshore3nm = pmin(cumsum_protarea_offshore3nm / totarea_offshore3nm * 100, 100)
  pct_protarea_ocean       = pmin(cumsum_protarea_ocean       / totarea_ocean       * 100, 100)
  pct_protarea_coastal     = pmin( (cumsum_protarea_inland1km + cumsum_protarea_offshore3nm) / (totarea_inland1km + totarea_offshore3nm) * 100, 100)
  LSP_status = ( pmin(pct_protarea_inland1km / 30, 1) + pmin(pct_protarea_offshore3nm / 30, 1) ) / 2 * 100
})

flds = c('rgn_id','year','new_protarea_inland1km','new_protarea_offshore3nm','new_protarea_ocean',
         'cumsum_protarea_inland1km','cumsum_protarea_offshore3nm','cumsum_protarea_coastal','cumsum_protarea_ocean',
         'totarea_inland1km','totarea_offshore3nm','totarea_ocean',
         'pct_protarea_inland1km','pct_protarea_offshore3nm','pct_protarea_coastal','pct_protarea_ocean',
         'LSP_status')

write.csv(r.yrs[,flds], file.path(wd, 'data', 'lsp_prot_area_details.csv'), row.names=F, na='') # TODO: trim intervening years between 0 and ~1880s when rest of dataset begins
write.csv(subset(r.yrs[,flds], year==2009), file.path(wd, 'data', 'lsp_prot_area_details_2009.csv'), row.names=F, na='')
write.csv(subset(r.yrs[,flds], year==2012), file.path(wd, 'data', 'lsp_prot_area_details_2012.csv'), row.names=F, na='')

