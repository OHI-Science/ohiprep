library(plyr)
library(gdata)
library(sqldf)

# get configuration based on host machine name
conf = list(
  'AMPHITRITE'=list(  # BB's Windows 8 on MacBook Pro VMWare
    root_git = 'G:/ohigit'
    root_big = 'N:/'))[[Sys.info()['nodename']]]

  
#wd = 'N:/model/GL-NCEAS-OceanRegions_v2013a'
wd = file.path(conf$root_code, 'Global/NCEAS-Regions_v2014')
setwd(wd)

# open eez to regions lookup with eez_key
rz = rename(read.csv('manual_output/eez_rgn_2013master.csv'),
            c('rgn_id_2013'='rgn_id','rgn_key_2013'='rgn_key','rgn_nam_2013'='rgn_nam'))[,c('eez_id','eez_key','rgn_typ','rgn_id','rgn_key','rgn_nam','rgn_iso2')]

# create eez details
z = read.csv('tmp/eez_v7_mol.csv', stringsAsFactors=F)
names(z) = tolower(names(z))
z = rename(z, c('iso_3digit'='eez_iso3',
                'country'   ='eez_nam',
                'sovereign' ='sov_nam',                
                'date_chang'='date_chg'))[,c('eez_id',
                                             'eez_iso3',
                                             'eez_nam',
                                             'sov_id',
                                             'sov_nam')]
z = merge(z, rz[,c('eez_id', 'eez_key')], all=T)
z = z[,c('eez_id','eez_key','eez_nam','sov_id','sov_nam','eez_iso3')]
write.csv(z, 'data/eez_details.csv', na='', row.names=F)

# create rgn details
m = merge(x=z, by='eez_id', all=T,
          y=rz[,c('eez_id','rgn_typ','rgn_id','rgn_key','rgn_nam','rgn_iso2')])

# combine eezs into regions
r = ddply(m, .(rgn_id), summarize,
          rgn_typ   = rgn_typ[1],
          rgn_key  = rgn_key[1],
          rgn_nam  = rgn_nam[1],
          rgn_iso2 = rgn_iso2[1],
          eez_cnt  = length(eez_id),
          eezs     = paste(sprintf('%s (%d|%s)', eez_nam, eez_id, eez_key), collapse=', '))
write.csv(r, 'data/rgn_details.csv', na='', row.names=F)
#shell.exec(file.path(wd,'data/rgn_details.csv'))

# create layer rtk_rgn_labels
rl.flds = c('rgn_id'='rgn_id', 'rgn_typ'='type', 'rgn_nam'='label')
rl = rename(r, rl.flds)[,rl.flds]; tail(rl); summary(rl)
write.csv(rl, 'data/rgn_labels.csv', na='', row.names=F)

# create layer rnk_rgn_global (subset to those analysed)
rg.flds = c('rgn_id'='rgn_id', 'rgn_typ'='type', 'rgn_nam'='label')
rg = rename(subset(r, rgn_typ=='eez' & rgn_nam!='DISPUTED'), rg.flds)[,rg.flds]; tail(rg); summary(rg)
write.csv(rg[,c('rgn_id','label')], 'data/rgn_global.csv', na='', row.names=F)

# s = rename(read.xls('manual_output/ISOcodes_UNregions_scraped.xlsx', na.strings=''),
#            c('Name'='iso_nam','Alpha2_code'='iso_2code','Alpha3_code'='iso_3code','Numeric_code'='iso_id','Status'='iso_status'))
# rzs = merge(rz, s, by.x='cntry_key', by.y='iso_3code', all=T)
# write.csv(rzs, 'tmp/rzs.csv', na='', row.names=F)
# shell.exec(file.path(wd,'tmp/rzs.csv'))
# 
# 
# rzs = read.xls('manual_output/rzs.xlsx', na.strings='')
# table(as.character(rzs$rgn_key_2013[duplicated(rzs$rgn_key_2013)]))
# table(as.character(rzs$iso_code[duplicated(rzs$iso_code)]))
# table(as.character(rzs$eez_key[duplicated(rzs$eez_key)]))
# 
# head(rzs)
# rzs$rgn_key_2013[duplicated(rzs$rgn_key_2013)]
# 
# z = read.csv('tmp/eez_v7_mol.csv', stringsAsFactors=F)
# head(z)
# m = read.csv('tmp/rzs_m.csv', stringsAsFactors=F)
# head(m)
# n = merge(m, z[,c('eez_id', 'sov_nam')], all.x=T)
# dim(m)
# dim(n)
# head(n)
# tail(n)
# 
# 
# read.csv('manual_output/rgn_eez_v2013a.csv')
# 
# paste(names(s), collapse="'='','")
# 
# r = read.xls('manual_output/eez_to_regions_v2013a.xlsx', na.strings='')
# rz = rename(merge(r, z, by='eez_id', all=T),
#             c('eez_nam.x'='eez_nam.r',
#               'eez_nam.y'='eez_nam.z'))
# head(rz)
# g = rename(read.xls('raw/NGA_GeopoliticalEntitiesandCodes_2010-04.xls', sheet=1, skip=1),
#            c('Code'='gec_key',
#              'Name'='gec_nam'))[,c('gec_key','gec_nam')]
# head(g)
# g$gec_nam_lower = tolower(g$gec_nam)
# rz$eez_nam_lower = tolower(rz$eez_nam.z)
# rzg = merge(rz, g, by.x='eez_nam_lower', by.y='gec_nam_lower', all=T)
# head(rzg)
# write.csv(rzg, 'tmp/rzg.csv', na='', row.names=F)
# shell.exec(file.path(wd,'tmp/rzg.csv'))             
# 
#              
#              
# [,c('eez_id','rgn_typ','rgn_id_2013','rgn_key_2013','rgn_nam_2013')],
#              c('rgn_id_2013'='rgn_id','rgn_key_2013'='rgn_key','rgn_nam_2013'='rgn_nam'))
# 
# m = merge(z, rz, by='eez_id', all=T)
# # combine eezs into regions
# r = ddply(m, .(rgn_id,rgn_typ), summarize,
#           rgn_key  = ifelse(length(eez_id)==1, ifelse(rgn_typ=='fao', rgn_key, eez_key), na.omit(rgn_key)),
#           rgn_nam  = ifelse(length(eez_id)==1, ifelse(rgn_typ=='fao', rgn_nam, eez_nam), na.omit(rgn_nam)),
#           eez_cnt  = length(eez_id),
#           eez_ids  = ifelse(length(eez_id)==1, eez_id, paste(eez_id, collapse=',')),
#           eez_keys = ifelse(length(eez_id)==1, eez_key, paste(eez_key, collapse=',')),
#           eez_nams = ifelse(length(eez_id)==1, eez_nam, paste(eez_nam, collapse=',')))
# write.csv(r, 'data/rgn_details.csv', na='', row.names=F)
# #shell.exec(file.path(wd,'data/rgn_details.csv'))


# TODO: get centroid labels

# TODO: MOVE...
# pesticides:
#   Chemical pollution: land-based organic
#   GL-NCEAS-CleanWatersPressures/chemicals/organic-model
#   GL-NCEAS-Halpern2008_Weighted_Distance/data/*_organic_3nm.* data
#   python -m ohi.flow.model --zonal-mean-results=scoremax,pressures
#   python -m ohi.flow.export --merge
#   impacts_organic
# We extract the habitat-weighted impact scores from Halpern et al. (2008)
# using 3nm and 50mi offshore masks. We use linear scale transformation
# (score max) where the maximum value is the global maximum for all cells
# (without any spatial restrictions) in the impact raster, and then run
# zonal statistics against our OHI regions.
# $OHI_MODELDIR/GL-NCEAS-Halpern2008_Weighted/data/weighted_masked_impacts_*.tif

# fertilizer:
#   7.51. Nutrient pollution
#   Where used: Status and Pressure for Clean Waters, Pressure for most other goals
#   GL-NCEAS-CleanWatersPressures/nutrients/nitrogen-model
#   impacts_nutrient
#   ZonalStatisticsAsTable(in_zone_data, zone_field, in_value_raster, out_table, {ignore_nodata}, {statistics_type})

# TODO: arcpy.BuildRasterAttributeTable_management("c:/data/image.tif", "Overwrite")
# TODO: check differences with 2012 regions