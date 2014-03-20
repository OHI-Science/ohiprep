# setup ----
library(reshape2)
library(plyr)
library(dplyr)

#  from get paths configuration based on host machine name
source('src/R/common.R') # set dir_neptune_data
# Otherwise, presume that scripts are always working from your default ohiprep folder
dir_d = 'Global/NCEAS-Regions_v2014'

# data paths
eez_dbf          = file.path(dir_neptune_data, 'git-annex/Global/MarineRegions_EEZ_v8/raw/World_EEZ_v8_2014_HR.dbf')
land_dbf         = file.path(dir_neptune_data, 'stable/GL-VLIZ-EEZs_v7/data/EEZ_land_v1.dbf')
eez_rgn_2013_csv = file.path(dir_neptune_data, 'model/GL-NCEAS-OceanRegions_v2013a/manual_output/eez_rgn_2013master.csv')
fao_dbf          = file.path(dir_neptune_data, 'model/GL-FAO-CCAMLR_v2014/data/fao_ccamlr_gcs.dbf')
eez_rgn_2014_csv = file.path(dir_d           , 'tmp/eez_rgn_2014.csv')

# create tmp dir if doesn't exist
dir.create(file.path(dir_d, 'tmp'), showWarnings=F)

# data prep ----

# read data tables ----
eez  = foreign::read.dbf(eez_dbf, as.is=T); head(eez); summary(eez)
fao  = foreign::read.dbf(fao_dbf, as.is=T); head(fao); summary(fao)
land = foreign::read.dbf(land_dbf, as.is=T); head(fao); summary(land)
z    = read.csv(eez_rgn_2013_csv, stringsAsFactors=F); head(z); tail(z); summary(z)

# split

# write new eez for 2014
write.csv(z, eez_rgn_2014_csv, na='', row.names=F)

# merge data ----
m = z %.%
  select(eez_id, eez_iso3,
         rgn_type = rgn_typ, 
         rgn_id   = rgn_id_2013,
         rgn_name = rgn_nam_2013) %.%
  merge(
    eez %.%
      select(eez_id       = EEZ_ID, 
             eez_name_shp = Country,
             eez_iso3_shp = ISO_3digit), 
    by='eez_id', all=T) %.%
  merge(
    fao %.%
      select(fao_source = SOURCE,
             fao_code   = F_CODE,
             fao_ocean  = OCEAN) %.%
      filter(fao_source!='CCAMLR') %.%  # exclude Antarctica (213)
      mutate(eez_id = as.integer(fao_code) + 1000) %.%
      select(eez_id, fao_code, fao_source, fao_ocean),
    by='eez_id', all=T) %.%
  merge(
    land %.%
      select(eez_iso3  = ISO_3digit,
             land_name = Country) %.%
      filter(eez_iso3!='-'), 
    by='eez_iso3', all=T) %.%
  select(rgn_type, rgn_id, rgn_name, rgn_name, eez_id, eez_name_shp, eez_iso3_shp, fao_source, fao_code, fao_ocean, land_name) %.%
  arrange(rgn_type, rgn_id, rgn_name, eez_id, eez_name_shp, fao_code, land_name)
head(m); tail(m)

# TODO: look at duplicates

# check for missing or mismatched eez's
print(subset(m, rgn_type=='eez' & ( eez_name_shp != rgn_name | is.na(eez_name_shp) | is.na(rgn_name) ), 
             c(eez_id, rgn_type, rgn_id, rgn_name, eez_name_shp)), row.names=F)  # only accented names showing up
#  eez_id rgn_type rgn_id            rgn_name             eez_name        eez_name_shp
#     252      eez    255            DISPUTED Disputed Sudan-Egypt               Egypt
#     100      eez    100 Republique du Congo  R_publique du Congo R?publique du Congo
#     244      eez    244             Curacao              Curacao             Cura?ao
#      32      eez     32             Reunion              R_union             R?union
# OK: just wierd accents in eez_name_shp so not matching eez_name

eez %.% arrange(EEZ, Country) %.% select(EEZ_ID, EEZ, Country, ISO_3digit)
names(eez)

# Antarctica
print(subset(m, rgn_name=='Antarctica'), row.names=F)
#  eez_iso3 eez_id rgn_type rgn_id   rgn_name   eez_name eez_name_shp fao_code fao_source fao_ocean  land_name
#       ATA    213      eez    213 Antarctica Antarctica   Antarctica     <NA>       <NA>      <NA> Antarctica

# check for missing fao's
print(subset(m, rgn_type=='fao' & is.na(fao_ocean), 
             c(eez_id, rgn_type, rgn_id, rgn_name, eez_name, fao_ocean)), row.names=F)
#  eez_id rgn_type rgn_id                             rgn_name                             eez_name fao_ocean
#    1048      fao    268                  Atlantic, Antarctic                  Atlantic, Antarctic      <NA>
#    1088      fao    278                   Pacific, Antarctic                   Pacific, Antarctic      <NA>
#    1037      fao    265          Mediterranean And Black Sea          Mediterranean And Black Sea      <NA>
#    1058      fao    271 Indian Ocean, Antarctic And Southern Indian Ocean, Antarctic And Southern      <NA>
# OK: just Antarctica or Mediterranean (which has no FAO non-EEZ area)
# remove these fao's
m = subset(m, !(rgn_type=='fao' & is.na(fao_ocean)))

# check for EEZs missing land
m_land_noeez = subset(m, rgn_type=='eez' & is.na(land_name) & rgn_name!='DISPUTED', 
                      c(eez_id, rgn_type, rgn_id, rgn_name, eez_iso3, eez_name, land_name))
print(m_land_noeez, row.names=F)
#  eez_id rgn_type rgn_id                          rgn_name eez_iso3                          eez_name land_name
#      85      eez     85                         Ascension      ASC                         Ascension      <NA>
#     250      eez    250                             Aruba       AW                             Aruba      <NA>
#     245      eez    245                           Bonaire       BQ                           Bonaire      <NA>
#      13      eez     13 Northern Mariana Islands and Guam      MNP Northern Mariana Islands and Guam      <NA>
#      88      eez     88                  Tristan da Cunha      TAA                  Tristan da Cunha      <NA>
# TODO MANUAL: inspect these regions manually to determine which ISO needs to be selected, split into parts and assigned new values
cat(sprintf("arcpy.Select_analysis('eez', 'eez_noland', \"ISO_3digit IN ('%s')\")", paste(m_land_noeez$eez_iso3, collapse="','")))
# These are cases where land iso3 needs to be replaced to erroneous iso3 of eez (land -> eez):
#   1) land is falsely associated with other EEZ iso3 and needs to be subsequently split: 
#      * SHN for ASC and TAA
#   2) land eez iso3 does not match eez iso3, so just rename land's iso3: 
#      * MNP++ -> MNP
#      * ABW   -> AW
#      * BES   -> BQ


# duplicate EEZs?
m_eez_duplicated = m %.%
  subset(rgn_type=='eez' & rgn_name!='DISPUTED' & 
           ( duplicated(eez_iso3, fromLast=T) | duplicated(eez_iso3, fromLast=F) )) %.%
  select(eez_iso3, land_name, rgn_name, eez_name, eez_id) %.%
  arrange(eez_iso3, land_name, rgn_name, eez_name, eez_id)
head(m_eez_duplicated)
tail(m_eez_duplicated)
#   eez_iso3                         land_name                               rgn_name                               eez_name eez_id
# 1      ATF French Southern & Antarctic Lands Amsterdam Island and Saint Paul Island Amsterdam Island and Saint Paul Island     92
# 2      ATF French Southern & Antarctic Lands                        Bassas da India                        Bassas da India     34
# 3      ATF French Southern & Antarctic Lands                         Crozet Islands                         Crozet Islands     91
# 4      ATF French Southern & Antarctic Lands                       Glorioso Islands                       Glorioso Islands     30
# 5      ATF French Southern & Antarctic Lands                             Ile Europa                             Ile Europa     35
# 6      ATF French Southern & Antarctic Lands                           Ile Tromelin                           Ile Tromelin     36
# ...
#    eez_iso3                            land_name              rgn_name              eez_name eez_id
# 47      UMI United States Minor Outlying Islands           Wake Island           Wake Island     12
# 48      USA                        United States         United States                Alaska    170
# 49      USA                        United States         United States                Hawaii    160
# 50      USA                        United States         United States         United States    163
# 51      ZAF                         South Africa Prince Edward Islands Prince Edward Islands     90
# 52      ZAF                         South Africa          South Africa          South Africa    102
# NOTE: repeats are because of either:
#   1) one region to many EEZs (eg rgn_name=United States and eez_names=Alaska,Hawaii,United States) OR
#   2) land parts are not getting distinguished, eg eez_iso3=ATF
# Strategy is to"
#   1) in R, output list of EEZs missing land and duplicates in R, then
#   2) in ArcPy, split EEZneighbors, dissolve land back to EEZ level, and aggregate up to regions

# get land and eez which need to be matched by neighbors
cat(sprintf("arcpy.MakeFeatureLayer_management('eez_noant', 'lyr_eez', \"ISO_3digit IN ('%s')\")", paste(sort(unique(c('ASC','TAA','SHN',m_eez_duplicated$eez_iso3))), collapse="','")))
cat(sprintf("arcpy.MakeFeatureLayer_management('land'     , 'lyr_land', \"ISO_3digit IN ('%s')\")", paste(sort(unique(c('ASC','TAA','SHN',m_eez_duplicated$eez_iso3))), collapse="','")))
# arcpy.MakeFeatureLayer_management('eez_noant', 'lyr_eez_noant', "ISO_3digit IN ('ASC','ATF','AUS','BES','BRA','CHL','DOM','ECU','ESP','IND','KIR','PER','PRT','SHN','TAA','TLS','UMI','USA','ZAF')")
# arcpy.MakeFeatureLayer_management('eez_noant', 'lyr_eez_noant', "ISO_3digit IN ('ASC','ATF','AUS','BES','BRA','CHL','DOM','ECU','ESP','IND','KIR','PER','PRT','SHN','TAA','TLS','UMI','USA','ZAF')")




unique(m_eez_duplicated$eez_iso3)
# "ATF" "AUS" "BES" "BRA" "CHL" "DOM" "ECU" "ESP" "IND" "KIR" "PER" "PRT" "TLS" "UMI" "USA" "ZAF"
# TODO: select these land regions, split, neighbor analysis and assign
# TODO: why are BES,DOM,PER showing up as duplicates since only one instance in z, so must be in merging?

# TODO: Chile & Peru selections

# check for multiple regions per single land iso


# check FAO regions
anti_join(eez, eez_rgn_2013, by=c('EEZ_ID', 'eez_id'))


er = read.csv('manual_output/eez_rgn_2013master.csv')
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
write.csv(r, file.path(dir_d, 'data/rgn_details.csv'), na='', row.names=F)
#shell.exec(file.path(wd,'data/rgn_details.csv'))

# create layer rtk_rgn_labels
rl.flds = c('rgn_id'='rgn_id', 'rgn_typ'='type', 'rgn_nam'='label')
rl = rename(r, rl.flds)[,rl.flds]; tail(rl); summary(rl)
write.csv(rl, file.path(dir_d, 'data/rgn_labels.csv'), na='', row.names=F)

# create layer rnk_rgn_global (subset to those analysed)
rg.flds = c('rgn_id'='rgn_id', 'rgn_typ'='type', 'rgn_nam'='label')
rg = rename(subset(r, rgn_typ=='eez' & rgn_nam!='DISPUTED'), rg.flds)[,rg.flds]; tail(rg); summary(rg)
write.csv(rg[,c('rgn_id','label')], file.path(dir_d, 'data/rgn_global.csv'), na='', row.names=F)

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