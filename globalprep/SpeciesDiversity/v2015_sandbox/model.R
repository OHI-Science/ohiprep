# TODO: check for final SPP status to range -0.5 to 0.5

library(foreign)
library(DBI)
library(RSQLite)
library(plyr)

# paths
root.data = '/Volumes/data_edit' # 'N:'
wd = file.path(root.data, 'model/GL-NCEAS-SpeciesDiversity_v2013a')
td        = file.path(wd, 'tmp')
dd        = file.path(wd, 'data')
cells.dbf = file.path(td, 'rgn_fao_am_pts_gcs_tbl.dbf')
#path.db   = 'D:/best/tmp/GL-NCEAS-SpeciesDiversity_v2013a/spp.db'
cells_3nm.dbf = file.path(td, 'rgn_fao_am_cells_cells_offshore3nm_gcs_tbl.dbf')
path.db   = file.path(td, 'spp.db')
redo.db   = FALSE

# set working directory
setwd(td)

# connect to db ----

# create persistent sqlite database

if (file.exists(path.db) & redo.db) file.remove(path.db) # get rid of old versions
db = dbConnect(dbDriver('SQLite'), path.db) # makes a new file # dbDisconnect(db) # rm(db) # dbSendQuery(db, 'VACUUM')

# load db ----

# load cells into db
if (!'cells' %in% dbListTables(db)){
  cells = foreign::read.dbf(cells.dbf)
  cells = rename(cells, c('CsquareCod'='csq'))[,c('cid','csq','rgn_id','rgn_typ','rgn_nam','area_km2')]
  dbWriteTable(db, 'cells', cells)
  rm(cells)
}

# load cells3nm into db
if (!'cells_3nm' %in% dbListTables(db)){
  cells_3nm = foreign::read.dbf(cells_3nm.dbf); head(cells_3nm)
  flds.cells_3nm = c('cid','grid_code'='rgn_id','area_km2')
  cells_3nm = rename(cells_3nm, flds.cells_3nm)[,flds.cells_3nm]; head(cells_3nm)
  dbWriteTable(db, 'cells_3nm', cells_3nm)
  rm(cells_3nm)
}

# load other tables
tables = c('cells_spp_iucn.csv'    = 'cells_spp',
           'spp_iucn.csv'          = 'spp',
           'am_spp_data.csv'       = 'spp_am',
           'am_cells_spp_data.csv' = 'cells_spp_am')
for (i in 1:length(tables)){
  csv = names(tables)[i]
  tbl = tables[[i]]
  if (!tbl %in% dbListTables(db)){
    d = read.csv(file.path(td, csv), na.strings= c('','NA'))
    dbWriteTable(db, tbl, d, row.names=F, overwrite=T)
    rm(d)
  }
}

#dbListTables(db)
#dbListFields(db, 'spp')

# create indexes
dbSendQuery(db, 'CREATE UNIQUE INDEX idx_cells ON cells(cid)')
dbSendQuery(db, 'CREATE UNIQUE INDEX idx_cells_spp ON cells_spp(cid, sid)')
dbSendQuery(db, 'CREATE UNIQUE INDEX idx_spp ON spp(sid)')


# test for species duplicates ----
# see https://github.com/OHI-Science/issues/issues/40

# check duplicate species names from spp.db on neptune
spp.n = dbReadTable(db, 'spp')
nrow(spp.n)
# 8173
sum(duplicated(spp.n$sciname))
# 0
head(spp.n)
table(spp.n$habitat)

# check duplicate species names from https://www.nceas.ucsb.edu/~bbest/ohi_spp/ per email SU: IUCN data
spp.o = read.csv('~/Documents/OHI/ohi_spp/data/spp.csv')
nrow(spp.o)
# 12840
sum(duplicated(spp.o$scientific))
# 54
nrow(spp.o) - nrow(spp.n)
# 4667
spp.o[duplicated(spp.o$scientific), c('scientific','class')]
# all Actinopterygii


# handle distributions, append AquaMaps ----

# flag species which have distribution determined already by IUCN rangemaps
dbSendQuery(db, 'ALTER TABLE spp ADD COLUMN src_distn TEXT')
dbSendQuery(db, "UPDATE spp SET src_distn = 'IUCN' WHERE sid IN (SELECT DISTINCT sid FROM cells_spp)")

# add sciname to aquamaps
dbSendQuery(db, 'ALTER TABLE spp_am ADD COLUMN sciname TEXT')
dbSendQuery(db, "UPDATE spp_am SET sciname = Genus || ' ' || Species")

# join aquamaps speciesid to spp
dbSendQuery(db, 'ALTER TABLE spp ADD COLUMN am_speciesid TEXT')
dbSendQuery(db, "
  UPDATE spp
  SET am_speciesid = ( SELECT spp_am.SPECIESID FROM spp_am WHERE spp_am.sciname = spp.sciname )
  WHERE EXISTS (       SELECT *                FROM spp_am WHERE spp_am.sciname = spp.sciname )")

# flag species for AquaMaps distribution if sciname matched and distribution not already handled by IUCN
dbSendQuery(db, "
  UPDATE spp SET src_distn = 'AquaMaps' 
  WHERE src_distn IS NULL AND am_speciesid IS NOT NULL")

# append cells_spp from AquaMaps with a min probability threshold of 0.4
dbSendQuery(db,"
  INSERT INTO cells_spp(sid, cid) 
  SELECT s.sid, c.cid 
  FROM cells_spp_am AS csa
  JOIN cells AS c ON c.csq = csa.CsquareCode
  JOIN spp AS s ON s.am_speciesid = csa.SpeciesID
  WHERE csa.probability >= 0.4 AND s.src_distn = 'AquaMaps'")

# calculate weighted averages per cell ----

# create lookup table of weights for extinction risk categories and population trends
weights = list(category   = list(linear = c('LC' = 0,
                                            'NT' = 0.2,
                                            'VU' = 0.4,
                                            'EN' = 0.6,
                                            'CR' = 0.8,
                                            'EX' = 1)),
               popn_trend = list(linear = c('Decreasing' = -0.5,
                                            'Stable'     =  0,                                           
                                            'Increasing' =  0.5)))

for (fld in names(weights)){ # cat(sprintf('%s\n', fld))
  for (scheme in names(weights[[fld]])){ # cat(sprintf('  %s\n', scheme))    
    lut = sprintf('lut_%s', fld)
    d = setNames(data.frame(scheme, names(weights[[fld]][[scheme]]), weights[[fld]][[scheme]]),
                 c('scheme', fld, 'weight'))              
    dbWriteTable(db, lut, d, row.names=F, overwrite=T)
  }
}

# calculate average weight and count per field and scheme
for (fld in names(weights)){
  for (scheme in names(weights[[fld]])){
    
    # calculate average extinction category weight per cell (note that category DD gets dropped b/c no entry in lut_category )
    lut = sprintf('lut_%s', fld)
    tbl = sprintf('cells_%s_%s', fld, scheme)
    
    dbSendQuery(db, sprintf('DROP TABLE IF EXISTS %s', tbl))
    d = dbGetQuery(db, sprintf("
      CREATE TABLE %s AS
        SELECT cs.cid AS cid, AVG(weight) AS avg, COUNT(s.sid) AS cnt
        FROM cells_spp AS cs
        JOIN (
          SELECT sid, %s
          FROM spp
        ) AS s USING (sid)
        JOIN (
          SELECT %s, weight 
          FROM %s 
          WHERE scheme='%s'
        ) AS w USING (%s)
        GROUP BY cs.cid", tbl, fld, fld, lut, scheme, fld))
    dbSendQuery(db, sprintf('CREATE UNIQUE INDEX idx_%s ON %s(cid)', tbl, tbl))
    
    # update cells with cnt and avg
    flds = setNames(sprintf('%s_%s_%s', fld, scheme, c('cnt','avg')), c('cnt','avg'))
    for (i in 1:length(flds)){
      fld.tbl   = names(flds)[i]
      fld.cells = flds[[i]]
      if (fld.cells %in% dbListFields(db, 'cells')){
        dbSendQuery(db, sprintf('UPDATE cells SET %s = NULL', fld.cells))
      } else {
        dbSendQuery(db, sprintf('ALTER TABLE cells ADD COLUMN %s REAL', fld.cells))
      }
      dbSendQuery(db, sprintf("
        UPDATE cells
        SET     %s = ( SELECT %s.%s FROM %s WHERE %s.cid = cells.cid )            
        WHERE EXISTS ( SELECT *     FROM %s WHERE %s.cid = cells.cid )", fld.cells, tbl, fld.tbl, tbl, tbl, tbl, tbl))
    }
  }
}

# calculate species status  per cell
if ('category_linear_score' %in% dbListFields(db, 'cells')){
  dbSendQuery(db, 'UPDATE cells SET category_linear_score = NULL')
} else {
  dbSendQuery(db, 'ALTER TABLE cells ADD COLUMN category_linear_score REAL')
}
# rescale lower end of the biodiversity goal to be 0 when 75% species are extinct, a level comparable to the five documented mass extinctions
dbSendQuery(db, 'UPDATE cells SET category_linear_score = ((1 - category_linear_avg) - 0.25) / 0.75 * 100')

# output cells
write.csv(dbReadTable(db, 'cells'), 
          sprintf('%s/cells.csv', dd), row.names=F, na='')

# calculate area-weighted regional scores ----
for (tbl in c('rgn_spp_score_2013', 'rgn_spp2013_trend')){ dbSendQuery(db, sprintf('DROP TABLE IF EXISTS %s', tbl)) }
fld.tbl = c('category_linear_score'='rgn_spp_score_2013', 'popn_trend_linear_avg'='rgn_spp_trend_2013')
for (i in 1:length(fld.tbl)){
  
  fld = names(fld.tbl)[i]
  tbl = fld.tbl[[i]]
  csv = sprintf('%s/%s.csv', td, tbl)
  
  dbSendQuery(db, sprintf('DROP TABLE IF EXISTS %s', tbl))
  dbSendQuery(db, sprintf("
  CREATE TABLE %s AS 
    SELECT   rgn_id, SUM(%s * area_km2) / SUM(area_km2) AS value
    FROM     cells
    WHERE    %s IS NOT NULL
    GROUP BY rgn_id", tbl, fld, fld))
  
  # write out csv
  r = dbGetQuery(db, sprintf('SELECT * FROM %s ORDER BY rgn_id', tbl))
  write.csv(r, csv, row.names=F, na='')
}

# calculate 3nm species diversity area-weighted regional scores for Resilience ----
dbSendQuery(db, 'DROP TABLE IF EXISTS rgn_spp_score_offshore3nm_2013')
dbSendQuery(db, "CREATE TABLE rgn_spp_score_offshore3nm_2013 AS 
  SELECT   c3.rgn_id AS rgn_id, SUM(c.category_linear_score * c3.area_km2) / SUM(c3.area_km2) / 100 AS score
  FROM     cells AS c
  JOIN     cells_3nm AS c3 USING (cid)
  WHERE    category_linear_score IS NOT NULL
  GROUP BY c3.rgn_id")
r = dbGetQuery(db, 'SELECT * FROM rgn_spp_score_offshore3nm_2013 ORDER BY rgn_id'); head(r)
write.csv(r, file.path(dd, 'rgn_spp_score_offshore3nm_2013.csv'), row.names=F, na='')


# load db 2012 ----

# load tables
tables = c('cells_2012.csv'     = 'cells_2012',
           'cells_spp_2012.csv' = 'cells_spp_2012',
           'spp_2012.csv'       = 'spp_2012')
for (i in 1:length(tables)){
  csv = names(tables)[i]
  tbl = tables[[i]]
  if (!tbl %in% dbListTables(db)){
    d = read.csv(file.path(td, csv), na.strings= c('','NA'))
    dbWriteTable(db, tbl, d, row.names=F, overwrite=T)
    rm(d)
  }
}

# create indexes
dbSendQuery(db, 'CREATE UNIQUE INDEX idx_cells_2012 ON cells_2012(cid)')
dbSendQuery(db, 'CREATE UNIQUE INDEX idx_cells_spp_2012 ON cells_spp_2012(cid, sid)')
dbSendQuery(db, 'CREATE UNIQUE INDEX idx_spp_2012 ON spp_2012(sid)')

# calculate weighted averages per cell 2012 ----
fld.tbl = c('spp_rsk_avg'='rgn_spp_score_2012data', 'spp_popn_trend'='rgn_spp_trend_2012data')
for (i in 1:length(fld.tbl)){ # i=1
  
  fld = names(fld.tbl)[i]
  tbl = fld.tbl[[i]]
  csv = sprintf('%s/%s.csv', td, tbl)
  
  dbSendQuery(db, sprintf('DROP TABLE IF EXISTS %s', tbl))
  dbSendQuery(db, sprintf("
                          CREATE TABLE %s AS 
                          SELECT   c.rgn_id AS rgn_id, SUM(c12.%s * c.area_km2) / SUM(c.area_km2) AS value
                          FROM     cells_2012 AS c12
                          JOIN     cells AS c ON c12.csquarecod=c.csq
                          WHERE    %s IS NOT NULL
                          GROUP BY c.rgn_id", tbl, fld, fld))
  
  # write out csv
  r = dbGetQuery(db, sprintf('SELECT * FROM %s ORDER BY rgn_id', tbl))
  write.csv(r, csv, row.names=F, na='')
}

# compare 2012 vs 2013 ----

# since can't do outer join in sqlite, use merge()
spp_2012 = rename(dbReadTable(db, 'spp_2012')[,c('scientific','status','popn_trend','class','src_distn')],
                  c('scientific' = 'sciname_2012',
                    'class'      = 'class_2012',
                    'status'     = 'category_2012',
                    'popn_trend' = 'popn_trend_2012',
                    'src_distn'  = 'src_distn_2012'))
spp_2013 = rename(dbReadTable(db, 'spp')[,c('sciname','category','popn_trend','class','src_distn')],
                  c('sciname'    = 'sciname_2013',
                    'class'      = 'class_2013',
                    'category'   = 'category_2013',
                    'popn_trend' = 'popn_trend_2013',
                    'src_distn'  = 'src_distn_2013'))
spp_2012$sciname = spp_2012$sciname_2012
spp_2013$sciname = spp_2013$sciname_2013
spp_all = merge(spp_2012, spp_2013, by='sciname', all=T)

# update fields so 2012 and 2013 match
spp_all$category_2012   = factor(mapvalues(spp_all$category_2012, c('NE','DD'), c(NA,NA)),
                                 levels=c('LC','NT','VU','EN','CR','EX'), ordered=T)
spp_all$category_2013   = factor(mapvalues(spp_all$category_2013, c('DD'), c(NA)),
                                 levels=c('LC','NT','VU','EN','CR','EX'), ordered=T)
spp_all$popn_trend_2012 = factor(mapvalues(spp_all$popn_trend_2012, c('unknown','decreasing','stable','increasing'), c(NA,'Decreasing','Stable','Increasing')),
                                 levels=c('Decreasing','Stable','Increasing', ordered=T))
spp_all$popn_trend_2013 = factor(mapvalues(spp_all$popn_trend_2013, c('Unknown'), c(NA)),
                                 levels=c('Decreasing','Stable','Increasing', ordered=T))
v = spp_all$class_2013
spp_all$class_2013 = ifelse(!is.na(v), sprintf('%s%s',substr(v,1,1), tolower(substr(v,2,nchar(v)))), NA)
spp_all$class = with(spp_all, ifelse(is.na(class_2013), class_2012, class_2013))

# spp_category_changed: 2013 and 2012 distributions exist, and category exists in 2012 but different in 2013
spp_category_changed = subset(spp_all, !is.na(src_distn_2013) & !is.na(src_distn_2012) & category_2012 != category_2013 & !is.na(category_2012), c(sciname,category_2012,category_2013))
write.csv(spp_category_changed, 'spp_2012to2013_category_changed.csv', row.names=F, na='')
write.csv(with(spp_category_changed, addmargins(table(category_2012, category_2013, useNA='ifany'))), 'spp_2012to2013_category_changed_summary.csv', row.names=T, na='')

# spp_category_missing: distribution and category exists in 2012 but not in 2013
spp_category_missing = subset(spp_all, (is.na(src_distn_2012) | is.na(sciname_2013) | is.na(category_2013)) & (!is.na(src_distn_2012) & !is.na(category_2012) & !is.na(sciname_2012)), c(sciname,class,category_2012))
write.csv(spp_category_missing, 'spp_2012to2013_category_missing.csv', row.names=F, na='')
write.csv(with(spp_category_missing, addmargins(table(category_2012, class, useNA='ifany'))), 'spp_2012to2013_category_missing_summary.csv', row.names=T, na='')

# spp_category_new: distribution and category exists in 2013 and not in 2012
spp_category_new = subset(spp_all, (!is.na(src_distn_2013) & !is.na(sciname_2013) & !is.na(category_2013)) & (is.na(src_distn_2012) | is.na(category_2012) | is.na(sciname_2012)), c(sciname,class,category_2013))
write.csv(spp_category_new, 'spp_2012to2013_category_new.csv', row.names=F, na='')
write.csv(with(spp_category_new, addmargins(table(category_2013, class, useNA='ifany'))), 'spp_2012to2013_category_new_summary.csv', row.names=T, na='')

# spp_popn_trend_changed: 2013 and 2012 distributions exist, and population trend exists in 2012 but different in 2013
spp_popn_trend_changed = subset(spp_all, !is.na(src_distn_2013) & !is.na(src_distn_2012) & popn_trend_2012 != popn_trend_2013 & !is.na(popn_trend_2012), c(sciname,popn_trend_2012,popn_trend_2013))
write.csv(spp_popn_trend_changed, 'spp_2012to2013_popn_trend_changed.csv', row.names=F, na='')
write.csv(with(spp_popn_trend_changed, addmargins(table(popn_trend_2012, popn_trend_2013, useNA='ifany'))), 'spp_2012to2013_popn_trend_changed_summary.csv', row.names=T, na='')

# spp_popn_trend_missing: distribution and population trend exists in 2012 but not in 2013
spp_popn_trend_missing = subset(spp_all, (is.na(src_distn_2013) | is.na(sciname_2013) | is.na(popn_trend_2013)) & (!is.na(src_distn_2012) & !is.na(popn_trend_2012) & !is.na(sciname_2012)), c(sciname,class,popn_trend_2012))
write.csv(spp_popn_trend_missing, 'spp_2012to2013_popn_trend_missing.csv', row.names=F, na='')
write.csv(with(spp_popn_trend_missing, addmargins(table(popn_trend_2012, class, useNA='ifany'))), 'spp_2012to2013_popn_trend_missing_summary.csv', row.names=T, na='')

# spp_popn_trend_new: exists in 2013 and not 2012
spp_popn_trend_new = subset(spp_all, (!is.na(src_distn_2013) & !is.na(sciname_2013) & !is.na(popn_trend_2013)) & (is.na(src_distn_2012) | is.na(popn_trend_2012) | is.na(sciname_2012)), c(sciname,class,popn_trend_2013))
write.csv(spp_popn_trend_new, 'spp_2012to2013_popn_trend_new.csv', row.names=F, na='')
write.csv(with(spp_popn_trend_new, addmargins(table(popn_trend_2013, class, useNA='ifany'))), 'spp_2012to2013_popn_trend_new_summary.csv', row.names=T, na='')

# entire summaries by year and measure, limited to those with distributions
write.csv(with(subset(spp_all, !is.na(src_distn_2012)), addmargins(table(category_2012  , class, useNA='no'))), 'spp_2012_category_summary.csv'  , row.names=T, na='')
write.csv(with(subset(spp_all, !is.na(src_distn_2013)), addmargins(table(category_2013  , class, useNA='no'))), 'spp_2013_category_summary.csv'  , row.names=T, na='')
write.csv(with(subset(spp_all, !is.na(src_distn_2012)), addmargins(table(popn_trend_2012, class, useNA='no'))), 'spp_2012_popn_trend_summary.csv', row.names=T, na='')
write.csv(with(subset(spp_all, !is.na(src_distn_2013)), addmargins(table(popn_trend_2013, class, useNA='no'))), 'spp_2013_popn_trend_summary.csv', row.names=T, na='')

# now generate a 2012 score and trend using the same data as 2013, except where extinction category or population trend is documented as changing for a given species ----
spp_category_2012chg   = read.csv('spp_2012to2013_category_changed.csv'  , na.strings=''); head(spp_category_2012chg)
spp_popn_trend_2012chg = read.csv('spp_2012to2013_popn_trend_changed.csv', na.strings=''); head(spp_popn_trend_2012chg)
dbWriteTable(db, 'spp_category_2012chg', spp_category_2012chg)
dbWriteTable(db, 'spp_popn_trend_2012chg', spp_popn_trend_2012chg)
dbGetQuery(db, 'select * from spp limit 5')

# # calculate average weight and count per field and scheme
# for (fld in names(weights)){  # 
fld = names(weights)[2] # DEBUG!
  for (scheme in names(weights[[fld]])){ # scheme = names(weights[[fld]])[1]
    
    # join spp_[fld]_2012chg to spp table
    fld.chg = sprintf('%s_2012chg', fld)
    spp.chg = sprintf('spp_%s_2012chg', fld)
    dbSendQuery(db, sprintf("ALTER TABLE spp ADD COLUMN %s TEXT", fld.chg)) # dbListFields(db, 'spp')
    dbSendQuery(db, sprintf("
            UPDATE spp
            SET     %s = ( SELECT %s.%s_2012 FROM %s WHERE %s.sciname = spp.sciname )
            WHERE EXISTS ( SELECT *          FROM %s WHERE %s.sciname = spp.sciname )", fld.chg, spp.chg, fld, spp.chg, spp.chg, spp.chg, spp.chg))
    dbSendQuery(db, sprintf("UPDATE spp SET %s = %s WHERE %s IS NULL", fld.chg, fld, fld.chg))
        
    # calculate average extinction category weight per cell (note that category DD gets dropped b/c no entry in lut_category )
    lut = sprintf('lut_%s', fld)
    tbl = sprintf('cells_%s_%s_2012chg', fld, scheme)
    
    dbSendQuery(db, sprintf('DROP TABLE IF EXISTS %s', tbl))
    d = dbGetQuery(db, sprintf("
                               CREATE TABLE %s AS
                               SELECT cs.cid AS cid, AVG(weight) AS avg, COUNT(s.sid) AS cnt
                               FROM cells_spp AS cs
                               JOIN (
                               SELECT sid, %s
                               FROM spp
                               ) AS s USING (sid)
                               JOIN (
                               SELECT %s, weight 
                               FROM %s 
                               WHERE scheme='%s'
                               ) AS w ON w.%s=s.%s
                               GROUP BY cs.cid", tbl, fld.chg, fld, lut, scheme, fld, fld.chg))
    dbSendQuery(db, sprintf('CREATE UNIQUE INDEX idx_%s ON %s(cid)', tbl, tbl))
    
    # update cells with cnt and avg
    flds = setNames(sprintf('%s_%s_2012chg_%s', fld, scheme, c('cnt','avg')), c('cnt','avg'))
    for (i in 1:length(flds)){
      fld.tbl   = names(flds)[i]
      fld.cells = flds[[i]]
      if (fld.cells %in% dbListFields(db, 'cells')){
        dbSendQuery(db, sprintf('UPDATE cells SET %s = NULL', fld.cells))
      } else {
        dbSendQuery(db, sprintf('ALTER TABLE cells ADD COLUMN %s REAL', fld.cells))
      }
      dbSendQuery(db, sprintf("
                              UPDATE cells
                              SET     %s = ( SELECT %s.%s FROM %s WHERE %s.cid = cells.cid )            
                              WHERE EXISTS ( SELECT *     FROM %s WHERE %s.cid = cells.cid )", fld.cells, tbl, fld.tbl, tbl, tbl, tbl, tbl))
    }
  }
#} # DEBUG!


# # calculate species status  per cell
# if ('category_linear_2012chg_score' %in% dbListFields(db, 'cells')){
#   dbSendQuery(db, 'UPDATE cells SET category_linear_2012chg_score = NULL')
# } else {
#   dbSendQuery(db, 'ALTER TABLE cells ADD COLUMN category_linear_2012chg_score REAL')
# }
# # rescale lower end of the biodiversity goal to be 0 when 75% species are extinct, a level comparable to the five documented mass extinctions
# dbSendQuery(db, 'UPDATE cells SET category_linear_2012chg_score = ((1 - category_linear_2012chg_avg) - 0.25) / 0.75 * 100')

# output cells with 2012chg
write.csv(dbReadTable(db, 'cells'), 
          sprintf('%s/cells.csv', dd), row.names=F, na='')

# calculate area-weighted regional scores 2012chg
fld.tbl = c('category_linear_2012chg_score'='rgn_spp_score_2012', 'popn_trend_linear_2012chg_avg'='rgn_spp_trend_2012')
#for (i in 1:length(fld.tbl)){
i=1 # DEBUG!

  fld = names(fld.tbl)[i]
  tbl = fld.tbl[[i]]
  csv = sprintf('%s/%s.csv', td, tbl)
  
  dbSendQuery(db, sprintf('DROP TABLE IF EXISTS %s', tbl))
  dbSendQuery(db, sprintf("
                          CREATE TABLE %s AS 
                          SELECT   rgn_id, SUM(%s * area_km2) / SUM(area_km2) AS value
                          FROM     cells
                          WHERE    %s IS NOT NULL
                          GROUP BY rgn_id", tbl, fld, fld))
  
  # write out csv
  r = dbGetQuery(db, sprintf('SELECT * FROM %s ORDER BY rgn_id', tbl))
  write.csv(r, csv, row.names=F, na='')
#} # DEBUG!


# generate a unified output with 2012chg and 2013 ----
d = read.csv('../../GL-NCEAS-OceanRegions_v2013a/data/rgn_details.csv')[,c('rgn_id','rgn_typ','rgn_key','rgn_nam')] # paste(names(d), collapse="','")
for (v in c('score','trend')){
  for (yr in 2012:2013){
    d = merge(x=d, all.x=T, by='rgn_id',
              y=rename(na.omit(read.csv(sprintf('%s/rgn_spp_%s_%d.csv', td, v, yr))),
                       c('value'=sprintf('spp_%s_%d', v, yr))))
  }
}
d$spp_score_dif = d$spp_score_2013 - d$spp_score_2012
d$spp_trend_dif = d$spp_trend_2013 - d$spp_trend_2012
dz = subset(d, rgn_typ=='eez' & !is.na(spp_score_dif)) # NA: Mediterranean And Black Sea
summary(dz[,c('spp_score_dif', 'spp_trend_dif')])
write.csv(d, sprintf('%s/rgn_spp.csv', dd), row.names=F, na='')


# inventory and vacuum of database ----
cat('\nTABLES: FIELDS in db\n')
for (tbl in dbListTables(db)){
  cat(sprintf('\n  %s: %s\n',tbl, paste(dbListFields(db, tbl), collapse=', ')))  
}
dbSendQuery(db, 'VACUUM') # vacuum to cleanup space

# get counts of species by class
dbListTables(db)
d = dbGetQuery(db, 'SELECT class, COUNT(sciname) AS count FROM spp GROUP BY class')
print(d, row.names=F)

# TODO: move db to model temp folder for archiving after finished: path.db = file.path(td, 'spp.db')