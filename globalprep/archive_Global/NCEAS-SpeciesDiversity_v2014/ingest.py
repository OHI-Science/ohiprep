# C:\Python27\ArcGISx6410.1\python.exe N:/model/GL-NCEAS-SpeciesDiversity_v2013a/ingest.py

import arcpy, os, subprocess, csv, sys, logging
import numpy as np
from arcpy.sa import *

# add helper
sys.path.append(r'N:\usr\local\src\python')
from ohi_arcpy import rename_fields 

# initial env
arcpy.CheckOutExtension('Spatial')
arcpy.env.overwriteOutput=1

# vars
wd      = 'N:/model/GL-NCEAS-SpeciesDiversity_v2013a' # working directory
td      = wd + '/tmp'            # temporary directory
rd      = wd + '/raw'            # raw directory
md      = wd + '/manual_output'  # manual output directory
dd      = wd + '/data'           # data directory
cd      = wd + '/cache'          # cache directory

##gdb     = td + '/geodb.gdb'      # file geodatabase
gdb     = 'D:/best/tmp/GL-NCEAS-SpeciesDiversity_v2013a/geodb.gdb'      # local file geodatabase
scratch = 'D:/best/tmp/GL-NCEAS-SpeciesDiversity_v2013a/scratch'          # scratch directory
log     = cd+'/ingest.log'

r_mol   = td+'/ocean.tif'        # reference raster for Mollweide snapping, extent, cellsize and projection
r_gcs   = td+'/ocean_gcs.tif'    # reference raster for WGS84 geographic coordinate system. created below
Rscript = r'C:\Program Files\R\R-3.0.1\bin\x64\Rscript.exe'
redo = False

if len(sys.argv)> 1:
    grp = sys.argv[1]
    scratch = 'D:/best/tmp/GL-NCEAS-SpeciesDiversity_v2013a/scratch_%s' % grp
    log = '%s/ingest_%s.log' % (cd, grp)
    if not os.path.exists(scratch): os.makedirs(scratch)

# log function
logging.basicConfig(format='%(asctime)s %(message)s',
                    datefmt='%Y-%m-%d %I:%M:%S %p',
                    filename=log,
                    level=logging.DEBUG)    

if len(sys.argv)> 1:
    logging.info('Running just group: %s' % grp)
    
# projections
sr_mol = arcpy.SpatialReference('Mollweide (world)') # projected Mollweide (54009)
sr_gcs = arcpy.SpatialReference('WGS 1984')          # geographic coordinate system WGS84 (4326)

# shapefiles don't have nulls, so use geodatabase
if not arcpy.Exists(gdb):
    arcpy.CreateFileGDB_management(os.path.dirname(gdb), os.path.basename(gdb))

# workspace & scratch space
arcpy.env.workspace = gdb; os.chdir(gdb)
arcpy.env.scratchWorkspace=scratch

# copy land and regions
for fc in ('land_gcs','rgn_fao_gcs'):
    arcpy.FeatureClassToGeodatabase_conversion('%s/%s.shp' % (td, fc), gdb)

# create aquamaps half degree cell fishnet and get centroid locations (for point in polygon later)
arcpy.CreateFishnet_management(gdb+'/am_cells_gcs', '-180 -90', '-180 90', '0.5', '0.5', '360', '720', '180 90', 'NO_LABELS', '-180 180 -90 90', 'POLYGON')
arcpy.DefineProjection_management(gdb+'/am_cells_gcs', sr_gcs)

# run R code to ingest aquamaps data (ingest_aquamaps.R)
logging.info('run R code to ingest aquamaps data (ingest_aquamaps.R)')
proc = subprocess.Popen([Rscript, wd+'/ingest_aquamaps.R'], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
stdout, stderr = proc.communicate(); print stdout; print stderr

# copy aquamaps tables into geodatabase, add indicies
logging.info('copy aquamaps tables into geodatabase')
for s in ('cells','cells_spp','spp'):
    logging.info('  am_%s_data' % s)
    arcpy.TableToTable_conversion('%s/am_%s_data.csv' % (td, s), gdb, 'am_%s_data' % s)
# create indicies for speeding up queries
logging.info('creating indicies\n  idx_cells_data')
arcpy.AddIndex_management('am_cells_data'    , ['CsquareCode']                          , 'idx_cells_data'    , 'UNIQUE'    , 'ASCENDING')
logging.info('  idx_cells_spp_data')
arcpy.AddIndex_management('am_cells_spp_data', ['CsquareCode','SpeciesID','probability'], 'idx_cells_spp_data', 'NON_UNIQUE', 'ASCENDING')
logging.info('  idx_spp_data')
arcpy.AddIndex_management('am_spp_data'      , ['SPECIESID']                            , 'idx_spp_data'      , 'NON_UNIQUE', 'ASCENDING')

# create points from aquamaps half-degree cells and identify which fishnet to which the CsquareCode belongs
arcpy.MakeXYEventLayer_management(gdb+'/am_cells_data', 'CenterLong', 'CenterLat', 'lyr_am_cell_pts_gcs', sr_gcs)
arcpy.Identity_analysis('lyr_am_cell_pts_gcs', gdb+'/am_cells', gdb+'/am_cells_pts_gcs', 'ALL')
arcpy.JoinField_management(gdb+'/am_cells_gcs', 'OID', gdb+'/am_cells_pts_gcs', 'FID_am_cells', ['CsquareCode']) # SLOW: 1.5 hrs

# erase land (to later use for sampling point in polygons per species rangemap)
arcpy.Erase_analysis(gdb+'/am_cells_gcs', gdb+'/land_gcs', gdb+'/am_cells_e_gcs')
arcpy.RepairGeometry_management(gdb+'/am_cells_e_gcs')
arcpy.FeatureToPoint_management(gdb+'/am_cells_e_gcs', gdb+'/am_cells_e_pts_gcs', 'INSIDE')
arcpy.Intersect_analysis([gdb+'/am_cells_e_gcs',gdb+'/rgn_fao_gcs'], gdb+'/rgn_fao_am_cells_gcs', 'ALL')

arcpy.RepairGeometry_management(gdb+'/rgn_fao_am_cells_gcs')
arcpy.AddField_management(      gdb+'/rgn_fao_am_cells_gcs', 'area_km2', 'DOUBLE')
arcpy.CalculateField_management(gdb+'/rgn_fao_am_cells_gcs', 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')
arcpy.FeatureToPoint_management(gdb+'/rgn_fao_am_cells_gcs', gdb+'/rgn_fao_am_pts_gcs', 'INSIDE')

# get list of marine species
arcpy.TableToTable_conversion(dd+'/spp_iucn_marine_global.csv', gdb, 'spp_iucn_marine_global')
arcpy.AddIndex_management(gdb+'/spp_iucn_marine_global', ['sciname','sid'], 'idx_sid_sciname', 'UNIQUE', 'ASCENDING')
spp_iucn_global = {}
with open(dd+'/spp_iucn_marine_global.csv') as f:
    #f = open(dd+'/spp_iucn_marine_global.csv')
    rdr = csv.reader(f, delimiter=',')
    hdr = rdr.next() # get header
    key_idx = hdr.index('sciname')
    #row = rdr.next()
    for row in rdr:
        spp_iucn_global[row[key_idx]] = dict(zip(hdr, row))

# IUCN RangeMaps
# amphibians are all terrestrial
shps = {'birds'       :{'path':'BirdLifeInternational_BirdSpeciesDistributionMaps/BOTW/2012_spp.gdb/All_Spp',
                        'sciname':'SCINAME',},
        'coral'       :{'path':'IUCN_2013.1_SpatialDataDownload/All_CORAL_Oct2012/VIEW_CORAL_ogr2ogr_skipfailures.shp',
                        'sciname':'BINOMIAL',},
        'fish'        :{'path':'IUCN_2013.1_SpatialDataDownload/COMPLETED_MARINE_FISHES/Marine_Fish.shp',
                        'sciname':'BINOMIAL',},
        'mammals'     :{'path':'IUCN_2013.1_SpatialDataDownload/MAMMARINE/Mammals_Marine.shp',
                        'sciname':'BINOMIAL',},
        'mangroves'   :{'path':'IUCN_2013.1_SpatialDataDownload/MANGROVES/MANGROVES.shp',
                        'sciname':'BINOMIAL',},
        'reptiles'    :{'path':'IUCN_2013.1_SpatialDataDownload/REPTILES/Reptiles.shp',
                        'sciname':'BINOMIAL',},
        'seacucumbers':{'path':'IUCN_2013.1_SpatialDataDownload/SEACUCUMBERS/SeaCucumbers.shp',
                        'sciname':'BINOMIAL',},
        'seagrasses'  :{'path':'IUCN_2013.1_SpatialDataDownload/SEAGRASSES/VIEW_SEAGRASSES.shp',
                        'sciname':'BINOMIAL',},}

# TODO: check for SUBSPECIES, SUBPOP, TAX_COMM

logging.info('rangemap shapefiles')
pts_cells = gdb+'/rgn_fao_am_pts_gcs'
groups = sorted(shps.keys())
if len(sys.argv)> 1:
    groups = [grp]
for grp in groups:
    #grp = 'coral' # DEBUG

    fc_shp = '%s/%s' % (rd, shps[grp]['path'])
    fc     = '%s/range_%s' % (gdb, grp)
    fc_sciname = shps[grp]['sciname']
    fc_mar = '%s/range_%s_mar' % (gdb, grp)    
    fc_sid = '%s/range_%s_mar_sid' % (gdb, grp)
    logging.info('  %s' % grp)
        
    if (not arcpy.Exists(fc) or redo): ## and grp!='coral':
        logging.info('    copy')
        arcpy.CopyFeatures_management(fc_shp, fc)
    if 'sid' not in [f.name for f in arcpy.ListFields(fc)] or redo:
        logging.info('    join')
        if 'sid' in [f.name for f in arcpy.ListFields(fc)]: arcpy.DeleteField_management(fc, 'sid')        
        arcpy.JoinField_management(fc, fc_sciname, gdb+'/spp_iucn_marine_global', 'sciname', 'sid')
    if not arcpy.Exists(fc_mar) or redo:        
        logging.info('    select')
        arcpy.Select_analysis(fc, fc_mar, '"sid" IS NOT NULL')    
        logging.info('    repair')
        arcpy.RepairGeometry_management(fc_mar)
    if not arcpy.Exists(fc_sid) or redo:        
        logging.info('    dissolve')
        arcpy.Dissolve_management(fc_mar, fc_sid, ['sid'])
        logging.info('    repair')
        arcpy.RepairGeometry_management(fc_sid)

    # iterate through individual species
    logging.info('    intersect')
    species_ids = [row[0] for row in arcpy.da.TableToNumPyArray(fc_sid, ('sid'))]
    for i, sid in enumerate(species_ids): # sid = species_ids[0]
        
        fc_pts = '%s/iucn_intersections/range_%s_mar_sid%d_pts.shp' % (cd, grp, sid)        
        if not arcpy.Exists(fc_pts) or redo:
            logging.info('      sid %d (%d of %d)' % (sid, i, len(species_ids)))
            arcpy.MakeFeatureLayer_management(fc_sid, 'lyr', '"sid"=%d' % sid)
            arcpy.Intersect_analysis([pts_cells, 'lyr'], fc_pts, 'ALL')
logging.info('FINISHED!')

# MANUAL: removed extraneous FID_* fieldnames and made field "cid" for rgn_fao_am_pts_gcs (FID_ORIG) and  rgn_fao_am_cells_gcs (OID)
pts_cells = gdb+'/rgn_fao_am_pts_gcs'
arcpy.TableToTable_conversion(pts_cells, td, 'rgn_fao_am_pts_gcs_tbl.dbf') # for reading in by R

# cleanup
[arcpy.Delete_management('%s/range_%s' % (gdb, grp)) for grp in groups if arcpy.Exists('%s/range_%s' % (gdb, grp))]
arcpy.Compact_management(gdb)