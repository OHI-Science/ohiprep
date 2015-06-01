# File locations:
# See file storage notes in github/ohiprep/globalprep/SpeciesDiversity/cco-data_org.md
# data:
# On Neptune - git-annex:
#   SPP git-annex:    <Neptune>/git-annex/globalprep/SpeciesDiversity
# Data:
#   Aqua Maps:        <SPP git-annex>/raw/AquaMaps_*/
#   IUCN shapefiles:  <SPP git-annex>/raw/iucn_shp/
# Cached data files (scraped data and intersections):
#   IUCN details:     <SPP git-annex>/cache/iucn_details/
#   IUCN intersects:  <SPP git-annex>/cache/iucn_intersections/
# Region files: * note, these are currently symbolic links (shortcuts) to the original files
#   FAO regions:      <SPP git-annex>/regions/rgn_fao_gcs.shp
#   land shapefile:   <SPP git-annex>/regions/land_gcs.shp
# Script output files (only large files and collections of files here!)
#   work in progress: <SPP git-annex>/v201x/intermediate   * files to save for archive/reference (e.g. am_cells_data.csv, spp_iucn_marine.csv)
#                     <SPP git-annex>/tmp                  * not in year folder! temporary = don't archive
#   
#
# On GitHub:
#   SPP github: ~github/ohiprep/globalprep/SpeciesDiversity
# Scripts:
#   main script(s):   <SPP github>/  
#   functions etc:    <SPP github>/R
# Script output files: (only SMALL outputs and collections of outputs in GitHub)
#   work in progress: <SPP github>/v201x/intermediate   * files to save for archive/reference (e.g. am_cells_data.csv, spp_iucn_marine.csv)
#                     <SPP github>/tmp                  * not in year folder! temporary = don't archive
#   final outputs:    <SPP github>/v201x/data
# 
#

# Process summary:
# * Create empty fishnet (GCS, half degree cells)
#     * GCS coord system, long -180:180, lat -90:90, resolution .5
# * Associate Aquamaps attributes with fishnet polygons
#     Create point layer based on Aquamaps cell centers
#     Attach Aquamaps attributes to these points
#     Attach attributes of these points to fishnet polygon features
#       This seems to have been a slow step ~ 1.5 hours
# * Associate Aquamaps fishnet with FAO marine regions: divide the AM fishnet by FAO regions, and assign a centroid to each divided polygon
#     Subtract the land polygons from the AM fishnet polygons
#     Intersect the land-removed AM fishnet polygons with FAO marine region polygons
#       Use these to calculate marine areas
#     Convert the region-clipped AM fishnet polygons into centroids
#       This section should be examined to see how area is allocated - 
#         this is where a single AM cell can end up divided between multiple regions
#         and a single AM cell would then require multiple cell identifiers (one for each region?)
# * Bring in IUCN rangemaps, and for each species (within each group), intersect it with the AM regional centroids
#     suggestion: Auto-create a list of the shapefiles, rather than manually writing it out: 
#       * maybe something like this to create a list of the shapefiles for every represented group:
#           grp_names <- list.files(<git-annex>/SpeciesDiversity/raw/iucn_shp) %>%
#                          str_split('.') %>%
#                          unique()
#           paste(grp_names, '.shp', sep = '')
#     Looping across such a list - for each species, within each IUCN group:
#       Create a single polygon feature for entire range
#       Intersect the species range polygon with the AM regional centroids
#         including all attributes from both IUCN and AM features?  Looks like it?
#       Save the intersection feature as a shapefile in iucn_intersections.
# * Done!

### Libraries and such -----
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
### Setup paths, incl geodatabase and reference rasters ----
# # vars
# wd      = 'N:/model/GL-NCEAS-SpeciesDiversity_v2013a' # working directory
# td      = wd + '/tmp'            # temporary directory
# rd      = wd + '/raw'            # raw directory
# md      = wd + '/manual_output'  # manual output directory
# dd      = wd + '/data'           # data directory
# cd      = wd + '/cache'          # cache directory
# 
# ##gdb     = td + '/geodb.gdb'      # file geodatabase
# gdb     = 'D:/best/tmp/GL-NCEAS-SpeciesDiversity_v2013a/geodb.gdb'      # local file geodatabase
# scratch = 'D:/best/tmp/GL-NCEAS-SpeciesDiversity_v2013a/scratch'          # scratch directory
# log     = cd+'/ingest.log'
# 
# r_mol   = td+'/ocean.tif'        # reference raster for Mollweide snapping, extent, cellsize and projection
# r_gcs   = td+'/ocean_gcs.tif'    # reference raster for WGS84 geographic coordinate system. created below
# Rscript = r'C:\Program Files\R\R-3.0.1\bin\x64\Rscript.exe'
# redo = False
### setup logging, and allow for running analysis on just groups passed as argument. -----
# if len(sys.argv)> 1:
#     grp = sys.argv[1]
#     scratch = 'D:/best/tmp/GL-NCEAS-SpeciesDiversity_v2013a/scratch_%s' % grp
#     log = '%s/ingest_%s.log' % (cd, grp)
#     if not os.path.exists(scratch): os.makedirs(scratch)
# 
# # log function
# logging.basicConfig(format='%(asctime)s %(message)s',
#                     datefmt='%Y-%m-%d %I:%M:%S %p',
#                     filename=log,
#                     level=logging.DEBUG)    
# 
# if len(sys.argv)> 1:
#     logging.info('Running just group: %s' % grp)
    
### Set up projections, and set up working environment ----
### ??? is Moll used here?
# # projections
# sr_mol = arcpy.SpatialReference('Mollweide (world)') # projected Mollweide (54009)
# sr_gcs = arcpy.SpatialReference('WGS 1984')          # geographic coordinate system WGS84 (4326)
# 
# # shapefiles don't have nulls, so use geodatabase
# if not arcpy.Exists(gdb):
#     arcpy.CreateFileGDB_management(os.path.dirname(gdb), os.path.basename(gdb))
# 
# # workspace & scratch space
# arcpy.env.workspace = gdb; os.chdir(gdb)
# arcpy.env.scratchWorkspace=scratch
### Bring in land and region shapefiles into geodatabase -----
# copy land and regions
for fc in ('land_gcs','rgn_fao_gcs'):
    arcpy.FeatureClassToGeodatabase_conversion('%s/%s.shp' % (td, fc), gdb)


### create aquamaps half degree cell fishnet and get centroid locations (for point in polygon later) ----
### these are created as polygons, for intersections later.
# arcpy.CreateFishnet_management(gdb+'/am_cells_gcs', '-180 -90', '-180 90', '0.5', '0.5', '360', '720', '180 90', 'NO_LABELS', '-180 180 -90 90', 'POLYGON')
# arcpy.DefineProjection_management(gdb+'/am_cells_gcs', sr_gcs)


### ??? Does ingest_aquamaps get run from within ingest.py?  Or is this just here in case it hasn't yet been run?
# # run R code to ingest aquamaps data (ingest_aquamaps.R)
# logging.info('run R code to ingest aquamaps data (ingest_aquamaps.R)')
# proc = subprocess.Popen([Rscript, wd+'/ingest_aquamaps.R'], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
# stdout, stderr = proc.communicate(); print stdout; print stderr


### Import Aquamaps data -----
### Take output tables from Aquamaps; pull into geodatabase as tables, add indices for easier queries.
### am_cells_data.csv, am_spp_data.csv, am_cells_spp_data.csv

# # copy aquamaps tables into geodatabase, add indicies
# logging.info('copy aquamaps tables into geodatabase')
# for s in ('cells','cells_spp','spp'):
#     logging.info('  am_%s_data' % s)
#     arcpy.TableToTable_conversion('%s/am_%s_data.csv' % (td, s), gdb, 'am_%s_data' % s)
# # create indicies for speeding up queries
# logging.info('creating indicies\n  idx_cells_data')
# arcpy.AddIndex_management('am_cells_data'    , ['CsquareCode']                          , 'idx_cells_data'    , 'UNIQUE'    , 'ASCENDING')
# logging.info('  idx_cells_spp_data')
# arcpy.AddIndex_management('am_cells_spp_data', ['CsquareCode','SpeciesID','probability'], 'idx_cells_spp_data', 'NON_UNIQUE', 'ASCENDING')
# logging.info('  idx_spp_data')
# arcpy.AddIndex_management('am_spp_data'      , ['SPECIESID']                            , 'idx_spp_data'      , 'NON_UNIQUE', 'ASCENDING')


### Map the Aquamaps data layers to the half-degree cells by creating a new layer of points ------
### MakeXYEventLayer: Using the X-Y cell center coordinates from Aquamaps am_cells_data, create a layer of points indicating cell centers (lyr_am_cell_pts_gcs)
###   ??? What attributes are created in this array of points?
### Identity: Overlay the am_cells layer (maybe this should be am_cells_data???) over the Aquamaps point layer; assign the layer attributes to the enclosed Aquamaps points.
###   ??? Which layer is being overlaid - the fishnet (am_cells_gcs)?  
###   ??? what are the fishnet attributes?  OID or FID used below?
### JoinField:  To the fishnet (am_cells_gcs), join the point layer
###   ??? OID - identifying field in input (am_cells_gcs) - is this an automatically assigned cell value when creating the fishnet?
###   ??? FID_am_cells - identifying field in joined table (am_cells_pts_gcs)
###   ??? CsquareCode - the field to be joined.
### Basically - spatially map out the Aquamaps cell_data table onto the fishnet polygons?
### 
# # create points from aquamaps half-degree cells and identify which fishnet to which the CsquareCode belongs
# arcpy.MakeXYEventLayer_management(gdb+'/am_cells_data', 'CenterLong', 'CenterLat', 'lyr_am_cell_pts_gcs', sr_gcs)
# arcpy.Identity_analysis('lyr_am_cell_pts_gcs', gdb+'/am_cells', gdb+'/am_cells_pts_gcs', 'ALL')
# arcpy.JoinField_management(gdb+'/am_cells_gcs', 'OID', gdb+'/am_cells_pts_gcs', 'FID_am_cells', ['CsquareCode']) # SLOW: 1.5 hrs


### Associate fishnet with marine regions -----
### Erase land polygons from the fishnet polygons.
###   am_cells_gcs --> am_cells_e_gcs
### Then turn the clipped fishnet polygons into individual points - marine centroids for each fishnet cell
###   am_cells_e_gcs --> am_cells_e_pts_gcs
###   ??? why?  these don't seem to get used?
### Intersect the clipped fishnet polygons with the FAO regions polygons.  
###   ??? Are the FAO regions polygons something like EEZ regions?
###   ??? This seems to carve the global fishnet into regional fishnet chunks.
### Then to the regional fishnet, add fields to calculate areas.
### Finally, turn these regional fishnet polygons into centroids of their own.
###   ??? This would also divide fishnet at region boundaries (country borders) - and place centroids in partial cells
###       ??? is this the Ben Best cell ID problem?
#
# # erase land (to later use for sampling point in polygons per species rangemap)
# arcpy.Erase_analysis(gdb+'/am_cells_gcs', gdb+'/land_gcs', gdb+'/am_cells_e_gcs')
# arcpy.RepairGeometry_management(gdb+'/am_cells_e_gcs')
# arcpy.FeatureToPoint_management(gdb+'/am_cells_e_gcs', gdb+'/am_cells_e_pts_gcs', 'INSIDE')
# arcpy.Intersect_analysis([gdb+'/am_cells_e_gcs',gdb+'/rgn_fao_gcs'], gdb+'/rgn_fao_am_cells_gcs', 'ALL')
# 
# arcpy.RepairGeometry_management(gdb+'/rgn_fao_am_cells_gcs')
# arcpy.AddField_management(      gdb+'/rgn_fao_am_cells_gcs', 'area_km2', 'DOUBLE')
# arcpy.CalculateField_management(gdb+'/rgn_fao_am_cells_gcs', 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')
# arcpy.FeatureToPoint_management(gdb+'/rgn_fao_am_cells_gcs', gdb+'/rgn_fao_am_pts_gcs', 'INSIDE')


### Imports the IUCN list of marine species, incl status and pop trend ----
###   (why does it have to be so complicated?)
#
# # get list of marine species
# arcpy.TableToTable_conversion(dd+'/spp_iucn_marine_global.csv', gdb, 'spp_iucn_marine_global')
# arcpy.AddIndex_management(gdb+'/spp_iucn_marine_global', ['sciname','sid'], 'idx_sid_sciname', 'UNIQUE', 'ASCENDING')
# spp_iucn_global = {}
# with open(dd+'/spp_iucn_marine_global.csv') as f:
#     #f = open(dd+'/spp_iucn_marine_global.csv')
#     rdr = csv.reader(f, delimiter=',')
#     hdr = rdr.next() # get header
#     key_idx = hdr.index('sciname')
#     #row = rdr.next()
#     for row in rdr:
#         spp_iucn_global[row[key_idx]] = dict(zip(hdr, row))


### set up a library for IUCN rangemap file locations, to loop through -----
### pretty much just setup for the IUCN intersections
#
# # IUCN RangeMaps
# # amphibians are all terrestrial
# shps = {'birds'       :{'path':'BirdLifeInternational_BirdSpeciesDistributionMaps/BOTW/2012_spp.gdb/All_Spp',
#                         'sciname':'SCINAME',},
#         'coral'       :{'path':'IUCN_2013.1_SpatialDataDownload/All_CORAL_Oct2012/VIEW_CORAL_ogr2ogr_skipfailures.shp',
#                         'sciname':'BINOMIAL',},
#         'fish'        :{'path':'IUCN_2013.1_SpatialDataDownload/COMPLETED_MARINE_FISHES/Marine_Fish.shp',
#                         'sciname':'BINOMIAL',},
#         'mammals'     :{'path':'IUCN_2013.1_SpatialDataDownload/MAMMARINE/Mammals_Marine.shp',
#                         'sciname':'BINOMIAL',},
#         'mangroves'   :{'path':'IUCN_2013.1_SpatialDataDownload/MANGROVES/MANGROVES.shp',
#                         'sciname':'BINOMIAL',},
#         'reptiles'    :{'path':'IUCN_2013.1_SpatialDataDownload/REPTILES/Reptiles.shp',
#                         'sciname':'BINOMIAL',},
#         'seacucumbers':{'path':'IUCN_2013.1_SpatialDataDownload/SEACUCUMBERS/SeaCucumbers.shp',
#                         'sciname':'BINOMIAL',},
#         'seagrasses'  :{'path':'IUCN_2013.1_SpatialDataDownload/SEAGRASSES/VIEW_SEAGRASSES.shp',
#                         'sciname':'BINOMIAL',},}
# 
# # TODO: check for SUBSPECIES, SUBPOP, TAX_COMM
# 
# logging.info('rangemap shapefiles')
# pts_cells = gdb+'/rgn_fao_am_pts_gcs'
# groups = sorted(shps.keys())
# if len(sys.argv)> 1:
#     groups = [grp]
# for grp in groups:
#     #grp = 'coral' # DEBUG
# 
#     fc_shp = '%s/%s' % (rd, shps[grp]['path'])
#     fc     = '%s/range_%s' % (gdb, grp)
#     fc_sciname = shps[grp]['sciname']
#     fc_mar = '%s/range_%s_mar' % (gdb, grp)    
#     fc_sid = '%s/range_%s_mar_sid' % (gdb, grp)
#     logging.info('  %s' % grp)
        


### For each IUCN rangemap shapefile, create a single polygon feature for each sid -----
###   Copy shapefile feature into gdb from file
###   Join the sid from the IUCN species list to the rangemap (using the scientific name from shapefile and the table to perform the join)
###   Select the features that now contain an sid - NULL features are dropped?
###   Merge all features with same sid into a single feature? feature is called fcsid
#     if (not arcpy.Exists(fc) or redo): ## and grp!='coral':
#         logging.info('    copy')
#         arcpy.CopyFeatures_management(fc_shp, fc)
#     if 'sid' not in [f.name for f in arcpy.ListFields(fc)] or redo:
#         logging.info('    join')
#         if 'sid' in [f.name for f in arcpy.ListFields(fc)]: arcpy.DeleteField_management(fc, 'sid')        
#         arcpy.JoinField_management(fc, fc_sciname, gdb+'/spp_iucn_marine_global', 'sciname', 'sid')
#     if not arcpy.Exists(fc_mar) or redo:        
#         logging.info('    select')
#         arcpy.Select_analysis(fc, fc_mar, '"sid" IS NOT NULL')    
#         logging.info('    repair')
#         arcpy.RepairGeometry_management(fc_mar)
#     if not arcpy.Exists(fc_sid) or redo:        
#         logging.info('    dissolve')
#         arcpy.Dissolve_management(fc_mar, fc_sid, ['sid'])
#         logging.info('    repair')
#         arcpy.RepairGeometry_management(fc_sid)



### Create intersections of IUCN rangemaps with Aquamaps marine region centroids -----
### for each group with IUCN rangemaps:
### ??? Loop over each individual sid in the currently active group?
###   MakeFeatureLayer: create a temporary new layer 'lyr' using the feature for one single sid
###   Intersect: intersect the species-specific polygon with the AM marine region centroid point layer
###     Save the result as a shapefile in iucn_intersections folder, with all attributes attached.
# 
#     # iterate through individual species
#     logging.info('    intersect')
#     species_ids = [row[0] for row in arcpy.da.TableToNumPyArray(fc_sid, ('sid'))]
#     for i, sid in enumerate(species_ids): # sid = species_ids[0]
#         
#         fc_pts = '%s/iucn_intersections/range_%s_mar_sid%d_pts.shp' % (cd, grp, sid)        
#         if not arcpy.Exists(fc_pts) or redo:
#             logging.info('      sid %d (%d of %d)' % (sid, i, len(species_ids)))
#             arcpy.MakeFeatureLayer_management(fc_sid, 'lyr', '"sid"=%d' % sid)
#             arcpy.Intersect_analysis([pts_cells, 'lyr'], fc_pts, 'ALL')
# logging.info('FINISHED!')


### Wrap it up - output the AM marine region centroid point information -----
### cleaning up attribute names?
### Output the attribute table of the AM marine region centroids to a .dbf.  This contains no IUCN data.
# # MANUAL: removed extraneous FID_* fieldnames and made field "cid" for rgn_fao_am_pts_gcs (FID_ORIG) and  rgn_fao_am_cells_gcs (OID)
# pts_cells = gdb+'/rgn_fao_am_pts_gcs'
# arcpy.TableToTable_conversion(pts_cells, td, 'rgn_fao_am_pts_gcs_tbl.dbf') # for reading in by R
#
# cleanup
#[arcpy.Delete_management('%s/range_%s' % (gdb, grp)) for grp in groups if arcpy.Exists('%s/range_%s' % (gdb, grp))]
#arcpy.Compact_management(gdb)


