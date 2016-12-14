# digest_simplify.py - create simplified regions
#
# Create simplified regions from original 300 MB regions shapefile to
# something coarser but usable for quickly plotting results in R and
# the OHI mapping toolbox as GeoJSON or TopoJSON.
#
#

# TODO:
# - create details for area

import arcpy, os, re, numpy, socket

# configuration based on machine name
conf = {
    'vulcan':
    {'dir_ohiprep':'C:/Users/bbest/Documents/GitHub/ohiprep',
     'dir_data':'N:',
     'dir_tmp':'C:/tmp',
     }}[socket.gethostname()]

# paths
td  = '{0}/GL-NCEAS-Regions_v2014'.format(conf['dir_tmp']) # temp directory
dd  = '{0}/model/GL-NCEAS-Regions_v2014'.format(conf['dir_data'])    # data directory
#gd      = '{0}/Global/NCEAS-Regions_v2014'.format(conf['dir_ohiprep']) # git directory
gdb = '{0}/geodb.gdb'.format(td)                     # file geodatabase

# inputs
rgns_gcs = '{0}/data/rgns_gcs.shp'.format(dd)

# create td dir and gdb if doesn't exist
if not os.path.exists(td): os.makedirs(td)
if not arcpy.Exists(gdb): arcpy.CreateFileGDB_management(os.path.dirname(gdb), os.path.basename(gdb)) 

# projections
sr_mol = arcpy.SpatialReference('Mollweide (world)') # projected Mollweide (54009)
sr_gcs = arcpy.SpatialReference('WGS 1984')          # geographic coordinate system WGS84 (4326)

# environment
arcpy.env.workspace       = gdb
arcpy.env.overwriteOutput = True

# copy data inputs into gdb
for v in ['rgns_gcs']:
    if not arcpy.Exists('%s/%s' % (gdb, v)):
        arcpy.FeatureClassToFeatureClass_conversion(eval(v), gdb, v)

arcpy.Dice_management('rgns_gcs', 'rgn_dice10k', '10000')
arcpy.SimplifyPolygon_cartography('rgn_dice10k', 'rgn_dice10k_simplify30km',
                                  'BEND_SIMPLIFY', '30 Kilometers', '0 Unknown', 'NO_CHECK', 'KEEP_COLLAPSED_POINTS')
arcpy.Dissolve_management('rgn_gcs_dice10k_simplify30km', 'rgn_gcs_dice10k_simplify30km_d', ['rgn_type','rgn_id','rgn_name'])
arcpy.RepairGeometry_management('rgn_gcs_dice10k_simplify30km_d','DELETE_NULL')
