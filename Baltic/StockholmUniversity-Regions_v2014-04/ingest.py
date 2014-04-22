import arcpy, os, socket

# configuration based on machine name
conf = {
    'amphitrite':
    {'dir_git'    :'G:/ohiprep',
     'dir_neptune':'N:',
     'dir_tmp'    :'C:/tmp',
     },
    'optimus':
    {'dir_git'    :'D:/best/docs/GitHub/ohiprep',
     'dir_neptune':'N:',
     'dir_tmp'    :'D:/best/tmp',
     }}[socket.gethostname().lower()]

# paths
nm      = 'Baltic/StockholmUniversity-Regions_v2014-04'       # name of data product
td      = '{0}/{1}'.format(conf['dir_tmp'], nm)               # temp directory on local filesystem
ad      = '{0}/git-annex/{1}'.format(conf['dir_neptune'], nm) # git annex directory on neptune
gd      = '{0}/{1}'.format(conf['dir_git'], nm)               # git directory on local filesystem
gdb     = '{0}/geodb.gdb'.format(td)                          # file geodatabase

# data inputs
basins = '{0}/raw/BASINS_Clip.shp'.format(ad)
eez    = '{0}/raw/BalticEconomicZones.shp'.format(ad)

# data outputs
rgn_shp  = '{0}/data/rgn_gcs.shp'.format(ad)
sp_shp   = '{0}/data/sp_gcs.shp'.format(ad)
rgn_csv  = '{0}/data/rgn_data.csv'.format(gd)
sp_csv   = '{0}/data/sp_data.csv'.format(gd)

# projections (see http://resources.arcgis.com/en/help/main/10.2/018z/pdf/projected_coordinate_systems.pdf|geographic_coordinate_systems.pdf)
sr_gcs = arcpy.SpatialReference('WGS 1984')  # geographic coordinate system WGS84 (4326)

# environment
if not os.path.exists(td): os.makedirs(td)
if not arcpy.Exists(gdb): arcpy.CreateFileGDB_management(os.path.dirname(gdb), os.path.basename(gdb))
arcpy.env.workspace       = gdb
arcpy.env.overwriteOutput = True
arcpy.env.outputCoordinateSystem = sr_gcs

# copy data inputs into gdb
for v in ['basins', 'eez']:
    if not arcpy.Exists('%s/%s' % (gdb,v)):
        arcpy.FeatureClassToFeatureClass_conversion(eval(v), gdb, v) 

# intersect
arcpy.Intersect_analysis(['eez','basins'], 'eez_basins')
arcpy.AddField_management('eez_basins', 'rgn_name', 'TEXT')
arcpy.CalculateField_management('eez_basins', 'rgn_name', "'%s_%s' % (!Name!, !Name_1!)", 'PYTHON_9.3')