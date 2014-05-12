import arcpy, os, socket, numpy

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

# add name field specific to shapefile
arcpy.AddField_management('eez', 'eez_name', 'TEXT')
arcpy.CalculateField_management('eez', 'eez_name', "!Name!", 'PYTHON_9.3')
arcpy.AddField_management('basins', 'basin_name', 'TEXT')
arcpy.CalculateField_management('basins', 'basin_name', "!Name!", 'PYTHON_9.3')

# get other basin and merge with basins
arcpy.Erase_analysis('eez','basins','eez_e') # NOTE: slivers of eez beyond basins to exclude
arcpy.MultipartToSinglepart_management('eez_e', 'eez_e_m')
arcpy.Select_analysis('eez_e_m', 'eez_e_m_s', '"Shape_Area" > 1')
arcpy.Dissolve_management('eez_e_m_s', 'basin_other')
arcpy.AddField_management('basin_other', 'basin_name', 'TEXT')
arcpy.CalculateField_management('basin_other', 'basin_name', "'OT'", 'PYTHON_9.3')
arcpy.Merge_management(['basins','basin_other'], 'basins_m')

# TODO: voronoi basins to extent of joint eez, intersect with basins

### intersect
##arcpy.Intersect_analysis(['eez','basins_m'], 'eez_basins_m')
##arcpy.AddField_management('eez_basins_m', 'rgn_name', 'TEXT')
##arcpy.CalculateField_management('eez_basins_m', 'rgn_name', "'%s_%s' % (!eez_name!, !basin_name!)", 'PYTHON_9.3')
##arcpy.Dissolve_management('eez_basins_m', 'eez_basins', ['eez_name','basin_name','rgn_name'])
##arcpy.AddField_management('eez_basins', 'rgn_id', 'SHORT')
##arcpy.CalculateField_management('eez_basins', 'rgn_id', "!OBJECTID!", 'PYTHON_9.3')


##r = arcpy.da.TableToNumPyArray('eez_basins', ['rgn_name','eez_name','basin_name','OBJECTID'])
##rgn_id = numpy.zeros((len(r),), dtype=[('rgn_id', '<i4')])
##rgn_id[:] = numpy.array(range(1, len(r)+1))
##rf = numpy.lib.recfunctions.merge_arrays([r, rgn_id], flatten=True)
##arcpy.da.ExtendTable('eez_basins', 'OBJECTID', rf, 'OBJECTID', append_only=False)


# TODO: calculate area

# TODO: buffers

# TODO: simplify and geojson