import arcpy, os, socket, numpy, pandas, time

# Run on cmd:
#  amphitrite: C:\Python27\ArcGIS10.2\python.exe G:\ohiprep\Baltic\StockholmUniversity-Regions_v2014-04\ingest.py

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
eez_basins_shp  = '{0}/data/eez_basins.shp'.format(ad)
eez_basins_csv  = '{0}/data/eez_basins_data.csv'.format(gd)

# projections (see http://resources.arcgis.com/en/help/main/10.2/018z/pdf/projected_coordinate_systems.pdf|geographic_coordinate_systems.pdf)
sr_mol = arcpy.SpatialReference('Mollweide (world)') # projected Mollweide (54009)
sr_gcs = arcpy.SpatialReference('WGS 1984')          # geographic coordinate system WGS84 (4326)

# environment
if not os.path.exists(td): os.makedirs(td)
if not arcpy.Exists(gdb): arcpy.CreateFileGDB_management(os.path.dirname(gdb), os.path.basename(gdb))
arcpy.env.workspace       = gdb
arcpy.env.overwriteOutput = True
arcpy.env.outputCoordinateSystem = sr_gcs

### copy data inputs into gdb
##for v in ['basins', 'eez']:
##    if not arcpy.Exists('%s/%s' % (gdb,v)):
##        arcpy.FeatureClassToFeatureClass_conversion(eval(v), gdb, v) 
##
### add name field specific to shapefile
##arcpy.AddField_management('eez', 'eez_name', 'TEXT')
##arcpy.CalculateField_management('eez', 'eez_name', "!Name!", 'PYTHON_9.3')
##arcpy.AddField_management('basins', 'basin_name', 'TEXT')
##arcpy.CalculateField_management('basins', 'basin_name', "!Name!", 'PYTHON_9.3')
##
### get other basin and merge with basins
##arcpy.Erase_analysis('eez','basins','eez_e') # NOTE: slivers of eez beyond basins to exclude
##arcpy.MultipartToSinglepart_management('eez_e', 'eez_e_m')
##arcpy.Select_analysis('eez_e_m', 'eez_e_m_s', '"Shape_Area" > 1')
##arcpy.Dissolve_management('eez_e_m_s', 'basin_other')
##arcpy.AddField_management('basin_other', 'basin_name', 'TEXT')
##arcpy.CalculateField_management('basin_other', 'basin_name', "'OT'", 'PYTHON_9.3')
##arcpy.Merge_management(['basins','basin_other'], 'basins_m')
##
### intersect
##arcpy.Intersect_analysis(['eez','basins_m'], 'eez_basins_m')
##arcpy.AddField_management('eez_basins_m', 'rgn_name', 'TEXT')
##arcpy.CalculateField_management('eez_basins_m', 'rgn_name', "'%s_%s' % (!eez_name!, !basin_name!)", 'PYTHON_9.3')
##arcpy.Dissolve_management('eez_basins_m', 'eez_basins', ['eez_name','basin_name','rgn_name'])
##arcpy.AddField_management('eez_basins', 'rgn_id', 'SHORT')
##arcpy.CalculateField_management('eez_basins', 'rgn_id', "!OBJECTID!", 'PYTHON_9.3')
##
### add area
##arcpy.AddField_management('eez_basins', 'area_km2', 'DOUBLE')
##arcpy.CalculateField_management('eez_basins', 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')
##
### setup for theissen polygons 
##arcpy.Buffer_analysis('eez_basins', 'eez_basins_buf200km', '200 kilometers', dissolve_option='ALL')
##arcpy.env.extent = 'eez_basins_buf200km'
##arcpy.env.outputCoordinateSystem = sr_mol
##arcpy.CopyFeatures_management('eez_basins', 'thie')
##arcpy.Densify_edit('thie', 'DISTANCE', '1 Kilometers')
##arcpy.FeatureVerticesToPoints_management('thie', 'thie_pts', 'ALL')
## 
### delete interior points
##arcpy.Dissolve_management('thie', 'thie_d')
##arcpy.MakeFeatureLayer_management('thie_pts', 'lyr_pts')
##arcpy.SelectLayerByLocation_management('lyr_pts', 'WITHIN_CLEMENTINI', 'thie_d')
##arcpy.DeleteFeatures_management('lyr_pts')
## 
### generate thiessen polygons
##arcpy.CreateThiessenPolygons_analysis('thie_pts', 'thie_polys', 'ALL')
##arcpy.Dissolve_management('thie_polys', 'eez_basins_thiessen_mol', ['rgn_id','rgn_name'])
##arcpy.RepairGeometry_management('eez_basins_thiessen_mol')
##arcpy.env.outputCoordinateSystem = sr_gcs
##arcpy.CopyFeatures_management('eez_basins_thiessen_mol', 'eez_basins_thiessen_gcs')

### copy global and buffers
##countries = ['Denmark','Estonia','Finland','Germany','Latvia','Lithuania','Poland','Russia','Sweden']
##gl_gdb = r'C:\tmp\Global\NCEAS-Regions_v2014\geodb.gdb'
##bufs = ('','_inland1km','_offshore3nm','_inland25km','_offshore1km','_inland50km')
##for buf in bufs:
##    fc_in = 'sp%s_gcs' % buf
##    print('%s (%s)' % (fc_in, time.strftime('%H:%M:%S')))
##    
##    # copy fc
##    arcpy.Select_analysis('%s/%s' % (gl_gdb, fc_in), fc_in, '"sp_name" IN (\'%s\')' % "','".join(countries))
##    arcpy.DeleteField_management(fc_in, ['rgn_id', 'rgn_name', 'rgn_type']) 
##    arcpy.AddField_management(fc_in, 'rgn_type', 'TEXT')
##    arcpy.CalculateField_management(fc_in, 'rgn_type', '!sp_type!', 'PYTHON_9.3')
##
##    # intersect and dissolve
##    arcpy.Intersect_analysis([fc_in, 'eez_basins_thiessen_gcs', 'eez_basins_buf200km'], 'thie%s_gcs' % buf)
##    arcpy.Dissolve_management('thie%s_gcs' % buf, 'rgn%s_gcs' % buf, ['rgn_type','rgn_id', 'rgn_name'])
##
##    # add area
##    arcpy.AddField_management('%s/rgn%s_gcs' % (gdb, buf), 'area_km2', 'DOUBLE')
##    arcpy.CalculateField_management('%s/rgn%s_gcs' % (gdb, buf), 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')
##
##    # export shp and csv
##    arcpy.CopyFeatures_management('%s/rgn%s_gcs' % (gdb, buf), '%s/data/rgn%s_gcs.shp' % (ad, buf))
##    d = pandas.DataFrame(arcpy.da.TableToNumPyArray('rgn%s_gcs' % buf, ['rgn_type','rgn_id','rgn_name','area_km2']))
##    d.to_csv('%s/data/rgn%s_data.csv' % (gd, buf), index=False, encoding='utf-8')

# simplify for geojson
print('simplify (%s)' % (time.strftime('%H:%M:%S')))
arcpy.cartography.SmoothPolygon('%s/rgn_gcs' % gdb, 'rgn_smooth_gcs', 'PAEK', 100, 'FIXED_ENDPOINT', 'FLAG_ERRORS') # , 100, "FLAG_ERRORS")
arcpy.CopyFeatures_management('%s/rgn_smooth_gcs' % gdb, '%s/data/rgn_smooth_gcs.shp' % ad)
print('done (%s)' % (time.strftime('%H:%M:%S')))
