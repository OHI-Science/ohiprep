# Run on cmd: C:\Python27\ArcGISx6410.2\python.exe G:\ohiprep\Israel\Hamaarag-Regions_v2014a\model.py

# packages
import arcpy, os, socket, numpy, pandas, time

# paths
wd      = 'G:/ohiprep/Israel/Hamaarag-Regions_v2014a'
gdb     = '%s/tmp/geodb.gdb'             % wd
shp_in  = '%s/raw/Subregions_WGS.shp'    % wd
shp_out = '%s/data/regions_gcs.shp'      % wd
csv_out = '%s/data/regions_gcs_data.shp' % wd
gl_gdb  = r'C:\tmp\Global\NCEAS-Regions_v2014\geodb.gdb'

# projections
sr_mol = arcpy.SpatialReference('Mollweide (world)') # projected Mollweide (54009)
sr_gcs = arcpy.SpatialReference('WGS 1984')          # geographic coordinate system WGS84 (4326)

# initialize
os.chdir(wd)
if not os.path.exists('tmp'): os.makedirs('tmp')
if not os.path.exists('data'): os.makedirs('data')
if not arcpy.Exists(gdb): arcpy.CreateFileGDB_management(os.path.dirname(gdb), os.path.basename(gdb))

# environment
arcpy.env.workspace              = gdb
arcpy.env.overwriteOutput        = True
arcpy.env.outputCoordinateSystem = sr_gcs

### copy
##if not arcpy.Exists('%s/s' % gdb):
##    arcpy.FeatureClassToFeatureClass_conversion(shp_in, gdb, 'a')
##
### add rgn_id, rgn_name specific to input shapefile
##arcpy.AddField_management(      'a', 'rgn_id'  , 'SHORT')
##arcpy.CalculateField_management('a', 'rgn_id'  , '!Region!', 'PYTHON_9.3')
##arcpy.AddField_management(      'a', 'rgn_name', 'TEXT')
##arcpy.CalculateField_management('a', 'rgn_name', '!Name!'  , 'PYTHON_9.3')
##
### add area
##arcpy.AddField_management(      'a', 'area_km2', 'DOUBLE')
##arcpy.CalculateField_management('a', 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')
##
### setup for theissen polygons
##print('buffer (%s)' % (time.strftime('%H:%M:%S')))
##arcpy.Buffer_analysis('a', 'a_buf200km', '200 kilometers', dissolve_option='ALL')
##arcpy.env.extent = 'a_buf200km'
##arcpy.env.outputCoordinateSystem = sr_mol
##
##print('copy, densify, pts (%s)' % (time.strftime('%H:%M:%S')))
##arcpy.CopyFeatures_management('a', 'thie')
##arcpy.Densify_edit('thie', 'DISTANCE', '1 Kilometers')
##arcpy.FeatureVerticesToPoints_management('thie', 'thie_pts', 'ALL')
## 
### delete interior points
##print('dissolve (%s)' % (time.strftime('%H:%M:%S')))
##arcpy.Dissolve_management('thie', 'thie_d')
##arcpy.MakeFeatureLayer_management('thie_pts', 'lyr_pts')
##arcpy.SelectLayerByLocation_management('lyr_pts', 'WITHIN_CLEMENTINI', 'thie_d')
##arcpy.DeleteFeatures_management('lyr_pts')
## 
### generate thiessen polygons
##print('thiessen (%s)' % (time.strftime('%H:%M:%S')))
##arcpy.CreateThiessenPolygons_analysis('thie_pts', 'thie_polys', 'ALL')
##arcpy.Dissolve_management('thie_polys', 'a_thiessen_mol', ['rgn_id','rgn_name'])
##print('repair (%s)' % (time.strftime('%H:%M:%S')))
##arcpy.RepairGeometry_management('a_thiessen_mol')
##arcpy.env.outputCoordinateSystem = sr_gcs
##arcpy.CopyFeatures_management('a_thiessen_mol', 'a_thiessen_gcs')

# copy global and buffers
countries = ['Israel']
bufs = ('','_inland1km','_offshore3nm','_inland25km','_offshore1km','_inland50km')
for buf in bufs:
    fc_in = 'sp%s_gcs' % buf
    print('%s (%s)' % (fc_in, time.strftime('%H:%M:%S')))
    
    # copy fc
    arcpy.Select_analysis('%s/%s' % (gl_gdb, fc_in), fc_in, '"sp_name" IN (\'%s\')' % "','".join(countries))
    arcpy.DeleteField_management(fc_in, ['rgn_id', 'rgn_name', 'rgn_type']) 
    arcpy.AddField_management(fc_in, 'rgn_type', 'TEXT')
    arcpy.CalculateField_management(fc_in, 'rgn_type', '!sp_type!', 'PYTHON_9.3')

    # intersect and dissolve
    arcpy.Intersect_analysis([fc_in, 'a_thiessen_gcs', 'a_buf200km'], 'thie%s_gcs' % buf)
    arcpy.Dissolve_management('thie%s_gcs' % buf, 'rgn%s_gcs' % buf, ['rgn_type','rgn_id', 'rgn_name'])

    # add area
    arcpy.AddField_management('%s/rgn%s_gcs' % (gdb, buf), 'area_km2', 'DOUBLE')
    arcpy.CalculateField_management('%s/rgn%s_gcs' % (gdb, buf), 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')

    # export shp and csv
    arcpy.CopyFeatures_management('%s/rgn%s_gcs' % (gdb, buf), '%s/data/rgn%s_gcs.shp' % (wd, buf))
    d = pandas.DataFrame(arcpy.da.TableToNumPyArray('rgn%s_gcs' % buf, ['rgn_type','rgn_id','rgn_name','area_km2']))
    d.to_csv('%s/data/rgn%s_data.csv' % (wd, buf), index=False, encoding='utf-8')

# simplify for geojson
print('simplify (%s)' % (time.strftime('%H:%M:%S')))
arcpy.cartography.SmoothPolygon('%s/rgn_gcs' % gdb, 'rgn_smooth_gcs', 'PAEK', 100, 'FIXED_ENDPOINT', 'FLAG_ERRORS') # , 100, "FLAG_ERRORS")
arcpy.CopyFeatures_management('%s/rgn_smooth_gcs' % gdb, '%s/data/rgn_smooth_gcs.shp' % wd)
print('done (%s)' % (time.strftime('%H:%M:%S')))
