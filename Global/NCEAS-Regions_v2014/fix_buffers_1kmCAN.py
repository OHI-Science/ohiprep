# Run on cmd:
#  amphitrite: C:\Python27\ArcGIS10.2\python.exe G:\ohiprep\Global\NCEAS-Regions_v2014\fix_buffers_1kmCAN.py
#  optimus:    C:\Python27\ArcGISx6410.1\python.exe D:\best\docs\GitHub\ohiprep\Global\NCEAS-Regions_v2014\fix_buffers_1kmCAN.py

# modules
import arcpy, numpy, os, sys, re, socket, pandas, time, math

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
nm      = 'NCEAS-Regions_v2014'                                      # name of data product
td      = '{0}/Global/{1}'.format(conf['dir_tmp'], nm)                      # temp directory on local filesystem
gdb     = '{0}/geodb.gdb'.format(td)                                 # file geodatabase
ad      = '{0}/git-annex/Global/{1}'.format(conf['dir_neptune'], nm) # git annex directory on neptune
gd      = '{0}/Global/{1}'.format(conf['dir_git'], nm)               # git directory on local filesystem

# environment
arcpy.env.workspace       = gdb
arcpy.env.overwriteOutput = True

# inputs
buffers = ['_inland1km','_offshore1km'] # 'offshore3nm','inland25km','inland50km'
dict_CAN = {
    'sp_id'     :218,
    'sp_name'   :'Canada',
    'sp_key'    :'CAN',
    'rgn_id'    :218,
    'rgn_name'  :'Canada',
    'rgn_key'   :'CAN',
    'cntry_id12':'CAN',
    'rgn_id12'  :166,
    'rgn_name12':'Canada'}


# TODO: merge scripts to fix CAN and RUS. fix erroneous land, apply RUS fix
sp_landfix_manual
# TODO: erase sp_landRUSfix_manual

# loop buffers
#for buf in buffers:
buf='_inland1km'

sp_buf  = '%s/sp%s_gcs' % (gdb, buf)
rgn_buf = '%s/rgn%s_gcs' % (gdb, buf)

if arcpy.Exists('lyr'): arcpy.Delete_management('lyr')
arcpy.MakeFeatureLayer_management(sp_buf, 'lyr', '"sp_name" IS NULL')

# update field values to Canada
for fld, val in dict_CAN.iteritems():
    print fld, val
    if type(val) is str:
        val_str = "'%s'" % val
    else:
        val_str = '%g' % val
    arcpy.CalculateField_management('lyr', fld, val_str, 'PYTHON_9.3')

# update {sp|rgn}_type to land or eez
if buf == '_inland1km':
    sp_type = 'land'
else:
    sp_type = 'eez'
arcpy.CalculateField_management('lyr',  'sp_type', "'%s'" % sp_type, 'PYTHON_9.3')
arcpy.CalculateField_management('lyr', 'rgn_type', "'%s'" % sp_type, 'PYTHON_9.3')

# dissolve and rename
arcpy.Dissolve_management(sp_buf, '%s_1kmCANfix' % sp_buf, dict_CAN.keys()) 
arcpy.Rename('%s/sp%s_gcs' % (gdb, buf), '%s/sp%s_gcs_pre1kmCANfix' % (gdb, buf))
arcpy.Rename('%s/%s_1kmCANfix' % (gdb, sp_buf), '%s/%s' % (gdb, sp_buf))

# add area, dissolve to regions
arcpy.AddField_management(sp_buf, 'area_km2', 'DOUBLE')
arcpy.CalculateField_management(sp_buf, 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')
arcpy.Dissolve_management(sp_buf, rgn_buf, ['rgn_type','rgn_id','rgn_name','rgn_key'])
arcpy.AddField_management(      rgn_buf, 'area_km2', 'DOUBLE')
arcpy.CalculateField_management(rgn_buf, 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')

# export shp and csv
print('  export shp and csv (%s)' % time.strftime('%H:%M:%S'))
arcpy.CopyFeatures_management(sp_buf , '{0}/data/sp{1}_gcs.shp'.format(ad, buf))
arcpy.CopyFeatures_management(rgn_buf, '{0}/data/rgn{1}_gcs.shp'.format(ad, buf))
d = pd.DataFrame(arcpy.da.TableToNumPyArray(
    sp_buf,
    ['sp_type','sp_id','sp_name','sp_key','area_km2',
     'rgn_type','rgn_id','rgn_name','rgn_key',
     'cntry_id12','rgn_id12','rgn_name12']))
d.to_csv('{0}/data/sp{1}_gcs.shp'.format(gd, buf), index=False)
d = pd.DataFrame(arcpy.da.TableToNumPyArray(
    rgn_buf,
    ['rgn_type','rgn_id','rgn_name','rgn_key','area_km2']))
d.to_csv('{0}/data/rgn{1}_gcs.shp'.format(gd, buf), index=False)
print('done (%s)' % time.strftime('%H:%M:%S'))