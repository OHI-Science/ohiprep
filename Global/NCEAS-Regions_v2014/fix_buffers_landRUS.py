# Run on cmd:
#  amphitrite: C:\Python27\ArcGIS10.2\python.exe G:\ohiprep\Global\NCEAS-Regions_v2014\fix_buffers_landRUS.py
#  optimus:    C:\Python27\ArcGISx6410.1\python.exe D:\best\docs\GitHub\ohiprep\Global\NCEAS-Regions_v2014\fix_buffers_landRUS.py

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

# inputs
buffers = ['inland1km','offshore3nm','inland25km','offshore1km','inland50km']

# environment
arcpy.env.workspace       = gdb
arcpy.env.overwriteOutput = True

# expand area to cover slivers and offshore
##arcpy.Buffer_analysis('sp_landRUSfix_manual', 'sp_landRUSfix_buf100km', '100 kilometers')

#print('repairing sp_inland1km_gcs (%s)' % (time.strftime('%H:%M:%S')))
##arcpy.RepairGeometry_management('sp_inland1km_gcs')

# loop buffers
for buf in buffers: # buf='inland1km'

sp_buf  = 'sp_%s_gcs' % buf
rgn_buf = 'rgn_%s_gcs' % buf
print('%s... (%s)' % (sp_buf, time.strftime('%H:%M:%S')))

print('  fixing Svalbard (%s)' % (time.strftime('%H:%M:%S')))
arcpy.MakeFeatureLayer_management(sp_buf, 'lyr', '"sp_name"=\'Svalbard\'')
arcpy.CalculateField_management('lyr', 'sp_id', '253')
arcpy.CalculateField_management('lyr', 'sp_key', '"SVA"')

print('  erasing erroneous offshore RUS land (%s)' % (time.strftime('%H:%M:%S')))
arcpy.Erase_analysis(sp_buf, 'sp_landRUSfix_buf100km', 'sp_%s_eRUSfix' % buf)

print('  dissolving to %s (%s)' % (sp_buf, time.strftime('%H:%M:%S')))
arcpy.Dissolve_management(
    'sp_%s_eRUSfix' % buf, sp_buf,
    ['sp_type','sp_id','sp_name','sp_key',  # note: exclude area_km2
     'rgn_type','rgn_id','rgn_name','rgn_key',
     'cntry_id12','rgn_id12','rgn_name12'])
arcpy.AddField_management(sp_buf, 'area_km2', 'DOUBLE')

# post-hoc update areas
print('  dissolve to rgns and calculate areas (%s)' % time.strftime('%H:%M:%S'))
arcpy.CalculateField_management(sp_buf, 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')
arcpy.Dissolve_management(sp_buf, rgn_buf, ['rgn_type','rgn_id','rgn_name','rgn_key'])
arcpy.AddField_management(      rgn_buf, 'area_km2', 'DOUBLE')
arcpy.CalculateField_management(rgn_buf, 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')

# export shp and csv
print('  export shp and csv (%s)' % time.strftime('%H:%M:%S'))
arcpy.CopyFeatures_management('%s/%s' % (gdb, sp_buf) , '{0}/data/{1}.shp'.format(ad, sp_buf))
arcpy.CopyFeatures_management('%s/%s' % (gdb, rgn_buf), '{0}/data/{1}.shp'.format(ad, sp_buf))
d = pd.DataFrame(arcpy.da.TableToNumPyArray(
    sp_buf,
    ['sp_type','sp_id','sp_name','sp_key','area_km2',
     'rgn_type','rgn_id','rgn_name','rgn_key',
     'cntry_id12','rgn_id12','rgn_name12']))
d.to_csv('{0}/data/sp_gcs_data.csv'.format(gd), index=False)
d = pd.DataFrame(arcpy.da.TableToNumPyArray(
    rgn_buf,
    ['rgn_type','rgn_id','rgn_name','rgn_key','area_km2']))
d.to_csv('{0}/data/rgn_gcs_data.csv'.format(gd), index=False)
print('done (%s)' % time.strftime('%H:%M:%S'))