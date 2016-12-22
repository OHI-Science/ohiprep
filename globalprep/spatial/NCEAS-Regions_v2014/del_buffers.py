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

print('deleting buffers (%s)' % time.strftime('%H:%M:%S'))

# loop buffers
for fc in arcpy.ListFeatureClasses('buf_*'):
    print('  %s (%s)' % (fc, time.strftime('%H:%M:%S')))
    arcpy.Delete_management(fc)