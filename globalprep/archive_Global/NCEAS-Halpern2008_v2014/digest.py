# cmd: C:\Python27\ArcGISx6410.2\python.exe G:\ohiprep\Global\NCEAS-Halpern2008\digest.py

import arcpy, os, subprocess, csv, sys, socket, time
from arcpy.sa import *

# configuration based on machine name
dirs = {
    'amphitrite':
    {'git'    :'G:/ohiprep',
     'neptune':'N:',
     'tmp'    :'C:/tmp',
     },
    'optimus':
    {'git'    :'D:/best/docs/GitHub/ohiprep',
     'neptune':'N:',
     'tmp'    :'D:/best/tmp',
     }}[socket.gethostname().lower()]

prod     = 'Global/NCEAS-Halpern2008'                  # name of product
dir_git  = '%s/%s' % (dirs['git'], prod)               # github directory inside ohiprep
dir_tmp  = '%s/%s' % (dirs['tmp'], prod)               # temp directory on local filesystem
dir_anx  = '%s/git-annex/%s' % (dirs['neptune'], prod) # git annex directory on neptune
dir_mdl  = '%s/model/GL-NCEAS-Halpern2008/data' % (dirs['neptune']) # git model directory on neptune with original rasters
gdb      = '%s/geodb.gdb' % dir_tmp                    # file geodatabase

# initial env
arcpy.CheckOutExtension('Spatial')
arcpy.env.overwriteOutput=1

# projections
sr_mol = arcpy.SpatialReference('Mollweide (world)') # projected Mollweide (54009)
sr_gcs = arcpy.SpatialReference('WGS 1984')          # geographic coordinate system WGS84 (4326)

# shapefiles don't have nulls, so use geodatabase
if not os.path.exists(dir_tmp):
    os.makedirs(dir_tmp)
if not arcpy.Exists(gdb):
    arcpy.CreateFileGDB_management(os.path.dirname(gdb), os.path.basename(gdb))

# copy tifs locally
arcpy.env.workspace = dir_mdl
for r in arcpy.ListRasters():
    print(' copying %s (%s)' % (r, time.strftime('%H:%M:%S')))
    arcpy.Copy_management(r, '%s/%s' % (dir_tmp, r))
# stopped: copying masked_ecosystems_d_s_bottom.tif (00:40:33)

# FILLED UP DISK, so skipping this script...