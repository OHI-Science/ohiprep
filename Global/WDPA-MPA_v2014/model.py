# cmd: C:\Python27\ArcGISx6410.2\python.exe G:\ohiprep\Global\WDPA-MPA_v2014\model.py

import arcpy, os, subprocess, csv, sys, socket
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

# paths
prod     = 'Global/WDPA-MPA_v2014'                     # name of product
dir_git  = '%s/%s' % (dirs['git'], prod)               # github directory inside ohiprep
dir_tmp  = '%s/%s' % (dirs['tmp'], prod)               # temp directory on local filesystem
dir_anx  = '%s/git-annex/%s' % (dirs['neptune'], prod) # git annex directory on neptune
gdb      = '%s/geodb.gdb' % dir_tmp                    # file geodatabase

# inputs
poly_wdpa = '%s/raw/WDPA_Apr2014_Public1/WDPA_Apr2014.gdb/WDPA_poly_Apr2014' % dir_anx
dir_rgn   = '%s/Global/NCEAS-Regions_v2014/data' % dirs['tmp']

# outputs
ply = '%s/poly_wdpa_d' % gdb
tif = '%s/wdpa_designated_mol.tif' % dir_tmp
msk = '%s/rgn_offshore3nm_inland1km_mol.tif' % dir_tmp

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

# workspace & scratch space
arcpy.env.workspace = gdb; os.chdir(dir_tmp)
arcpy.env.scratchWorkspace = dir_tmp

# develop coastal raster to use as a mask
r = Con(IsNull(Raster('%s/rgn_offshore3nm_mol.tif' % dir_rgn)), Raster('%s/rgn_inland1km_gcs.tif' % dir_rgn), Raster('%s/rgn_offshore3nm_mol.tif' % dir_rgn))
r.save(msk)
arcpy.env.snapRaster = arcpy.env.cellSize = arcpy.env.outputCoordinateSystem = arcpy.env.extent = msk

# select only designated
arcpy.Select_analysis(poly_wdpa, ply, '"STATUS"=\'Designated\'')

# create priority field, prioritizing DESIG_TYPE=National(1) over International and then the earliest STATUS_YR (inverse). highest number gets priority.
arcpy.AddField_management(ply, 'poly_priority', 'FLOAT')
code_block = """
def get_priority(typ,yr):
    return({'International':0.0,'National':2.0}[typ] + 1/(float(yr)+1))
"""
arcpy.CalculateField_management(ply, 'poly_priority', 'get_priority(typ=!DESIG_TYPE!, yr=!STATUS_YR!)', 'PYTHON_9.3', code_block)

# polygon to raster
arcpy.env.snapRaster = arcpy.env.cellSize = arcpy.env.outputCoordinateSystem = arcpy.env.extent = tmp+'/rgn_coastal_mol.tif'
arcpy.PolygonToRaster_conversion(ply, 'STATUS_YR', tmp+'/wdpa_poly_designated_mol.tif', 'MAXIMUM_COMBINED_AREA', 'poly_priority', tmp+'/rgn_coastal_mol.tif')

# tabulate 
# note: zone raster must have an attribute table, automatically created for integer rasters
TabulateArea('%s/rgn_offshore3nm_mol.tif' % dir_rgn, 'VALUE', tif, 'VALUE', '%s/rgn_offshore3nm_wdpa.dbf' % dir_tmp, msk)
TabulateArea('%s/rgn_inland1km_mol.tif'   % dir_rgn, 'VALUE', tif, 'VALUE', '%s/rgn_inland1km_wdpa.dbf'   % dir_tmp, msk)

### move all from tmp
##arcpy.env.workspace = tmp
##dat = ['geodb.gdb','wdpa_poly_designated_mol.tif','rgnoffshore3nm_vs_wdpapolydesignated.dbf','rgninland1kmmol_vs_wdpapolydesignated.dbf','rgnfao_vs_wdpapolydesignated.dbf']
##for d in dat[1:]:
##    arcpy.Copy_management(tmp+'/'+d, td+'/'+d)
##    arcpy.Delete_management(tmp+'/'+d)
