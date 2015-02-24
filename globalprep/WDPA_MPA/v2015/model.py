#cmd: C:\Python27\ArcGISx6410.2\python.exe G:\ohiprep\Global\WDPA-MPA_v2014\model.py

import arcpy, os, subprocess, csv, sys, socket
from arcpy.sa import *

# configuration based on machine name
dirs =  {'git'    :'C:/Users/visitor/Documents/github/ohiprep', 
         'neptune':'N:',
         'tmp'    :'C:/Users/visitor/tmp',
         }
print 'Setting pathnames...'
print dirs

# paths
prod     = 'globalprep/WDPA_MPA/v2015'                 # name of product
dir_git  = '%s/%s' % (dirs['git'], prod)               # local github directory inside ohiprep
dir_tmp  = '%s/%s' % (dirs['tmp'], prod)               # local temp directory
dir_anx  = '%s/git-annex/%s' % (dirs['neptune'], prod) # git annex directory on neptune
gdb      = '%s/geodb.gdb' % dir_tmp                    # file geodatabase
print 'dir_git = ' + dir_git
print 'dir_tmp = ' + dir_tmp
print 'dir_anx = ' + dir_anx

# inputs
poly_wdpa  = '%s/raw/WDPA_Jan2015_Public/WDPA_Jan2015_Public.gdb/WDPA_poly_Jan2015' % dir_anx
dir_rgn    = '%s/git-annex/Global/NCEAS-Regions_v2014/data' % dirs['neptune']

##rgn_offshore_3nm   = '%s/rgn_offshore3nm_mol.tif' % dir_rgn
##rgn_inland_1km     = '%s/new_rgn_ohara/rgn_inland1km_mol_1kmcell.tif'   % dir_rgn
rgn_offshore_3nm   = '%s/new_rgn_ohara/rgn_offshore3nm_mol_500mcell.tif' % dir_rgn
rgn_inland_1km     = '%s/new_rgn_ohara/rgn_inland1km_mol_500mcell.tif'   % dir_rgn

print 'input poly_wdpa = ' + poly_wdpa
print 'input dir_rgn = ' + dir_rgn
print 'input rgn_offshore_3nm = ' + rgn_offshore_3nm
print 'input rgn_inland_1km = ' + rgn_inland_1km


# outputs
ply = '%s/poly_wdpa_d' % gdb
tif = '%s/wdpa_designated_mol.tif' % dir_tmp
msk = '%s/rgn_offshore3nm_inland1km_mol.tif' % dir_tmp
print 'output ply = ' + ply
print 'output tif = ' + tif
print 'output msk = ' + msk


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
print 'Developing coastal raster to use as a mask'
r = Con(IsNull(Raster(rgn_offshore_3nm)), Raster(rgn_inland_1km), Raster(rgn_offshore_3nm))
r.save(msk)

print 'Setting environment based on msk'
arcpy.env.snapRaster = arcpy.env.cellSize = arcpy.env.outputCoordinateSystem = arcpy.env.extent = msk

# select only designated
print 'Selecting only designated features and creating new feature class based on STATUS = Designated'
arcpy.Select_analysis(poly_wdpa, ply, '"STATUS"=\'Designated\'')

# create priority field, prioritizing DESIG_TYPE: Regional(2) > National(1) > International (0) and then the earliest STATUS_YR (inverse). Highest number gets priority.
print 'Creating and calculating priority field'
arcpy.AddField_management(ply, 'poly_priority', 'FLOAT')
code_block = """
def get_priority(typ,yr):
    return({'International':0.0,'National':1.0,'Regional':2.0}[typ] + 1/(float(yr)+1))
"""
arcpy.CalculateField_management(ply, 'poly_priority', 'get_priority(typ=!DESIG_TYPE!, yr=!STATUS_YR!)', 'PYTHON_9.3', code_block)

# polygon to raster
print 'From Designated polygons, create raster .tif'
arcpy.env.snapRaster = arcpy.env.cellSize = arcpy.env.outputCoordinateSystem = arcpy.env.extent = msk
arcpy.PolygonToRaster_conversion(ply, 'STATUS_YR', tif, 'MAXIMUM_COMBINED_AREA', 'poly_priority', msk)

# tabulate
# note: zone raster must have an attribute table, automatically created for integer rasters
print 'Tabulating cell counts in Designated raster that overlap with offshore regions'
TabulateArea(rgn_offshore_3nm, 'VALUE', tif, 'VALUE', '%s/tmp/rgn_offshore3nm_wdpa.dbf' % dir_git, msk)
print 'Tabulating cell counts in Designated raster that overlap with inland regions'
TabulateArea(rgn_inland_1km, 'VALUE', tif, 'VALUE', '%s/tmp/rgn_inland1km_wdpa.dbf'   % dir_git, msk)
	# final .dbfs saved into local github/tmp location

# copy tmp to neptune
# paths = [gdb, tif, msk]
# if not os.path.exists('%s/tmp' % dir_anx):
#     os.makedirs('%s/tmp' % dir_anx)
# for fro in paths:
#     arcpy.Copy_management(fro, '%s/tmp/%s' % (dir_anx, os.path.basename(fro)))
#     #arcpy.Delete_management(tmp+'/'+d)
