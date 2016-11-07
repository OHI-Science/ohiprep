# File: ohiprep/globalprep/lsp/v2016/rasterize_wdpa.py
#
# Modified June 2016 by Casey O'Hara
# * Inputs: 
#   * A prepared .shp package - WDPA-MPA database filtered
#     for appropriate "STATUS" and "MANG_PLAN" fields (and whatever
#     else deemed necessary).  This filtering will be done in R.  The
#     .shp projection should be Mollweide, as should the .tif base raster.
#   * A .tif with desired projection and resolution, e.g. Mollweide 500m.
#     This will be used as the base raster for the raster creation.
# * Output: 
#   * A .tif of cell values based on "STATUS_YR" with priority given
#     to oldest year of protection.

import arcpy, os, subprocess, csv, sys, socket
from arcpy.sa import *

# configuration based on machine name
dirs =  {'mazu':'M:'}
         
scenario = 'v2016'

print ('Setting pathnames...')
print (dirs)

# paths
prod     = 'globalprep/lsp/%s'    % scenario             # name of product
dir_anx      = '%s/git-annex/globalprep' % dirs['mazu']
print ('dir_anx = ' + dir_anx)

# inputs
poly_wdpa = '%s/_raw_data/wdpa_mpa/d2016/shps/WDPA_May2016_shp_xformed.shp' % dir_anx
rast_base = '%s/spatial/v2015/data/rgn_raster_500m/rgn_offshore3nm_mol_500mcell.tif' % dir_anx

print ('input poly_wdpa = ' + poly_wdpa)
print ('base raster = ' + rast_base)

# outputs
rast_out  = '%s/lsp/%s/int/wdpa_designated_mol.tif' % (dir_anx, scenario)
print ('output raster = ' + rast_out)

# initial env
arcpy.CheckOutExtension('Spatial')
arcpy.env.overwriteOutput = 1

print ('Setting environment based on rast_base.')
arcpy.env.snapRaster             = rast_base
arcpy.env.cellSize               = rast_base
arcpy.env.outputCoordinateSystem = rast_base
arcpy.env.extent                 = rast_base

# create priority field, prioritizing by earliest STATUS_YR (take inverse of STATUS_YR)
print ('Creating and calculating priority field')
arcpy.AddField_management(poly_wdpa, 'poly_priority', 'FLOAT')
arcpy.CalculateField_management(poly_wdpa, 'poly_priority', '1/(!STATUS_YR! + 1)', 'PYTHON_9.3')

# polygon to raster
print ('From pre-prepared polygons, create raster .tif')
arcpy.PolygonToRaster_conversion(poly_wdpa, 'STATUS_YR', rast_out, 'MAXIMUM_COMBINED_AREA', 'poly_priority', rast_base)


