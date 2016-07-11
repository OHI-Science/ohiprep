# File: ohiprep/globalprep/lsp/v2016/rasterize_rgn.py
#
# Modified June 2016 by Casey O'Hara
# * Inputs: 
#   * An EEZ region .shp package.  This will be filtered to just eez. The
#     .shp projection should be Mollweide, as should the .tif base raster.
#   * A .tif with desired projection and resolution, e.g. Mollweide 500m.
#     This will be used as the base raster for the raster creation.
# * Output: 
#   * A .tif of cell values based on "rgn_id" with priority given
#     to maximum area.

print ('Create a raster of OHI regions')

print ('importing from arcpy...')

import arcpy, os, subprocess, csv, sys, socket
from arcpy.sa import *

### configuration based on machine name
mazu = 'M:'
         
### paths
data_yr = 'd2014'
dir_anx = '%s/git-annex/globalprep/spatial/%s/data' % (mazu, data_yr)
print ('dir_anx = ' + dir_anx)


### inputs
poly_rgn_all  = '%s/regions_mol.shp' % dir_anx
rast_base = '%s/rgn_mol_raster_500m/rgn_offshore3nm_mol_500mcell.tif' % dir_anx

print ('input poly_rgn_all = ' + poly_rgn_all)
print ('input rast_base    = ' + rast_base)
if not os.path.exists(poly_rgn_all):
    print('poly_rgn_all: ' + poly_rgn_all + ' not found')
if not os.path.exists(rast_base):
    print('rast_base: ' + rast_base + ' not found')


### outputs
if not os.path.exists('%s/tmp' % dir_anx):
    os.makedirs('%s/tmp' % dir_anx)
poly_eez_tmp  = '%s/tmp/regions_mol_eez.shp' % dir_anx
rast_out      = '%s/rgn_mol_raster_500m/rgn_eez_mol_500mcell.tif' % dir_anx

print ('output poly_eez_tmp = ' + poly_eez_tmp)
print ('output rast_out     = ' + rast_out)

### initial env
arcpy.CheckOutExtension('Spatial')
arcpy.env.overwriteOutput = 1

print ('Setting environment based on rast_base.')
arcpy.env.snapRaster             = rast_base
arcpy.env.cellSize               = rast_base
arcpy.env.outputCoordinateSystem = rast_base
arcpy.env.extent                 = rast_base

### select only EEZ and FAO regions
print 'Selecting only EEZ and FAO regions from poly_rgn_all and saving to poly_eez_tmp.'
arcpy.Select_analysis(poly_rgn_all, poly_eez_tmp, '("rgn_typ" = \'eez\') OR ("rgn_typ" = \'fao\')')

# polygon to raster
print ('From filtered EEZ and FAO polygons, create raster .tif with cell values based on rgn_id.')
arcpy.PolygonToRaster_conversion(poly_eez_tmp, 'rgn_id', rast_out, 'MAXIMUM_COMBINED_AREA', cellsize = rast_base)

print ('All done! find the new raster at ' + rast_out)

