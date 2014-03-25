# C:\Python27\ArcGISx6410.1\python.exe N:/model/GL-NCEAS-SpeciesDiversity_v2013a/model_resilience3nm.py

import arcpy, os, subprocess, csv, sys, logging
import numpy as np
from arcpy.sa import *

# initial env
arcpy.CheckOutExtension('Spatial')
arcpy.env.overwriteOutput=1

# vars
wd      = 'N:/model/GL-NCEAS-SpeciesDiversity_v2013a' # working directory
td      = wd + '/tmp'            # temporary directory
rd      = wd + '/raw'            # raw directory
md      = wd + '/manual_output'  # manual output directory
dd      = wd + '/data'           # data directory
cd      = wd + '/cache'          # cache directory

##gdb     = td + '/geodb.gdb'      # file geodatabase
gdb     = td +'/geodb.gdb'      # local file geodatabase
log     = cd+'/ingest.log'

r_mol   = td+'/ocean.tif'        # reference raster for Mollweide snapping, extent, cellsize and projection
r_gcs   = td+'/ocean_gcs.tif'    # reference raster for WGS84 geographic coordinate system. created below

# projections
sr_mol = arcpy.SpatialReference('Mollweide (world)') # projected Mollweide (54009)
sr_gcs = arcpy.SpatialReference('WGS 1984')          # geographic coordinate system WGS84 (4326)

# set workspace
arcpy.env.workspace = gdb

# get regions 
arcpy.RasterToPolygon_conversion(td+'/rgn_offshore_3nm_mol.tif', gdb+'/rgn_offshore3nm_mol', 'NO_SIMPLIFY')
arcpy.Project_management(gdb+'/rgn_offshore3nm_mol', gdb+'/rgn_offshore3nm_gcs', sr_gcs)
arcpy.RepairGeometry_management(gdb+'/rgn_offshore3nm_gcs')

# get intersection
arcpy.Intersect_analysis([gdb+'/rgn_offshore3nm_gcs',gdb+'/rgn_fao_am_cells_gcs'], 'rgn_fao_am_cells_cells_offshore3nm_gcs')
arcpy.RepairGeometry_management(gdb+'/rgn_fao_am_cells_cells_offshore3nm_gcs')

# get area
if 'area_km2' in [f.name for f in arcpy.ListFields(gdb+'/rgn_fao_am_cells_cells_offshore3nm_gcs')]:
    arcpy.DeleteField_management(gdb+'/rgn_fao_am_cells_cells_offshore3nm_gcs', 'area_km2')
arcpy.AddField_management(      gdb+'/rgn_fao_am_cells_cells_offshore3nm_gcs', 'area_km2', 'DOUBLE')
arcpy.CalculateField_management(gdb+'/rgn_fao_am_cells_cells_offshore3nm_gcs', 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')

# export for reading by R
arcpy.TableToTable_conversion(gdb+'/rgn_fao_am_cells_cells_offshore3nm_gcs', td, 'rgn_fao_am_cells_cells_offshore3nm_gcs_tbl.dbf') # for reading in by R

arcpy
