# C:\Python27\ArcGISx6410.1\python.exe N:\model\GL-NCEAS-CoastalPopulation_v2013\model.py

import arcpy, os, subprocess, csv, sys, math
from arcpy.sa import *

# add helper
sys.path.append(r'N:\usr\local\src\python')
from ohi_arcpy import rename_fields 

# initial env
arcpy.CheckOutExtension('Spatial')
arcpy.env.overwriteOutput=1

# vars
wd      = 'N:/model/GL-NCEAS-CoastalPopulation_v2013' # working directory
td      = wd + '/tmp'                                 # temporary directory
dd      = wd + '/data'                                # data directory
scratch = 'D:/best/tmp'                               # scratch directory

# projections
sr_mol = arcpy.SpatialReference('Mollweide (world)') # projected Mollweide (54009)
sr_gcs = arcpy.SpatialReference('WGS 1984')          # geographic coordinate system WGS84 (4326)

# workspace & scratch space
arcpy.env.workspace = dd; os.chdir(dd)
arcpy.env.scratchWorkspace=scratch
arcpy.env.snapRaster = arcpy.env.cellSize = arcpy.env.outputCoordinateSystem = arcpy.env.extent = td+'/rgn_inland_25mi_mol.tif'

# convert to mol
for yr in (2005,2010,2015):
    arcpy.AddMessage('  popdensity_%d_gcs.tif' % yr)
    arcpy.ProjectRaster_management('%s/popdensity_%d_gcs.tif' % (td, yr),
                                   '%s/popdensity_%d_mol.tif' % (td, yr), sr_mol, resampling_type='NEAREST')

# setup values for equations
r_rgn  = Raster(td+'/rgn_inland_25mi_mol.tif')
r_2005 = Raster(td+'/popdensity_2005_mol.tif')
r_2010 = Raster(td+'/popdensity_2010_mol.tif')
r_2015 = Raster(td+'/popdensity_2015_mol.tif')
cell_km2 = math.pow(float(arcpy.env.cellSize) / 1000, 2) # since original rasters were in population density (# people / km2)

# using output from model_equations.R
equations = {2005:"r_2005",
             2006:"0.8 * r_2005 + 0.2 * r_2010",
             2007:"0.6 * r_2005 + 0.4 * r_2010",
             2008:"0.4 * r_2005 + 0.6 * r_2010",
             2009:"0.2 * r_2005 + 0.8 * r_2010",
             2010:"r_2010",
             2011:"0.8 * r_2010 + 0.2 * r_2015",
             2012:"0.6 * r_2010 + 0.4 * r_2015",
             2013:"0.4 * r_2010 + 0.6 * r_2015",
             2014:"0.2 * r_2010 + 0.8 * r_2015",
             2015:"r_2015",}

# calculate number of people in a pixel (vs density) and extract sum per region
for yr in range(2005,2016):
    r_out = '%s/popdensity_%d_mol.tif'       % (dd, yr)
    d_out = '%s/rgn_popsum%d_inland25mi.dbf' % (dd, yr)
    arcpy.AddMessage('  %d interpolate and sum by region' % yr)
    
    r = eval('(%s) * %g' % (equations[yr], cell_km2))    
    r.save(r_out)    
    ZonalStatisticsAsTable(td+'/rgn_inland_25mi_mol.tif', 'VALUE', r, d_out, 'DATA', 'SUM')

# build pyramids
arcpy.AddMessage('build pyramids')
arcpy.BuildPyramidsandStatistics_management(dd, 'INCLUDE_SUBDIRECTORIES', 'BUILD_PYRAMIDS', 'CALCULATE_STATISTICS', skip_existing='SKIP_EXISTING')
