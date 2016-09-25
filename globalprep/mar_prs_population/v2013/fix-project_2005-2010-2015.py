# C:\Python27\ArcGISx6410.2\python.exe N:\model\GL-NCEAS-CoastalPopulation_v2013\fix-project_2005-2010-2015.py

import arcpy, os, time
from arcpy.sa import *

# initial env
arcpy.CheckOutExtension('Spatial')
arcpy.env.overwriteOutput=1

# vars
wd   = 'N:/model/GL-NCEAS-CoastalPopulation_v2013'     # working directory
td   = 'C:/tmp/GL-NCEAS-CoastalPopulation_v2013'       # temp directory
mask = 'N:/model/GL-NCEAS-Halpern2008/data/masked_model.tif' # mask for extent, cellsize, snapraster

# projections
sr_mol = arcpy.SpatialReference('Mollweide (world)') # projected Mollweide (54009)
sr_gcs = arcpy.SpatialReference('WGS 1984')          # geographic coordinate system WGS84 (4326)

# workspace & scratch space
arcpy.env.workspace        = td; os.chdir(td)
arcpy.env.scratchWorkspace = td

# set env
m = '%s/%s' % (td, os.path.basename(mask))
if not arcpy.Exists(m):
    arcpy.Copy_management(mask, m)
arcpy.env.snapRaster = m
arcpy.env.cellSize = m
arcpy.env.outputCoordinateSystem = m
arcpy.env.extent = m

# convert to mol
#for yr in (2005,2010,2015):
for yr in (2010,2015):    

    tif_in  = '%s/tmp/popdensity_%d_mol.tif' % (wd, yr)
    tif     = '%s/popdensity_%d.tif'     % (td, yr)
    tif_p   = '%s/popdensity_%d_p.tif'   % (td, yr)
    tif_p_c = '%s/popdensity_%d_p_c.tif' % (td, yr)    
    tif_out = '%s/data/popdensity_%d_projected_mol.tif' % (wd, yr)

    print '%s -> %s (%s)' % (os.path.basename(tif_in), os.path.basename(tif_out), time.strftime('%H:%M:%S'))
    arcpy.Copy_management(tif_in, tif)
    #arcpy.DefineProjection_management(tif, m)
    arcpy.ProjectRaster_management(tif, tif_p, sr_mol, cell_size=arcpy.env.cellSize, resampling_type='BILINEAR')
    # Extent ({XMin}, {YMin}, {XMax}, {YMax}, {ZMin}, {ZMax}, {MMin}, {MMax})
    env = ' '.join(str(Raster(m).extent).split(' ')[0:4])
    # Clip: X-Minimum, Y-Minimum, X-Maximum, Y-Maximum
    arcpy.Clip_management(tif_p, env, tif_p_c)

    for r in (m, tif, tif_p, tif_p_c):
        print '%s: %d x %d, %s' % (os.path.basename(r), Raster(r).width, Raster(r).height, Raster(r).extent)

    arcpy.Copy_management(tif_p_c, tif_out)