# Run on cmd: C:\Python27\ArcGISx6410.2\python.exe G:\ohiprep\Israel\Hamaarag-Regions_v2014a\model.py

# packages
import arcpy, os, socket, numpy, numpy.lib.recfunctions, pandas, time

# paths
wd        = 'G:/ohiprep/Israel/Hamaarag-Regions_v2014a'
tmp       = 'C:/tmp/Israel/Hamaarag-Regions_v2014a'
gdb       = '%s/geodb.gdb'                 % tmp
eco_shp   = r'N:\git-annex\Israel\Hamaarag-Regions_v2014a\Subregions_Int.shp'
eco_haifabay_shp   = '%s/raw\HaifaBay_offshore_inland5km_manualedit.shp' % wd
eco_redsea_shp     = '%s/raw\RedSea_rectangle_for_removal.shp' % wd
shp_out   = '%s/data/regions_gcs.shp'      % wd
csv_out   = '%s/data/regions_gcs_data.shp' % wd
sp_gdb    = r'C:\tmp\Global\NCEAS-Regions_v2014\geodb.gdb'              # neptune_data:git-annex/Global/NCEAS-Regions_v2014/geodb.gdb
pol_gadm  = r'C:\tmp\Global\GL-GADM-AdminAreas_v2\data\gadm2.gdb\gadm2' # neptune_data:stable/GL-GADM-AdminAreas_v2/data/gadm2.gdb
countries = ['Israel','Palestina'] # special case for Israel: excise Palestina in GADM from Israel in MarineRegions (offshore and inland)
country = 'Israel'

# projections
sr_mol = arcpy.SpatialReference('Mollweide (world)') # projected Mollweide (54009)
sr_gcs = arcpy.SpatialReference('WGS 1984')          # geographic coordinate system WGS84 (4326)

# initialize
os.chdir(wd)
if not os.path.exists('tmp'): os.makedirs('tmp')
if not os.path.exists('data'): os.makedirs('data')
if not os.path.exists(tmp): os.makedirs(tmp)
if not arcpy.Exists(gdb): arcpy.CreateFileGDB_management(os.path.dirname(gdb), os.path.basename(gdb))

# environment
arcpy.env.workspace              = gdb
arcpy.env.overwriteOutput        = True
arcpy.env.outputCoordinateSystem = sr_gcs

# copy
if not arcpy.Exists('eco'):
    arcpy.CopyFeatures_management(eco_shp, 'eco')
if not arcpy.Exists('eco_haifabay'):
    arcpy.CopyFeatures_management(eco_haifabay_shp, 'eco_haifabay')
    arcpy.AlterField_management('eco_haifabay', 'Region_Eng', 'NAME_1', 'NAME_1')
if not arcpy.Exists('eco_redsea'):
    arcpy.CopyFeatures_management(eco_redsea_shp, 'eco_redsea')
if not arcpy.Exists('pol'):
    arcpy.Select_analysis(pol_gadm, 'pol', "\"NAME_0\" IN ('%s')" % "','".join(countries))
if not arcpy.Exists('sp'):
    arcpy.Select_analysis('%s/sp_gcs' % sp_gdb, 'sp', "\"sp_name\" = '%s'" % country)

# dissolve to district for Israel and Palestina districts to remove after extending: West Bank, Gaza
arcpy.Dissolve_management('pol', 'pol_d', ['NAME_1'])

# setup for theissen polygons
arcpy.env.extent = 'sp'
arcpy.env.outputCoordinateSystem = sr_mol
arcpy.CopyFeatures_management('pol_d', 'pol_t')
arcpy.Densify_edit('pol_t', 'DISTANCE', '1 Kilometers')
arcpy.FeatureVerticesToPoints_management('pol_t', 'pol_t_pts', 'ALL')
 
# delete interior points to make thiessen calculate faster
arcpy.Dissolve_management('pol_t', 'pol_t_d')
arcpy.MakeFeatureLayer_management('pol_t_pts', 'lyr_pts')
arcpy.SelectLayerByLocation_management('lyr_pts', 'WITHIN_CLEMENTINI', 'pol_t_d')
arcpy.DeleteFeatures_management('lyr_pts')
arcpy.Delete_management('lyr_pts')

# generate thiessen polygons, in Mollweide
arcpy.CreateThiessenPolygons_analysis('pol_t_pts', 'pol_thie', 'ALL')
arcpy.Dissolve_management('pol_thie', 'pol_thie_d', ['NAME_1'])
arcpy.RepairGeometry_management('pol_thie_d')

# burn land pol back in
arcpy.Erase_analysis('pol_thie_d', 'pol_d', 'pol_thie_d_e')
arcpy.Merge_management(['pol_d', 'pol_thie_d_e'], 'pol_thie_m')
arcpy.Dissolve_management('pol_thie_m', 'pol_thie_mol', ['NAME_1'])
    
# remove districts in Palestina: West Bank, Gaza
# remove districts without any offshore waters: Golan, Jerusalem
arcpy.MakeFeatureLayer_management('pol_thie_mol', 'lyr')
arcpy.SelectLayerByAttribute_management('lyr', 'NEW_SELECTION', "NAME_1 IN ('West Bank','Gaza','Golan','Jerusalem')")
arcpy.DeleteFeatures_management('lyr')
arcpy.Delete_management('lyr')

# setup Haifa Bay with inland component
## arcpy.Select_analysis(eco_shp, 'eco_haifabay', "\"Region_Eng\" = 'Haifa Bay'")
## arcpy.Buffer_analysis('eco_haifabay', 'eco_haifabay_buf5km', '5 kilometers')
## manually: copied feature class and edited vertices to create HaifaBay_shp
arcpy.Erase_analysis('pol_thie_mol', 'eco_haifabay', 'pol_thie_mol_e')
arcpy.Merge_management(['pol_thie_mol_e', 'eco_haifabay'], 'pol_thie_mol_e_m')

# dissolve to just field NAME_1 and rename to rgn_name
arcpy.Dissolve_management('pol_thie_mol_e_m', 'pol_thie_mol_e_m_d', ['NAME_1'])
arcpy.AlterField_management('pol_thie_mol_e_m_d', 'NAME_1', 'rgn_name_e', 'rgn_name_e')

# add fields rgn_name (in Hebrew) and rgn_id
r = arcpy.da.TableToNumPyArray('pol_thie_mol_e_m_d', ['OBJECTID','rgn_name_e'])
h = dict(arcpy.da.TableToNumPyArray('eco', ['Region_Eng','Region_Heb']))
h[u'HaZafon']   = h[u'Northern']
h[u'HaMerkaz']  = h[u'Central']
h[u'HaDarom']   = h[u'Southern']
rgn_name = numpy.array([(h[x],) for x in r['rgn_name_e']], dtype=[('rgn_name', '<U50')])
rgn_id = numpy.array([(x,) for x in range(1, len(r)+1)], dtype=[('rgn_id', '<i4')])
r = numpy.lib.recfunctions.merge_arrays([r, rgn_id, rgn_name], flatten=True)
arcpy.da.ExtendTable('pol_thie_mol_e_m_d', 'OBJECTID', r, 'OBJECTID', append_only=False)

# create geographic coordinate system version
arcpy.env.outputCoordinateSystem = sr_gcs
arcpy.CopyFeatures_management('pol_thie_mol_e_m_d', 'thie_gcs')

# copy global and buffers
bufs = ('offshore','inland','offshore1km','inland1km','offshore3nm','inland25km')
arcpy.env.outputCoordinateSystem = sr_gcs
for buf in bufs:

    print('%s (%s)' % (buf, time.strftime('%H:%M:%S')))
    
    # copy buffer from global dissolve to no fields
    if buf == 'inland25km': # redo inland25km
        arcpy.Buffer_analysis('rgn_offshore_gcs', 'rgn_offshore_buf25km', '25 kilometers', 'FULL', 'ROUND', 'ALL')
        arcpy.Clip_analysis('rgn_offshore_buf25km', 'rgn_inland_gcs', 'sp_inland25km')
    else:
        arcpy.Select_analysis('%s/sp_%s_gcs' % (sp_gdb, buf), 'sp_%s' % buf, "sp_name = '%s'" % country)
    arcpy.Dissolve_management('sp_%s' % buf, 'sp_%s_d' % buf)

    # intersect and dissolve
    arcpy.Intersect_analysis(['thie_gcs', 'sp_%s_d' % buf], 'thie_%s' % buf)

    # exclude Red Sea (as rectangle), except for 'inland'
    if buf != 'inland':
        arcpy.Erase_analysis('thie_%s' % buf, 'eco_redsea', 'thie_%s_e' % buf)
    else:
        arcpy.CopyFeatures_management('thie_%s' % buf, 'thie_%s_e' % buf)

    # dissolve to only needed fields
    if arcpy.Exists('rgn_%s_gcs' % buf): arcpy.Delete_management('rgn_%s_gcs' % buf)
    arcpy.Dissolve_management('thie_%s_e' % buf, 'rgn_%s_gcs' % buf, ['rgn_id', 'rgn_name', 'rgn_name_e'])

    # add area
    arcpy.AddField_management(      'rgn_%s_gcs' % buf, 'area_km2', 'DOUBLE')
    arcpy.CalculateField_management('rgn_%s_gcs' % buf, 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')

    # export shp and csv
    arcpy.CopyFeatures_management('rgn_%s_gcs' % buf, '%s/data/rgn_%s_gcs.shp' % (wd, buf))
    d = pandas.DataFrame(arcpy.da.TableToNumPyArray('rgn_%s_gcs' % buf, ['rgn_id','rgn_name','rgn_name_e','area_km2']))
    d.to_csv('%s/data/rgn_%s_data.csv' % (wd, buf), index=False, encoding='utf-8')

# simplify offshore to geojson for rendering in toolbox
##arcpy.cartography.SmoothPolygon('%s/rgn_offshore_gcs' % gdb, 'rgn_offshore_smooth_gcs', 'PAEK', 100, 'FIXED_ENDPOINT', 'FLAG_ERRORS') # , 100, "FLAG_ERRORS")
##arcpy.CopyFeatures_management('%s/rgn_offshore_smooth_gcs' % gdb, '%s/data/rgn_offshore_smooth_gcs.shp' % wd)
