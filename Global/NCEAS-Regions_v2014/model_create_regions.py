# model_create_regions.py: create OHI 2014 regions
#
# bbest@nceas.ucsb.edu 2014-02-27
#
# Description of product. The OHI 2014 regions cover the entire earth with non-overlapping regions with the following fields:
#  * rgn_type, having possible values:
#    - eez: exclusive economic zone (EEZ)
#    - land: terrestrial land
#    - fao: offshore Food & Agriculture Organization (FAO) Major Fishing Areas, with EEZs erased
#    - land-noeez: land without any EEZ
#    - disputed-eez: disputed EEZ
#    - disputed-land: disputed land
#  * rgn_id: unique identifier (within same rgn_type)
#  * rgn_name: name for region
#
# Inputs.
#  * EEZ, EEZ_land (http://marineregions.org)
#  * FAO: Food & Agriculture Organization (FAO) Major Fishing Areas, including CCAMLR Antarctica regions (http://www.fao.org/fishery/area/search/en)
#  * Z: master lookup table to go from EEZ to OHI regions from 2013 regions
#
# Process.
#  * remove Antarctica from EEZ
#  * erase EEZ_land from FAO
#  * dissolve CCAMLR regions in FAO to create Antarctica EEZ
#  * add 1000 to FAO ids to create FAO rgn_id
#  * erase EEZ from EEZ_land to get land
#  * replace some iso3 in land to match EEZ ('MNP++' to 'MNP', 'ABW' to 'AW', 'BES' to 'BQ')
#  * select out land parts either misidentified ('SHN' for eezs 'ASC', 'TAA') or iso3 is duplicated having several eez_ids
#    iso3 IN ('SHN','ATF','AUS','BRA','CHL','ECU','ESP','IND','KIR','PRT','TLS','UMI','USA','ZAF')
#  * associate these land parts with the neighboring EEZs
#  * create Antarctica land by erasing rest from earth box and dissolving every polygon with a centroid less than 60 degrees latitude
#  * go through slivers of FAO and holes from earth box erased by the rest and manually associate with legit region
#  * convert EEZ of Caspian and Black Seas to land
#  * merge all products and peform checks for overlap and geometry repair
#
# Changes since OHI 2013
# * New EEZ splits:
#   - 140 Guadeloupe and Martinique
#   - 116 Puerto Rico and Virgin Islands of the United States

# TODO:
#  * integrate Caspian and Black Sea removal earlier
#  * remove overlapping Peru / Chile land
#  * check for and remove any land slivers next to FAO high seas
#  * split Guadalupe & Martinique

import arcpy, os, re, numpy as np, socket, pandas as pd
from collections import Counter
from numpy.lib import recfunctions
arcpy.SetLogHistory(True) # C:\Users\bbest\AppData\Roaming\ESRI\Desktop10.2\ArcToolbox\History

# configuration based on machine name
conf = {
    'Amphitrite':
    {'dir_git'    :'G:/ohiprep',
     'dir_neptune':'N:',
     'dir_tmp'    :'C:/tmp',
     }}[socket.gethostname()]

# paths
nm      = 'NCEAS-Regions_v2014'                                      # name of data product
td      = '{0}/{1}'.format(conf['dir_tmp'], nm)                      # temp directory on local filesystem
gdb     = '{0}/geodb.gdb'.format(td)                                 # file geodatabase
ad      = '{0}/git-annex/Global/{1}'.format(conf['dir_neptune'], nm) # git annex directory on neptune

# data inputs
# EEZ plus land (http://marineregions.org)
##eez        = '{0}/stable/GL-VLIZ-EEZs_v7/data/eez_v7_gcs.shp'.format(conf['dir_neptune'])
eez        = '{0}/git-annex/Global/MarineRegions_EEZ_v8/raw/World_EEZ_v8_2014_HR.shp'.format(conf['dir_neptune'])
eezland    = '{0}/stable/GL-VLIZ-EEZs_v7/data/EEZ_land_v1.shp'.format(conf['dir_neptune'])
# FAO for open ocean regions, with CCAMLR Antarctica regions
fao        = '{0}/model/GL-FAO-CCAMLR_v2014/data/fao_ccamlr_gcs.shp'.format(conf['dir_neptune'])
# master lookup table to go from EEZ to OHI regions
z_csv      = '{0}/model/GL-NCEAS-OceanRegions_v2013a/manual_output/eez_rgn_2013master.csv'.format(conf['dir_neptune'])
# manual overrides: slivers and polygon surrounding Caspian and Black Seas to convert from EEZ to land for OHI purposes
slivers    = '{0}/manual_output/slivers_tofix_manual.shp'.format(ad)
eez_inland_area  = '{0}/manual_output/CaspianBlackSeas_EEZexclusionpoly.shp'.format(ad)

# data outputs
# Antarctica CCAMLR
ant_ccamlr_all = '{0}/git-annex/Global/{1}/data/antarctica_ccamlr_alleez_gcs.shp'.format(conf['dir_neptune'], nm)
ant_ccamlr_ohi = '{0}/git-annex/Global/{1}/data/antarctica_ccamlr_ohi2014_gcs.shp'.format(conf['dir_neptune'], nm)
# final regions product
rgns_gcs   = '{0}/rgns_gcs.shp'.format(td)

# projections
sr_mol = arcpy.SpatialReference('Mollweide (world)') # projected Mollweide (54009)
sr_gcs = arcpy.SpatialReference('WGS 1984')          # geographic coordinate system WGS84 (4326)

# environment
if not os.path.exists(td): os.makedirs(td)
if not arcpy.Exists(gdb): arcpy.CreateFileGDB_management(os.path.dirname(gdb), os.path.basename(gdb))
arcpy.env.workspace       = gdb
arcpy.env.overwriteOutput = True
arcpy.env.outputCoordinateSystem = sr_gcs

# copy data inputs into gdb
for v in ['eez', 'eezland', 'fao', 'eez_inland_area']:
    if not arcpy.Exists('%s/%s' % (gdb,v)):
        arcpy.FeatureClassToFeatureClass_conversion(eval(v), gdb, v) 
arcpy.TableToTable_conversion(z_csv, gdb, 'z')
    
# Antarctica: remove from eez, eezland and fao
arcpy.Select_analysis('eez', 'eez_noant', "Country <> 'Antarctica'")
arcpy.Select_analysis('eezland', 'eezland_noant', "Country <> 'Antarctica'")
arcpy.Select_analysis('fao', 'fao_noant', "SOURCE <> 'CCAMLR'")

# fao: erase eezland
arcpy.Erase_analysis('fao', 'eezland_noant', 'fao_noeez')

# Antarctica: extract CCAMLR from FAO, erase EEZ, dissolve to create OHI region
arcpy.Select_analysis('fao_noeez', 'fao_noeez_noant', "SOURCE <> 'CCAMLR'")
arcpy.Select_analysis('fao_noeez', 'fao_noeez_ant'  , "SOURCE  = 'CCAMLR'")
arcpy.Select_analysis('fao'   , 'fao_ant', "SOURCE = 'CCAMLR'")
arcpy.AddField_management(      'fao_ant', 'area0_km2', 'FLOAT')
arcpy.CalculateField_management('fao_ant', 'area0_km2', '!shape.area@squarekilometers!', 'PYTHON_9.3')
arcpy.CopyFeatures_management('fao_noeez_ant', ant_ccamlr_all)
arcpy.AddField_management(      'fao_noeez_ant', 'area_km2', 'FLOAT')
arcpy.CalculateField_management('fao_noeez_ant', 'area_km2', '!shape.area@squarekilometers!', 'PYTHON_9.3')
arcpy.Intersect_analysis(      ['fao_noeez_ant', 'fao_ant'], 'fao_ant_inx')
arcpy.AddField_management(      'fao_ant_inx', 'area0_pct', 'FLOAT')
arcpy.CalculateField_management('fao_ant_inx', 'area0_pct', '!area_km2!/!area0_km2!*100', 'PYTHON_9.3')
arcpy.JoinField_management('fao_noeez_ant', 'F_CODE2', 'fao_ant_inx', 'F_CODE2', ['area0_km2','area0_pct'])
# export Antarctica shapefiles with and without EEZ clipped
arcpy.CopyFeatures_management('fao_noeez_ant', ant_ccamlr_ohi)
arcpy.CopyFeatures_management('fao_ant'      , ant_ccamlr_all)
### dissolve CCAMLR to get OHI version of single Antarctica EEZ
##arcpy.Dissolve_management('fao_noeez_ant', 'ant_eez')
##r = np.rec.fromrecords(
##    [(1, 213, u'eez', u'Antarctica', u'ATA')],
##    formats = '<i4, <i4, <U255, <U255, <U255',
##    names   = 'OBJECTID, orig_id, orig_type, orig_name, orig_code')
##arcpy.da.ExtendTable('ant_eez', 'OBJECTID', r, 'OBJECTID', append_only=False)
arcpy.CopyFeatures_management('fao_ant', 'ant_ccamlr')
r = arcpy.da.TableToNumPyArray('ant_ccamlr', ['OBJECTID','F_CODE'])
r.dtype.names = [{'F_CODE'    :'raw_name'}.get(x, x) for x in r.dtype.names]
raw_type = np.zeros((len(r),), dtype=[('raw_type','<U20')]); raw_type.fill('antarctica-ccamlr')
raw_id   = np.zeros((len(r),), dtype=[('raw_id'  ,'<i4' )]); raw_id[:] = r['OBJECTID']
raw_code = np.zeros((len(r),), dtype=[('raw_code','<U10')]) #; raw_type.fill('')
rf = np.lib.recfunctions.merge_arrays([r, raw_type, raw_id, raw_code], flatten=True)
arcpy.da.ExtendTable('ant_ccamlr', 'OBJECTID', rf, 'OBJECTID', append_only=False)

# Antarctica land
arcpy.CreateFishnet_management('ant_box', '-180 -90', '-180 -80', '360', '30', '1', '1', geometry_type='POLYGON')
arcpy.Erase_analysis('ant_box', 'fao', 'ant_land')
r = np.rec.fromrecords(
    [(1, u'eez', 213, u'Antarctica', u'ATA')],
    formats = '<i4, <U20, <i4, <U255, <U10', # ESRI bug: for some reason the text strings double in size on arcpy.da.ExtendTable
    names   = 'OBJECTID, raw_type, raw_id, raw_name, raw_code')
arcpy.da.ExtendTable('ant_land', 'OBJECTID', r, 'OBJECTID') # , append_only=False

# eez-inland: Caspian and Black Seas
r = arcpy.da.TableToNumPyArray('eez', ['OBJECTID','EEZ_ID','Country','ISO_3digit'])
r.dtype.names = [{'EEZ_ID'    :'raw_id',
                  'Country'   :'raw_name',
                  'ISO_3digit':'raw_code'}.get(x, x) for x in r.dtype.names]
raw_type = np.zeros((len(r),), dtype=[('raw_type','<U20')]); raw_type.fill('eez')
rf = np.lib.recfunctions.merge_arrays([r, raw_type], flatten=True)
arcpy.da.ExtendTable('eez', 'OBJECTID', rf, 'OBJECTID', append_only=False)
arcpy.MultipartToSinglepart_management('eez', 'eez_p')
arcpy.Intersect_analysis(['eez_p', 'eez_inland_area'], 'eez_p_inland')
arcpy.CalculateField_management('eez_p_inland', 'raw_type', "'eez-inland'", 'PYTHON_9.3')
arcpy.Erase_analysis('eez_p', 'eez_p_inland', 'eez_p_noeezinland')
arcpy.Merge_management(['eez_p_noeezinland','eez_p_inland'], 'eez_p_inland_eez')
arcpy.Dissolve_management('eez_p_inland_eez', 'eez_typed', ['raw_type','raw_id','raw_name','raw_code'])

# fao: prep for merging
r = arcpy.da.TableToNumPyArray('fao_noeez_noant', ['OBJECTID','F_CODE'])
r.dtype.names = [{'F_CODE'    :'raw_name'}.get(x, x) for x in r.dtype.names]
raw_type = np.zeros((len(r),), dtype=[('raw_type','<U20')]); raw_type.fill('antarctica-ccamlr')
raw_id   = np.zeros((len(r),), dtype=[('raw_id'  ,'<i4' )]); raw_id[:] = r['raw_name'].astype('<i4')
raw_code = np.zeros((len(r),), dtype=[('raw_code','<U10')]) #; raw_type.fill('')
rf = np.lib.recfunctions.merge_arrays([r, raw_type, raw_id, raw_code], flatten=True)
arcpy.da.ExtendTable('fao_noeez_noant', 'OBJECTID', rf, 'OBJECTID', append_only=False)

# land: erase eez, split into parts
arcpy.Erase_analysis('eezland_noant', 'eez', 'land')
# TODO: overlaps with Peru & Chile. overlap check above
arcpy.PolygonNeighbors_analysis('eez' , 'eez_nbrs' , ['OBJECTID','EEZ_ID','Country','ISO_3digit'], 'AREA_OVERLAP', out_linear_units='kilometers')
arcpy.PolygonNeighbors_analysis('land', 'land_nbrs', ['OBJECTID','Country','ISO_3digit'], 'AREA_OVERLAP', out_linear_units='kilometers')
arcpy.PolygonNeighbors_analysis('fao' , 'fao_nbrs' , ['OBJECTID','F_CODE'], 'AREA_OVERLAP', out_linear_units='kilometers')

'eez', 'eez_noant', "Country <> 'Antarctica'")
arcpy.Select_analysis('eezland', 'eezland_noant', "Country <> 'Antarctica'")
arcpy.Select_analysis('fao'


r = arcpy.da.TableToNumPyArray('eez', ['OBJECTID','Country','ISO_3digit'])
r.dtype.names = [{'Country'   :'raw_name',
                  'ISO_3digit':'raw_code'}.get(x, x) for x in r.dtype.names]
raw_type = np.zeros((len(r),), dtype=[('raw_type','<U20')]); raw_type.fill('land')
raw_id   = np.zeros((len(r),), dtype=[('raw_id','<U20')])
rf = np.lib.recfunctions.merge_arrays([r, raw_type, raw_id], flatten=True)
                      
arcpy.da.ExtendTable('land', 'OBJECTID', rf, 'OBJECTID', append_only=False)
arcpy.MultipartToSinglepart_management('eez', 'eez_p')


# slivers
arcpy.Merge_management(['ant_ccamlr','ant_land','eez_typed','fao_noeez_noant','s_land_p'],'m1')
arcpy.Dissolve_management('m1', 'm1_d')

# merge: []
arcpy.Merge_management(['ant_ccamlr','ant_land','eez_typed','fao_noeez_noant','s_land_p'],'s_m1')


# TODO: clip to earth box
arcpy.AddField_management(      'fao_noeez_ant', 's_code', 'TEXT')
arcpy.CalculateField_management('fao_noeez_ant', 's_code', "'FAO-CCAMLR_'+!F_CODE!", 'PYTHON_9.3')
arcpy.Dissolve_management(      'fao_noeez_ant', 's_ant', 's_code')

##arcpy.Dissolve_management('fao_noeez_ant', 'eez_ant')
##a = numpy.array([(1, 213, 'eez', 'Antarctica')],
##                numpy.dtype([('OBJECTID', '<i4'  ),
##                             ('rgn_id'  , '<i4'  ),
##                             ('rgn_type', '<U255'),
##                             ('rgn_name', '<U255')]))
##arcpy.da.ExtendTable('eez_ant', 'OBJECTID', a, 'OBJECTID')

### master eez OHI region lookup, rename fields
##z = arcpy.da.TableToNumPyArray('z', ['eez_id','eez_iso3', 'rgn_typ', 'rgn_id_2013', 'rgn_nam_2013'])
##z.dtype.names = [{'rgn_id_2013' :'rgn_id',
##                  'rgn_typ'     :'rgn_type',
##                  'rgn_nam_2013':'rgn_name'}.get(x, x) for x in z.dtype.names]

### fao: add 1000 to get unique eez_id, extend with OHI region fields
##arcpy.AddField_management(      'fao_noeez_noant', 'eez_id', 'LONG')
##arcpy.CalculateField_management('fao_noeez_noant', 'eez_id', "int(!F_CODE!) + 1000", 'PYTHON_9.3')
##arcpy.da.ExtendTable('fao_noeez_noant', 'eez_id',
##                     z[z['rgn_type']=='fao'],
##                     'eez_id', append_only=False)

# fao
arcpy.AddField_management(      'fao_noeez_noant', 's_code', 'TEXT')
arcpy.CalculateField_management('fao_noeez_noant', 's_code', "'FAO_'+!F_CODE!", 'PYTHON_9.3')
arcpy.Dissolve_management(      'fao_noeez_ant'  , 's_fao', 's_code')

# land: erase eez
arcpy.Erase_analysis('eezland_noant', 'eez', 'land')
arcpy.AddField_management(      'land', 's_code', 'TEXT')
arcpy.CalculateField_management('land', 's_code', "'land_%s[%s]' % (!Country!, !ISO_3digit!)", 'PYTHON_9.3')
arcpy.Dissolve_management(      'land', 's_land', 's_code')


# s_eez
arcpy.AddField_management(      'eez', 's_code', 'TEXT')
arcpy.CalculateField_management('eez', 's_code', "'eez_%s[%s]{%d}' % (!Country!, !ISO_3digit!, !EEZ_ID!)", 'PYTHON_9.3')
arcpy.Dissolve_management(      'eez', 's_eez', 's_code')

# merge eez, fao and land parts. run neighbor analysis. use max LENGTH shared to assign to EEZ.
arcpy.MultipartToSinglepart_management('s_land', 's_land_p')


####
arcpy.Merge_management(['s_eez', 's_land_p', 's_fao'],'s_m1')
arcpy.PolygonNeighbors_analysis('s_m1', 's_m1_nbrs', ['OBJECTID','s_code'], 'NO_AREA_OVERLAP', out_linear_units='kilometers')
nbrs = arcpy.da.TableToNumPyArray('s_m1_nbrs', ['src_OBJECTID','src_s_code','nbr_s_code','LENGTH'],
                                  "src_s_code LIKE 'land_%' AND nbr_s_code LIKE 'eez_%' AND LENGTH > 0")
df = pd.DataFrame(nbrs)
idx = df.groupby(['src_OBJECTID'])['LENGTH'].transform(max) == df['LENGTH'] # 17173
arr = df[idx][['src_OBJECTID','nbr_s_code']].to_records()
arr = arr.astype([('src_OBJECTID', '<i4'), ('nbr_s_code', '<U255')])
##arcpy.da.ExtendTable('s_m1', 'OBJECTID', arr, 'src_OBJECTID', append_only=False)

# landonly, land DISPUTED if ISO_3digit='-'
code_block = """
def get(fld, objid, iso, cntry, rgn_type, rgn_id, rgn_name):
    if eval(fld) is None:
        if iso == '-':
            return {'rgn_type':'disputed-land',
                    'rgn_id'  : 2550,
                    'rgn_name':'DISPUTED'}[fld]
        else:
            return {'rgn_type':'land-noeez',
                    'rgn_id'  : 3000 + objid,
                    'rgn_name': cntry}[fld]
    else:
        return {'rgn_type':'land',
                'rgn_id'  : rgn_id,
                'rgn_name': rgn_name}[fld]
"""
arcpy.CalculateField_management('land_eezmatched', 'rgn_type', "get('rgn_type', !OBJECTID!, !ISO_3digit!, !Country!, !rgn_type!, !rgn_id!, !rgn_name!)",'PYTHON_9.3', code_block)
arcpy.CalculateField_management('land_eezmatched', 'rgn_id'  , "get('rgn_id'  , !OBJECTID!, !ISO_3digit!, !Country!, !rgn_type!, !rgn_id!, !rgn_name!)",'PYTHON_9.3', code_block)
arcpy.CalculateField_management('land_eezmatched', 'rgn_name', "get('rgn_name', !OBJECTID!, !ISO_3digit!, !Country!, !rgn_type!, !rgn_id!, !rgn_name!)",'PYTHON_9.3', code_block)




# ( (src_s_code LIKE 'land_%' AND nbr_s_code LIKE 'eez_%') OR (src_s_code LIKE 'eez_%' AND nbr_s_code LIKE 'land_%') ) AND NODE_COUNT > 0 AND
# src_OBJECTID=277 OR nbr_OBJECTID=277


# land: assign eez_id where either land misidentified ('SHN' for eezs 'ASC', 'TAA') or eez_iso3 is duplicated having several eez_ids
arcpy.MultipartToSinglepart_management('land', 'land_p')
arcpy.SpatialJoin_analysis('land_p','eez_noant','land_p_z', 'JOIN_ONE_TO_MANY','KEEP_ALL',"#",'BOUNDARY_TOUCHES')
# TODO MANUAL: review Join_Count > 1. OK.
# TODO MANUAL: associate land touching high seas FAO with the high seas FAO
arcpy.MakeFeatureLayer_management('land_p_z', 'lyr_land_p_z')
arcpy.SelectLayerByLocation_management('lyr_land_p_z', 'BOUNDARY_TOUCHES', 'fao_noeez_noant')
arcpy.



arcpy.Dissolve_management('land_p_j', 'land_p_j_d', 'EEZ_ID')








### land: replace values to match EEZ
##a_land = arcpy.da.TableToNumPyArray('land', ['OBJECTID', 'ISO_3digit'])
##d_iso3 = {
##    'MNP++':'MNP',
##    'ABW':'AW',
##    'BES':'BQ'}
##for x in a_land['ISO_3digit']:
##    a_land['ISO_3digit'][a_land['ISO_3digit']==x] = d_iso3.get(x, x)
##arcpy.da.ExtendTable('land', 'OBJECTID', a_land, 'OBJECTID', append_only=False)

# land: assign eez_id where either land misidentified ('SHN' for eezs 'ASC', 'TAA') or eez_iso3 is duplicated having several eez_ids
z_eez = z[ numpy.all([z['rgn_type']=='eez', z['rgn_name']!='DISPUTED'], axis=0) ]
eez_iso3_dupes = sorted([item for item, count in Counter(z_eez['eez_iso3']).iteritems() if count > 1])
z_eez_iso3_nodupes = z_eez[ numpy.where([x not in eez_iso3_dupes for x in z_eez['eez_iso3'].tolist()])[0] ]
arcpy.MakeFeatureLayer_management('land', 'lyr_land', "ISO_3digit IN ('SHN','%s')" % "','".join(eez_iso3_dupes))
arcpy.MultipartToSinglepart_management('lyr_land', 'land_p')
arcpy.DeleteFeatures_management('lyr_land')
arcpy.da.ExtendTable('land', 'ISO_3digit', z_eez_iso3_nodupes,'eez_iso3', append_only=False)
# TODO MANUAL: review Join_Count > 1. OK.
arcpy.SpatialJoin_analysis('land_p','eez_noant','land_p_j', 'JOIN_ONE_TO_ONE','KEEP_ALL',"#",'BOUNDARY_TOUCHES')
arcpy.Dissolve_management('land_p_j', 'land_p_j_d', 'EEZ_ID')
arcpy.da.ExtendTable('land_p_j_d', 'EEZ_ID', z[ z['rgn_type']=='eez' ],'eez_id', append_only=False)
arcpy.Merge_management(['land', 'land_p_j_d'],'land_eezmatched')

# landonly, land DISPUTED if ISO_3digit='-'
code_block = """
def get(fld, objid, iso, cntry, rgn_type, rgn_id, rgn_name):
    if eval(fld) is None:
        if iso == '-':
            return {'rgn_type':'disputed-land',
                    'rgn_id'  : 2550,
                    'rgn_name':'DISPUTED'}[fld]
        else:
            return {'rgn_type':'land-noeez',
                    'rgn_id'  : 3000 + objid,
                    'rgn_name': cntry}[fld]
    else:
        return {'rgn_type':'land',
                'rgn_id'  : rgn_id,
                'rgn_name': rgn_name}[fld]
"""
arcpy.CalculateField_management('land_eezmatched', 'rgn_type', "get('rgn_type', !OBJECTID!, !ISO_3digit!, !Country!, !rgn_type!, !rgn_id!, !rgn_name!)",'PYTHON_9.3', code_block)
arcpy.CalculateField_management('land_eezmatched', 'rgn_id'  , "get('rgn_id'  , !OBJECTID!, !ISO_3digit!, !Country!, !rgn_type!, !rgn_id!, !rgn_name!)",'PYTHON_9.3', code_block)
arcpy.CalculateField_management('land_eezmatched', 'rgn_name', "get('rgn_name', !OBJECTID!, !ISO_3digit!, !Country!, !rgn_type!, !rgn_id!, !rgn_name!)",'PYTHON_9.3', code_block)

# merge with common fields
arcpy.Merge_management(['eez_noant', 'eez_ant', 'fao_noeez_noant', 'land_eezmatched'], 'all')
arcpy.Dissolve_management('all', 'all_d', ['rgn_type','rgn_id','rgn_name'])

# get earth box for defining Antarctica and clipping to extent
arcpy.CreateFishnet_management('box', "-180 -90", "-180 -80", "360", "180", "1", "1", "", "NO_LABELS", "-180 -90 180 90", "POLYGON")
arcpy.DefineProjection_management('box', sr_gcs)
arcpy.Clip_analysis('all_d', 'box', 'all_d_c')
arcpy.Erase_analysis('box', 'all_d_c', 'boxnotall')
arcpy.MultipartToSinglepart_management('boxnotall', 'boxnotall_p')
arcpy.AddField_management('boxnotall_p', 'rgn_name', 'TEXT')
code_block = """
def get(y):
    if y < -60:
        return 'Antarctica'"""
arcpy.CalculateField_management('boxnotall_p', 'rgn_name', 'get(!shape.centroid.Y!)', 'PYTHON_9.3', code_block)
arcpy.MakeFeatureLayer_management('boxnotall_p', 'lyr_land_ant', "rgn_name = 'Antarctica'")
arcpy.Dissolve_management('lyr_land_ant', 'land_ant', 'rgn_name')
a = numpy.array([(213, 'land', 'Antarctica')],
                numpy.dtype([('rgn_id'  , '<i4'  ),
                             ('rgn_type', '<U255'),
                             ('rgn_name', '<U255')]))
arcpy.da.ExtendTable('land_ant', 'rgn_name', a, 'rgn_name')
arcpy.DeleteFeatures_management('lyr_land_ant')

# deal with slivers, some FAO slivers inland
arcpy.MakeFeatureLayer_management('all_d_c', 'lyr_fao', "rgn_type = 'fao'")
arcpy.MultipartToSinglepart_management('lyr_fao', 'fao_p')
arcpy.MakeFeatureLayer_management('fao_p', 'lyr_fao', 'Shape_Area < 1 OR ( Shape_Area > 4.5 AND Shape_Area < 5 )')
arcpy.Merge_management(['fao_slivers_tofix', 'boxnotall_p'], 'slivers_tofix')

# TODO MANUAL: copy to slivers_tofix -> slivers_tofix_manual and edit with appropriate info

# replace slivers, merge with Antarctica land
arcpy.FeatureClassToFeatureClass_conversion(slivers, gdb, 'slivers')
arcpy.Erase_analysis('all_d_c', 'slivers', 'all_d_c_e')
arcpy.Merge_management(['slivers', 'all_d_c_e', 'land_ant'], 'all_d_c_e_m')

# change rgn_type='eez'->'disputed-eez' where rgn_name='DISPUTED'
arcpy.MakeFeatureLayer_management('all_d_c_e_m', 'lyr_all', "rgn_id = 255")
arcpy.CalculateField_management('lyr_all', 'rgn_type', "'disputed-eez'")

# dissolve to regions
arcpy.Dissolve_management('all_d_c_e_m', 'all_d_c_e_m_d', ['rgn_type','rgn_id','rgn_name'])

# check that no more missing slivers and no overlap
arcpy.Erase_analysis('box', 'all_d_c_e_m_d', 'boxnotall2') # GOOD: zero rows
arcpy.PolygonNeighbors_analysis('all_d_c_e_m_d', 'nbrs_all', 'OBJECTID', 'AREA_OVERLAP') # GOOD: all AreaOverlap is 0

# final 
arcpy.RepairGeometry_management('all_d_c_e_m_d')

# post-hoc fixes: rgn_type of '0'
arcpy.MakeFeatureLayer_management("all_d_c_e_m_d", "lyr_other", "rgn_id = 255 OR rgn_type = '0' OR rgn_name = 'DISPUTED'")
arcpy.CopyFeatures_management('all_d_c_e_m_d', 'rgns')
code_block = """
def get(fld, objid, rgn_type, rgn_id, rgn_name):
    if rgn_type=='0' and rgn_name!='DISPUTED':
        return {'rgn_type':'land-noeez',
                'rgn_id'  : 3500 + objid,
                'rgn_name': rgn_name}[fld]
    elif rgn_type=='0' and rgn_name=='DISPUTED':
            return {'rgn_type':'disputed-eez',
                    'rgn_id'  : 255,
                    'rgn_name':'DISPUTED'}[fld]
    else:
        return {'rgn_type': rgn_type,
                'rgn_id'  : rgn_id,
                'rgn_name': rgn_name}[fld]
"""
arcpy.CalculateField_management('rgns', 'rgn_type', "get('rgn_type', !OBJECTID!, !rgn_type!, !rgn_id!, !rgn_name!)",'PYTHON_9.3', code_block)
arcpy.CalculateField_management('rgns', 'rgn_id'  , "get('rgn_id'  , !OBJECTID!, !rgn_type!, !rgn_id!, !rgn_name!)",'PYTHON_9.3', code_block)
arcpy.CalculateField_management('rgns', 'rgn_name', "get('rgn_name', !OBJECTID!, !rgn_type!, !rgn_id!, !rgn_name!)",'PYTHON_9.3', code_block)

# convert Caspian and Black Seas from rgn_type eez to land
arcpy.MultipartToSinglepart_management('rgns', 'rgns_p')
arcpy.MakeFeatureLayer_management('rgns_p', 'lyr_eez', "rgn_type = 'eez'")
arcpy.Intersect_analysis([eeztoland, 'lyr_eez'], 'eez_CaspianBlackSea')
arcpy.CalculateField_management('eez_CaspianBlackSea', 'rgn_type', "'land'", 'PYTHON_9.3')
arcpy.Erase_analysis('rgns', 'eez_CaspianBlackSea', 'rgns_e')
arcpy.Merge_management(['rgns_e','eez_CaspianBlackSea'], 'rgns_e_m')
arcpy.Dissolve_management('rgns_e_m', 'rgns_e_m_d', ['rgn_type','rgn_id','rgn_name'])

# copy final
arcpy.CopyFeatures_management('rgns_e_m_d', 'rgns_gcs')
#arcpy.RepairGeometry_management('rgns_gcs')
arcpy.CopyFeatures_management('rgns_gcs', rgns_gcs)

# TODO: simplify and TopoJSON
# Simplify lake polygons.
arcpy.cartography.SimplifyPolygon('rgns_gcs', 'rgns_simplify_gcs', 'POINT_REMOVE', 0.01, 200, "RESOLVE_ERRORS", "KEEP_COLLAPSED_POINTS", "CHECK")
 
# Smooth lake polygons.
arcpy.cartography.SmoothPolygon(simplifiedFeatures, smoothedFeatures, "PAEK", 100, "FLAG_ERRORS")


# TODO: check that rgn_id is unique, and that all rgn_type=='eez' have a matching 'rgn_type'=='land'




