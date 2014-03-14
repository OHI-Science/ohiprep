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
arcpy.SetLogHistory(True) # %USERPROFILE%\AppData\Roaming\ESRI\Desktop10.2\ArcToolbox\History

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

### copy data inputs into gdb
##for v in ['eez', 'eezland', 'fao', 'eez_inland_area']:
##    if not arcpy.Exists('%s/%s' % (gdb,v)):
##        arcpy.FeatureClassToFeatureClass_conversion(eval(v), gdb, v) 
##arcpy.TableToTable_conversion(z_csv, gdb, 'z')
##    
### Antarctica: remove from eez, eezland and fao
##arcpy.Select_analysis('eez', 'eez_noant', "Country <> 'Antarctica'")
##arcpy.Select_analysis('eezland', 'eezland_noant', "Country <> 'Antarctica'")
##arcpy.Select_analysis('fao', 'fao_noant', "SOURCE <> 'CCAMLR'")
##
### fao: erase eezland
##arcpy.Erase_analysis('fao', 'eezland_noant', 'fao_noeez')
##
### Antarctica: extract CCAMLR from FAO, erase EEZ
##arcpy.Select_analysis('fao_noeez', 'fao_noeez_noant', "SOURCE <> 'CCAMLR'")
##arcpy.Select_analysis('fao_noeez', 'fao_noeez_ant'  , "SOURCE  = 'CCAMLR'")
##arcpy.Select_analysis('fao'   , 'fao_ant', "SOURCE = 'CCAMLR'")
##arcpy.AddField_management(      'fao_ant', 'area0_km2', 'FLOAT')
##arcpy.CalculateField_management('fao_ant', 'area0_km2', '!shape.area@squarekilometers!', 'PYTHON_9.3')
##arcpy.CopyFeatures_management('fao_noeez_ant', ant_ccamlr_all)
##arcpy.AddField_management(      'fao_noeez_ant', 'area_km2', 'FLOAT')
##arcpy.CalculateField_management('fao_noeez_ant', 'area_km2', '!shape.area@squarekilometers!', 'PYTHON_9.3')
##arcpy.Intersect_analysis(      ['fao_noeez_ant', 'fao_ant'], 'fao_ant_inx')
##arcpy.AddField_management(      'fao_ant_inx', 'area0_pct', 'FLOAT')
##arcpy.CalculateField_management('fao_ant_inx', 'area0_pct', '!area_km2!/!area0_km2!*100', 'PYTHON_9.3')
##arcpy.JoinField_management('fao_noeez_ant', 'F_CODE2', 'fao_ant_inx', 'F_CODE2', ['area0_km2','area0_pct'])
### export Antarctica shapefiles with and without EEZ clipped
##arcpy.CopyFeatures_management('fao_noeez_ant', ant_ccamlr_ohi)
##arcpy.CopyFeatures_management('fao_ant'      , ant_ccamlr_all)
##### dissolve CCAMLR to get OHI version of single Antarctica EEZ
####arcpy.Dissolve_management('fao_noeez_ant', 'ant_eez')
####r = np.rec.fromrecords(
####    [(1, 213, u'eez', u'Antarctica', u'ATA')],
####    formats = '<i4, <i4, <U255, <U255, <U255',
####    names   = 'OBJECTID, orig_id, orig_type, orig_name, orig_code')
####arcpy.da.ExtendTable('ant_eez', 'OBJECTID', r, 'OBJECTID', append_only=False)
##arcpy.CopyFeatures_management('fao_ant', 'ant_ccamlr')
##r = arcpy.da.TableToNumPyArray('ant_ccamlr', ['OBJECTID','F_CODE'])
##r.dtype.names = [{'F_CODE'    :'raw_name'}.get(x, x) for x in r.dtype.names]
##raw_type = np.zeros((len(r),), dtype=[('raw_type','<U20')]); raw_type.fill('ccamlr')
##raw_id   = np.zeros((len(r),), dtype=[('raw_id'  ,'<i4' )]); raw_id[:] = r['OBJECTID']
##raw_code = np.zeros((len(r),), dtype=[('raw_code','<U10')]) #; raw_type.fill('')
##rf = np.lib.recfunctions.merge_arrays([r, raw_type, raw_id, raw_code], flatten=True)
##arcpy.da.ExtendTable('ant_ccamlr', 'OBJECTID', rf, 'OBJECTID', append_only=False)
##
### Antarctica land
##arcpy.CreateFishnet_management('ant_box', '-180 -90', '-180 -80', '360', '30', '1', '1', geometry_type='POLYGON')
##arcpy.Erase_analysis('ant_box', 'fao', 'ant_land')
##r = np.rec.fromrecords(
##    [(1, u'eez', 213, u'Antarctica', u'ATA')],
##    formats = '<i4, <U20, <i4, <U255, <U10', # ESRI bug: for some reason the text strings double in size on arcpy.da.ExtendTable
##    names   = 'OBJECTID, raw_type, raw_id, raw_name, raw_code')
##arcpy.da.ExtendTable('ant_land', 'OBJECTID', r, 'OBJECTID') # , append_only=False
##
### eez-inland: Caspian and Black Seas
##r = arcpy.da.TableToNumPyArray('eez_noant', ['OBJECTID','EEZ_ID','Country','ISO_3digit'])
##r.dtype.names = [{'EEZ_ID'    :'raw_id',
##                  'Country'   :'raw_name',
##                  'ISO_3digit':'raw_code'}.get(x, x) for x in r.dtype.names]
##raw_type = np.zeros((len(r),), dtype=[('raw_type','<U20')]); raw_type.fill('eez')
##rf = np.lib.recfunctions.merge_arrays([r, raw_type], flatten=True)
##arcpy.da.ExtendTable('eez_noant', 'OBJECTID', rf, 'OBJECTID', append_only=False)
##arcpy.MultipartToSinglepart_management('eez_noant', 'eez_noant_p')
##arcpy.Intersect_analysis(['eez_noant_p', 'eez_inland_area'], 'eez_noant_p_inland')
##arcpy.CalculateField_management('eez_noant_p_inland', 'raw_type', "'eez-inland'", 'PYTHON_9.3')
##arcpy.Erase_analysis('eez_noant_p', 'eez_noant_p_inland', 'eez_noant_p_noeezinland')
##arcpy.Merge_management(['eez_noant_p_noeezinland','eez_noant_p_inland'], 'eez_noant_p_inland_eez')
##arcpy.Dissolve_management('eez_noant_p_inland_eez', 'eez_noant_typed', ['raw_type','raw_id','raw_name','raw_code'])
##
### fao: prep for merging
##r = arcpy.da.TableToNumPyArray('fao_noeez_noant', ['OBJECTID','F_CODE'])
##r.dtype.names = [{'F_CODE'    :'raw_name'}.get(x, x) for x in r.dtype.names]
##raw_type = np.zeros((len(r),), dtype=[('raw_type','<U20')]); raw_type.fill('fao')
##raw_id   = np.zeros((len(r),), dtype=[('raw_id'  ,'<i4' )]); raw_id[:] = r['raw_name'].astype('<i4')
##raw_code = np.zeros((len(r),), dtype=[('raw_code','<U10')]) #; raw_type.fill('')
##rf = np.lib.recfunctions.merge_arrays([r, raw_type, raw_id, raw_code], flatten=True)
##arcpy.da.ExtendTable('fao_noeez_noant', 'OBJECTID', rf, 'OBJECTID', append_only=False)
##
### land: erase eez, split into parts
##arcpy.Erase_analysis('eezland_noant', 'eez', 'land')
### fix overlaps with Peru & Chile [arcpy.PolygonNeighbors_analysis('land', 'land_nbrs', ['OBJECTID','Country','ISO_3digit'], 'AREA_OVERLAP', out_linear_units='kilometers')]
##arcpy.MakeFeatureLayer_management('land','lyr_land', "Country IN ('Peru (Chilean point of view)','Chile (Peruvian point of view)')")
##arcpy.DeleteFeatures_management('lyr_land')
##r = arcpy.da.TableToNumPyArray('land', ['OBJECTID','Country','ISO_3digit'])
##r.dtype.names = [{'Country'   :'raw_name',
##                  'ISO_3digit':'raw_code'}.get(x, x) for x in r.dtype.names]
##raw_type = np.zeros((len(r),), dtype=[('raw_type','<U20')]); raw_type.fill('land')
##raw_id   = np.zeros((len(r),), dtype=[('raw_id','<i4')])
##rf = np.lib.recfunctions.merge_arrays([r, raw_type, raw_id], flatten=True)
##arcpy.da.ExtendTable('land', 'OBJECTID', rf, 'OBJECTID', append_only=False)
##arcpy.MultipartToSinglepart_management('land', 'land_p')
##
### merge, earth box, slivers
##arcpy.Merge_management(['ant_ccamlr','ant_land','eez_noant_typed','fao_noeez_noant','land_p'],'m')
###arcpy.CreateFishnet_management('box', "-180 -90", "-180 -80", "360", "180", "1", "1", "", "NO_LABELS", "-180 -90 180 90", "POLYGON")
###arcpy.DefineProjection_management('box', sr_gcs)
##arcpy.Clip_analysis('m', 'box', 'm_c')
##arcpy.Erase_analysis('box','m_c', 'm_other')
##arcpy.MultipartToSinglepart_management('m_other', 'slivers')
##r = arcpy.da.TableToNumPyArray('slivers', ['OBJECTID'])
##f = np.zeros((len(r),), dtype=[('raw_type','<U20'),('raw_id','<i4'),('raw_name','<U255'),('raw_code','<U10')]); f['raw_type'].fill('sliver')
##rf = np.lib.recfunctions.merge_arrays([r, f], flatten=True)
##arcpy.da.ExtendTable('slivers', 'OBJECTID', rf, 'OBJECTID', append_only=False)
##arcpy.Merge_management(['m_c','slivers'],'m_c_s')
##arcpy.PolygonNeighbors_analysis('m_c_s', 'nbrs_m_c_s', ['OBJECTID','raw_type','raw_id','raw_name','raw_code'], 'NO_AREA_OVERLAP')

# get merged data, add empty spatial sp_* fields and use PANDAS data frame
m = arcpy.da.TableToNumPyArray('m_c_s', ['OBJECTID','raw_type','raw_id','raw_name','raw_code'])
f = np.zeros((len(m),), dtype=[('sp_type','<U20'),('sp_id','<i4'),('sp_name','<U255'),('sp_code','<U10')])
m = pd.DataFrame(np.lib.recfunctions.merge_arrays([m, f], flatten=True), index=m['OBJECTID'])

# fao bordering land: presume land gap filled by fao
print('fao bordering land: presume land gap filled by fao')
# OLD...
d = n[n.groupby(['src_OBJECTID'])['LENGTH'].transform(max) == n['LENGTH']]
for i,r in d.iterrows(): # r = next(d.iterrows())[1]
    idx = m.OBJECTID == r['src_OBJECTID']
    m.ix[idx, 'sp_type'] = 'fao-land'
    m.ix[idx, 'sp_id']   = r['nbr_raw_id']
    m.ix[idx, 'sp_name'] = r['nbr_raw_name']
    m.ix[idx, 'sp_code'] = r['nbr_raw_code']
# NEW...
n = pd.DataFrame(arcpy.da.TableToNumPyArray(
    'nbrs_m_c_s',
    ['src_OBJECTID','src_raw_type','src_raw_id','src_raw_name','src_raw_code',
     'nbr_OBJECTID','nbr_raw_type','nbr_raw_id','nbr_raw_name','nbr_raw_code','LENGTH'],
    "src_raw_type = 'fao' AND nbr_raw_type = 'land' AND LENGTH > 0"))
d = n.groupby(['src_OBJECTID']).agg(lambda df: df.iloc[df['LENGTH'].values.argmax()])
m.loc[d.index, 'sp_type'] = 'fao-land' # d['sp_type']
m.loc[d.index, 'sp_id']   = d['nbr_raw_id']
m.loc[d.index, 'sp_name'] = d['nbr_raw_name']
m.loc[d.index, 'sp_code'] = d['nbr_raw_code']

# iso_code='TUR'; group = n.ix[n.groupby(['src_OBJECTID']).groups[iso_code]]; print(iso_code); print(group)
idx = n.iloc[n['LENGTH'].argmax()]

groups = n.groupby(['src_OBJECTID'])
g = groups.ix[0]
.agg(lambda x: x.iloc[x['LENGTH'].argmax()])



       

# land bordering fao: presume overextended land from landeez
print('land bordering fao: presume overextended land from landeez')
n = pd.DataFrame(arcpy.da.TableToNumPyArray(
    'nbrs_m_c_s',
    ['src_OBJECTID','src_raw_type','src_raw_id','src_raw_name','src_raw_code',
     'nbr_OBJECTID','nbr_raw_type','nbr_raw_id','nbr_raw_name','nbr_raw_code','LENGTH'],
    "src_raw_type = 'land' AND nbr_raw_type = 'fao' AND LENGTH > 0"))
d = n[n.groupby(['src_OBJECTID'])['LENGTH'].transform(max) == n['LENGTH']]
for i,r in d.iterrows(): # r = next(d.iterrows())[1]
    idx = m.OBJECTID == r['src_OBJECTID']
    m.ix[idx, 'sp_type'] = 'land-fao'
    m.ix[idx, 'sp_id']   = r['nbr_raw_id']
    m.ix[idx, 'sp_name'] = r['nbr_raw_name']
    m.ix[idx, 'sp_code'] = r['nbr_raw_code']

# land bordering eez: apply eez with greatest shared LENGTH
print('land bordering eez: apply eez with greatest shared LENGTH')
n = pd.DataFrame(arcpy.da.TableToNumPyArray(
    'nbrs_m_c_s',
    ['src_OBJECTID','nbr_raw_id','nbr_raw_name','nbr_raw_code','LENGTH'],
    "src_raw_type = 'land' AND nbr_raw_type = 'eez' AND LENGTH > 0"))
d = n[n.groupby(['src_OBJECTID'])['LENGTH'].transform(max) == n['LENGTH']]
for i,r in d.iterrows(): # r = next(d.iterrows())[1]
    idx = m.OBJECTID == r['src_OBJECTID']
    m.ix[idx, 'sp_type'] = 'land'
    m.ix[idx, 'sp_id']   = r['nbr_raw_id']
    m.ix[idx, 'sp_name'] = r['nbr_raw_name']
    m.ix[idx, 'sp_code'] = r['nbr_raw_code']

# land not bordering eez: use raw values
print('land not bordering eez: use raw values')
idx = (m['sp_name'] == '') & (m['raw_type']=='land')
m.ix[idx, 'sp_type'] = 'land'
m.ix[idx, 'sp_id']   = r['nbr_raw_id']
m.ix[idx, 'sp_name'] = r['nbr_raw_name']
m.ix[idx, 'sp_code'] = r['nbr_raw_code']

# determine land-noeez
d = m[(m['sp_id'] == 0) & (m['sp_name']!='Australia')].groupby(['raw_name'])
for raw_name, group in d: # name, group = next(d.groups.iteritems)
    # iso_code='TUR'; group = m.ix[d.groups[iso_code]]; print(iso_code); print(group)
    idx = (m['raw_name']==raw_name) & (m['raw_type']=='eez')
    if sum(idx) > 0:
        v = m[idx].iloc[0]
        m.ix[idx, 'sp_type'] = 'land'
        m.ix[idx, 'sp_id']   = v['raw_id']
        m.ix[idx, 'sp_name'] = v['raw_name']
        m.ix[idx, 'sp_code'] = v['raw_code']
    else:
        m.ix[idx, 'sp_type'] = 'land-noeez'
# TODO: apply unique sp_id's to land-noeez, which are so far sp_id=0

# copy the rest
print('copy the rest')
idx = (m['sp_name'] == '')
m.ix[idx, 'sp_type'] = m[idx]['raw_type']
m.ix[idx, 'sp_id']   = m[idx]['raw_id']
m.ix[idx, 'sp_name'] = m[idx]['raw_name']
m.ix[idx, 'sp_code'] = m[idx]['raw_code']

# TODO: check USA eez m.OBJECTID=31645 changing to land?

# apply new fields and dissolve
r = m.to_records(index=False)
r = r.astype([('OBJECTID', '<i4'),('sp_type','<U20'),('sp_id','<i4'),('sp_name','<U255'),('sp_code','<U10')])
arcpy.da.ExtendTable('m_c_s', 'OBJECTID', r, 'OBJECTID', append_only=False)
arcpy.Dissolve_management('m_c_s', 'm_c_s_d', ['sp_type','sp_id','sp_name','sp_code'])

# copy final features
arcpy.CopyFeatures_management('m_c_s_d', 'sp_gcs')


# TODO: manually: assign slivers, check fao-land / land-fao, assign sp_ids

# TODO: inspect map by creating Relate:nbrs_src on m_c_s:OBJECTID <-> nbrs_m_c_s:src_OBJECTID, Relate:nbrs_nbr on m_c_s:OBJECTID <-> nbrs_m_c_s:nbr_OBJECTID


# TODO: create lookup from OHI fine spatial (sp*) to OHI regions (rgns*)

##arcpy.Dissolve_management('rgns_e_m', 'rgns_e_m_d', ['rgn_type','rgn_id','rgn_name'])
##
### copy final
##arcpy.CopyFeatures_management('rgns_e_m_d', 'rgns_gcs')
##arcpy.RepairGeometry_management('rgns_gcs')
##arcpy.CopyFeatures_management('rgns_gcs', rgns_gcs)
##
### TODO: simplify and TopoJSON
### Simplify lake polygons.
##arcpy.cartography.SimplifyPolygon('rgns_gcs', 'rgns_simplify_gcs', 'POINT_REMOVE', 0.01, 200, "RESOLVE_ERRORS", "KEEP_COLLAPSED_POINTS", "CHECK")
## 
### Smooth lake polygons.
##arcpy.cartography.SmoothPolygon(simplifiedFeatures, smoothedFeatures, "PAEK", 100, "FLAG_ERRORS")
##
##
### TODO: check that rgn_id is unique, and that all rgn_type=='eez' have a matching 'rgn_type'=='land'




