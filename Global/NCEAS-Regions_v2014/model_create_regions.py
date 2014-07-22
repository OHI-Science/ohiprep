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
#  * convert EEZ of Black Sea to eez-inland
#  * merge all products and peform checks for overlap and geometry repair
#
# Built using:
#  ArcGIS 10.2.1
#  Python Data Analysis Library (pandas) installed with easy_install
#
# Changes since OHI 2013
# * New EEZ splits:
#   - 140 Guadeloupe and Martinique
#   - 116 Puerto Rico and Virgin Islands of the United States

import arcpy, os, re, numpy as np, socket, pandas, time
from collections import Counter
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
td      = '{0}/Global/{1}'.format(conf['dir_tmp'], nm)               # temp directory on local filesystem
gdb     = '{0}/geodb.gdb'.format(td)                                 # file geodatabase
ad      = '{0}/git-annex/Global/{1}'.format(conf['dir_neptune'], nm) # git annex directory on neptune
gd      = '{0}/Global/{1}'.format(conf['dir_git'], nm)               # git directory on local filesystem

# data inputs
# EEZ plus land (http://marineregions.org)
##eez        = '{0}/stable/GL-VLIZ-EEZs_v7/data/eez_v7_gcs.shp'.format(conf['dir_neptune'])
eez        = '{0}/git-annex/Global/MarineRegions_EEZ_v8/raw/World_EEZ_v8_2014_HR.shp'.format(conf['dir_neptune'])
eezland    = '{0}/stable/GL-VLIZ-EEZs_v7/data/EEZ_land_v1.shp'.format(conf['dir_neptune'])
# FAO for open ocean regions, with CCAMLR Antarctica regions
fao        = '{0}/model/GL-FAO-CCAMLR_v2014/data/fao_ccamlr_gcs.shp'.format(conf['dir_neptune'])
# master lookup table to go from EEZ to OHI regions
z_2013_csv = '{0}/model/GL-NCEAS-OceanRegions_v2013a/manual_output/eez_rgn_2013master.csv'.format(conf['dir_neptune'])
# manual overrides: slivers and polygon surrounding Black Sea to convert from eez to eez-inland for OHI purposes
eez_inland_area  = '{0}/manual_output/BlackSea_eez-inland_idpoly.shp'.format(ad)

# data outputs
# Antarctica CCAMLR
ant_ccamlr_all = '{0}/git-annex/Global/{1}/data/antarctica_ccamlr_alleez_gcs.shp'.format(conf['dir_neptune'], nm)
ant_ccamlr_ohi = '{0}/git-annex/Global/{1}/data/antarctica_ccamlr_ohi2014_gcs.shp'.format(conf['dir_neptune'], nm)
# manual outputs: slivers, rgns
sp_slivers = '{0}/git-annex/Global/{1}/manual_output/sp_manual_slivers_gcs.shp'.format(conf['dir_neptune'], nm)
sp_rgn_csv = '{0}/Global/{1}/manual_output/sp_rgn_manual.csv'.format(conf['dir_git'], nm)

# final products
rgn_shp  = '{0}/data/rgn_gcs.shp'.format(ad)
sp_shp   = '{0}/data/sp_gcs.shp'.format(ad)
rgn_csv  = '{0}/data/rgn_gcs_data.csv'.format(gd)
sp_csv   = '{0}/data/sp_gcs_data.csv'.format(gd)

# projections (see http://resources.arcgis.com/en/help/main/10.2/018z/pdf/projected_coordinate_systems.pdf|geographic_coordinate_systems.pdf)
sr_mol = arcpy.SpatialReference('Mollweide (world)') # projected Mollweide (54009)
sr_gcs = arcpy.SpatialReference('WGS 1984')          # geographic coordinate system WGS84 (4326)
sr_ant = arcpy.SpatialReference(3031)                # WGS_1984_Antarctic_Polar_Stereographic (3031)

# environment
if not os.path.exists(td): os.makedirs(td)
if not arcpy.Exists(gdb): arcpy.CreateFileGDB_management(os.path.dirname(gdb), os.path.basename(gdb))
arcpy.env.workspace       = gdb
arcpy.env.overwriteOutput = True
arcpy.env.outputCoordinateSystem = sr_gcs
#arcpy.env.outputCoordinateSystem = sr_ant

# copy data inputs into gdb
for v in ['eez', 'eezland', 'fao', 'eez_inland_area']:
    if not arcpy.Exists('%s/%s' % (gdb,v)):
        arcpy.FeatureClassToFeatureClass_conversion(eval(v), gdb, v) 
arcpy.TableToTable_conversion(z_csv, gdb, 'z')
    
# Antarctica: remove from eez, eezland and fao
arcpy.Select_analysis('eez', 'eez_noant', "Country <> 'Antarctica'")
arcpy.Select_analysis('eezland', 'eezland_noant', "Country <> 'Antarctica'")

# fao: erase eezland
arcpy.Erase_analysis('fao', 'eezland_noant', 'fao_noeez')

# Antarctica: extract CCAMLR from FAO, erase EEZ
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
arcpy.CopyFeatures_management('fao_noeez_ant', 'ant_ccamlr_noeez')
r = arcpy.da.TableToNumPyArray('ant_ccamlr_noeez', ['OBJECTID','F_CODE'])
r.dtype.names = [{'F_CODE'    :'raw_name'}.get(x, x) for x in r.dtype.names]
raw_type = np.zeros((len(r),), dtype=[('raw_type','<U20')]); raw_type.fill('ccamlr')
raw_id   = np.zeros((len(r),), dtype=[('raw_id'  ,'<i4' )]); raw_id[:] = r['OBJECTID']
raw_key = np.zeros((len(r),), dtype=[('raw_key','<U10')]) #; raw_type.fill('')
rf = np.lib.recfunctions.merge_arrays([r, raw_type, raw_id, raw_key], flatten=True)
arcpy.da.ExtendTable('ant_ccamlr_noeez', 'OBJECTID', rf, 'OBJECTID', append_only=False)

# Antarctica land
arcpy.CreateFishnet_management('ant_box', '-180 -90', '-180 -80', '360', '30', '1', '1', geometry_type='POLYGON')
arcpy.Erase_analysis('ant_box', 'fao', 'ant_land')
r = np.rec.fromrecords(
    [(1, u'land', 213, u'Antarctica', u'ATA')],
    formats = '<i4, <U20, <i4, <U255, <U10', # ESRI bug: for some reason the text strings double in size on arcpy.da.ExtendTable
    names   = 'OBJECTID, raw_type, raw_id, raw_name, raw_key')
arcpy.da.ExtendTable('ant_land', 'OBJECTID', r, 'OBJECTID') # , append_only=False

# eez-inland: Black Sea
r = arcpy.da.TableToNumPyArray('eez_noant', ['OBJECTID','EEZ_ID','Country','ISO_3digit'])
r.dtype.names = [{'EEZ_ID'    :'raw_id',
                  'Country'   :'raw_name',
                  'ISO_3digit':'raw_key'}.get(x, x) for x in r.dtype.names]
raw_type = np.zeros((len(r),), dtype=[('raw_type','<U20')]); raw_type.fill('eez')
rf = np.lib.recfunctions.merge_arrays([r, raw_type], flatten=True)
arcpy.da.ExtendTable('eez_noant', 'OBJECTID', rf, 'OBJECTID', append_only=False)
arcpy.MultipartToSinglepart_management('eez_noant', 'eez_noant_p')
arcpy.Intersect_analysis(['eez_noant_p', 'eez_inland_area'], 'eez_noant_p_inland')
arcpy.CalculateField_management('eez_noant_p_inland', 'raw_type', "'eez-inland'", 'PYTHON_9.3')
arcpy.Erase_analysis('eez_noant_p', 'eez_noant_p_inland', 'eez_noant_p_noeezinland')
arcpy.Merge_management(['eez_noant_p_noeezinland','eez_noant_p_inland'], 'eez_noant_p_inland_eez')
arcpy.Dissolve_management('eez_noant_p_inland_eez', 'eez_noant_typed', ['raw_type','raw_id','raw_name','raw_key'])

# fao: prep for merging
r = arcpy.da.TableToNumPyArray('fao_noeez_noant', ['OBJECTID','F_CODE'])
r.dtype.names = [{'F_CODE'    :'raw_name'}.get(x, x) for x in r.dtype.names]
raw_type = np.zeros((len(r),), dtype=[('raw_type','<U20')]); raw_type.fill('fao')
raw_id   = np.zeros((len(r),), dtype=[('raw_id'  ,'<i4' )]); raw_id[:] = r['raw_name'].astype('<i4')
raw_key = np.zeros((len(r),), dtype=[('raw_key','<U10')]) #; raw_type.fill('')
rf = np.lib.recfunctions.merge_arrays([r, raw_type, raw_id, raw_key], flatten=True)
arcpy.da.ExtendTable('fao_noeez_noant', 'OBJECTID', rf, 'OBJECTID', append_only=False)

# land: erase eez, split into parts
arcpy.Erase_analysis('eezland_noant', 'eez', 'land')
# fix overlaps with Peru & Chile [arcpy.PolygonNeighbors_analysis('land', 'land_nbrs', ['OBJECTID','Country','ISO_3digit'], 'AREA_OVERLAP', out_linear_units='kilometers')]
arcpy.MakeFeatureLayer_management('land','lyr_land', "Country IN ('Peru (Chilean point of view)','Chile (Peruvian point of view)')")
arcpy.DeleteFeatures_management('lyr_land')
r = arcpy.da.TableToNumPyArray('land', ['OBJECTID','Country','ISO_3digit'])
r.dtype.names = [{'Country'   :'raw_name',
                  'ISO_3digit':'raw_key'}.get(x, x) for x in r.dtype.names]
raw_type = np.zeros((len(r),), dtype=[('raw_type','<U20')]); raw_type.fill('land')
raw_id   = np.zeros((len(r),), dtype=[('raw_id','<i4')])
rf = np.lib.recfunctions.merge_arrays([r, raw_type, raw_id], flatten=True)
arcpy.da.ExtendTable('land', 'OBJECTID', rf, 'OBJECTID', append_only=False)

 split land and fao into parts
arcpy.MultipartToSinglepart_management('land', 'land_p')
arcpy.MultipartToSinglepart_management('fao_noeez_noant', 'fao_noeez_noant_p')

# merge
print('merge all, pre slivers (%s)' % time.strftime('%H:%M:%S'))
arcpy.Merge_management(['ant_ccamlr_noeez','ant_land','eez_noant_typed','fao_noeez_noant_p','land_p'],'m')
'fao_noeez_ant'

# create slivers
print('create slivers (%s)' % time.strftime('%H:%M:%S'))
arcpy.CreateFishnet_management('box', "-180 -90", "-180 -80", "360", "180", "1", "1", "", "NO_LABELS", "-180 -90 180 90", "POLYGON")
arcpy.DefineProjection_management('box', sr_gcs)
arcpy.Clip_analysis('m', 'box', 'm_c')
arcpy.Erase_analysis('box','m_c', 'm_other')
arcpy.MultipartToSinglepart_management('m_other', 'slivers')
r = arcpy.da.TableToNumPyArray('slivers', ['OBJECTID'])
f = np.zeros((len(r),), dtype=[('raw_type','<U20'),('raw_id','<i4'),('raw_name','<U255'),('raw_key','<U10')]); f['raw_type'].fill('sliver')
rf = np.lib.recfunctions.merge_arrays([r, f], flatten=True)
arcpy.da.ExtendTable('slivers', 'OBJECTID', rf, 'OBJECTID', append_only=False)

# merge slivers
print('merge slivers (%s)' % time.strftime('%H:%M:%S'))
arcpy.Merge_management(['m_c','slivers'],'m_c_s')

# neighbor analysis: SLOW (~ 5 min)
print('neighbor analysis (%s)' % time.strftime('%H:%M:%S'))
arcpy.PolygonNeighbors_analysis('m_c_s', 'nbrs_m_c_s', ['OBJECTID','raw_type','raw_id','raw_name','raw_key'], 'NO_AREA_OVERLAP')

# get merged data, add empty spatial sp_* fields and use PANDAS data frame
print('get merged data, add empty spatial sp_* fields and use PANDAS data frame (%s)' % time.strftime('%H:%M:%S'))
m = arcpy.da.TableToNumPyArray('m_c_s', ['OBJECTID','raw_type','raw_id','raw_name','raw_key','Shape_Area'])
f = np.zeros((len(m),), dtype=[('sp_type','<U20'),('sp_id','<i4'),('sp_name','<U255'),('sp_key','<U10')])
m = pandas.DataFrame(np.lib.recfunctions.merge_arrays([m, f], flatten=True), index=m['OBJECTID'])

# fao bordering land: presume land gap filled by fao if small
print('fao bordering land: presume land gap filled by fao (%s)' % time.strftime('%H:%M:%S'))
n = pandas.DataFrame(arcpy.da.TableToNumPyArray(
    'nbrs_m_c_s',
    ['src_OBJECTID','src_raw_type','src_raw_id','src_raw_name','src_raw_key',
     'nbr_OBJECTID','nbr_raw_type','nbr_raw_id','nbr_raw_name','nbr_raw_key','LENGTH'],
    "src_raw_type = 'fao' AND nbr_raw_type = 'land' AND LENGTH > 0"))
d = n.groupby(['src_OBJECTID']).agg(lambda df: df.iloc[df['LENGTH'].values.argmax()])
d = d[m.loc[d.index]['Shape_Area'] < 500] # exclude big areas
id_done = d.index
m.loc[d.index, 'sp_type'] = 'fao-land'
m.loc[d.index, 'sp_id']   = d['nbr_raw_id'].astype('int32')
m.loc[d.index, 'sp_name'] = d['nbr_raw_name']
m.loc[d.index, 'sp_key'] = d['nbr_raw_key']

# land bordering fao: presume overextended land from landeez
print('land bordering fao: presume overextended land from landeez (%s)' % time.strftime('%H:%M:%S'))
n = pandas.DataFrame(arcpy.da.TableToNumPyArray(
    'nbrs_m_c_s',
    ['src_OBJECTID','src_raw_type','src_raw_id','src_raw_name','src_raw_key',
     'nbr_OBJECTID','nbr_raw_type','nbr_raw_id','nbr_raw_name','nbr_raw_key','LENGTH'],
    "src_raw_type = 'land' AND nbr_raw_type = 'fao' AND LENGTH > 0"))
d = n.groupby(['src_OBJECTID']).agg(lambda df: df.iloc[df['LENGTH'].values.argmax()])
d = d[m.loc[d.index]['Shape_Area'] < 20] # exclude big areas
d = d[~d.index.isin(id_done)]
id_done = set(id_done).union(d.index)
m.loc[d.index, 'sp_type'] = 'land-fao'
m.loc[d.index, 'sp_id']   = d['nbr_raw_id'].astype('int32')
m.loc[d.index, 'sp_name'] = d['nbr_raw_name']
m.loc[d.index, 'sp_key'] = d['nbr_raw_key']

# land bordering eez: apply eez with greatest shared LENGTH
print('land bordering eez: apply eez with greatest shared LENGTH (%s)' % time.strftime('%H:%M:%S'))
n = pandas.DataFrame(arcpy.da.TableToNumPyArray(
    'nbrs_m_c_s',
    ['src_OBJECTID','nbr_raw_id','nbr_raw_name','nbr_raw_key','LENGTH'],
    "src_raw_type = 'land' AND nbr_raw_type = 'eez' AND LENGTH > 0"))
d = n.groupby(['src_OBJECTID']).agg(lambda df: df.iloc[df['LENGTH'].values.argmax()])
d = d[~d.index.isin(id_done)]
id_done = set(id_done).union(d.index)
m.loc[d.index, 'sp_type'] = 'land'
m.loc[d.index, 'sp_id']   = d['nbr_raw_id'].astype('int32')
m.loc[d.index, 'sp_name'] = d['nbr_raw_name']
m.loc[d.index, 'sp_key']  = d['nbr_raw_key']

# land not bordering eez: use raw values
print('land not bordering eez: use raw values (%s)' % time.strftime('%H:%M:%S'))
idx = (m['sp_name'] == '') & (m['raw_type']=='land') & (~m.index.isin(id_done))
id_done = set(id_done).union(m[idx].index)
m.loc[idx, 'sp_type'] = 'land'
m.loc[idx, 'sp_id']   = m.loc[idx,'raw_id']
m.loc[idx, 'sp_name'] = m.loc[idx,'raw_name']
m.loc[idx, 'sp_key']  = m.loc[idx,'raw_key']

# determine land-noeez
print('determine land-noeez (%s)' % time.strftime('%H:%M:%S'))
d = m[(m['sp_type'] == 'land') & (m['sp_id'] == 0) & (m['sp_name']!='Australia')].groupby(['raw_name']) #  & (~m.index.isin(id_done))
for raw_name, group in d: # raw_name, group = next(d.groups.iteritems())
    if sum((m['raw_name']==raw_name) & (m['raw_type']=='eez')) == 0:
        m.loc[(m['raw_name']==raw_name) & (m['raw_type']=='land'), 'sp_type'] = 'land-noeez'

# copy the rest
print('copy the rest (%s)' % time.strftime('%H:%M:%S'))
idx = ~m.index.isin(id_done)
m.ix[idx, 'sp_type'] = m[idx]['raw_type']
m.ix[idx, 'sp_id']   = m[idx]['raw_id']
m.ix[idx, 'sp_name'] = m[idx]['raw_name']
m.ix[idx, 'sp_key']  = m[idx]['raw_key']

# apply new fields and dissolve
print('apply new fields and dissolve (%s)' % time.strftime('%H:%M:%S'))
r = m[[x for x in m.columns if x!='Shape_Area']].to_records(index=False)
r = r.astype([('OBJECTID', '<i4'),('sp_type','<U20'),('sp_id','<i4'),('sp_name','<U255'),('sp_key','<U10')])
arcpy.da.ExtendTable('m_c_s', 'OBJECTID', r, 'OBJECTID', append_only=False)
arcpy.Dissolve_management('m_c_s', 'm_c_s_d', ['sp_type','sp_id','sp_name','sp_key'])

# copy features for manual inspection
arcpy.MakeFeatureLayer_management('m_c_s_d', 'lyr_m', "sp_type IN ('fao-land','land-fao','sliver')")
arcpy.CopyFeatures_management('lyr_m', 'sp_manual')
arcpy.DeleteFeatures_management('lyr_m')

# copy updated manual features. CAUTION: do not uncomment below and overwrite manual output unless redoing
#arcpy.Dissolve_management('sp_manual', sp_slivers, ['sp_type','sp_id','sp_name','sp_key'])
# TODO: manually edit looking at ESRI oceans basemap, neighbors and original underlying layers.

# merge slivers back and dissolve
arcpy.CopyFeatures_management(sp_slivers, 'sp_slivers')
arcpy.Merge_management(['m_c_s_d', sp_slivers], 'sp_m')
# shapefiles introduce an extra space for otherwise blank or null values
arcpy.CalculateField_management('sp_m','sp_key', 'strip(!sp_key!)', 'PYTHON_9.3', "def strip(s): return(s.strip())")
arcpy.CalculateField_management('sp_m','sp_name', 'strip(!sp_name!)', 'PYTHON_9.3', "def strip(s): return(s.strip())")
arcpy.Dissolve_management('sp_m', 'sp_m_d', ['sp_type','sp_id','sp_name','sp_key'])

# merge and export to git/manual_output/sp_rgn_manual.csv for editing
d = pandas.DataFrame(arcpy.da.TableToNumPyArray('sp_m_d', ['OBJECTID','sp_type','sp_id','sp_name','sp_key','Shape_Area']))
z = pandas.io.parsers.read_csv(z_2013_csv, encoding='utf-8') #print(set(d['sp_type'])) # set([u'ccamlr', u'land', u'eez-inland', u'fao', u'eez',])
m_eez  = pandas.merge(d[d['sp_type']=='eez'] , z[z['rgn_typ']=='eez'], how='outer', left_on='sp_name', right_on='eez_nam')
m_land = pandas.merge(d[d['sp_type']=='land'], z[z['rgn_typ']=='eez'], how='outer', left_on='sp_name', right_on='eez_nam')
d.ix[d['sp_type']=='fao','sp_id'] = d[d['sp_type']=='fao']['sp_id'] + 1000
m_fao  = pandas.merge(d[d['sp_type']=='fao'], z[z['rgn_typ']=='fao'], how='outer', left_on='sp_id', right_on='eez_id')
m_eezinland = pandas.merge(d[d['sp_type']=='eez-inland'], z[z['rgn_typ']=='eez-inland'], how='outer', left_on='sp_name', right_on='eez_nam')
m_ccamlr = pandas.merge(d[d['sp_type']=='ccamlr'], z[z['rgn_typ']=='ccamlr'], how='outer', left_on='sp_name', right_on='eez_nam')
m = pandas.concat([m_eez, m_land, m_fao, m_eezinland, m_ccamlr])
for col in ['sp_id','sp_type','sp_name','sp_key']:
    m[col+'_orig'] = m[col]
    m[col] = None
# CAUTION: do not uncomment below and overwrite manual output unless redoing
#m.to_csv(sp_rgn_csv, index=False, encoding='utf-8')

# import and merge git/manual_output/sp_rgn_manual.csv for editing
#arcpy.Dissolve_management('sp_m', 'sp_m_d', ['sp_type','sp_id','sp_name','sp_key']) # redo sp_id ...
d = pandas.DataFrame(arcpy.da.TableToNumPyArray('sp_m_d', ['OBJECTID','sp_type','sp_id','sp_name','sp_key'])) # print(set(d['sp_type'])) # set([u'ccamlr', u'land', u'eez', u'land-noeez', u'fao', u'eez-inland'])
# convert from Unicode to ASCII for matching lookup
for u,a in {u'Curaçao':'Curacao', u'République du Congo':'Republique du Congo', u'Réunion':'Reunion'}.iteritems(): # u=u'Réunion'; a='Reunion'
    d.ix[d.sp_name==u,'sp_name'] = a
d = d.rename(columns={'sp_type':'sp_type_orig','sp_name':'sp_name_orig', 'sp_id':'sp_id_orig','sp_key':'sp_key_orig'})
z = pandas.io.parsers.read_csv(sp_rgn_csv) # , encoding='utf-8') #z_cols = ['sp_type','sp_name_orig','sp_id','sp_name','sp_key','rgn_typ','rgn_id','rgn_name','rgn_key','country_id_2012','region_id_2012','region_name_2012']
m = pandas.merge(d, z, how='left', on=['sp_type_orig','sp_name_orig'])
# missing and duplicate checks should return 0 rows:
#  m[m.sp_name.isnull()][['sp_type_orig','sp_name_orig']]
#  m[m.duplicated('OBJECTID')].sort(['sp_type_orig','sp_name_orig'])[['sp_type_orig','sp_name_orig']]
arcpy.AddField_management('sp_m_d', 'OID', 'LONG')
arcpy.CalculateField_management('sp_m_d', 'OID', '!OBJECTID!', 'PYTHON_9.3')
arcpy.CopyFeatures_management('sp_m_d','sp_m_d_i')
arcpy.DeleteField_management('sp_m_d_i', [x.name for x in arcpy.ListFields('sp_m_d_i') if x.name not in ('OBJECTID','OID','Shape','Shape_Length','Shape_Area')])
r = m[['OBJECTID',
       'sp_type','sp_id','sp_name','sp_key',
       'rgn_type','rgn_id','rgn_name','rgn_key',
       'cntry_id12','rgn_id12','rgn_name12']].to_records(index=False) # m[[x for x in m.columns if x!='Shape_Area']]
r = r.astype(
    [('OBJECTID', '<i4'),
     ('sp_type'     , '<U20'), ('sp_id'     , '<i4'), ('sp_name'     , '<U255'), ('sp_key'     , '<U10'),
     ('rgn_type'    , '<U20'), ('rgn_id'    , '<i4'), ('rgn_name'    , '<U255'), ('rgn_key'    , '<U10'),
     ('cntry_id12'  ,'<U255'), ('rgn_id12'  , '<i4'), ('rgn_name12'  , '<U255'), ('notes'      , '<U255')])
arcpy.da.ExtendTable('sp_m_d_i', 'OID', r, 'OBJECTID', append_only=False)
arcpy.Dissolve_management('sp_m_d_i', 'sp_gcs' , ['sp_type','sp_id','sp_name','sp_key','rgn_type','rgn_id','rgn_name','rgn_key','cntry_id12','rgn_id12','rgn_name12'])
arcpy.Dissolve_management('sp_m_d_i', 'rgn_gcs', ['rgn_type','rgn_id','rgn_name','rgn_key'])
arcpy.RepairGeometry_management('sp_gcs')
arcpy.RepairGeometry_management('rgn_gcs')

 add areas
print('add areas (%s)' % time.strftime('%H:%M:%S'))
arcpy.AddMessage('calculate areas')
arcpy.AddField_management(      'sp_gcs' , 'area_km2', 'DOUBLE')
arcpy.CalculateField_management('sp_gcs' , 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')
arcpy.AddField_management(      'rgn_gcs', 'area_km2', 'DOUBLE')
arcpy.CalculateField_management('rgn_gcs', 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')

 export shp and csv
print('export shp and csv (%s)' % time.strftime('%H:%M:%S'))
arcpy.CopyFeatures_management('sp_gcs' , sp_shp)
arcpy.CopyFeatures_management('rgn_gcs', rgn_shp)
d = pandas.DataFrame(arcpy.da.TableToNumPyArray('sp_gcs', ['sp_type','sp_id','sp_name','sp_key','area_km2','rgn_type','rgn_id','rgn_name','rgn_key','cntry_id12','rgn_id12','rgn_name12']))
d.to_csv(sp_csv, index=False)
d = pandas.DataFrame(arcpy.da.TableToNumPyArray('rgn_gcs', ['rgn_type','rgn_id','rgn_name','rgn_key','area_km2']))
d.to_csv(rgn_csv, index=False)
print('done (%s)' % time.strftime('%H:%M:%S'))

# Black sea ad-hoc re-add (https://github.com/OHI-Science/ohicore/issues/63)
arcpy.Erase_analysis('CaspianBlackSeas_EEZexclusionpoly', 'BlackSea_eez-inland_idpoly', 'BlackSea_poly')
arcpy.MakeFeatureLayer_management('sp_gcs','lyr_sp_eezinland',"rgn_type = 'eez-inland'")
arcpy.Intersect_analysis(['lyr_sp_eezinland', 'BlackSea_poly'], 'sp_Black')
arcpy.CalculateField_management('sp_Black', 'sp_type', "'eez'", 'PYTHON_9.3')
arcpy.CalculateField_management('sp_Black', 'rgn_type', "'eez'", 'PYTHON_9.3')
flds_del = list(set([x.name for x in arcpy.ListFields('sp_Black')]).difference(set([x.name for x in arcpy.ListFields('sp_gcs')])))
arcpy.DeleteField_management('sp_Black', flds_del)
arcpy.CopyFeatures_management('sp_gcs', 'sp_inlandeezBlack')
arcpy.Erase_analysis('sp_inlandeezBlack', 'sp_Black', 'sp_noBlack')
arcpy.Merge_management(['sp_noBlack_gcs', 'sp_Black'], 'sp_okBlack')
names_Black = ['Bulgaria','Georgia','Romania','Russia','Turkey','Ukraine']
arcpy.MakeFeatureLayer_management('sp_okBlack', 'lyr', "sp_type = 'land-noeez' AND sp_name IN ('%s')" % "','".join(names_Black))
arcpy.CalculateField_management('lyr', 'sp_type', "'land'", 'PYTHON_9.3')
arcpy.CalculateField_management('lyr', 'rgn_type', "'land'", 'PYTHON_9.3')
# still need to dissolve

# add-hoc Antarctica
arcpy.env.outputCoordinateSystem = sr_ant
arcpy.MakeFeatureLayer_management('sp_okBlack', 'lyr', "sp_type = 'land' AND sp_name ='Antarctica'")
arcpy.Dissolve_management('lyr', 'aq_land')
arcpy.DeleteFeatures_management('lyr')
arcpy.MakeFeatureLayer_management('sp_okBlack', 'lyr', "sp_type = 'ccamlr'")
arcpy.CalculateField_management('lyr','sp_type',"'eez-ccamlr'",'PYTHON_9.3')
arcpy.Select_analysis('sp_okBlack', 'aq_ccamlr', "sp_type = 'ccamlr'")

# create theissen polygons used to split slivers
arcpy.Densify_edit('aq_ccamlr', 'DISTANCE', '10 Kilometers')
arcpy.FeatureVerticesToPoints_management('aq_ccamlr', 'aq_ccamlr_pts', 'ALL')
 
# delete interior points
arcpy.Dissolve_management('aq_ccamlr', 'aq_ccamlr_d')
arcpy.MakeFeatureLayer_management('aq_ccamlr_pts', 'lyr_aq_ccamlr_pts')
arcpy.SelectLayerByLocation_management('lyr_aq_ccamlr_pts', 'WITHIN_CLEMENTINI', 'aq_ccamlr_d')
arcpy.DeleteFeatures_management('lyr_aq_ccamlr_pts')
 
# generate thiessen polygons of gadm for intersecting with land slivers
arcpy.env.extent = 'aq_land'
arcpy.CreateThiessenPolygons_analysis('aq_ccamlr_pts', 'aq_thiessen', 'ALL')
arcpy.Dissolve_management('aq_thiessen', 'aq_thiessen_d', ['sp_type','sp_id','sp_name','sp_key','rgn_type','rgn_id','rgn_name','rgn_key','cntry_id12','rgn_id12','rgn_name12'])
arcpy.RepairGeometry_management('aq_thiessen_d')
    
# get slivers, which are land but not identified by gadm, intersect with thiessen so break at junctions
arcpy.Intersect_analysis(['aq_thiessen_d', 'aq_land'], 'aq_thiessen_d_land')
arcpy.CalculateField_management('aq_thiessen_d_land','sp_type',"'land-ccamlr'",'PYTHON_9.3')
arcpy.CalculateField_management('aq_thiessen_d_land','rgn_type',"'land'",'PYTHON_9.3')

# return to geographic coordinate system
arcpy.env.outputCoordinateSystem = sr_gcs
arcpy.CopyFeatures_management('aq_thiessen_d_land','aq_landccamlr_gcs')
arcpy.RepairGeometry_management('aq_landccamlr_gcs')
flds_del = list(set([x.name for x in arcpy.ListFields('aq_landccamlr_gcs')]).difference(set([x.name for x in arcpy.ListFields('sp_okBlack')])))
arcpy.DeleteField_management('aq_landccamlr_gcs', flds_del)
arcpy.Merge_management(['sp_okBlack', 'aq_landccamlr_gcs'], 'sp_okAQ')

arcpy.Clip_analysis('sp_okAQ', 'box', 'sp_okAQ_c')
arcpy.Dissolve_management('sp_okAQ_c', 'sp_gcs' ,['sp_type' ,'sp_id' ,'sp_name' ,'sp_key',
                                                  'rgn_type','rgn_id','rgn_name','rgn_key',
                                                  'cntry_id12','rgn_id12','rgn_name12'])
arcpy.Dissolve_management('sp_okAQ_c', 'rgn_gcs',['rgn_type','rgn_id','rgn_name','rgn_key'])
arcpy.RepairGeometry_management('sp_gcs')
arcpy.RepairGeometry_management('rgn_gcs')

# add areas
print('add areas (%s)' % time.strftime('%H:%M:%S'))
arcpy.AddMessage('calculate areas')
arcpy.AddField_management(      'sp_gcs' , 'area_km2', 'DOUBLE')
arcpy.CalculateField_management('sp_gcs' , 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')
arcpy.AddField_management(      'rgn_gcs', 'area_km2', 'DOUBLE')
arcpy.CalculateField_management('rgn_gcs', 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')

# post-hoc fix Svalbard ID (see ohiprep:check_sp_id.R: Colombia also seperated but skipping that for now)
arcpy.MakeFeatureLayer_management('sp_gcs', 'lyr', '"sp_name"=\'Svalbard\'')
arcpy.CalculateField_management('lyr', 'sp_id', '253')
arcpy.CalculateField_management('lyr', 'sp_key', '"SVA"')

# post-hoc fix of Norway, Svalbard, Russia
#  ohicore#74 regions: fix Svalbard sp_id 223 to 253, land Norway Russia actually fao 27
# evaluate all sp_gcs land after erasing eezland
# b/c eezland included this area
arcpy.MakeFeatureLayer_management('sp_gcs', 'lyr', '"sp_type"=\'land\' AND "sp_name" IN (\'Svalbard\',\'Russia\')')
arcpy.MultipartToSinglepart_management('lyr','sp_landRUSfix')
# NOTE: manually selected Svalbard and Russa erroneous land from sp_landRUSfix.
#       Then: arcpy.Dissolve_management('sp_landRUSfix', 'sp_landRUSfix_manual')
flds = [f.name for f in arcpy.ListFields('sp_gcs') if f.name not in ('Shape','Shape_Length','Shape_Area')]
r = arcpy.da.TableToNumPyArray('sp_gcs', flds,'"sp_id"=1027')[1]
r['OBJECTID'] = 1
arcpy.da.ExtendTable('sp_landRUSfix_manual', 'OBJECTID', r, 'OBJECTID', append_only=False)
arcpy.Erase_analysis('sp_gcs', 'sp_landRUSfix_manual', 'sp_landRUSfix_e')
arcpy.Merge_management(['sp_landRUSfix_e','sp_landRUSfix_manual'], 'sp_landRUSfix_e_m')
arcpy.Dissolve_management('sp_landRUSfix_e_m', 'sp_gcs', flds)
# TODO: check before replacing sp_gcs

# AGAIN, after found slivers remaining from buffer operation and by dissolving with area_km2 had duplicate FAO sp
arcpy.MakeFeatureLayer_management('sp_gcs', 'lyr', '"sp_type"=\'land\' AND "sp_name" IN (\'Svalbard\',\'Russia\',\'Norway\')')
arcpy.MultipartToSinglepart_management('lyr','sp_landRUSfix2')
# NOTE: manually selected Svalbard and Russa erroneous land from sp_landRUSfix2.
#       Then: arcpy.Dissolve_management('sp_landRUSfix2', 'sp_landRUSfix2_manual')
flds = [f.name for f in arcpy.ListFields('sp_gcs') if f.name not in ('Shape','Shape_Length','Shape_Area','area_km2')]
r = arcpy.da.TableToNumPyArray('sp_gcs', flds,'"sp_id"=1027')
r = r[1:]
r['OBJECTID'] = 1
arcpy.da.ExtendTable('sp_landRUSfix2_manual', 'OBJECTID', r, 'OBJECTID', append_only=False)
arcpy.Erase_analysis('sp_gcs', 'sp_landRUSfix2_manual', 'sp_landRUSfix2_e')
arcpy.Merge_management(['sp_landRUSfix2_e','sp_landRUSfix2_manual'], 'sp_landRUSfix2_e_m')
arcpy.Dissolve_management('sp_landRUSfix2_e_m', 'sp_gcs', flds)
arcpy.AddField_management('sp_gcs', 'area_km2', 'DOUBLE')

# post-hoc update affected areas
#arcpy.MakeFeatureLayer_management('sp_gcs', 'lyr', '"sp_name" IN (\'Svalbard\',\'Russia\',\'Norway\',\'Atlantic, Northeast\')')
arcpy.CalculateField_management('sp_gcs', 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')
arcpy.Dissolve_management('sp_gcs', 'rgn_gcs',['rgn_type','rgn_id','rgn_name','rgn_key'])
arcpy.AddField_management(      'rgn_gcs', 'area_km2', 'DOUBLE')
arcpy.CalculateField_management('rgn_gcs', 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')

# export shp and csv
print('export shp and csv (%s)' % time.strftime('%H:%M:%S'))
arcpy.CopyFeatures_management(gdb+'/sp_gcs' , sp_shp)
arcpy.CopyFeatures_management(gdb+'/rgn_gcs', rgn_shp)
d = pandas.DataFrame(arcpy.da.TableToNumPyArray('sp_gcs', ['sp_type','sp_id','sp_name','sp_key','area_km2','rgn_type','rgn_id','rgn_name','rgn_key','cntry_id12','rgn_id12','rgn_name12']))
d.to_csv(sp_csv, index=False)
d = pandas.DataFrame(arcpy.da.TableToNumPyArray('rgn_gcs', ['rgn_type','rgn_id','rgn_name','rgn_key','area_km2']))
d.to_csv(rgn_csv, index=False)
print('done (%s)' % time.strftime('%H:%M:%S'))

# TODO: clean up old sp_data.csv and rgn_data.csv and shapefiles on neptune/git-annex

# TODO: clip to earth

### TODO: simplify and TopoJSON
### Simplify lake polygons.
##arcpy.cartography.SimplifyPolygon('rgns_gcs', 'rgns_simplify_gcs', 'POINT_REMOVE', 0.01, 200, "RESOLVE_ERRORS", "KEEP_COLLAPSED_POINTS", "CHECK")
### Smooth lake polygons.
##arcpy.cartography.SmoothPolygon(simplifiedFeatures, smoothedFeatures, "PAEK", 100, "FLAG_ERRORS")
##
### TODO: check that rgn_id is unique, and that all rgn_type=='eez' have a matching 'rgn_type'=='land'