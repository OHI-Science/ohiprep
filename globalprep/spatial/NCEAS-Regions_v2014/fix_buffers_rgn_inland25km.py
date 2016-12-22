# Run on cmd:
#  amphitrite 64: C:\Python27\ArcGISx6410.2\python.exe G:\ohiprep\Global\NCEAS-Regions_v2014\fix_buffers.py
#  optimus      : C:\Python27\ArcGISx6410.1\python.exe D:\best\docs\GitHub\ohiprep\Global\NCEAS-Regions_v2014\fix_land-mid-eez-RUS-SVA-UK_duplicate-spid-SVA_null-inland1km-offshore1km-CAN.py

# modules
import arcpy, numpy, os, sys, re, socket, pandas, time, math, re

# configuration based on machine name
conf = {
    'bumblebee':
    {'dir_git'    :'C:/Users/visitor/Documents/github/ohiprep',
     'dir_neptune':'N:',
     'dir_tmp'    :'C:/Users/visitor/bbest/tmp',
     },
    'amphitrite':
    {'dir_git'    :'G:/ohiprep',
     'dir_neptune':'N:',
     'dir_tmp'    :'C:/tmp',
     },
    'optimus':
    {'dir_git'    :'D:/best/docs/GitHub/ohiprep',
     'dir_neptune':'N:',
     'dir_tmp'    :'D:/best/tmp',
     }}[socket.gethostname().lower()]

# paths
nm      = 'NCEAS-Regions_v2014'                                      # name of data product
td      = '{0}/Global/{1}'.format(conf['dir_tmp'], nm)               # temp directory on local filesystem
gdb     = '{0}/geodb.gdb'.format(td)                                 # file geodatabase
ad      = '{0}/git-annex/Global/{1}'.format(conf['dir_neptune'], nm) # git annex directory on neptune
gd      = '{0}/Global/{1}'.format(conf['dir_git'], nm)               # git directory on local filesystem

# environment
arcpy.env.workspace       = gdb
arcpy.env.overwriteOutput = True

# inputs
##buffers = ['inland','offshore']
buffers = ['inland1km','offshore1km','offshore3nm','inland25km','inland50km']
dict_CAN = {
    'sp_id'     :218,
    'sp_name'   :'Canada',
    'sp_key'    :'CAN',
    'rgn_id'    :218,
    'rgn_name'  :'Canada',
    'rgn_key'   :'CAN',
    'cntry_id12':'CAN',
    'rgn_id12'  :166,
    'rgn_name12':'Canada'}

# fields
sp_flds      = [
    'sp_type','sp_id','sp_name','sp_key',
    'rgn_type','rgn_id','rgn_name','rgn_key',
    'cntry_id']     
rgn_flds      = [
    'rgn_type','rgn_id','rgn_name','rgn_key']


buf = 'inland25km'
#sp_buf  = 'sp_%s_gcs' % buf # SKIPPING sp_inland25km b/c it only had a few shapefiles
rgn_buf = 'rgn_%s_gcs' % buf
print('%s... (%s)' % (sp_buf, time.strftime('%H:%M:%S')))

# redoing
rgn_buf_shp = '%s/data/%s.shp' % (ad, rgn_buf)
arcpy.CopyFeatures_management(rgn_buf_shp, rgn_buf)

for fc in ['sp_landfix_buf60km']:
arcpy.CopyFeatures_management('%s/data/sp_gcs.shp' % ad, 'sp_gcs')
arcpy.CopyFeatures_management('%s/data/sp_gcs.shp' % ad, 'sp_gcs')
arcpy.CopyFeatures_management(r'N:\git-annex\Global\NCEAS-Regions_v2014\manual_output\sp_landfix_buf60km_id.shp', '%s/sp_landfix_buf60km' % gdb)

arcpy.MakeFeatureLayer_management('rgn_gcs', 'lyr_fao', "rgn_type='fao'")
arcpy.MultipartToSinglepart_management('lyr_fao', 'fao_p')
# manually selected FAO region inside and exported to N:\git-annex\Global\NCEAS-Regions_v2014\manual_output\sp_landfix_buf60km_id_fao.shp
arcpy.Merge_management(['sp_landfix_buf60km_id_fao', 'sp_landfix_buf60km'], 'sp_landfix_buf60kmfao')

print('  erasing erroneous mid-EEZ land buffer (%s)' % (time.strftime('%H:%M:%S')))
arcpy.Erase_analysis('%s/%s' % (gdb, rgn_buf), '%s/%s' % (gdb, 'sp_landfix_buf60kmfao'), '%s/%s' % (gdb, 'rgn_%s_e' % buf))

fc = rgn_buf

# rename / delete if old fields lingering
flds = [f.name for f in arcpy.ListFields(fc)]
for fld_fro, fld_to in {'cntry_id12':'cntry_id'}.iteritems():
    if fld_fro in flds: 
        arcpy.AlterField_management(fc, fld_fro, fld_to, fld_to)
for fld in ['rgn_id12', 'rgn_name12']:
    if fld_fro in flds: 
        arcpy.DeleteField_management(fc, fld)

# NOTE: Svalbard only problem with sp_*, already Norway in rgn_*

# convert any NULL rows to Canada
if arcpy.Exists('lyr'): arcpy.Delete_management('lyr')
arcpy.MakeFeatureLayer_management('%s/rgn_%s_e' % (gdb, buf), 'lyr', '"rgn_name" IS NULL OR "rgn_id" IS NULL')
n = int(arcpy.GetCount_management('lyr').getOutput(0))
print 'count: %d, where rgn_name or rgn_id is NULL' % n
# n=0 for inland25km
if (n > 0):
    print '  WARNING!: %s has %d rows where sp_name is NULL. Presuming Canada.' % (sp_buf, n)   

    arcpy.CalculateField_management('lyr',  'sp_name', "'Canada'", 'PYTHON_9.3')
    arcpy.Delete_management('lyr')
    arcpy.MakeFeatureLayer_management('%s/sp_%s_e' % (gdb, buf), 'lyr', '"sp_name" = \'Canada\'')
    # update field values to Canada
    for fld, val in dict_CAN.iteritems():
        print '    ',fld, val
        if type(val) is str:
            val_str = "'%s'" % val
        else:
            val_str = '%g' % val
        arcpy.CalculateField_management('lyr', fld, val_str, 'PYTHON_9.3')

    # update {sp|rgn}_type to land or eez
    if re.compile('.*inland.*').match(buf):
        sp_type = 'land'
    else: # assume offshore
        sp_type = 'eez'
    arcpy.CalculateField_management('lyr',  'sp_type', "'%s'" % sp_type, 'PYTHON_9.3')
    arcpy.CalculateField_management('lyr', 'rgn_type', "'%s'" % sp_type, 'PYTHON_9.3')


# copy erased fc and update areas
arcpy.CopyFeatures_management('%s/rgn_%s_e' % (gdb, buf), fc)
arcpy.RepairGeometry_management(fc)
arcpy.CalculateField_management(fc, 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')

# export shp and csv
arcpy.CopyFeatures_management('%s/%s' % (gdb, fc) , '{0}/data/{1}.shp'.format(ad, fc))
d = pandas.DataFrame(arcpy.da.TableToNumPyArray(fc, rgn_flds + ['area_km2']))
d.to_csv('{0}/data/{1}_data.csv'.format(gd, fc), index=False)


# TODO: create rgn_inland25km_mol.tif. then extract just country and overlay with N:\model\GL-NCEAS-CoastalPopulation_v2013\data\popdensity_2014_mol.tif.
