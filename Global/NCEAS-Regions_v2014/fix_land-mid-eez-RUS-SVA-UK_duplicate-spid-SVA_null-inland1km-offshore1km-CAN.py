# Run on cmd:
#  amphitrite: C:\Python27\ArcGIS10.2\python.exe G:\ohiprep\Global\NCEAS-Regions_v2014\fix_land-mid-eez-RUS-SVA-UK_duplicate-spid-SVA_null-inland1km-offshore1km-CAN.py
#  optimus:    C:\Python27\ArcGISx6410.1\python.exe D:\best\docs\GitHub\ohiprep\Global\NCEAS-Regions_v2014\fix_land-mid-eez-RUS-SVA-UK_duplicate-spid-SVA_null-inland1km-offshore1km-CAN.py

# modules
import arcpy, numpy, os, sys, re, socket, pandas, time, math, re

# configuration based on machine name
conf = {
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
##buffers = ['inland1km','offshore1km','offshore3nm','inland25km','inland50km']
buffers = ['offshore1km','offshore3nm','inland25km','inland50km']
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

sp_flds = [
    'sp_type','sp_id','sp_name','sp_key',     # note: exclude area_km2 for dissolving
    'rgn_type','rgn_id','rgn_name','rgn_key',
    'cntry_id12','rgn_id12','rgn_name12']
sp_area_flds = [
    'sp_type','sp_id','sp_name','sp_key','area_km2',
    'rgn_type','rgn_id','rgn_name','rgn_key',
    'cntry_id12','rgn_id12','rgn_name12']

### replace sp_landfix_manual in sp_gcs
##arcpy.Erase_analysis('sp_gcs', 'sp_landfix_manual', 'sp_gcs_e')
##arcpy.Merge_management(['sp_gcs_e', 'sp_landfix_manual'], 'sp_gcs_e_m')
##arcpy.Dissolve_management('sp_gcs_e_m', 'sp_gcs_e_m_d', sp_flds)
### TODO: check that former RUS land dissolved into single Atlantic, NW FAO
##arcpy.Rename_management('%s/sp_gcs' % gdb, '%s/sp_gcs_prelandfix' % gdb)
##arcpy.Rename_management('%s/sp_gcs_e_m_d' % gdb, '%s/sp_gcs' % gdb)
##
### expand area to cover slivers and offshore
##arcpy.Buffer_analysis('sp_landfix_manual', 'sp_landfix_buf60km', '60 kilometers')
##print('repairing sp_landfix_buf60km (%s)' % (time.strftime('%H:%M:%S')))
##arcpy.RepairGeometry_management('sp_landfix_buf60km')

# loop buffers
for buf in buffers: # buf = 'inland1km'

    sp_buf  = 'sp_%s_gcs' % buf
    rgn_buf = 'rgn_%s_gcs' % buf
    print('%s... (%s)' % (sp_buf, time.strftime('%H:%M:%S')))

    if buf not in ['inland1km','offshore1km']:
        
        print('  repairing (%s)' % (time.strftime('%H:%M:%S')))
        arcpy.RepairGeometry_management(sp_buf)

        print('  copying backup up to %s... (%s)' % ('%s_bkup' % sp_buf, time.strftime('%H:%M:%S')))
        arcpy.CopyFeatures_management(sp_buf, '%s_bkup' % sp_buf)

        print('  fixing Svalbard (%s)' % (time.strftime('%H:%M:%S')))
        arcpy.MakeFeatureLayer_management('%s/%s' % (gdb, sp_buf), 'lyr', '"sp_name"=\'Svalbard\'')
        arcpy.CalculateField_management('lyr', 'sp_id', '253')
        arcpy.CalculateField_management('lyr', 'sp_key', '"SVA"')

        print('  erasing erroneous mid-EEZ land buffer (%s)' % (time.strftime('%H:%M:%S')))
        arcpy.Erase_analysis('%s/%s' % (gdb, sp_buf), '%s/%s' % (gdb, 'sp_landfix_buf60km'), '%s/%s' % (gdb, 'sp_%s_e' % buf))

        # convert any NULL rows to Canada
        if arcpy.Exists('lyr'): arcpy.Delete_management('lyr')
        arcpy.MakeFeatureLayer_management('%s/sp_%s_e' % (gdb, buf), 'lyr', '"sp_name" IS NULL OR "sp_id" IS NULL')
        n = int(arcpy.GetCount_management('lyr').getOutput(0))
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

        print('  repairing sp_%s_e (%s)' % (buf, time.strftime('%H:%M:%S')))
        arcpy.RepairGeometry_management('%s/sp_%s_e' % (gdb, buf))

    try:
        print('  dissolve to sp_buf (%s)' % time.strftime('%H:%M:%S'))
        arcpy.Dissolve_management('%s/sp_%s_e' % (gdb, buf), '%s/%s' % (gdb, sp_buf), sp_flds)
        # TODO: check that not multiple Canadas, Alaska per sp_type

    except Exception as e:
        print e.message
        print('   FAILED: dissolve to sp_buf. Copying to sp_buf (%s)' % time.strftime('%H:%M:%S'))
        arcpy.CopyFeatures_management('%s/sp_%s_e' % (gdb, buf), '%s/%s' % (gdb, sp_buf))

    try:
        print('  calculate sp areas (%s)' % time.strftime('%H:%M:%S'))        
        arcpy.AddField_management(      sp_buf, 'area_km2', 'DOUBLE')
        arcpy.CalculateField_management(sp_buf, 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')

        # export shp and csv
        print('  export sp shp and csv (%s)' % time.strftime('%H:%M:%S'))
        ##    arcpy.CopyFeatures_management('%s/%s' % (gdb, sp_buf) , '{0}/data/{1}.shp'.format(ad, sp_buf))
        # TEMP SHP
        arcpy.CopyFeatures_management('%s/%s' % (gdb, sp_buf) , '{0}/data/{1}.shp'.format(td, sp_buf))
        d = pandas.DataFrame(arcpy.da.TableToNumPyArray(sp_buf, sp_area_flds))
        d.to_csv('{0}/data/sp_gcs_data.csv'.format(gd), index=False)
    except Exception as e:
        print e.message
        print('  FAILED: calculate sp areas, or export sp shp and csv (%s)' % time.strftime('%H:%M:%S'))    
    
    try:
        print('  dissolve to rgns (%s)' % time.strftime('%H:%M:%S'))
        arcpy.Dissolve_management('%s/%s' % (gdb, sp_buf), '%s/%s' % (gdb, rgn_buf), ['rgn_type','rgn_id','rgn_name','rgn_key'])

        print('  calculate rgn areas (%s)' % time.strftime('%H:%M:%S'))        
        arcpy.AddField_management(      rgn_buf, 'area_km2', 'DOUBLE')
        arcpy.CalculateField_management(rgn_buf, 'area_km2', '!shape.area@SQUAREKILOMETERS!', 'PYTHON_9.3')

        # export shp and csv
        print('  export rgn shp and csv (%s)' % time.strftime('%H:%M:%S'))
        ##    arcpy.CopyFeatures_management('%s/%s' % (gdb, rgn_buf), '{0}/data/{1}.shp'.format(ad, sp_buf))
        # TEMP SHP
        arcpy.CopyFeatures_management('%s/%s' % (gdb, rgn_buf), '{0}/data/{1}.shp'.format(td, sp_buf))
        d = pandas.DataFrame(arcpy.da.TableToNumPyArray(rgn_buf, ['rgn_type','rgn_id','rgn_name','rgn_key','area_km2']))
        d.to_csv('{0}/data/rgn_gcs_data.csv'.format(gd), index=False)

    except Exception as e:
        print e.message
        print('  FAILED: dissolve to rgns, calculate sp areas, or export sp shp and csv (%s)' % time.strftime('%H:%M:%S'))
            
print('done (%s)' % time.strftime('%H:%M:%S'))


# TODO: erase sp_landRUSfix_manual','sp_landRUSfix_buf100km'