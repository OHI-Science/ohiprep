import arcpy as ap, pandas as pd, socket, os

# configuration based on machine name
conf_dir = {
    'Amphitrite':
    {'git'    :'G:/ohiprep',
     'neptune':'N:',
     'halpern2008':'Y:',
     'tmp'    :'C:/tmp',
     }}[socket.gethostname()]

# paths
lu_csv      = '{git}/Global/SAUP-FishCatchByGearType_Halpern2008/ingest_fish_lookup.csv'.format(**conf_dir)
dir_fis_fro = '{halpern2008}/mnt/storage/marine_threats/work/fisheries'.format(**conf_dir)
dir_fis_to  = '{neptune}/git-annex/Global/SAUP-FishCatchByGearType_Halpern2008'.format(**conf_dir)

#/catch_rasters/catch1
#/normalized/fishprod1

dict = locals()
df = pd.io.parsers.read_csv(lu_csv, header=0)
for r in df.iterrows():
    dict.update(r[1])
    
    p_fro = '{dir_fis_fro}/{name_neptune}'.format(**dict)
    p_to  = '{dir_fis_to}/{name_globalmarine}.tif'.format(**dict)
    #arcpy.CopyRaster_management(





