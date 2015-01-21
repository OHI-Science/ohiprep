library(raster)

# set temporary directory to folder on neptune disk big enough to handle it
tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)

source('~/ohiprep/src/R/common.R')

#paths:
path_save <- file.path(dir_neptune_data, "git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/tmp")


#exploring fisheries data from 2008 and 2013
# This was a file created by Ben B. relatively recently to make sense of these data:
# data from 2008 standardized by productivity
dem_d_2008_fp  = raster(file.path(dir_neptune_data, 'git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data/fishprod_dem_d_gcs.tif'))

# data from 2008 - not standardized by productivity
dem_d_2008_catch = raster(file.path(dir_neptune_data, 'git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data/catch_dem_d_gcs.tif'))


## These data are in one of the model folders:

data1 <- raster(file.path(dir_neptune_data, "model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/tmp/fp_com_hb_dem.tif")) # basically 0 to 1
data2 <- raster(file.path(dir_neptune_data, "model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/tmp/fp_com_hb_dem_m1000int.tif")) # times 1000
data3 <- raster(file.path(dir_neptune_data, "model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/tmp/fp_com_hb_dem_pctchg.tif")) # data1 multiplied by 1+pctchg/100 
                                                                                                                                 # pct change is saup_pct_chg_mol.tif, corresponds to csv file
data3b <- raster(file.path(dir_neptune_data, "model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/data/fp_com_hb_dem_2013_raw.tif")) # same as data 3

data4 <- raster(file.path(dir_neptune_data, "model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/tmp/fp_com_hb_dem_pctchg_m1000int.tif")) #data 3 multiplied by 1000
data5 <- raster(file.path(dir_neptune_data, "model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/tmp/fp_com_hb_dem_pctchg_rescaled.tif")) # r = (cell - min) / (max - min)
data5b <- raster(file.path(dir_neptune_data, "model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/data/fp_com_hb_dem_2013_rescaled.tif")) # same as data 5

data6 <- raster(file.path(dir_neptune_data, "model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/tmp/fp_com_hb_dem_pctchg_rescaled_m1000int.tif")) # times 1000



### Question 1: where does data1 come from?
###  The dem_d_2008_fp data? 
calc(dem_d_2008_fp, fun=function(x){x/cellStats(dem_d_2008_fp, stat='max')}, filename=file.path(path_save, "dem_d_2008_fp_normalized"), overwrite=TRUE, progress="text")
dem_d_2008_fp_normalized <- raster(file.path(path_save, "dem_d_2008_fp_normalized"))
plot(dem_d_2008_fp_normalized)
# No

## How about if it is logged?
calc(dem_d_2008_fp, fun=function(x){log(x + 1)}, filename=file.path(path_save, "dem_d_2008_fp_log.tif"), overwrite=TRUE, progress="text")
dem_d_2008_fp_log <- raster(file.path(path_save, "dem_d_2008_fp_log.tif"))
plot(dem_d_2008_fp_log)
calc(dem_d_2008_fp_log, fun=function(x){x/cellStats(dem_d_2008_fp_log, stat='max')}, filename=file.path(path_save, "dem_d_2008_fp_log_normalized.tif"), overwrite=TRUE, progress="text")
dem_d_2008_fp_log_normalized <- raster(file.path(path_save, "dem_d_2008_fp_log_normalized.tif"))
plot(dem_d_2008_fp_log_normalized)

file.path(path_save, "dem_d_2008_fp_log")

# reproject data
projectRaster(dem_d_2008_fp_log_normalized, data1, filename=file.path(path_save, "dem_d_2008_fp_log_normalized_mol"), progress="text")
dem_d_2008_fp_log_normalized_mol <- raster(file.path(path_save, "dem_d_2008_fp_log_normalized_mol"))
max <- cellStats(dem_d_2008_fp_log_normalized_mol, stat="max")
min <- cellStats(dem_d_2008_fp_log_normalized_mol, stat="min")
calc(dem_d_2008_fp_log_normalized_mol, function(x){(x-min)/
                                                     (max-min)},
     filename=file.path(path_save, "dem_d_2008_fp_log_normalized_mol_norm"), progress="text")

dem_d_2008_fp_log_normalized_mol_norm <- raster(file.path(path_save, "dem_d_2008_fp_log_normalized_mol_norm"))
plot(dem_d_2008_fp_log_normalized_mol_norm)

s <- stack(dem_d_2008_fp_log_normalized_mol_norm, data1)
overlay(s, fun=function(x,y) x-y, 
        filename=file.path(path_save, "data1_rel2_dem_d_2008_fp_log_normalized_mol_norm"),
        progress="text", overwrite=TRUE)

data1_rel2_dem_d_2008_fp_log_normalized_mol_norm <- raster(file.path(path_save, "data1_rel2_dem_d_2008_fp_log_normalized_mol_norm"))
plot(data1_rel2_dem_d_2008_fp_log_normalized_mol_norm)
hist(data1_rel2_dem_d_2008_fp_log_normalized_mol_norm)
click(data1_rel2_dem_d_2008_fp_log_normalized_mol_norm)

### The dem_d_2008_catch data? 
calc(dem_d_2008_catch, fun=function(x){x/cellStats(dem_d_2008_catch, stat='max')}, filename=file.path(path_save, "dem_d_2008_catch_normalized"), overwrite=TRUE, progress="text")
dem_d_2008_catch_normalized <- raster(file.path(path_save, "dem_d_2008_catch_normalized"))
plot(dem_d_2008_catch_normalized)
# No

### Question 2: What data did John use for cia?

## 2008 data
dem_d_f = raster(file.path(dir_halpern2008, 
                           'mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2008_interim/old_layers/demersal_destructive_fishing/moll_nontrans_unclipped_1km/demersal_destructive_fishing.tif'))
# it appears that he used this file (but projected it to mollweide which messes with the numbers a bit)

## 2013 data
dem_d_f_2013 = raster(file.path(dir_halpern2008, 'mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2013_interim/new_layers/demersal_destructive_fishing/moll_nontrans_unclipped_1km/demersal_destructive_fishing.tif'))



## Ignore below stuff for now


projectRaster(dem_d_2008_fp_normalized, data1, filename=file.path(path_save, "dem_d_2008_fp_normalized_mol"), progress="text")
dem_d_2008_fp_normalized_mol <- raster(file.path(path_save, "dem_d_2008_fp_normalized_mol"))

s <- stack(dem_d_2008_fp_normalized_mol, data1)
overlay(s, fun=function(x,y) x/y, 
        filename=file.path(path_save, "data1_rel2_dem_d_2008_fp_normalized_mol"),
        progress="text", overwrite=TRUE)

data1_rel2_dem_d_2008_fp_normalized_mol <- raster(file.path(path_save, "data1_rel2_dem_d_2008_fp_normalized_mol"))

# data from 2013 CHI - assumed to be the percent change applied to 2008 data to create final raster layer (before rescaled)
dem_d_pctchg     = raster(file.path(dir_neptune_data, 'model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/tmp/fp_com_hb_dem_pctchg.tif'))

# email from John on 1/8/2015 suggests that this file is the raw file that was then updated by percent change.
# Our theory: this is the same as the file above (actually the percent change rather than the raw data)
dem_d_2013_raw = raster(file.path(dir_neptune_data, 'model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/data/fp_com_hb_dem_2013_raw.tif')) 
projectRaster(dem_d_2008_fp, data1, filename=file.path(path_save, "dem_d_2008_fp_mol"), progress="tex")

#
dem_d_f = raster(file.path(dir_halpern2008, 'mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/final_impact_layers/threats_2013_interim/new_layers/demersal_destructive_fishing/moll_nontrans_unclipped_1km/demersal_destructive_fishing.tif'))



#to check

#(1) is 'dem_d_2013_raw' the same as 'dem_d_pctchg'?


s <- stack(dem_d_2013_raw, dem_d_pctchg)
               
overlay(s, fun=function(x,y) x/y, 
                       filename='difference',
                       progress="text", overwrite=TRUE)
               
#(2) If we rescale (i.e. standardize/normalize) 'dem_d_2013_raw.tif' do we get the same output as the final
#     layer used in 2013 ()


#(3) Is dem_d_f equal to dem_d_2008_fp (2008 layer standardized by productivity) * dem_d_pctchg (percent change in fisheries from 2009-2011)

#     Doubtful - see #4

# (4) What exactly is dem_d_pctchg?

#       The range of values in dem_d_pctchg (0-1.422) are not the same as those in the calculated percent 
#       change file (N:\model\GL-NCEAS-Pressures_CommercialFisheries_v2013a\tmp\pct_chg_saup_2009to2011_vs_1999to2003.csv)

#       New theory: this layer is actually the fb_com_hb_dem (from 2008) * pctchg.csv
#       
#       This updated raster layer was then rescaled to create 'N:\model\GL-NCEAS-Pressures_CommercialFisheries_v2013a\tmp\fp_com_hb_dem_pctchg_rescaled.tif'
#       which ranges from 0-1. If this is true, this would be the final layer that should be used in CHI 2013 analysis.

# (5) Is 'N:/model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/tmp/fp_com_hb_dem.tif' the rescaled version of: 'N:\git-annex\Global\SAUP-FishCatchByGearType_Halpern2008\data\fishprod_dem_d_gcs.tif'?




#------------------------------------------------------
#                   NEW THEORY
#------------------------------------------------------


#   Based on 'N:\model\GL-NCEAS-Pressures_CommercialFisheries_v2013a\model.py' it seems that the updates went as follows:

#     FILES
#------------------------

#   (1)   fishprod_dem_d_gcs.tif          <- INITIAL INPUT

#         Location: 'N:\git-annex\Global\SAUP-FishCatchByGearType_Halpern2008\data\fishprod_dem_d_gcs.tif'

#         Description: Catch data from 2008 analysis, standardized by productivity 

#         Range of values: 0 - 106.74

#--------------------------------------------------------------------------------

#   (2)   fp_com_hb_dem.tif

#         Location: 'N:\model\GL-NCEAS-Pressures_CommercialFisheries_v2013a\tmp\fp_com_hb_dem.tif'

#         Description: This is file #1 rescaled from 0-1                             #### CAN WE CONFIRM THIS?? (JA 1/8/15)

#         Range of values: 0 - 1

#--------------------------------------------------------------------------------

#   (3)   saup_pct_chg_mol.tif

#         Location: 'N:\model\GL-NCEAS-Pressures_CommercialFisheries_v2013a\tmp\saup_pct_chg_mol.tif'

#         Description: This is a rasterized version of the calculated percent changes in FAO catch data from 1999-2003 to 2009-2011

#             Note: The percent changes per SAUP/FAO region can be found here: 'N:\model\GL-NCEAS-Pressures_CommercialFisheries_v2013a\tmp\pct_chg_saup_2009to2011_vs_199to2003.csv'

#         Range of values: -99.9158 - 499.371

#--------------------------------------------------------------------------------

#   (4)   fp_com_hb_dem_pctchg.tif

#         Location: 'N:\model\GL-NCEAS-Pressures_CommercialFisheries_v2013a\tmp\fp_com_hb_dem_pctchg.tif'

#         Description: This is the result of file #2 times file #3 according to the following formula:

#                fp_com_hb_dem_pctchg.tif = (1 + saup_pct_chg_mol.tif/100) * fp_com_hb_dem.tif          ### TEST THIS TOO (JA 1/8/2015)

#         Range of values: 0 - 1.42202

#--------------------------------------------------------------------------------

#   (5)   fp_com_hb_dem_pctchg_rescaled.tif         <- FINAL OUTPUT

#         Location: 'N:\model\GL-NCEAS-Pressures_CommercialFisheries_v2013a\tmp\fp_com_hb_dem_pctchg_rescaled.tif'

#         Description: This is file number 4 above rescaled linearly from 0 to 1 according to the following formula:

#               out  = (out - min(out))/(max(out) - min(out))       *'out' represents 'fp_com_hb_dem_pctchg_rescaled.tif' here

#         Range of values: 0 - 1


# NOTE: It seems file #4 is the same as the file 'N:\model\GL-NCEAS-Pressures_CommercialFisheries_v2013a\data\fp_com_hb_dem_2013_raw.tif' which may or may not have been
#       the 'raw' data used in the 2013 cumulative impacts analysis.


# Lingering questions:
#----------------------------

#   (1) What is the 'raw' data John P is talking about? Is it 'fp_com_hb_dem_2013_raw.tif'? And if so, was the only modification a rescaling from 0-1?

#   (2) What is the the demersal destructive fishing layer found here: 'H:\mnt\storage\marine_threats\impact_layers_2013_redo\impact_layers\final_impact_layers\threats_2013_interim\new_layers\demersal_destructive_fishing\moll_nontrans_unclipped_1km\demersal_destructive_fishing.tif'
#         This layer goes from 0 to 144.586 and IT SEEMS that this is the layer that was rescaled and ultimately produced the final stressor layer for CHI 2013.

#         If the theory above is correct, why isnt file #5 the data used in CHI 2013 analysis? It seems the changes above were done for OHI, but outline the same kind of 
#         changes that CHI 2013 did....

#   (3) Attempt to understand how exactly percent change was calculated for fisheries from 1999-2003 and 2009-2011 since this will need to be redone with more recent fishing data
