test -d data || mkdir data
test -d tmp || mkdir tmp

#ln -sf $OHI_MODELDIR/GL-NCEAS-Countries_v2013a/data/ocean_regions_inland_50mi.* tmp
#ln -sf $OHI_MODELDIR/GL-NCEAS-Landsea/data/rgn_inland_50mi.tif tmp
ln -sf $OHI_MODELDIR/GL-NCEAS-OceanRegions_v2013a/data/rgn_inland_25mi_mol.t* tmp
#ln -sf $OHI_STABLEDIR/GL-SEDAC-GPWv3/data/glcount00.tif tmp
#ln -sf $OHI_STABLEDIR/GL-SEDAC-GPWv3/data/glcountries.* tmp
ln -sf $OHI_MODELDIR/GL-CIESIN-CoastalPop_v2012/data/popdensity_*_gcs.t* tmp