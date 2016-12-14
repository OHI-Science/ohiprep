test -d data || mkdir data
test -d tmp || mkdir tmp

ln -sf $OHI_MODELDIR/GL-NCEAS-OceanRegions_v2013a/data/rgn_offshore_3nm_mol.t* tmp
ln -sf $OHI_MODELDIR/GL-NCEAS-OceanRegions_v2013a/data/rgn_inland_1km_mol.t* tmp
ln -sf $OHI_MODELDIR/GL-NCEAS-OceanRegions_v2013a/data/rgn_fao_mol.t* tmp

ln -sf $OHI_MODELDIR/GL-NCEAS-OceanRegions_v2013a/data/rgn_area_{offshore3nm,inland1km}.csv tmp
ln -sf $OHI_MODELDIR/GL-NCEAS-OceanRegions_v2013a/data/rgn_fao_mol_area.csv tmp
