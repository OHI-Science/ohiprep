mkdirs data tmp

# link regions shapefile in geographic coordinate system
ln -sf $OHI_MODELDIR/GL-NCEAS-OceanRegions_v2013a/data/rgn_fao_gcs.{shp,dbf,shx,prj,sbn,sbx,shp.xml} tmp
ln -sf $OHI_MODELDIR/GL-NCEAS-OceanRegions_v2013a/data/land_gcs.{shp,dbf,shx,prj,sbn,sbx,shp.xml} tmp
ln -sf $OHI_MODELDIR/GL-NCEAS-OceanRegions_v2013a/data/rgn_offshore_3nm_mol.t* tmp

# get last year's species to check for change in status (especially for those that have gone EXtinct)
ln -sf $OHI_MODELDIR/GL-NCEAS-SpeciesDiversity/data/spp.csv tmp/spp_2012.csv

# spp for 2012
for f in cells spp cells cells_spp; do
	echo $OHI_MODELDIR/GL-NCEAS-SpeciesDiversity_v2012/data/${f}.csv tmp/${f}_2012.csv
done
