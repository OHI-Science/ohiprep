README for raw/GL-CI-ThreatenedSpecies

Downloaded most data from:
  http://www.iucnredlist.org/technical-documents/spatial-data
ANGELFISH
BUTTERFLYFISH
GROUPERS
MAMMMARINE
MANGROVES
PARROTFISH
SEAGRASSES
WRASSES  

Except:
All_CORAL_Oct2010.gdb
hex_9

Additional files provided by Liz Selig at CI:
All Hagfishes.zip
Seasnakes.zip
TunasBillfishes1.zip
TunasBillfishes2.zip

2011-07-06
Invalid zip archives: GROUPERS.zip (640 KB), SEAGRASSES.zip (55.2 MB).
  Redownloaded from: http://www.iucnredlist.org/technical-documents/spatial-data
  which also links to birds...
Should request Bird species distribution maps of the world (all 10,000 + species) here:
  http://www.birdlife.org/datazone/info/spcdownload3

# PG database
shp2pgsql -c -d -D -s 4326 -i -I tmp/ANGELFISH.shp public.gl_ci_angelfishD > gl_ci_angelfishD.sql
psql -d best -f gl_ci_angelfishD.sql

/var/data/ohi/ingest/GL-CI-ThreatenedSpecies/import_pgsql.sh
db=best
srid=4326 # 4326=wgs84
pfx=gl_ci

for f in tmp/*.shp; do
  tbl=$pfx_`basename $f .shp`
  sql=tmp/$tbl.sql
  echo "$f > $sql > pgsql table: $tbl"
  #shp2pgsql -c -d -D -s $srid -i -I $f public.$tbl > $sql
  shp2pgsql -c -d -D -G -i -I $f public.$tbl > $sql
  psql -d $db -f $sql
  psql -c "DELETE FROM $tbl WHERE 'PRESENCE' NOT IN (1,2) AND 'SEASONAL'<=0 and 'ORIGIN'!-
# where='(PRESENCE=1 or PRESENCE=2) and SEASONAL>0 and ORIGIN=1'
done


# Redoing with ArcGIS
for shp in shps:
  Select WHERE presence IN (1,2) AND seasonal IN (0,1) AND origin IN (0,1)
  Dissolve
  Dice
  Intersect
  ?: Need to aggregate at Intersect?  
  ? Repair geometry
  for sp in shp:
	If area > area_limit:
		Dice
	Intersect


On neptune see non-versioned: /var/data/model/GL-IUCN-RedList
               and versioned: /usr/local/ohi/src/data/model/GL-IUCN-RedList
