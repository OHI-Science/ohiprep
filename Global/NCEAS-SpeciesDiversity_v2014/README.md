Multiple Sources:

1. Fisheries. I presume the attached Davis & Baum is the suggested article on translating B/Bmsy to IUCN status. Did we want to still just use RAM (and not additional ICES datasets like the article mentions or FAO below)? I'm still not sure which version of RAM is best and available (static, live or development).
RAM Legacy database in 3 flavors: 1) static copy, 2) live database, 3) development version. More on development version:
"The development version is the most up-to-date version of the database and is different from that used in Ricard et al. (in press), because of stock updates, new assessments and resolution of data errors found in Version 1.0. We plan on releasing new versions of the database for the foreseeable future as updates to existing stocks become accessible and as new stocks are added."

FAO using rfisheries R package to connect to OpenFisheries.org API
"Currently the site's data holdings include global capture fishing landings from the Food and Agriculture Organization (FAO) of the United Nations. In near future we hope to augment this information with spatial species distribution data and expand our collection with fish price data where available."

2. IUCN. We previously downloaded shapefiles from the IUCN Spatial Data site, plus some other files, particularly for corals from Liz. So I can get the master list of all species from http://api.iucnredlist.org/index/all.csv and query individual species for marine habitats, extinction risk and whether subpopulation/subspecies (for exclusion) on a per species basis using the rest of the IUCN API with R and xpath. A new IUCN Discovery portal suggests 7,632 marine species, of which only 1647 are data deficient. More still are probably without rangemaps (but we could perhaps use countries listed if present and map not). I could winnow down to the marine species by looking for the intersection of species lists with the World Register of Marine Species (for other taxonomic querying, the R package taxize could be useful). Does this sound like a reasonable approach? Would we be missing anything else? Should we seek proper permissions? Should we try to include the birds? Is there a web service for the IUCN range maps?

3. AquaMaps. I've asked to join the AquaMaps app as part of the iMarine gCube EU portal, but pending approval. The app looks promising, but probably better to communicate directly Kristin Kaschner (Kristin.Kaschner@biologie.uni-freiburg.de) per their services page. Last time we got a MySQL dump of tables. Is there a web service available or do we need to repeat that process?

Please cite individual maps as: Computer Generated Native Distribution Map for Squalus acanthias (Picked dogfish). www.aquamaps.org, version of Aug. 2013. Web. Accessed 6 Aug. 2013.
 
You can cite AquaMaps as a whole as: Kaschner, K., J. Rius-Barile, K. Kesner-Reyes, C. Garilao, S.O. Kullander, T. Rees, and R. Froese. 2013. AquaMaps: Predicted range maps for aquatic species. World wide web electronic publication, www.aquamaps.org, Version 08/2013.


IUCN treatment
 * removing 30 subspecies entries of the 8380 marine
 * only include EXtinct species if previously showed up in 2012 not as EXtinct. "Oncorhynchus nerka" (135301; sockeye salmon) only species and just for 5 local subpopulations.
 * INCLUDE EXtinct subpopulations
 * subpopulations

 Pressures:
  * 18 layers (17 from before + SLR)
  * 2012 and 2013, so need to update for both
  * oil_rigs for benthic structures  
\\neptune\halpern2008_edit\mnt\storage\marine_threats\impact_layers_2013_redo


See OHI_2013_Supp_methods_doc_draft_v3.doc

4.10.1 Species
Updates in data used for this goal are detailed below in Section 5.

 * marine species: 2012 vs 2013
   new data can lower score b/c better assessment. so not just sense of health, but how well reporting data. quality of data input. uncertainty? measurement / observation error.

TODO: intersect 2012 cells with 2013 regions. Text in suppl methods.
      test for any spp in 2012 and not 2013.
      check how many spp with category   & country but not IUCN or AM shp
              "               popn_trend & country       "

              

spatial data
http://www.iucnredlist.org/technical-documents/spatial-data
found the following marine species assessed (including DD data deficient):
	birds: 838
	coral: 746
	fish: 1001
	mammals: 131
	mangroves: 66
	reptiles: 79
	seacucumbers: 366
	seagrass: 72

pts to shp ID
FID_rgn_fa
ORIG-FID 

IUCN suggestions for revision:
 * match API with WWW
   - habitats more specific at WWW
 * include the few missing subpopulations in full list
 * post non-corrupt coral shapefile


SPP Population Trend
Unknown

  * SEA ICE
  * ICO - 
  * carry fwd from 2012? YES, ck seasnakes.

bbest@neptune:/var/data/ohi/model/GL-NCEAS-SpeciesDiversity_v2013a/raw/IUCN_2013.1_SpatialDataDownload/All_CORAL_Oct2012$ ogr2ogr -f "ESRI Shapefile" -skipfailures VIEW_CORAL_ogr2ogr_skipfailures.shp VIEW_CORAL.shp VIEW_CORAL
ERROR 1: Corrupted .shp file : shape 0, nPoints=165035, nParts=3658, nEntitySize=2645372.

ERROR 1: Error in fread() reading object of size 5778756 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2709100 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2643660 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2627384 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2500660 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 5194352 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 4302460 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 5496724 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 4173636 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 3410596 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 3311636 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 385416 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 271016 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2708180 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 3877176 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 4766804 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 232504 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 4370908 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 15720 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 101628 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 9680 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 304516 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 41268 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 343616 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 3170948 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2451528 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 747548 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 5167020 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 3045484 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2892620 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 121836 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2878308 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2874644 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 4893184 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 3696896 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 593816 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2699604 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 5793904 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 79780 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 339916 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2866864 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 295800 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 91336 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 394544 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 53528 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 240856 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2692232 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2578928 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2605256 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2776148 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2739836 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2240972 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2441320 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2380212 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2112348 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 1708568 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2003264 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2077556 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2125708 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 1746936 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 1919896 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 1714512 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 423060 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 580968 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 1814744 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 364728 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2325416 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 931844 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 746764 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 118688 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 814512 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 728944 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 202312 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 425008 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 5648352 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 4342028 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 3231236 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 4510496 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 5209904 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2787784 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 3973220 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 4280380 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 3320660 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 3965496 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2017944 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 29176 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 830300 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2795912 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2306620 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2203456 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 1709360 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2397088 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 1909736 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2137136 at offset 2147628668 from .shp file
ERROR 1: Error in fread() reading object of size 2655244 at offset 2147628668 from .shp file