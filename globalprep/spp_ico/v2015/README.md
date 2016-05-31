
**Data Sources for 2015 assessment:**
* Master list of IUCN species:
  * http://api.iucnredlist.org/index/all.csv
* IUCN range map shape files:
  * http://www.iucnredlist.org/technical-documents/spatial-data
  * Additional shape files from Liz, especially Corals
* IUCN Red List API pages for individual species
  * http://api.iucnredlist.org/details/XXX/0, where XXX is the IUCN species ID
  * info on habitat to filter down to only Marine species
  * population trend information (decreasing, stable, increasing)
  * info on country listings for ICO
* AquaMaps. 
  * @BBest: 
  > I've asked to join the AquaMaps app as part of the iMarine gCube EU portal, but pending approval. The app looks promising, but probably better to communicate directly Kristin Kaschner (Kristin.Kaschner@biologie.uni-freiburg.de) per their services page. Last time we got a MySQL dump of tables. Is there a web service available or do we need to repeat that process?
  > Please cite individual maps as: Computer Generated Native Distribution Map for Squalus acanthias (Picked dogfish). www.aquamaps.org, version of Aug. 2013. Web. Accessed 6 Aug. 2013.
  > You can cite AquaMaps as a whole as: Kaschner, K., J. Rius-Barile, K. Kesner-Reyes, C. Garilao, S.O. Kullander, T. Rees, and R. Froese. 2013. AquaMaps: Predicted range maps for aquatic species. World wide web electronic publication, www.aquamaps.org, Version 08/2013.

**Process for 2015 SPP assessment:**
* Determine list of species for analysis
  * Use IUCN master list to scrape IUCN API site.  These scraped .htm files are stored on Neptune in `git-annex/globalprep/SpeciesDiversity/cache/iucn_details` and contain info on habitat, population trend, and country presence.
  * Filter IUCN master list to only Marine species.
  * Identify which species have IUCN range maps available.  Spatial data from IUCN range maps is preferred over Aquamaps.
    * Pull scientific names and (where applicable) IUCN species ID numbers from the IUCN range map files (.dbf attribute tables).
  * Identify which species have Aquamaps range maps available.  These are used if a species does not have an IUCN range map.
* Spatial Analysis
  * Create half-degree cell grid based upon Aquamaps cells.  This will be the basis for spatial analysis.
  * Overlay regions shapefile using raster::extract(); create lookup table of AM cells to regions, including proportion of each cell included within a region.  Includes also the raw area (in km^2) of each cell.
  * For species with IUCN shape files, raster::extract() the proportional presence of each species within each half-degree cell.
  * For species with Aquamaps range files, use a 40% probability as cutoff to indicate presence vs absence within a given cell.
* Region status and trend calculations
  * For each half-degree cell, determine an average extinction risk and population trend across all present species.
    * note that this is done separately for IUCN and AM species; and then determine a species-count-weighted average between the two.
  * For each region, determine an area-weighted average category and population trend across all cells.
  * Status is determined from (mean category - .25)/.75, so that a status of zero corresponds to extinction of 75% of all present species
**Suggested Improvements for SPP 2016:**
* Incorporate subpopulation-level category and trend values.  Subpopulations are identified on IUCN master list with a unique IUCN species ID number, but these were excluded from 2015 assessment; only parent values were used due to spatial data limitations.
  * As of 2015, Aquamaps range maps do not include differentiation among subpopulations identified in IUCN site
  * As of 2015, IUCN subpopulations do not typically have spatially-differentiated range maps.  For example, many marine mammals have identified subpopulations, but in the MAMMMARINE.shp file, no IUCN species ID numbers are used to differentiate the range of these subpopulations.
  * Press IUCN to update range maps for subpopulations with subpop ID numbers.
* Incorporate ingest_aquamaps functionality into R rather than in SQL.

**Process for 2015 ICO assessment:**
* Use list of iconic species from 2011 ICO spreadsheet.  Species identified from WWF priority and flagship species, with included species from country-level iconic lists.
* For each species on iconic list, including parent/subpopulations, identify the list of countries in which the species is known to be present.
  * country lists are available in the data scraped for habitat and trend for SPP.
  * also note "possibly extinct" and "regionally extinct" countries from IUCN scraped data.
* For each region, determine average extinction risk category and population trend (note: this is done in the toolbox, function.R, rather than here in data_prep_ICO.R)
  * For species with multiple listings (i.e. parent/subpop listings), find mean category/trend across all populations prior to calculating mean across region.
  * Status is not rescaled to the same (75% loss = 0 status) criterion as SPP.
**Suggested Improvements for ICO 2016:**
* Update ICO species list based on more recent WWF listings and additional country-specific lists.
* Examine ways to incorporate more spatially-explicit data sources (IUCN range maps, AquaMaps range maps)
  * May not provide better inclusion of species, but may provide finer-scale understanding of species presence/absence within a region.
  