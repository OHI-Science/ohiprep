These changes in the spatial data correct an issue that raster::rasterize has dealing with polygon holes.

Instead of using the raster package we used fasterize to create the raster which has fewer issues according to our tests.

I also cleaned the polygon file so it is more useful.

Code is updated to use the sf (simple feature) package for dealing with spatial data.

regions_2017_update: all the land/eez/fao/antarctica regions with improved labeling and corrected topology issues (intersections and holes outside polygons, etc.).  This is the file that should be used moving forward. (Derived from this file: Mazu: git-annex\Global\NCEAS-Regions_v2014\data\sp_mol)

The following raster files were made with the regions_2017_update shapefile:

regions_eez_with_fao_ant.tif: This includes all the ocean regions (eez/fao/antarctica), but the raster cell values correspond to the rgn_ant_id in regions_2017_update.  This file is most often used to extract pressure values for each region.

regions_land_ocean.tif: This includes the land and ocean regions, with the raster cell values corresponding to the rgn_ant_id in regions_2017_update.  This file isn't frequently used.  But it has been used to extract region populations.  

regionData.csv: Uses same variable names as in the regions_2017_update.shp data

Other data files were moved from "ohiprep/src/LookupTables": 
georegion_labels.csv
georegions.csv
rgn_eez_v2013a_synonyms.csv
rgn_uninhabited_islands.csv
