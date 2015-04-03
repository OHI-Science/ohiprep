# LSP notes and notes on the WDPA database

The original data and additional tmp files are located on Neptune:
`N:\git-annex\globalprep\WDPA_MPA\v2015`

##Files and Folders
* `model.py:` Imports polygons and filters to count 'Designated' protected areas, avoiding 'non-MPA Programmatic Management Plans' (see **Issues** below).  Creates raster based upon earliest protection date, and tallies protected areas within each 3nm offshore and 1km inland coastal region.
    * Uses pre-prepared rasters of 3nm offshore and 1 km inland regions to convert WDPA-MPA polygons to a raster. Note: cell size for WDPA-MPA raster is driven by region rasters, currently pointing to 500 m rasters
    * Extracts data for protected areas within the two coastal regions, outputs as tables (.dbf).
    * More information about the .py script is found in the `README_dataDescription.html` (in the `v2014_archive` folder)
* `digest.R:` Prepares the final data (using processed data within .\tmp) for the Toolbox.
* `.\tmp:` Contains processed data (.dbf outputs) from model.py.  
    * `.\tmp\new_rasters_500mcell` contains the outputs from a preliminary analysis based upon 500 m raster cells, with old (outdated) filtering method. Going forward, LSP analyses are now based on 500m cells, so this folder should probably be deleted.
    * `.\tmp\iucn_dbfs` contains .dbf outputs from `model_iucn.py`.
    * `.\tmp\mang_plan_dbfs` contains .dbf outputs based upon current `model.py` which cuts out `MANG_PLAN == 'non-MPA Programmatic Management Plan'` areas.  This is the 'latest and greatest' model for v2015, so these data should be moved to `.\tmp\`
* `.\data:` Contains final data (.csv outputs) from `digest.R` for use in the Toolbox.

##Exploratory Files and Folders 
* `model_iucn.py:` An early attempt to resolve an issue with the 2015 WDPA database.  See **Issues** below.
* `digest_iucn.R:` reworked to process IUCN-filtered data.
* `scenario_compare.R:` Script to compare outputs of various filtering schemes.
* `.\DataExplore:` Additional analysis to compare and verify results of basing analysis on finer-resolution rasters (500 m vs 1000 m).  
    * `explore.R` performs analyses, and reports outputs of these analyses in the other files within this directory.

##Issues on WDPA_MPA database for LSP
The original `model.py` script imports a polygon layer denoting global protected areas according to the WDPA MPA database, published in January 2015.  This version of the database includes a number of very large protected areas, previously designated, that indicated fishery management areas and the like.  After discussion, we decided that these "problem areas" do not fit the philosophy of a "lasting special place," and should not be included.

Based on a paper by Ban et al, 2014(?) we looked at using IUCN protected area categories as a filtering mechanism.  Rather than weighting different categories, we elected to exclude any Category V and VI areas that are not specifically denoted as "no take" areas (`NO_TAKE == 'All'`).  While this method eliminated the "problem areas," it also eliminated other areas due to the broad definition of what is included in IUCN Category V, including Marine National Monuments and National Marine Sanctuaries, which we decided was not acceptable.  This model is coded in `model_iucn.py`

Examining the `MANG_PLAN` field, it was clear that the "problem areas" were tagged as "non-MPA Programmatic Species Management Plan" (or Habitat or Fishery Management Plan).  Filtering to exclude polygons with this designation eliminated all the problem areas for this year.

However, this use of the `MANG_PLAN` field does not comply with the WDPA_MPA standards -- it is meant to be used to provide a hyperlink or a bibliographic reference to a published plan.  The US seems to be the only country that uses the field to code for 'MPA' and 'non-MPA' management plans; if the use or definition of this field changes in future data sets, this filtering may need to be re-examined.
