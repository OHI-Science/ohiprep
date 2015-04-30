# LSP notes and notes on the WDPA database

The WDPA_MPA database files and additional tmp files are located in git-annex on Neptune.
* Jan2015 data are in globalprep: `git-annex/globalprep/WDPA_MPA/v2015`
* Apr2014 data are in Global: `git-annex/Global/WDPA_MPA_v2014`

##Files and Folders
* `model.py:` Imports polygons and filters to count 'Designated' protected areas, avoiding 'non-MPA Programmatic Management Plans' (see **Issues** below).  Creates raster based upon earliest protection date, and tallies protected areas within each 3nm offshore and 1km inland coastal region.
    * Uses pre-prepared rasters of 3nm offshore and 1 km inland regions to convert WDPA-MPA polygons to a raster. Note: cell size for WDPA-MPA raster is driven by region rasters, currently pointing to 500 m rasters
    * Extracts data for protected areas within the two coastal regions, outputs as tables (.dbf).
    * More information about the .py script is found in the `README_dataDescription.html` (in the `v2014_archive` folder)
* `digest.R:` Prepares the final data (using processed data within `v201x/tmp`) for the Toolbox.
* `v2014_archive`: files and folders specific to 2014 scenario, copied from ohiprep/global.
* `v2015`: files and folders specific to 2015 scenario.
* `v201x/tmp:` Contains processed data (.dbf outputs) from `model.py`.
* `v201x/data:` Contains final data (.csv outputs) from `digest.R` for use in the Toolbox.

##Future improvements to code 
* Migrate away from using ArcPy in `model.py` - use open-source spatial analysis functions.
* Convert as much of the Python code to R as is feasible.  
    * e.g. use Python to create the rasters, but migrate the tabulation over to R (in `data_prep.R` or `digest.R`).
* use `data_prep.R` as a top-level script, that calls `model.py` and `digest.R` (or incorporates them directly). Use `data_prep.R` to automate the changes in file location for next year's analysis.
    * e.g. if we run the analysis exactly the same way, all we need is to change one variable in `data_prep.R` to point to the v2016 database (on git-annex) and file locations (on Github)

##Issues on WDPA_MPA database for LSP
For more discussion of these issues, see issues #435, 387, and 188.
###Non-MPA Programmatic Management Plan areas in US
The `model.py` script imports a polygon layer denoting global protected areas according to the WDPA MPA database, published in January 2015.  This version of the database includes a number of very large protected areas, previously designated, that indicated fishery management areas and the like (Apr2014  WDPA_MPA database included some similar areas, though these were included in the assessment at that time).  After discussion, we decided that these "problem areas" do not fit the philosophy of a "lasting special place," and should not be included.
* Based on a paper by Ban et al, 2014(?) we looked at using IUCN protected area categories as a filtering mechanism.  Rather than weighting different categories, we elected to exclude any Category V and VI areas that are not specifically denoted as "no take" areas (`NO_TAKE == 'All'`).  While this method eliminated the "problem areas," it also eliminated other areas due to the broad definition of what is included in IUCN Category V, including Marine National Monuments and National Marine Sanctuaries, which we decided was not acceptable.
* Examining the `MANG_PLAN` field, it was clear that the "problem areas" were tagged as "non-MPA Programmatic Species Management Plan" (or Habitat or Fishery Management Plan).  Filtering to exclude polygons with this designation eliminated all the problem areas for this year.
* However, this use of the `MANG_PLAN` field does not comply with the WDPA_MPA standards -- it is meant to be used to provide a hyperlink or a bibliographic reference to a published plan.  The US seems to be the only country that uses (misuses) the field to code for 'MPA' and 'non-MPA' management plans; if the use or definition of this field changes in future data sets, this filtering will need to be re-examined.

###Changes in scores between 2014 and 2015
Between the Apr2014 WDPA_MPA database and the Jan2015 WDPA_MPA database, a few regions changed dramatically in scores, when examining the scores from 2013.

```
  region_id |       region_name       | old 2013 score  | new 2013 score
  --------- | ----------------------- | --------------- | ---------------
     91     | Crozet Islands          |     100         |      0
     92     | Amsterdam/St Paul Is.   |     100         |      0
     93     | Kerguelen Islands       |     100         |      0
    232     | Bosnia and Herz.        |      98         |     27
    108     | Bermuda                 |      97         |     43
    113     | Cayman Islands          |      80         |     35
     90     | Prince Edward Is.       |     100         |     55
     82     | Albania                 |      60         |     18
     32     | Reunion                 |      77         |     42
     79     | Israel                  |      36         |      8
     48     | Oman                    |      44         |     20
     18     | Fiji                    |      96         |     72
    187     | Croatia                 |      56         |     33
    163     | US                      |     100         |     79
```

* Due to the change in raster resolution, from 1 km to 500 m cells, many regions experienced a small decrease in score due to "edge effects" around protected area polygons.  The 1 km cells systematically overestimated protected area.  In some cases, particularly small countries with small coastal parks, this edge effect appears significant.
    * e.g. Israel, Bosnia, Bermuda, Reunion, Fiji, Cayman Islands
* In some cases, new protected areas overlapped/superceded older protected areas, and the older protected areas seem to have been removed from the database.  This may skew trend upward, as it will interpret this as creation of a new protected area rather than continuation of an existing protected area.
    * e.g. when examining the 2013 scenario using the Apr2014 data vs the Jan2015 data, Croatia showed a significant drop, despite showing a significant increase in protected area according to the Jan2015 database).  The new protected areas (status year 2013) overlapped previously-reported protected areas, and the polygons for the older protected areas were clipped accordingly.  
    * So back-tracking to determine trend, 2013 appears to create far more protected area than it should.
    * And for the 2013 scenario (which considers areas up through 2012), the score will appear to drop precipitously, as previously-reported protected areas (from Apr2014 data) no longer appear.
* In some cases, score changes are due to changes in reporting of protected areas between the Apr2014 and Jan 2015 databases.
    * e.g. for Albania and the French Southern Territories (Crozet, Amsterdam/St Paul, Kerguelen Islands), protected areas reported within Apr2014 database were no longer reported in the Jan2015 database.
    * e.g. for Oman, a park reported in the Apr2014 data was changed to a smaller size in the Jan2015 data.
    * e.g. for U.S., 2014 assessment included some of the fishery management zones that are now excluded.