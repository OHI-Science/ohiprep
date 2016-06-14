## Ocean Health Index 2016: Fisheries Sub-goal

See full data prep details [here](LINK TO HTML FILE).

If using these data, please see our [citation policy](http://ohi-science.org/citation-policy/).



### Additional information
A description of some of the files so far:

NOTE: now catch data is int/catch_saup.csv...make sure dataprep script is in here...

* saup_raster_data_download.R: Goal was to use the seaaroundus R package to download the SAUP raster cell data.  However, this will take too long. Abandoned, but keeping script as archive.

* getSAUPdata.R: Uses the seaaroundus R package to download the catch data for each SAUP region. Outputs:

   - 'git-annex/globalprep/fis/raw/SAUP_catch_taxon_tons_eezs.csv'
   - 'git-annex/globalprep/fis/raw/SAUP_catch_taxon_tons_highseas.csv'

* saup_rasters_to_ohi_rgns: Assigns the SAUP raster cells to OHI regions and FAO regions.  This will be useful when we get the catch data at the raster scale. Outputs:

   - git-annex/globalprep/fis/v2015/raw/saup_rasters_to_ohi_rgns_noLand.csv (ignore, use combined below)
   - git-annex/globalprep/fis/v2015/raw/saup_rasters_to_fao_rgns.csv (ignore, use combined below)
   - combines ohi and fao data: git-annex/globalprep/fis/v2015/raw/saup_rasters_to_ohi_fao_rgns_noLand.csv
   
* saup_rgns_FAO.R: assigns the saup regions to FAO regions. This file is used to aggregate the catch to FAO region, which is the scale that B/Bmsy is calculated. Outputs:

   - globalprep/fis/v2016/int/saup_rgn_to_fao_rough_draft.csv (this file was hand-edited to eliminate slivers)
   - globalprep/fis/v2016/int/saup_rgn_to_fao.csv (final draft with slivers < 1% of a saup region falling in an fao region)
   
   
* saup_to_ohi_rgn.R: assigns saup regions to ohi regions. This file is used to aggregate/split catch from the saup spatial scale to the ohi spatial scale.  We are hoping to eventually have raster data.  If we get that, this file will be obsolete.  Outputs:

   - globalprep/fis/v2016/int/saup_to_ohi_rough_draft.csv
   - globalprep/fis/v2016/int/saup_to_ohi_key.csv (this is mostly a hand correction of the above data)
   
   




  
