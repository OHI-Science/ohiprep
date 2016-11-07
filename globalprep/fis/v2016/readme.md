## Ocean Health Index 2016: Fisheries Sub-goal

See full details for how the SAUP catch data was prepped [here](https://cdn.rawgit.com/OHI-Science/ohiprep/master/globalprep/fis/v2016/catch_data_prep.html).

See full details for how BBmsy was calculated [here](https://cdn.rawgit.com/OHI-Science/ohiprep/master/globalprep/fis/v2016/calculate_bbmsy.html).

If using these data, please see our [citation policy](http://ohi-science.org/citation-policy/).



### Additional information
A description of some of the files so far:

* saup_raster_data_download.R: Goal was to use the seaaroundus R package to download the SAUP raster cell data.  However, this will take too long. Abandoned, but keeping script as archive.

* catch_data_prep.Rmd: Preps the spatialized catch data (at half degree cells) for use in goal weighting and stock status calculations. Outputs:
  
   - 'git-annex/globalprep/fis/v2016/int/spatial_catch_saup.csv'
   - 'globalprep/fis/v2016/int/spatial_catch_pre_bbmsy.csv'

* catch_data_prep_old.Rmd: Uses the seaaroundus R package to download the catch data for each SAUP region. This was used as a first round calculation but has been updated in catch_data_prep.Rmd. Outputs:

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
   
* meanCatch.R: calculates the mean catch data that is used to weight the B/Bmsy values.  It also generates a datafile describing total catch for each region and year to use as the weight in the FP goal calculation.

  - data/FP_fis_catch.csv: weighting for MAR vs. FIS to calculate FP goal
  - data/mean_catch.csv: used in the toolbox to weight the b/bmsy values for each region
  - data/mean_catch_hs.csv: catch dat for the high seas regions
  
* Stock_resilience.R: Used to calculate resilience of stocks based on mora scores of regions where they are fished.  This is used to determine whether we should use a constrained or uniform prior in the cmsy model used to calculate B/Bmsy values.  Based on analyses, it never makes sense to use anything except the constrained prior.  So this is probably irrelevant moving forward.
 
   - stock_resil_06cutoff_2016.csv
   
* calculate_bbmsy.Rmd: Calculates B/Bmsy estimates for all stocks using 3 catch only models   
   
* format_bbmsy_data.R: Used to explore and combine various B/Bmsy scores into a single score.  This also merges in the RAM B/Bmsy scores when they are available. Also gets the B/Bmsy values to the correct spatial scale.
 
    - fis_bbmsy.csv: Final B/Bmsy scores used for ohi-global calculations
    - fis_bbmsy_gf.csv: Gapfilling done on RAM data when scores didn't go to 2010
    
   
   
   




  
