## Ocean Health Index 2017: Fisheries Sub-goal

See full details for how the SAUP catch data was prepped [here](https://cdn.rawgit.com/OHI-Science/ohiprep/master/globalprep/fis/v2017/catch_data_prep.html).

See full details for how BBmsy was calculated [here](https://cdn.rawgit.com/OHI-Science/ohiprep/master/globalprep/fis/v2017/calculate_bbmsy.html).

If using these data, please see our [citation policy](http://ohi-science.org/citation-policy/).



### Additional information
A description of some of the files so far:

* `clean_cells.R` cleans up the half-degree cell data, removing overlaps between land and oceanic regions, and calculats the total proportion of each cell within each OHI region. The output of this script is `cells.csv`

* catch_data_prep.Rmd: Preps the spatialized catch data (at half degree cells) for use in goal weighting and stock status calculations. Outputs:
  
   - `git-annex/globalprep/fis/v2017/int/stock_catch_by_rgn.csv`
   - `data/stock_catch.csv`

* calculate_bbmsy.Rmd: Calculates B/Bmsy estimates for all stocks using catch-MSY. Outputs:
  
  - `data/cmsy_bbmsy.csv`
    
   
   
   




  
