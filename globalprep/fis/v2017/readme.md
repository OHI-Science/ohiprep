## Ocean Health Index 2017: Fisheries Sub-goal

See full details for how the SAUP catch data was prepped [here](https://cdn.rawgit.com/OHI-Science/ohiprep/master/globalprep/fis/v2017/catch_data_prep.html).

See full details for how BBmsy was calculated [here](https://cdn.rawgit.com/OHI-Science/ohiprep/master/globalprep/fis/v2017/calculate_bbmsy.html).

If using these data, please see our [citation policy](http://ohi-science.org/citation-policy/).



### Additional information
A description of files:

* `clean_cells.R` cleans up the half-degree cell data, removing overlaps between land and oceanic regions, and calculates the total proportion of each cell within each OHI region. The output of this script is `cells.csv`

* catch_data_prep.Rmd: Preps the spatialized catch data (at half degree cells) for use in goal weighting and stock status calculations. Outputs:
  
   - `git-annex/globalprep/fis/v2017/int/stock_catch_by_rgn.csv`
   - `data/stock_catch.csv`
   - `data/mean_catch.csv`
   - 'data/FP_fis_catch.csv'

* calculate_bbmsy.Rmd: Calculates B/Bmsy estimates for all stocks using catch-MSY. Outputs:
  
  - `data/cmsy_bbmsy.csv`
    
   
* RAM_data_prep.Rmd: Prepares the RAM B/Bmsy data by gapfilling RAM data and identifying which FAO/OHI regions each RAM stock is present. Outputs:

  - `int/ram_stock_bmsy_gf.csv`
  - `int/RAM_fao_ohi_rgns.csv`
  - `int/ram_bmsy.csv`
   

* RAM_CMSY_combine.Rmd: Combines the B/Bmsy values from the RAM and CMSY data, with preference given to RAM data.
 
   - `int/cmsy_b_bmsy_mean5yrs.csv`
   - `data/fis_bbmsy_gf.csv`
   - `data/fis_bbmsy.csv`


  
