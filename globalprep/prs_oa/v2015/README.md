##Ocean Acidification Pressure Layer for OHI 2015

NOTE (June 2 2016, MRF): These data are obsolete after the 2015 assessment due to changes in methods.  These are saved for archival purposes.

If the methodology or data produced here is used in publication, please make attribution clear. Please see our [citation policy](http://ohi-science.org/citation-policy/).

See full data prep details [here](http://htmlpreview.github.io/?https://github.com/OHI-Science/ohiprep/blob/master/globalprep/prs_oa/v2015/dataprep.html) and creation of the final pressures layer of OHI 2015 [here](http://htmlpreview.github.io/?https://github.com/OHI-Science/ohiprep/blob/master/globalprep/prs_oa/v2015/oa_create_layer_2015.html).



### Scripts

1. `dataprep.Rmd` takes raw NetCDF data and creates annual mean rasters (.tif) for all years
2. `oa_create_layer_2015.Rmd` creates final pressures layer used in OHI 2015
3. `OA_interpolation.py` uses arcpy to interpolate the raster to fill in NA cells
4. `interpolated_cells.R` creates a single raster that shows all interpolated raster cells

`run_python_script.txt` explains how to run the python script (OA_interpolation.py)

**You will also see `oa_dataprep.R` and `create_global_oa_pressures_layer.R` in this folder. These two scripts are the same as `dataprep.Rmd` and `oa_create_layer_2015.Rmd` respectively. This redundancy is a result of OHI trying to move towards using .Rmd files instead of .R files.**

