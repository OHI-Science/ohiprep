##Ocean Acidification Pressures Layer for OHI 2015

### Data

Raw data was provided by Woods Hole on December 16, 2014. This data is an update to the work done by [Feely et al. (2009)](http://www.tos.org/oceanography/archive/22-4_feely.pdf).  

**Resolution**: 1 degree  
**Values**: Surface &#937; aragonite saturation state  
  **Time Range**: 1880-1889 and 2005-2014 (monthly data was provided for each year)  
**Format**: NetCDF

### Scripts

1. `dataprep.Rmd` takes raw NetCDF data and creates annual mean rasters (.tif) for all years
2. `oa_create_layer_2015.Rmd` creates final pressures layer used in OHI 2015
3. `OA_interpolation.py` uses arcpy to interpolate the raster to fill in NA cells
4. `interpolated_cells.R` creates a single raster that shows all interpolated raster cells

`run_python_script.txt` explains how to run the python script (OA_interpolation.py)

**You will also see `oa_dataprep.R` and `create_global_oa_pressures_layer.R` in this folder. These two scripts are the same as `dataprep.Rmd` and `oa_create_layer_2015.Rmd` respectively. This redundancy is a result of OHI trying to move towards using .Rmd files instead of .R files.**

### Updates from previous assessment

Previous assessments did not have updated ocean acidification data after the 2012 global assessment. 

This dataset provides more detailed temporal changes and includes more recent years of data. In addition, a biological reference point was set rather than the maximum value. All oceanic cells with values of &#937; aragonite at or below 1 were assigned a value of 1. This threshold was chosen based on evidence from the literature that once undersaturation is reached (<=1), dissolution of CaCO3 occurs and negatively impacts biological processes including calcification.
  
  ***
  
### Methods Overview
  
  There are two parts to creating this layer:  
  (1) Data prep to get raw data into the correct format: see [dataprep.html](http://htmlpreview.github.io/?https://github.com/OHI-Science/ohiprep/blob/master/globalprep/Pressures_OceanAcidification/v2015/dataprep.html)  
  (2) Creating the pressure layer for OHI which includes averaging across the most recent 5 years and rescaling values from 0 to 1 [oa_create_layer_2015.html](http://htmlpreview.github.io/?https://github.com/OHI-Science/ohiprep/blob/master/globalprep/Pressures_OceanAcidification/v2015/oa_create_layer_2015.html)

For each year in the dataset provided, an average annual aragonite saturation state layer was created. Using these annual averages, a decadal average for both 1880-1889 and 2005-2014 was calculated.


The change in aragonite saturation state was calculated for each year from 2005-2014 by subtracting the historical global mean. 

![](./images/old_new_change.png)


Values at or below an aragonite saturation state of 1 were set equal to 1 in the final pressure layer (the highest possible value). This threshold was chosen based on expert consensus that once undersaturation is reached (<=1), dissolution of CaCO3 occurs and negatively impacts biological processes including calcification. All values greater than 1 are then rescaled according to the amount of change since the 1880s.

Here is the final layer for 2014.

![](./images/oa_final_2014.png)

For OHI 2015, data from the years 2011-2014 were averaged for the final pressure layer.




**References**  
Woods Hole Oceanographic Institution. 2014 update to data originally published in: Feely, R.A., S.C. Doney, and
S.R. Cooley. 2009. Ocean acidification: Present conditions and future changes in a high-CO2 world.
Oceanography 22(4):36–47
