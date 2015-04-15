The Sea Level Rise pressure layer was updated with new data for the year 2013. Previous data went through 2012.  


###Data
  
Data for mean sea level rise (in mm) between January 1993 and June 2014 was downloaded on 1/12/15 from [AVISO](http://www.aviso.altimetry.fr/en/data/products/ocean-indicators-products/mean-sea-level/products-images.html).


[!alt Image](https://github.com/OHI-Science/ohiprep/blob/master/globalprep/AVISO-SeaLevelRise_v2015/images/slr_mm_raster_93_14.png)


#### Data prep for OHI 2015

The original sea level rise data (plotted above) was prepped for OHI 2015 through the following steps:

- All negative values, indicating decreases in sea level, were set to zero  
- Data was then log transformed
- Data was resampled from the native c(0.25 degree) resolution to ~ 1km
- Data was rescaled by setting all values greater than or equal to the 99.99th quantile (6.24183381230408) to 1, and dividing the remaining values by the 99.99th quantile
- All NA cells were filled in through nearest neighbor interpolation


For full procedure and code see [slr_procedure.pdf](https://github.com/OHI-Science/ohiprep/blob/master/globalprep/AVISO-SeaLevelRise_v2015/slr_procedure.pdf)

### Final Layer

[!alt image](https://github.com/OHI-Science/ohiprep/blob/master/globalprep/AVISO-SeaLevelRise_v2015/images/slr_final.png)