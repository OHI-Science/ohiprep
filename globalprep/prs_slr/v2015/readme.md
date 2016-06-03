##Sea Level Rise Pressure Layer for OHI 2015 


If the methodology or data produced here is used in publication, please make attribution clear. Please see our [citation policy](http://ohi-science.org/citation-policy/).

See documented methods [here](https://cdn.rawgit.com/OHI-Science/ohiprep/master/globalprep/Pressures_SeaLevelRise/v2015/slr_create_layer.html).

***

#Data Source

**Reference**:  The altimeter products were produced and distributed by Aviso (http://www.aviso.altimetry.fr/), as part of the Ssalto ground processing segment. [AVISO](http://www.aviso.altimetry.fr/en/data/products/ocean-indicators-products/mean-sea-level/products-images.html)

**Downloaded**: January 12, 2015  

**Description**:  Annual rate of sea level rise in mm

**Native data resolution**: 0.25 degree grid cells

**Time range**: January 1993 - June 2014  

**Format**:  NetCDF

***

### Methods Overview

The sea level rise data was prepped for OHI 2015 as follows:

- All negative values, indicating decreases in sea level, were set to zero  
- Data was resampled from the native cell resolution (0.25 degrees) to ~ 1km
- The reference point was set as the 99.99th quantile of the data distribution to rescale all values from 0 to 1
- All NA cells were filled in through nearest neighbor interpolation

