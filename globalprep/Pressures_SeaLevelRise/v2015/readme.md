##Sea Level Rise Pressure Layer for OHI 2015 

See documented methods [here](https://cdn.rawgit.com/OHI-Science/ohiprep/master/globalprep/Pressures_SeaLevelRise/v2015/slr_create_layer.html).

***

### Methods Overview

The sea level rise data was prepped for OHI 2015 as follows:

- All negative values, indicating decreases in sea level, were set to zero  
- Data was resampled from the native cell resolution (0.25 degrees) to ~ 1km
- The reference point was set as the 99.99th quantile of the data distribution to rescale all values from 0 to 1
- All NA cells were filled in through nearest neighbor interpolation

