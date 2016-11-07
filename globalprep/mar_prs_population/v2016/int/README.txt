Source: GL-CIESIN-CoastalPop_v2012

Population density was interpolated based on input years (2005, 2010, 2015) like so:
  
  popdens_2013 = 0.4 * popdens_2010 + 0.6 * popdens_2015

The original ~ 0.5 degree (0.041666667 degree) geographic rasters were in units of population density (# people per square kilometer). These rasters were projected to ~ 1 km (934.478877011219 m) Mollweide rasters, multiplied by the cell area and summed per region to arrive at total population inland of 25 miles.

see also
/Volumes/data_edit/model/GL-CIESIN-CoastalPop_v2012/README.md