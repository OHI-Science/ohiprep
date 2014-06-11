# Israel Hamaarag-Regions_v2014a

## Recomendations

1. **Geographic Projections**. You should generally use geographic projection for the OHI Toolbox. ArcGIS will use geodesic methods to accurately calculate distance or area (and similar methods are available in R). Note that these data files have different projections.

1. **Intersected Regions**. I recommend the EEZ and Basins should be intersected to define the spatial analytic units (ie regions) of the OHI calculation.
  1. Include or Exclude EEZ without Basin?


## Figure
Here's the first attempt at this.

![tmp/baltic_regions.mxd](fig/baltic_regions.png)

## Input Files

Directory on neptune\data_edit:

```
 git-annex\Baltic\StockholmUniversity-Regions_v2014-04\raw\
```

* BASINS_Clip.shp

  ```
Projected Coordinate System:	ETRS_1989_LAEA
Projection:	Lambert_Azimuthal_Equal_Area
False_Easting:	4321000.00000000
False_Northing:	3210000.00000000
Central_Meridian:	10.00000000
Latitude_Of_Origin:	52.00000000
Linear Unit: 	Meter
Geographic Coordinate System:	GCS_ETRS_1989
Datum: 	D_ETRS_1989
Prime Meridian: 	Greenwich
Angular Unit: 	Degree
```

* BalticEconomicZones.shp

  ```
Geographic Coordinate System:	GCS_WGS_1984
Datum: 	D_WGS_1984
Prime Meridian: 	Greenwich
Angular Unit: 	Degree
```