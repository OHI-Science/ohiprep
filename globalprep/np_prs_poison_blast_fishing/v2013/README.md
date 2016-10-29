Global WRI Reefs at Risk
========================================================

Average value for all impacted regions within 3 nm.

1.  **thr_blast**: Areas of observed blast (dynamite) fishing.  Compiled at WRI from various sources including ReefCheck, Tanzania Dynamite Fishing Monitoring Network, and local experts.

1.  **thr_poison**: Areas of observed poison fishing.  Compiled at WRI from various sources including ReefCheck and local experts.

We extracted two impact assessments of destructive fishing as rasters at 1km2 resolution, and then projected into Mollweide. We also reclassified their log-scale scoring system from:

1. 0 -> 1
1. 100 -> 2
1. 1000 -> 3

The pressures score for each layer is $mean - 1$ per region where $mean > 1$, otherwise the score is $NA$.

For the reef extent data, we aggregated using summation and a cell window of 9 to resize the 500m raster into 4500m cell sizes. We then projected
the data into Mollweide, and exported the data as 8-bit GeoTIFF with cell values from 1 to 81 (9x9 neighborhood) in units of $(500m)^2 = 0.25 {km}^2$.

- source: neptune_data:stable/GL-WRI-ReefsAtRisk/README.txt
- Citation: Burke et al. (2011)

## 2013
Extracted from 2013 regions.

## 2014
TODO...
