## Ocean Health Index: Blast and poison fishing for natural products goal (risk variable) and two pressures

This folder describes the methods used to prepare blast and poison fishing data. These data are used in the natural products goal (as a risk variable) and for artisanal high bycatch and hard bottom habitat destruction pressures.

More information about the natural products goal is available [here](http://ohi-science.org/goals/#natural-products).

The folders in this file include the metadata, R scripts, and data for each assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!


##### The information in v2013 was moved from: ohiprep/Global/WRI-ReefsAtRisk_v2013

Originally, this script only focused on preparing data for the natural products goal. But, it now houses the pressure data as well (habitat destruction subtidal hard bottom and food provision artisanal high bycatch)

##### The raw data files are now on Mazu: git-annex/globalprep/_raw_data  (these reference the d2013 data)



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
