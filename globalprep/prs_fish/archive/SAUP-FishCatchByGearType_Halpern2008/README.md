# Sea Around Us Project - Fisheries Catch Rate by Gear Type

These rasters are copies of the original fisheries rasters from Halpern et al (2008) in the format:

    [product]_[gear type | area]_[projection]

Files are to be found at:

    \\neptune.nceas.ucsb.edu\data_edit\git-annex\Global\SAUP-FishCatchByGearType_Halpern2008\data

## Product

1. **catch**: average catch rate (tons per km2 per year) by gear type for all reported fish species caught 1999-2003. 360 rows and 720 columns.
1. **fishprod**: catch divided by average productivity (g Carbon/m2/yr) using Vertically Generalized Production Model (VGPM). 1024 rows and 2048 columns.

An area raster (in km2) is given for each product.

## Gear Types

id | code      | label
--:|:---------:|:-----
1  | pel_lb    | Pelagic Low-Bycatch Fishing
2  | pel_hb    | Pelagic High-Bycatch Fishing
3  | dem_d     | Demersal Destructive Fishing
4  | dem_nd_lb | Demersal Non-Destructive, Low-Bycatch Fishing
5  | dem_nd_hb | Demersal Non-Destructive, High-Bycatch Fishing

## Projection

Both sets of products are in geographic coordinate system (gcs), but with different dimensions.

## Other

For more details, see the supplement of Halpern et al (2008) and inspect paths of R script.

