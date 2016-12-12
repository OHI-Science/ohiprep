## Ocean Health Index: Rocky reef data for the exposure component of the natural products goal

This folder includes data describing habitat extent of rocky reef habitat.  

These data are used to calculate the exposure component of the natural products goal.  More information about the natural products goal is available [here](http://ohi-science.org/goals/#natural-products).

The folders in this file include the metadata, R scripts, and data for each assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!


#### Additional files

SupplementaryHabitatFiles.R:
Uses the habitat data to create the following supplementary data files that are used to weight the resilience and pressure components:

- pressures CP: cp_habitat_extent_rank
- pressures CS: cs_habitat_extent
- pressures HAB: hab_pressence

These files do not typically need to be updated because extent does not change with assessment (unless improved data are found).


SeparatingHabitatData.R is included for reference only (this should never need to be run again).  This separated the single habitat file from the <2015 analysis into the individual habitat layers so they can more easily be updated.

gap_filling.R: This generates the gap-filling data for the habitat: health, extent, trend data