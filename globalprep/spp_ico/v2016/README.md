## Ocean Health Index: Species (Biodiversity) and Iconic Species (Sense of Place)

See full data prep details: SPP v2016 [here](https://rawgit.com/OHI-Science/ohiprep/master/globalprep/spp_ico/v2016/spp_data_prep.html).

See full data prep details: ICO v2016 [here](https://rawgit.com/OHI-Science/ohiprep/master/globalprep/spp_ico/v2016/ico_data_prep.html).

If using these data, please see our [citation policy](http://ohi-science.org/citation-policy).


### Additional information

Structure of R Markdown files in this folder:

* `data_prep_spp.Rmd`: This is the master Rmd script for the Species subgoal.  It sources a number of other sub-scripts:
    * `../R/am_extract_2015.R`: processes AquaMaps data from August 2015 (d2015) from .sql into .csv.
    * `../R/ingest_iucn.R`: gathers data from IUCN API to create a list of all IUCN-listed marine species including extinction risk, population trend, subpopulation, and other relevant info.
    * `spp_fxn.R`:  supporting functions for data processing and calculations
    * `layer_prep_spp_global.R`: calls functions from `spp_fxn.R` with necessary parameters to calculate global status and trend scores
    * `layer_prep_spp_3nm.R`: similar for 3 nautical mile coastal zone
    * `layer_prep_spp_hs_aq.R`: similar for High Seas and Antarctic regions
    * `score_plot_spp.R`
