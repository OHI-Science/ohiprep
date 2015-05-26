# README

Raw raw data accessed by @jafflerbach May 22, 2015 with FishStatJ. Global dataset for 1950-2013 downloaded first from [this site](http://www.fao.org/fishery/statistics/software/fishstatj/en#2) then imported into FishStatJ before exporting as .csv.

NOTE: the original file downloaded has duplicate columns: `Species (ASFIS species)` is for both scientific name and for common name. This has been removed using `resave_fao_captureproduction.r` and the data file to be used is called `FAO_captureproduction_1950_2013.csv`. 
