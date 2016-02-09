GL-NCEAS-Fisheries_2014a
====

## Other files
Global/FIS_Bbmsy
Global/NCEAS-Fisheries_2014a/tmp/CMSY data prep.R  = data used to prepare the B/Bmsy data

## Data
Some of the larger datasets are located on Neptune: model/GL-NCEAS-FIS_2014a/raw

1. model/GL-NCEAS-FIS_2014a/raw/cmsy.ohi.df_Jul292014.csv data was provided by Kristin Kleisner on 7/29/2014 to KL and MRF.

Notes: CMSY code does not support the use of resilience as an r prior, so the default prior is used. This is how the code was run before, but just wanted you guys to be aware when we are writing up methods.

Also, we never actually used the geometric mean. It was always the arithmetic mean. I double checked this in the code. Not sure where that confusion came in, but in the WG Coilin had argued for the arithmetic mean over the geometric mean, so that's what we went with. I did modify the code for Hex to output the geometric mean and median so you guys could explore those options. You'll see that in the output as extra columns. I did not output the upper and lower CI bounds at this point and for the WG we are still using the arithmetic mean. One of Andy's concerns was that the B/Bmsy's were a bit 'pessimistic'...so using the geometric mean would likely result in lower B/Bmsys, so probably arithmetic mean is better?

## Scripts

1. **cnk_fis_meancatch_datamaker.R** 

Prepares files: cnk_fis_meancatch.csv and fnk_fis_b_bmsy_lyr.csv for Toolbox    


## History
Based heavily on scripts located here: N:\model\GL-NCEAS_FIS_v2013a\RevisingFIS

However, the catch and b_bmsy data has been updated due to some errors that were discovered.
