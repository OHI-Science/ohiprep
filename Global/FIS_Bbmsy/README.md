Generating B_bmsy data and FIS status/trend for Antarctica
=====================

R files
=====================
* cmsy_ohi.R: prepares catch data for cmsy and runs cmsy functions. Outputs b_bmsy_AQ_with_zeros.csv. (NOTE: if this is run again, changes need to be made to this file - see notes in R script)

* FIS_dataPrep.R: Prepares data for FIS function to calculate status and trend
- subsets b/bmsy data (b_bmsy_AQ_with_zeros.csv)
- adds additional stock with catch over catchmsy (raw/Ant_C_Cmsy .csv)
- saves b/bmsy data as: tmp\\fnk_fis_ccmsy.csv
- prepares catch data
- adds zero catch data (after first recorded catch)
- calculates mean catch, which is used to weight b/bmsy scores
- save catch data as: tmp\\cnk_fis_meancatch.csv


Notes for preparing data for CMSY analysis
===========================================
Data should look like this for CMSY scripts:

stock_id            res           ct   yr
Ablennes hians_51   NA            27.05 1985
Ablennes hians_51   NA            38.18 1990
Ablennes hians_51   NA            54.10 1991
...

Analysis is done at the FAO scale (id following species name)

1.  select species (taxonkey>=600000)
2. create unique stock_id by pasting taxon name and fao region
3. sum catch for duplicated stock from the same FAO region/year (especially if data is reported at SAUP level)
4. limit to stock with 10 years of non-zero/NA reported catch
5. order by stock_id and year


