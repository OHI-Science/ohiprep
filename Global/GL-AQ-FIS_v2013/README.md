Generating B_bmsy data and FIS status/trend for Antarctica
=====================

R files
=====================
* cmsy_ohi.R: prepares catch data for cmsy and runs cmsy functions. Outputs b_bmsy_AQ_with_zeros.csv. (NOTE: if this is run again, changes need to be made to this file - see notes in R script)

* FIS_dataPrep.R: Prepares data for FIS function to calculate status and trend
- subsets b/bmsy data (b_bmsy_AQ_with_zeros.csv)
- formats additional stock with catch over catchmsy (raw/Ant_C_Cmsy .csv)
- saves b/bmsy data as: tmp\\fnk_fis_ccmsy.csv
- prepares catch data
- adds zero catch data (after first recorded catch)
- calculates mean catch, which is used to weight b/bmsy scores
- save catch data as: tmp\\cnk_fis_meancatch.csv

* FIS_status_trend.R = 
- converts c/cmsy and b/bmsy to scores
- calculates status and trend using scores and mean catch to weight scores.

Katie Notes
===========================================
Antarctica analysis:
Dropbox/FIS_calculations/Data_toUse/Antarctica/CCAMLR_t_LH.csv
and 
Dropbox/FIS_calculations/Data_toUse/Antarctica/CCAMLR_w.csv

The first one, “CCAMLR_t_LH.csv”, I had already sent last week and is intended for the CMSY calculations. I removed all taxa that were irrelevant to food provision, but that still leaves quite a few! If it’s taking too long in terms of computational time, we might need to reduce the number of taxa we calculate a B/Bmsy value for.
Please refer to this version, even if you had downloaded the one I sent last week (in this latest version however I removed the taxa for which we have an assessment (4 of them: 2 tooth fish, 1 krill, 1 mackerel icefish)).
Notice that this is only for taxa reported at the species level and that each species has a unique time-series, as opposed to separate time-series per CCAMLR region. So the resulting B/Bmsy values will be applied to all CCAMLR regions.
The second file is to calculate the weights to be used in the geometric mean for the status score, i.e. the contribution of each taxon to the overall score. This version includes all taxonomic levels, so that a separate weight is applied for every taxon_region combination. Remember that to obtain the weights you need to calculate the mean catch of each taxon in each CCAMLR area, across all years.



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


