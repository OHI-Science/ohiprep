Generating B_bmsy data and FIS status/trend for Antarctica
=====================

R files
=====================
* Ant_FIS_catch datamaker.R and Ant_FIS_dataprep.R prepare data for b/bmsy and mean catch calculations using the raw catch data (need to ask Katie what the difference is).  Outputs tmp/CCAMLR_ct_rgn_Aug29_2014.csv
* FIS_meanCatch datamaker.R: prepares the data for the 3 cmsy taxa and calcualtes the mean catch using the "tmp/CCAMLR_ct_rgn_Aug29_2014.csv" data.  Outputs: data/fnk_fis_ccmsy.csv and data/cnk_fis_meancatch.csv (which are read by the toolbox)
* FIS_cmsy_calc.R: further prepares catch data for cmsy and runs cmsy functions.
* FIS_draft CMSY datamaker.R: This is one of Katie's files.  I'm not sure what this does.


Relevant data files
============================
 


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


