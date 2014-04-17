NCEAS-Livelihoods_v2014
=====================


Files produced
==============
FILES: 

* cntry_jobs_2014a.csv

\*\* Note: new FAO and WTTC data are processed as rgn_ids (rgn_fao_jobs_fismar_v2012.csv; rgn_wttc_empt_2014a.csv), joined with cntry_key using the lookuptable src/LookupTables/cntry_rgn_2013.csv, and inserted into the original data file liv_jobs.csv used in 2013a. This last step of changing to cntry_key can easily be excluded and the file could be saved as rgn_jobs_2014a.csv. 


Description
===========

update_liv.r:: 

adds new jobs data from FAO (aq = mariculture, cf = commercial fishing) and WTTC (tour = direct contribution to tourism employees) to the 2013a file (liv_jobs.csv). Removes duplicates. 


