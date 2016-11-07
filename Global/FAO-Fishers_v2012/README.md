FAO-Fishers_v2012
=====================


Files produced
==============

* rgn_fao_jobs_fismar_v2012.csv

Further processed using Global/NCEAS-Livelihoods_v2014/update_jobs.r and saved as: cntry_jobs_2014a.csv


Description
===========
ACCESS: 
Katie Longo emailed someone at FAO to get these data. 

PREPROCESSING NOTE:
Employment_for_Longo_trimCols.csv was saved from the original file (Employment_for_Longo.xls) with a small modification:
copy the desired columns into a new xls window; save as a .csv. There were horrendous comma-separated blanks in the original file.


R scripts involved
==================
* data_prep.r

job_status information:
----------------------

From Nature 2012 SOM: Employment is disaggregated into full-time, part- time, occasional, and unspecified statuses. These categories are defined as full time workers having > 90% of their time or livelihood from fishing/aquaculture, part time workers are between 30-90% time (or 30-90% of their livelihood) and occasional workers are < 30% time. Unspecified status workers could fall anywhere from 0-100% time. Taking the midpoints of those ranges, we assume that 1 part time worker = 0.6 full time workers, 1 occasional worker = 0.15 full time workers, and 1 unspecified worker = 0.5 full time workers, which we used as a weighting scheme for determining total numbers of jobs.


Methods 2014
============
Data come from the United Nations Food and Agriculture Organization (FAO) Fisheries and Aquaculture Department which provides a Global Number of Fishers (GNF) data set: http://www.fao.org/fishery/statistics/global-fishers/en. At our request, we received updated data through 2012. The dataset contains yearly total numbers of employees in commercial fishing, subsistence fishing, and aquaculture from 2005- 2012 in 143 OHI regions.

The dataset includes the following occupational categories: Aquaculture, Inland Waters Fishing, Marine Waters Fishing, Subsistence, Unspecified. We omitted jobs with an Unspecified category to avoid
overestimating employment for marine fishing or aquaculture. We omitted jobs in the Subsistence
category since subsistence opportunities are captured within the Artisanal Fishing Opportunity Goal of the OHI. Data were used for Aquaculature and Commercial fishing separately. 

For commercial fishing and aquaculture, we eliminated inland waters fishing for each country in each year.

Employment is disaggregated into Full time, Part time, Occasional, and Status Unspecified. These categories are defined as full time workers having > 90% of their time or livelihood from fishing/aquaculture, part time workers are between 30-90% time (or 30-90% of their livelihood) and occasional workers are < 30% time. Unspecified status workers could fall anywhere from 0-100% time. Taking the midpoints of those ranges, we assume that:

* a part time worker = 0.6 full time workers 
* an occasional worker = 0.15 full time workers
* an unspecified worker = 0.5 full time workers, which we used as a weighting scheme for determining total numbers of jobs.

It is important to note that while these data came from FAO sources, they are not considered official FAO statistics because they have not undergone official validation and consistency checks from FAO. The data also contain significant gaps, but they provide the most comprehensive source of global data on commercial fishing and aquaculture employment.

Gapfilling 
==========

