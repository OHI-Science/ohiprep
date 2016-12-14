GL-WHOUNICEF-Sanitation_v2012
===========================

Data from WHO/UNICEF JMP: Joint Monitoring Programme, 1990-2012 

Nature 2012 SOM p. 51: 'Access to improved sanitation facilities is defined as the percentage of the population within a country with at least adequate access to excreta disposal facilities that can effectively prevent human, animal, and insect contact with excreta. These data are a country-wide average (not specific to the coastal region) and the most recent year available is 2008. Percentages (0-100) for each country were rescaled to 0-1 based on a maximum target of 100% of the population with access to improved sanitation, and a minimum value of 0'


Files produced
==============
FILES: 

* rgn_jmp_san_2014a.csv
* rgn_jmp_san_2014a_attr.csv

PATH: 

* ohiprep/Global/WHOUNICEF-Sanitation_v2012/data/


Description
===========
ACCESS:

* Data accessed from JMP: http://www.wssinfo.org/data-estimates/tables/. Website has been updated since OHI 2013a. WHOUNICEF_SanitationDataQuery.png file shows the exact queries used.


R scripts involved
==================
SCRIPT:
* data_prep.r

DETAILS: 


Gapfilling 
==========

CATEGORY: SG

EXCEPTIONS: NA

DETAILS:

* Southern Islands = NA


2014 Resolutions
================


Methods 2013
============

5.57. Pathogen pollution
Update: additional year(s) available
Description: Updated percent of population with access to improved sanitation facilities data were available from 1990-2011from the World Health Organization and United Nations Children's Fund's Joint Monitoring Programme (WHO/UNICEF's JMP: www.wssinfo.org/data-estimates/table/, accessed May 2013). Processing included the function "na.locf" from the "zoo" package in R (Zeileis & Grothendieck 2005). 


Citation: Achim Zeileis and Gabor Grothendieck (2005). zoo: S3 Infrastructure for Regular and Irregular Time Series. Journal of Statistical Software, 14(6), 1-27. URL http://www.jstatsoft.org/v14/i06/


Metadata
========
