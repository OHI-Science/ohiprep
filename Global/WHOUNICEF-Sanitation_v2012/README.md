GL-WHOUNICEF-Sanitation_v2012
===========================

Data from WHO/UNICEF JMP: Joint Monitoring Programme, 1990-2011 

Files produced
==============
FILES: 

* rgn_jmp_san_2012a.csv
* rgn_jmp_san_2013a.csv
* rgn_jmp_san_allyears.csv

PATH: 

* model/GL-WHOUNICEF-Sanitation_v2012/data


Description
===========
ACCESS:

* Data accessed from JMP: http://www.wssinfo.org/data-estimates/table/. WHOUNICEF_SanitationDataQuery.png file isthe exact queries are stored with the .cvs files in the raw folder. Headers were reformatted so they are all on one line and file was saved as wssinfo_data_sanitation_reformatted.csv

DATA:

Data from 1990-2011. Variables:

* Country
* Year 
* PopNtl (x1000)
* ImprovedTotal (x1000)  \*\* Improved = access to improved sanitation
* ImprovedTotal (%)
* UnimprovedTotal (x1000)  \*\* Unimproved = access to rudimentary sanitation/no sanitation
* UnimprovedTotal (%)
* Proportion of the 2010 population that gained access since 1995 (%)


R scripts involved
==================
SCRIPT:
* clean_WHOUNICEF.r

DETAILS: cleaning

* wssinfo_data_sanitation.csv is the product of just providing data for countries that do not have access to improved sanitation conditions: this is called "unimproved". The reformatted file is what is called by clean_WHOUNICEF.r


Gapfilling 
==========

CATEGORY: SG

EXCEPTIONS: NA

DETAILS:

* Southern Islands = NA


2014 Resolutions
================


Methods
=======

5.57. Pathogen pollution
Update: additional year(s) available
Description: Updated percent of population with access to improved sanitation facilities data were available from 1990-2011from the World Health Organization and United Nations Children's Fund's Joint Monitoring Programme (WHO/UNICEF's JMP: www.wssinfo.org/data-estimates/table/, accessed May 2013). Processing included the function "na.locf" from the "zoo" package in R (Zeileis & Grothendieck 2005). 


Citation: Achim Zeileis and Gabor Grothendieck (2005). zoo: S3 Infrastructure for Regular and Irregular Time Series. Journal of Statistical Software, 14(6), 1-27. URL http://www.jstatsoft.org/v14/i06/


Metadata
========
