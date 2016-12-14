GL-UNDP-HDI_v2012
=================

Human Development Index: updated


Files produced
==============
FILES: 

* rgn_undp_hdi.csv

Description
===========

ACCESS:

* HDR_2013_EN_complete.pdf was downloaded from http://hdr.undp.org/en/reports/global/hdr2013/. 
* Data were copied into a .txt file: HDR_Table1_Development.txt and then resaved as 2 separate files: 
	+ HDR_Table1_Development-Developed.txt (all countries with "Very high human development") 
	+ HDR_Table1_Development-Developing.txt (all others) 
* Reformatting removed the footnote letters, replaced spaces with commas but then removed them between country names
	+ \s[a-z]\s replaced with \s
	+ ([a-z]),([A-Z]) \1 \2
	+ ([a-z]),([a-z]) \1 \2


 
R scripts involved
==================
SCRIPT:

* clean_UNDP.r

DETAILS: cleaning

* developed countries = 1
* developing = 0
* sovereign regions get parent value
* all other regions get 0 


Gapfilling 
==========

CATEGORY: SG

EXCEPTIONS: NA

DETAILS: Only sovereign gapfilling: no further gapfilling: missing countries should get 0 instead of 1. 


2014 Resolutions
================

* include updated file  in toolbox
* update methods: see below
* combine add_gapfill_sov.r into add_gapfill


Methods
=======

WAS: 

5.28. Human Development Index (HDI)
Update: no update included.


SHOULD HAVE BEEN:

Updated HDI assessments were available for 2012 to be used as multipliers for the Livelihoods and Economies goal from the United Nations Development Programme Human Development Reports (UNDP HDR: hdr.undp.org/en/reports/global/hdr2013/). Countries were classified as developed (“very high human development” category) or developing (all others). In 2012, there were 32 countries in the "Developed" category. In 2013 there were 39: Added countries were Brunei, Chile, Lithuania, Latvia, Argentina, Seychelles, Croatia and Bahrain moved into the "Developing" category.

Metadata
========
