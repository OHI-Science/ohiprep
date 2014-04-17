FAO-CW-Trends_v2011
=====================

Data are in a new format since 2013a. Available from http://faostat3.fao.org/faostat-gateway/go/to/download/R/*/E

Files produced
==============

* 

Description
===========
ACCESS: 

PREPROCESSING NOTE:

.xls data files downloaded from http://faostat3.fao.org/faostat-gateway/go/to/download/R/*/E are huge; they crash .xls unless opened from within Excel. Save as .csv files as soon as possible. 

R scripts involved
==================
* data_prep.r



Methods 2014
============



From Methods 2013
-----------------

5.19. Fertilizer trends
Update: additional year(s) available
Description: Updated fertilizer consumption data were available through 2010 from FAO’s statistical database FAOSTAT (faostat3.fao.org/home/index.html#DOWNLOAD). Data were summed across all fertilizer compounds and reported in metric tons. Upon inspection the data included multiple 0 values that are most likely data gaps in the time-series, so they were treated as such and replaced with NA. In addition, regions with only 1 data point and regions where the most recent data point was prior to 2005 were excluded. The data gaps were then filled using coastal population trends for the corresponding reporting region. Uninhabited countries were assumed to have no fertilizer use and thus excluded. Nine regions were inhabited but had no fertilizer or population data. Of these, two were considered close enough to large countries to receive influence of their pollution and were gapfilled using regional trends (i.e., Juan da Nova and Glorioso Islands), and the remaining 7 were considered too remote, hence their trend was assumed to be 0. For the 2013 assessment, the 2010 values were used as the most recent year. When data for 2010 was missing, the trend for 2013 is identical to the trend for the 2012 assessment.


5.58. Pesticides trends
Update: additional year(s) available
Description: Updated pesticide consumption data were available through 2010 from FAO’s statistical database FAOSTAT (faostat3.fao.org/home/index.html#DOWNLOAD). Data were summed across all pesticide active ingredients and reported in metric tons. The gap-filling and processing was done in the same way as described for the fertilizer trends data (see section 5.19).




Gapfilling 
==========

