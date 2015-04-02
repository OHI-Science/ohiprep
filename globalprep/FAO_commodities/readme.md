#FAO_commodities
FAO Commodities data are used to determine the Natural Products goal.
At this base level:
data_prep.R script prepares the raw FAO commodities data based upon the most current year's report.  
This preparation includes interpreting various FAO-specific codes and flags, eliminating NAs prior to first 'reporting' year (first non-NA report),
filtering commodities by product group, gap-filling, smoothing, and processing status results based on harvest relative to buffered peak value.
commodities2products.csv: 
Folders include:
R: contains R scripts related to data processing.
./R/np_fxn.R: functions called from within ./data_prep.R for gap-filling etc.
./v2015: contains raw, tmp, and data files for processing data posted in 2015.
./v2015/raw: raw FAO commodities data (see below).
./v2015/tmp: temporary files created during data processing, useful for debugging, or for examining data prior to gapfilling/smoothing processes.
./v2015/data: outputs from ./data_prep.R, including np_gapfill_report.csv and status score outputs.
./v2014_test: similar to ./v2015; contains raw, tmp, and data files for processing data posted in March 2014.
