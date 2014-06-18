GL-WDPA-MPA_v2013
========================


September 23, 2013 (JStewart):
==============================


Final file in model/GL-WDPA-MPA_v2013 used in OHI 2013a and 2012a:
	+  lsp_prot_area_offshore3nmb.csv

These files differ from the original files in the same folder in 1 way:
	+  lsp_prot_area_offshore3nm.csv

* 1) values identified as having LSP scores of NA in a previous version of the results were found to be absent from lsp_prot_area_offshore3nm.csv and therefore were NA's (which show up as blanks). SO these rgns were added, with 0's, to the dataset (year was chosen to represent recent year):

	+ 1      2009    0
	+ 8      2009    0
	+ 9      2009    0
	+ 10     2009    0
	+ 11     2009    0
	+ 30     2009    0
	+ 33     2009    0
	+ 34     2009    0
	+ 35     2009    0
	+ 36     2009    0
	+ 39     2009    0
	+ 45     2009    0
	+ 47     2009    0
	+ 52     2009    0
	+ 56     2009    0
	+ 67     2009    0
	+ 77     2009    0
	+ 85     2009    0
	+ 88     2009    0
	+ 95     2009    0
	+ 97     2009    0
	+ 98     2009    0
	+ 99     2009    0
	+ 103    2009    0
	+ 105    2009    0
	+ 107    2009    0
	+ 118    2009    0
	+ 119    2009    0
	+ 121    2009    0
	+ 125    2009    0
	+ 127    2009    0
	+ 141    2009    0
	+ 144    2009    0
	+ 146    2009    0
	+ 147    2009    0
	+ 148    2009    0
	+ 154    2009    0
	+ 156    2009    0
	+ 161    2009    0
	+ 186    2009    0
	+ 212    2009    0
	+ 215    2009    0
	+ 228    2009    0
	+ 249    2009    0
	+ 250    2009    0


# note! This should probably be done for the other input file ID'd in layers_navigation to be super tidy: model/GL-WDPA-MPA_v2013/data/lsp_prot_area_inland1km.csv, but it works appropriately this way.



Documentation of flags settings for run: BB's layers_2013.r /Volumes/local_edit/src/toolbox/code/layers_2013.R
-------------------------------------------------

# flags for running
get.googledoc=T; gen.nav=T; check.nav=T; open.nav=F; assemble.nav=T
#get.googledoc=T; gen.nav=T; check.nav=T; open.nav=T; assemble.nav=T
do.pressures=F; do.resilience=F
#do.pressures=T; do.resilience=T
do.ICO=F; do.CS=F; do.CP=F; do.HAB=F; do.AO=F; do.LSP=T; do.FIS=F; do.MAR=F; do.SPP=F; do.TR=F; do.CW=F; do.NP=F; do.LIV.ECO=F
#do.ICO=T; do.CS=T; do.CP=T; do.HAB=T; do.AO=T; do.LSP=T; do.FIS=T; do.MAR=T; do.SPP=T; do.TR=T; do.CW=T; do.NP=T; do.LIV.ECO=T
do.summary=T
do.maps = F; do.asters=T



RESULTS found on Neptune: 
-------------------------
* Results files: local_edit/src/toolbox/scenarios/global_2013a/results/
* Aster Flower plots: local_edit/src/toolbox/scenarios/global_2013a/results/fig/flowers/


* to run with do.maps = T:
	+  install.packages('maptools')
	+  install.packages('sp')
	+  install.packages('rgdal')
