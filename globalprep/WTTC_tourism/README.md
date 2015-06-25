#### WTTC_Tourism data

See github/ohiprep/globalprep/TourismRecreation for processing scripts.

Data for 2014 is now available, MRF downloaded these data on May 6 2015.

Data extends past 2014, but these are "predicted" data:
![](https://github.com/OHI-Science/ohiprep/blob/master/globalprep/WTTC_tourism/v2015/images/WTTC_dataPlot.png)

To get the data: 
![](https://github.com/OHI-Science/ohiprep/blob/master/globalprep/WTTC_tourism/v2015/images/WTTC_gettingData1.png)
![](https://github.com/OHI-Science/ohiprep/blob/master/globalprep/WTTC_tourism/v2015/images/WTTC_getting%20data2.png)

We have selected the following data (easiest to select each one individually):
* gn_wttc_empd_2013.csv : Direct Contribution To Employment: The number of direct jobs within travel and tourism
* rgn_wttc_empt_2013.csv : Total Contribution To Employment: The number of jobs generated directly in the Travel and Tourism sector plus the indirect and induced contributions
* rgn_wttc_gdpt_2013.csv : Total Contribution to GDP: GDP generated directly by the Travel and Tourism sector plus its indirecdt and induced impacts


R scripts involved
==================
* data_prep.r

DETAILS: cleaning

This reads the .xls files in directly , anything in the raw file. It takes care of the funky formatting where the data is stored on every other line of the table (reformatting that was done by hand for 2013a has been added to the script). 

Currently these data are not gapfilled and are as rgn_id. To see what happened to these data beyond data_prep.R in 2013a, see on Neptune:
* see model/GL-NCEAS-TR_v2013a/README.md for further handling
* processed here then further in manage_revenue.r
	+ See model/GL-NCEAS-Livelihoods_v2013a/README.md for further handling

Some definitions that might help with gdp data:
![](https://github.com/OHI-Science/ohiprep/blob/master/globalprep/WTTC_tourism/v2015/images/GDPdefinitions.png)
