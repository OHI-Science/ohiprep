NOTE: Oct 3 2016, MRF moved from: ohiprep:Global/WorldBank-Statistics_v2012/data

GL-WorldBank-Statistics_v2012
=============================

\* next time try using WorldBank R packaged: https://github.com/OHI-Science/ohiprep/issues/11. https://github.com/vincentarelbundock/WDI, http://cran.r-project.org/web/packages/WDI/index.html. This does not work for WGI, but it does for all other World Bank indicators

Files produced
==============

Updated data available for three of the World Bank data categories: GDP, Total Labor Force, and Percent Unemployment
/* note: World Bank abbreviates 'Total Labor Force' as 'tlf' so in 2014 we will use this; in 2013 we used 'lab'

* github/ohiprep/Global/WorldBank-Statistics_v2012/data/rgn_wb_gdp_2014a.csv
* github/ohiprep/Global/WorldBank-Statistics_v2012/data/rgn_wb_tlf_2014a.csv
* github/ohiprep/Global/WorldBank-Statistics_v2012/data/rgn_wb_uem_2014a.csv


R scripts involved
==================

* github/ohiprep/Global/WorldBank-Statistics_v2012/data_prep.R

DETAILS: Unlike clean_WB.r, this data_prep.r works with .xlsx files

* Population data were not updated so no tweaks were required by hand like in 2013 (see 2013 README.md on Neptune data_edit /model/GL-WorldBank-Statistics_v2012).

Access
======

* gdp: Gross Domestic Product (current \$USD)
  + http://data.worldbank.org/indicator/NY.GDP.MKTP.CD
* uem = Unemployment, total (\% of total labor force) 
	+ http://data.worldbank.org/indicator/SL.UEM.TOTL.ZS
* tlf = Total Labor force, total (\# people)
	+ http://data.worldbank.org/indicator/SL.TLF.TOTL.IN
* ppppcgdp = GDP adjusted per capita by PPP
	+ http://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
  
  
  
  
  