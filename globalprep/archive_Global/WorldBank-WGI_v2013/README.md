GL-WorldBank-WGI_v2013
===========================

\* next time try using WorldBank R packaged: https://github.com/OHI-Science/ohiprep/issues/11

The Worldwide Governance Indicators, 2012 Update
Aggregate Indicators of Governance 1996-2011.

* Voice and Accountability
* Political Stability and Absence of Violence
* Government Effectiveness
* Regulatory Quality
* Rule of Law
* Control of Corruption


Files produced
==============
FILES by clean_WGI.r: 

* rgn_wb_wgi_all.csv
* rgn_wb_wgi_2012a.csv
* rgn_wb_wgi_2013a.csv


FILES by export_rescaled_layers.r: 

* rgn_wb_wgi_2012a_rescaled.csv
* rgn_wb_wgi_2013a_rescaled.csv
* rgn_wb_wgi_2012a_rescaled_inverse.csv
* rgn_wb_wgi_2013a_rescaled_inverse.csv

PATH: 

* model/GL-WorldBank-WGI_v2011/data



Description
===========

\>\> updated through 2011 (was through 2009 for 2012 OHI): http://info.worldbank.org/governance/wgi/index.asp

The WGI is actually six separate indicators (there is no aggregate "WGI")--we will combine them: mean.

* wgidataset.xls was downloaded from http://info.worldbank.org/governance/wgi/index.asp 
	+ each xls sheet is one of the six indicators: data is for each country for each year, with six variables:
	+ Estimate,Lower,NumSrc,P,StdErr,Upper. See Metadata below for legend.

\>\> NOTE: each xls sheet has identical format, except for the P/P-Rank/Rank identification. In the end, this variable will be named P. 


R scripts involved
==================
SCRIPTS:

* clean_WGI.r
* export_rescaled_layers.r

DETAILS: cleaning

* import each separate xls sheet (separate sheet for each of 6 indicators)
* combine with fun.aggregate = mean in dcast function. 


Gapfilling 
==========

CATEGORY: SCG

EXCEPTIONS: NA

DETAILS:


2014 Resolutions
================


Methods
=======

5.77. Worldwide Governance Indicators (WGI)
Update: additional year(s) available
Description: Updated WGI data were available through 2011 (Worldwide Governance Indicators (last updated: 14-Sep-2012: info.worldbank.org/governance/wgi/index.asp) and scores for the six World Governance Indicators were averaged together. The World Bank’s update includes revisions for two of the indicators: Rule of Law and Control of Corruption.



Metadata
========

All methodology is here: http://info.worldbank.org/governance/wgi/resources.htm

From wgidataset.xls: 

The Worldwide Governance Indicators project constructs aggregate indicators of six broad dimensions of governance:

Voice and Accountability
Political Stability and Absence of Violence
Government Effectiveness
Regulatory Quality
Rule of Law
Control of Corruption

The six aggregate indicators are based on  30 underlying data sources reporting the perceptions of governance of a large number of survey  respondents and expert assessments worldwide.  Details on the underlying data sources, the aggregation method, and the interpretation of the indicators, can be found in the WGI methodology paper:

Daniel Kaufmann, Aart Kraay and Massimo Mastruzzi (2010).  "The Worldwide Governance Indicators : A Summary of Methodology, Data and Analytical Issues". World Bank Policy Research  Working Paper No.  5430
http://papers.ssrn.com/sol3/papers.cfm?abstract_id=1682130

Full interactive access to the aggregate indicators, and the underlying source data, is available at www.govindicators.org.

Note that this Worldwide Governance Indicators update incorporates revisions to data for previous years, and so this data release supersedes data from all previous releases.

The Worldwide Governance Indicators (WGI) are a research dataset summarizing the views on the quality of governance provided by a large number of enterprise, citizen and expert survey respondents in industrial and developing countries. These data are gathered from a number of survey institutes, think tanks, non-governmental organizations, international organizations, and private sector firms. The WGI do not reflect the official views of the World Bank, its Executive Directors, or the countries they represent. The WGI are not used by the World Bank Group to allocate resources.

February 2013

From wgidataset.xls sheets: 

Legend
Estimate	Estimate of governance (ranges from approximately -2.5 (weak) to 2.5 (strong) governance performance)

* StdErr:	Standard error reflects variability around the point estimate of governance.	
* NumSrc:	Number of data sources on which estimate is based
* P-Rank:	Percentile rank among all countries (ranges from 0 (lowest) to 100 (highest) rank)
* Lower:	Lower bound of 90% confidence interval for governance, in percentile rank terms
* Upper:	Upper bound of 90% confidence interval for governance, in percentile rank terms

The Worldwide Governance Indicators (WGI) are a research dataset summarizing the views on the quality of governance provided by a large number of enterprise, citizen and expert survey respondents in industrial and developing countries. These data are gathered from a number of survey institutes, think tanks, non-governmental organizations, international organizations, and private sector firms. The WGI do not reflect the official views of the World Bank, its Executive Directors, or the countries they represent. The WGI are not used by the World Bank Group to allocate resources.


* Voice and Accountability:
	+ Reflects perceptions of the extent to which a country's citizens are able to participate in selecting their government, as well as freedom of expression, freedom of association, and a free media.	

* Political Stability and Absence of Violence
Reflects perceptions of the likelihood that the government will be destabilized or overthrown by unconstitutional or violent means, including politically-motivated violence and terrorism.

* Government Effectiveness
Reflects perceptions of the quality of public services, the quality of the civil service and the degree of its independence from political pressures, the quality of policy formulation and implementation, and the credibility of the government's commitment to such policies

* Regulatory Quality
Reflects perceptions of the ability of the government to formulate and implement sound policies and regulations that permit and promote private sector development.

* Rule of Law
Reflects perceptions of the extent to which agents have confidence in and abide by the rules of society, and in particular the quality of contract enforcement, property rights, the police, and the courts, as well as the likelihood of crime and violence.

* Control of Corruption
Reflects perceptions of the extent to which public power is exercised for private gain, including both petty and grand forms of corruption, as well as "capture" of the state by elites and private interests.




