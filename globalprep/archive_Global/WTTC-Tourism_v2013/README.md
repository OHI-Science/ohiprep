GL-WTTC-Tourism_v2013
=====================

WTTC: World Travel and Tourism Council www.wttc.org

Data processing data accessed through 2013
/*/* Note: WTTC now indicates what is real and what is projected data: 'Actual data to 2013, forecast data 2014 onwards'


Files produced
==============
FILES: 

* rgn_wttc_empd_2013.csv : Direct Contribution To Employment
* rgn_wttc_empt_2013.csv : Total Contribution To Employment
* rgn_wttc_gdpt_2013.csv : Total Contribution to GDP

Currently these data are not gapfilled and are as rgn_id. To see what happened to these data beyond data_prep.R in 2013a, see on Neptune:
* see model/GL-NCEAS-TR_v2013a/README.md for further handling
* processed here then further in manage_revenue.r
	+ See model/GL-NCEAS-Livelihoods_v2013a/README.md for further handling


Description
===========
ACCESS: 
Data downloaded from: http://www.wttc.org/research/economic-data-search-tool/
in March 2014 by J. Stewart Lowndes

Note: using this data search tool the user specifies which and how many different metrics to download in the same file. I chose only one metric with raw numbers (eg 2011 US$ bn or thousands of people) instead of a comparison (eg real growth % or % share). 

DATA:

File names changed from downloaded titles so they make sense--reformatted files saved; see data_prep.r : 

* WTTC_DirectContributionToEmployment.xls was originally WTTC_Data_Export_1394046247.xls
* WTTC_TotalContributionToEmployment.xls was originally WTTC_Data_Export_1394046309.xls
* WTTC_TotalContributionToGDP.xls was originally WTTC_Data_Export_1394046161.xls

\>\>Also, these data have some typos: Cote d'ivorie and Madagasca. They are fixed in r scripts.


R scripts involved
==================
* data_prep.r

DETAILS: cleaning

This reads the .xls files in directly , anything in the raw file. It takes care of the funky formatting where the data is stored on every other line of the table (reformatting that was done by hand for 2013a has been added to the script). 


Gapfilling 
==========
CATEGORY: NA

EXCEPTIONS: NA

DETAILS: will be gapfilled further along


2014 Resolutions
================
update marine jobs: rgn_wttc_empt_2013a.csv wasn't incorporated into v2013 due to lack of time. (it was the only jobs sector updated in 2013)


Methods 2013
=======

(TO REMOVE FROM METHODS: 5.39. Marine jobs: tourism
Update: additional year(s) available
Description: These job data are estimated from the travel and tourism total contribution to employment that are collated by the World Travel & Tourism Council (WTTC) (www.wttc.org/research/economic-data-search-tool/). Updated travel and tourism total contribution to employment data were available for 147 coastal countries through 2012. Total contribution measures the number of jobs generated directly in the travel and tourism industries plus the indirect and induced contributions (investment industries and suppliers).)

5.46. Marine revenue: tourism
Update: additional year(s) available
Description: Updated total contribution to GDP data were available through 2012 from the World Travel and Tourism Council (WTTC: www.wttc.org/research/economic-data-search-tool/). Total contribution includes revenue from sectors directly and indirectly associated with travel and tourism. This is the sole measure used to inform marine revenue from tourism; the supplemental information in 2012 incorrectly identified two other metrics that were not used to calculate the Economies sub-goal. 

5.75. Travel and Tourism Direct Contribution to Employment
Update: new data layer
Description: These data were used in the tourism and recreation goal as an indicator of the number of tourists visiting the coast (assuming that employment in the travel and tourism sectors are dynamic and would change to reflect increases or decreases in tourism). These data measure employment that is directly linked to the travel and tourism sectors (such as hotels, airlines, airports, travel agents and leisure & recreation services that deal directly with tourists). Data are available for 181 countries from 1988-2012 (www.wttc.org/research/economic-data-search-tool/); here we use data for 2007-2012.


Metadata
========

AVAILABLE DATASETS:

* Leisure Travel and Tourism Consumption- Spending on leisure travel within a country by residents and international visitors.

* Domestic Travel and Tourism Spending- Spending within a country by that country's residents for both business and leisure trips. Multi-use consumer durables are not included since they are not purchased solely for tourism purposes. This is consistent with total domestic tourism expenditure in table 2 of the TSA: RMF 2008. Outbound spending by residents abroad is not included here, but is separately identified according to the TSA: RMF 2008.

* Visitor Exports- Spending within the country by international tourists for both business and leisure trips, including transportation spending. This is consistent with total inbound tourism expenditure in table 1 of the TSA: RMF 2008.

* Travel and Tourism contribution to GDP- and leisure industries that deal directly with tourists. It is equivalent to total internal Travel & Tourism spending within a country minus purchases made by those industries (including imports). In terms of the UN's Tourism Satellite Account methodology it is consistent with total GDP calculated in table 6 of the TSA: RMF 2008.

* Travel and Tourism contribution to Employment- The number of direct jobs within the Travel & Tourism industries. This is consistent with total employment calculated in table 7 of the TSA: RMF 2008.

* Internal Travel and Tourism Consumption- Total revenue generated within a country by industries that deal directly with tourists including visitor exports, domestic spending and government individual spending. This does not include spending abroad by residents. This is consistent with total internal tourism expenditure in table 4 of the TSA: RMF 2008.


\*\* Note: TSA RMF 2008= http://unstats.un.org/unsd/tradekb/Knowledgebase/TSA-RMF-2008 Tourism Satellite Account: Recommended Methodological Framework
