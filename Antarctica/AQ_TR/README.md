The R files for preparing the data and calculating status/trend are located: https://github.com/OHI-Science/ohiprep/Global/GL-AQ-Tourism_v2014

## Notes about data and processing

Raw data: 
The data came from: http://iaato.org/tourism-statistics

I used the "Number of Visits per Site/per Activity" dataset for the 2013-2014 and the 2014-2015 years. The data have several tabs, and the summary is on the first tab, labeled "all-alpha", which is just the data sorted in alphabetical order by location. This shows the total number of "tourists" by location, and also breaks it down to individual activities as well. The data came from the "Total" column.


for site information: raw/ATCM35_att068_e.doc
Description of data processing: AQ_Tourism&Recreation.png

CalculatingFinalData.R = script to calculate status and trend data

The dataPrep file contains:
PreparingCoordinateData.R = script used to standardize Katie's lat/long coordinates and convert to decimal degrees
OverlayCoordinateData.R = script used to identify the CCAMLR regions that correspond to the locations based on their lat/longs

Notes from other documents:

Status – rate of visitor increase? (developing industry, so it gets a 100?)
Maybe just use this as a pressure? And use a TTCI-like proxy for status
Trend – 
Pressures – social pressures: access, regulations, economic incentives, facilities and infrastructure? (search & rescue is one of the big issues)
Resilience – regulations that encourage tourism? (treaties – how not to duplicate TTCI)

*NOTE: tourism is a pressure due to introduction of invasive spp, particularly plants (Chown et al., PNAS 2012), and as a major contributor to shipping traffic (and possibly waste?)
A consortium of “responsible tour operators” was created, IAATO
(http://iaato.org/home) comprising most of the operators, and includes visits to scientific bases (at least BAS allows them). So far comments found on the organization are positive. The website advertises its support to help get samples for Chown’s study.
“Tourism is loosely regulated by the 28-country Antarctic Treaty Consultative Committee. They’ve only made two mandatory rules since 1966 — that tourism operators be insured to cover rescue and medical evacuations and that ships carrying more than 500 passengers be prohibited from landing. Neither regulation has been put into practice yet” (http://blog.zagat.com/2013/03/is-tourism-threat-to-antarctica.html)

