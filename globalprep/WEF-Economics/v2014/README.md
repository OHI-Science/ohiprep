GL-WEF-Economics_v2013
===========================

WEF = World Economic Forum

* GCI = Global Competitiveness Index 
* TTCI = Travel and Tourist Competitiveness Index


Files produced
==============

* /ohiprep/Global/WEF-Economics_v2014/data/rgn_wef_gci_2014a_rescaled.csv
* Note:: Final WEF TTCI data was not updated; this is still on Neptune data_edit: /model/GL-WEF-Economics_v2013/data/rgn_wef_ttci_2013a.csv


Description
===========
Data taken from two separate pdf reports for GCI and TTCI. Both reports have extensive data for each country, including 2013's Index scores and rankings, comparisons to previous years, subIndices and Pillars, but this information is in several separate tables. Tables including previous years are processed here since this might be useful for trend calculation. 

ACCESS: 

http://www.weforum.org/issues/global-competitiveness

* http://www.weforum.org/issues/global-competitiveness: Table 3 (p. 15; 2013-2014)

http://www.weforum.org/issues/travel-and-tourism-competitiveness

* http://reports.weforum.org/travel-and-tourism-competitiveness-report-2013/: Table 1 (p.10)


R scripts involved
==================
SCRIPT:

* data_prep.r

DETAILS: cleaning

Original data downloaded as a pdf from [World Economic Forum](http://www.weforum.org/issues/global-competitiveness) at this [link] (http://www3.weforum.org/docs/WEF_GlobalCompetitivenessReport_2013-14.pdf). This pdf is stored on Neptune on data_edit: git-annex/Global/WEF-Economics_v2014/WEF_GlobalCompetitivenessReport_2013-14.pdf

To create .csv files from these pdf tables, follow these steps 
TODO: make a python script for this:

* To copy data, use Adobe Acrobat Pro 9.0 
* Table was copied into WEF_GCI_2013-2014_Table3.txt and WEF_TTCI_2013-2014_Table1.txt 
* Table saved as WEF_GCI_2013-2014_Table3_reformatted.csv and WEF_TTCI_2013-2014_Table1_reformatted.csv with TextWrangler: 
  + add quotes around countries with commas (~5 of these) 
  + add commas to make it csv: 
    - search '(\w) (\d)' replace '\1,\2'; 
    - search '(\d) (\d)' replace '\1,\2'; 
    - search ' (n\/a)' replace ',NA' 
    - simplify/rename headers with commas

WEF_GCI_2013-2014_Table3_reformatted.csv is processed further by data_prep.R in ohiprep/Global/WEF-Economics_v2014/

Gapfilling 
==========
CATEGORY: SG, XSI

EXCEPTIONS: NA

DETAILS: Southern Islands == NA

* used add_gapfill_singleyear


From 2013 README:
=================

DATA: 

GCI: Global Competitiveness Index 2013

\>\>\> Increased to 144 countries, from 142 last year:

* Added: Gabon, Guinea, Liberia, and Sierra Leone.
* Readded: Libya was re-included after a year of absence as we were not able to conduct the Survey because of civil unrest in 2011.
* Subtracted: Belize, Angola, Syria, Tunisia. See Metadata below. 


TTCI: Travel and Tourist Competitiveness Report

\>\>\> 140 countries assessed in 2013 (up from 139 in 2011 report). Accounts for >98% of the world GDP.

* Added: Seychelles, Guinea, Sierra Leone, Yemen, Haiti and Suriname (reinstated
after being absent in the last edition because of a lack of data)
* Subtracted: Angola, Libya, Syria, Timor-Leste, and Tunisia— not covered this year because of insufficient or unreliable data. See Metadata below. 

To work with data: 
  + To copy data, use Adobe Acrobat Pro 9.0
	+ Table was copied into WEF_GCI_2012-2013_Table3.txt and WEF_TTCI_2012-2013_Table1.txt
	+ Table saved as WEF_GCI_2012-2013_Table3_reformatted.csv  and WEF_TTCI_2012-2013_Table1_reformatted.csv with TextWrangler:
		- add quotes around countries with commas (~5). 
		- add commas to make it csv: 1) search '(\\w) (\\d)' replace '\\1,\\2'; 2)search '(\\d) (\\d)' replace '\\1,\\2'; 3) search ' (n\\/a)' replace ',NA'
		- simplify/rename headers 
		
		
\*\* Note: Other tables have subindex and pillar score values for each country as well, so there are other data available that could potentially be useful 


2014 Resolutions
================

* include updated gci
* use add_gapfill instead of add_gapfill_singleyear


Methods
=======
5.23. Global Competitiveness Index (GCI) 
Update: no update included

BUT SHOULD BE: 
Updated assessments of the GCI were accessed from the 2012-2013 Report (WEF 2013: http://reports.weforum.org/global-competitiveness-report-2012-2013/). There were 144 economies covered in 2012-2013, up from 142 in 2010-2011. Gabon, Guinea, Liberia, and Sierra Leone were added, Libya was readded (due to civil unrest in 2011), and Belize, Angola, Syria, Tunisia were removed due to insufficient data.

5.74. Travel and Tourism Competitiveness Index (TTCI)
Update: additional year(s) available
Description: Updated assessments of the TTCI were accessed from the 2012-2013 Report (WEF 2013, reports.weforum.org/travel-and-tourism-competitiveness-report-2013/). There were 140 economies covered in 2012-2013, up from 139 in 2010-2011. Seychelles, Guinea, Sierra Leone, Yemen and Haiti were added, Suriname was re-added (after being absent in the last edition because of a lack of data). Angola, Libya, Syria, Timor-Leste, and Tunisia were removed because of insufficient or unreliable data



Metadata
========

GCI:
Excerpt from pdf report http://reports.weforum.org/global-competitiveness-report-2012-2013 (p. 10):

ADJUSTMENTS TO THE GCI
A few minor adjustments have been made to the
GCI structure this year. Within the macroeconomic environment pillar (3rd), the interest rate spread has been removed from the Index because of limitations
in the international comparability of these data. Furthermore, mobile broadband was added to the technological readiness (9th) pillar in order to take into account the rapidly expanding access to the Internet
via mobile devices. And a variable capturing the extent to which governments provide services to the business community, which has been collected through the Executive Opinion Survey, was added to the institutions pillar (1st). For the patent indicator in the innovation pillar12th), the source has been changed to include data based on the Patents Co-operations Treaty instead of the US Patent and Trademark Office (USPTO), which had been used until now. These data are collected
and published jointly by the World Intellectual Property Organization and the Organisation for Economic Co- operation and Development (OECD). They record patent applications globally, not just in the United States, therefore eliminating a possible geographical bias.22 Finally, the Rigidity of Employment Index was dropped from the labor market efficiency pillar (7th), as the World Bank ceased to provide this indicator.23

COUNTRY COVERAGE
The coverage of this year has increased from 142 to 144 economies. The newly covered countries are Gabon, Guinea, Liberia, Seychelles, and Sierra Leone. Libya was re-included after a year of absence as we were not able to conduct the Survey because of civil unrest
in 2011. Three previously covered countries had to be excluded from this year’s Report. Survey data could not be collected in Belize and Angola; in Syria, the security situation did not allow the Survey to be carried out. In the case of Tunisia we decided not to report the results this year because an important structural break in the data makes comparisons with past years difficult. We hope to re-include these countries in the future.


TTCI:
Excerpt from WEF_TT_Competitiveness_Report_2013.pdf, p. xvii: 
The TTCI is based on three broad categories of
variables that facilitate or drive T&T competitiveness.
These categories are summarized into the three
subindexes of the Index: (1) the T&T regulatory
framework subindex; (2) the T&T business environment
and infrastructure subindex; and (3) the T&T human,
cultural, and natural resources subindex. The first
subindex captures those elements that are policy related
and generally under the purview of the government; the
second subindex captures elements of the business
environment and the “hard” infrastructure of each
economy; and the third subindex captures the “softer”
human, cultural, and natural elements of each country’s
resource endowments.
Each of these three subindexes is composed in turn
by a number of pillars of T&T competitiveness, of which
there are 14 in all. These are:
1. Policy rules and regulations
2. Environmental sustainability
3. Safety and security
4. Health and hygiene
5. Prioritization of Travel & Tourism
6. Air transport infrastructure
7. Ground transport infrastructure
8. Tourism infrastructure
9. ICT infrastructure
10. Price competitiveness in the T&T industry
11. Human resources
12. Affinity for Travel & Tourism
13. Natural resources
14. Cultural resources

p. 9:
ADJUSTMENTS TO THE TTCI
A few minor adjustments have been made to the TTCI
structure in this edition to ensure that the Index remains
highly relevant:
• Within the Policy rules and regulations pillar (1st),
the indicator Visa requirements (1.04), has been
updated to include the case of electronic visas
(eVisas) alongside the other visa possibilities. This
has become necessary because of the increasing
relevance of the facilitation of visa processes in the
policy debate.
• Within the ICT infrastructure pillar (9th), the indicator
Extent of business Internet use has been replaced
by two more specific indicators. These are ICT use
for business-to-business transactions and ICT use
for business-to-consumer transactions, and are
based on the Executive Opinion Survey. Also, an
indicator measuring Mobile broadband subscriptions
has been added to this pillar. These changes reflect
the growing importance of ICTs for the tourism
industry’s operations as well as their role as tools for
travelers.
• Within the Affinity for Travel & Tourism pillar (12th),
an indicator measuring the Degree of customer
orientation was added because of the importance of
customer satisfaction in the T&T sector.
• Finally, within the Natural resources pillar (13th), the
variables used to compute the extent of Protected
areas have been replaced by the indicators
Terrestrial biome protection and Marine protected
areas. These changes should be regarded as
data improvements, using more sophisticated
and accurate measures, and are in line with the
World Economic Forum’s work on Sustainable
Competitiveness.

COUNTRY COVERAGE
Six new economies have been included in the analysis
this year. These include three new African countries
(Seychelles, Guinea, and Sierra Leone); one Middle
Eastern county (Yemen); and two countries in the
Americas (Haiti and Suriname, which was reinstated
after being absent in the last edition because of a lack
of data). On the other hand, five countries covered in
the last Report—Angola, Libya, Syria, Timor-Leste,
and Tunisia—are not covered this year because of
insufficient or unreliable data. Thus this year’s edition
has a net increase in country coverage for a total
of 140 economies this year—one more than in the
2011 Report—covering all of the world’s regions and
accounting for over 98 percent of world GDP.