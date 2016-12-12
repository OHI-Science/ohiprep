GL-CITES-Signatories_v2013
===========================

Final files for model 
=====================
v2015/data/rgn_cites_eez2012.csv
... through ...
v2015/data/rgn_cites_eez201x.csv


Raw data files downloaded
=========================
Data accessed from CITES: Convention on International Trade in Endangered Species of Wild Fauna and Flora. 
www.cites.org. 

\*\*Data accessed July 2015: 187 total. 6 new countries are signatories since 2012 OHI paper: 
* Bahrain
* Maldives
* Lebanon
* Angola
* Iraq
* European Union

http://www.cites.org/eng/disc/parties/chronolo.php
copied and saved into 
* cites_member_countries_2015-07.xlsx
* cites_member_countries_2015-07.csv


Cleaning
========
Cleaned data has been processed from raw using data_prep.R (previously clean_CITES.R) and add_rgn_id.R:
Also, join all other regions (boolean==0)


Methods
=======
CITES is the Convention on International Trade in Endangered Species of Wild Fauna and Flora.


Metadata
========
(in chronological order)

Order: Chronological order
ISO: Two-letter ISO country code
Region: CITES region
	+ 1 = Africa
	+ 2 = Asia
	+ 3 = Central and South America and the Caribbean
	+ 4 = Europe
	+ 5 = North America
	+ 6 = Oceania

Date 1	+ (A) Accession
	+ (Ac) Acceptance
	+ (Ap) Approval
	+ (C) Continuation
	+ (S) Succession
	+ (R) Ratification
Date 2Date of entry into force