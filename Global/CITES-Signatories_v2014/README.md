GL-CITES-Signatories_v2013
===========================

Final file for model 
====================

GL-CITES-Signatories_v2013-cleaned.csv

Raw data files downloaded
=========================

Data accessed from CITES: Convention on International Trade in Endangered Species of Wild Fauna and Flora. 
www.cites.org. 

\*\*Data accessed April 2013: 178 total. 3 new countries are signatories since 2012 access: 

* Bahrain
* Maldives
* Lebanon

http://www.cites.org/eng/disc/parties/chronolo.php
copied and saved into CITES_MemberCountries_2013Apr.xlsx
saved as CITES_MemberCountries_2013Apr.csv (metadata removed and pasted below).


Cleaning
========

Cleaned data has been processed from raw using clean_CITES.r and add_ISO.r:

* clean_CO.r loads data from CITES_MemberCountries_2013Apr.csv and calls add_ISO.r to save as GL-CITES-Signatories_v2013-cleaned.csv

Methods
=======

An updated list of CITES signatories was accessed in April 2013 (http://www.cites.org/eng/disc/parties/chronolo.php). CITES is the Convention on International Trade in Endangered Species of Wild Fauna and Flora, and three additional countries had joined as CITES signatories since the 2012 OHI assessment: Bahrain, Maldives, and Lebanon. 


Metadata
========

The CITES Signatories list has been updated for the 2013 OHI assessment. Three new countries were added as signatories to CITES since the 2012 OHI assessment: Bahrain, Maldives and Lebanon.

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