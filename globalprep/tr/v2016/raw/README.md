# Travel warnings

Information from here: https://travel.state.gov/content/passports/en/alertswarnings.html
(Do not include land-locked countries)

Data downloaded: 7/16/2016

A few updates from last year: 
* I changed the Philippines warning to be regional, which seemed more appropriate given the description
* Data prior to 2014 (2012-2013) is just a duplicate of the 2014 data, which seemed more accurate than what we had (N. Korea not included, etc.)
* Changed Israel to not be regional (I should check on this, but Gaza and West Bank probably shouldn't be included in Israel boundary...and these are highly recommended to avoid)
* If a country has regional warnings, these are given their own line in the data and rgn_name_full includes the region information

## A few notes about getting data:

Add the data to tr_travelwarnings_???.xls
Most of the data can be cut and paste (after it is checked) from the previous year (update the data and year information).

If different regions have different warnings, these are put on two lines and combined in the R script:

assess_year  date  rgn_name  rgn_name_full 
2016		4-Feb-15	Cameroon	Cameroon		    risk 				
2016		4-Feb-15	Cameroon	Cameroon (North and Far North region)				avoid_all		regional

inform: information travelor should be aware of (election violence, be aware due to crime, etc)
risk: risks that trevelors should be aware of ("consider carefully risks of travel", "warns of risks")
avoid_nonessential travel: "defer non-essential travel"
avoid_all: "avoid all travel"
gtfo: get the f*** out!!!
regional: added if the warning only applies to specific regions


