###############################################################################
#### OHI Global MAR status and trend files in ‘mariculture_status’ folder
###############################################################################

### 3 sub-folders ###

tmp - documenting species name replacements to match Trujillo scores with FAO harvest
raw - 
data - layer .csv to be uploaded into the toolbox

### files description ###

 tmp files: 
 1) “Truj_sp_nm_replace.csv”
 documents some of the species names found in the FAO mariculture harvest data that were replaced to match the sustainability scores
the file has a comments field explaining whether the species was excluded, renamed, or whether the value from a similar species was used instead. NOTE: this is not a comprehensive list of the name substitutions

 2) “Truj_fao_sp_replace.csv”
lists some species for which there are Trujillo sustainability scores but did not match any species in the FAO mariculture harvest data. For each, the country where that species was assessed by Trujillo is also listed. These were compared manually to check whether the  mismatch was due to a different spelling of the species name, or simply the species no longer occurs in the mariculture data-base, or it is excluded from mariculture analyses because not for food purposes or strictly freshwater. A comment field describes which of these is true for each species name.

 raw files:
 1) “updated 2014_sp_list_v2”
list of unique species names occurring in the latest version of the FAO mariculture harvest database (queried on FishStatJ on May 20, 2015). 
Each species is associated with a code that tells whether to exclude it because freshwater or not cultivated for food purposes (seaweeds where uses are mixed, or mainly industrial, were excluded). Each species is also renamed if this helps match it to the Trujillo sustainability scores. Species are also categorized by broad taxonomic groups, represented by a letter code, that are used to gap-fill Trujillo sustainability scores when scores aren’t available at finer taxonomic level.
NOTE: This needs to be updated in the coming years if new species appear in the database.

 2) “Truj_sp_names”
list of unique species names occurring in the original Trujillo sustainability scores table

 3) “Truj_label_sust”
Trujillo sustainability scores for the original country-species-environment/fao region combos, and gap-filler values obtained averaging scores at several levels of aggregation. The levels of aggregation for the score are specified in the field ‘match_type’ (‘c’ = country, ’sp’=species, ‘rgn’=FAO region, ‘env’= brakish or freshwater environment), whether these are gap-filler values or originals is specified in the field ‘gapfill’
 4) “mariculture_status_raw”
Saved version of the data
 5) “MAR_data_2015_nofreshwater”
FAO mariculture harvest data with values from freshwater environment excluded (queried on FishStatJ on May 20, 2015).

 data files:
1) mar_sustainability_score_2015a_lyr
rgn_id = ohi ID
species = species name
sust_coeff = sustainability coefficient

*******(NOTE that two regions have two different stocks of the same species that have
different sustainability coefficients, which harvest tonnage gets assigned to which of the two stocks is impossible to determine because the two stocks are differentiated only through the species code, which is unique for all region-species-sustainability combinations)************

# 3) mar_harvest_tonnes_2015a_lyr
species code, ohi region, harvested tonnes

# 4) mar_harvest_species_2015a_lyr
species codes and species

##################################################################################
##### other changes made to match FAO and Trujillo data:

# Countries changed from Trujillo original:
FAOcountry	TrujilloCountry
Iran (Islamic Republic of)	Iran
Faroe Islands	Faeroe Ils.
Korea, Dem. People's Rep	Korea, Dem.
Republic of Korea	Korea
Russian Federation	Russian Fed.
Taiwan Province of China	Taiwan
United Kingdom of Great Britain and Northern Ireland	United Kingdom
United States of America	U.S. of America
Venezuela, Boliv Rep of	Venezuela

# FAO areas replaced from Trujillo original:
FAOarea	TrujilloRegion
Atantic	Atl
Pacific	Pac
Mediterranean and Black Sea	Med
Indian Ocean, Eastern	India
Indian Ocean, Eastern	Ind
Asia - Inland waters	East
Africa - Inland waters	Med

# Suffix removed because a unique time series reported for species-country pair:
Suffix	For
(Med)	Egypt, Flathead grey mullet
(India)	Indonesia, Banana prawn
(India)	Indonesia, Giant tiger prawn
(India)	Malaysia, Giant tiger prawn (only the "Inland waters" one is in the timeseries)
(Med)	Morocco_Pacific cupped oyster (only the Atlantic one appears int eh timeseries)
(Ind)	Thailand_Giant tiger prawn (only listed for Asia inland waters)
(Pac)	Whiteleg shrimp (br) §(only reported for US and only brackish)
(East)	Asia - Inland waters, (deleted just "(East)")
(med)	replaced with (Med)

removed: 
Taiwan Province of China	Groupers nei (br)
Taiwan Province of China	Groupers nei (Pac) (br)
because had the same sustainability as
Taiwan Province of China	Groupers nei