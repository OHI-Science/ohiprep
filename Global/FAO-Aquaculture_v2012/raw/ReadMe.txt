
############################
##### Truj_actuals.csv:
############
(from tab 2 of ‘MAR_sustainability_processing.xlsx’
Trujillo data, (typo corrected: Russian Fed. Sea trout (med) -> Russian Fed. Sea trout (Med)), and with avg of waste.water.treatment, seed & feed = mariculture sustainability.


###### Truj_sp_nm_replace.csv
############## 
from Sp2011_Replacement tab in ‘MAR_sustainability_processing.xlsx’
name changes, exclusions and substitutions to match trujillo scores to FAO data

####### Truj_nm_cn_fao.csv
#################
from ‘Trujillo regions’ tab in ‘GL-FAO-Mariculture_v2011-cleanedForStatus_wInland_explore_.xlsx’
it converts fao species names in cases where there are different species time-series for a given country, depending on whether it is brackish/marine, or which part of the country’s coast (e.g. whiteleg shrimp in Colombia has 2 values depending on whether it is Atlantic or Pacific coast)

###### Truj_sp_cn_sust
from ‘Tb 1 - Trujillo scores’ tab in ‘GL-FAO-Mariculture_v2011-cleanedForStatus_wInland_explore_.xlsx’
sustainability scores for each species country pair


##### Truj_sp_sust
from ‘Tb 2 - sp replacements’ tab in ‘GL-FAO-Mariculture_v2011-cleanedForStatus_wInland_explore_.xlsx’
sustainability scores for each species (averaged across countries)

##### Truj_tax_sust
from ‘Tb 3a - taxon replacements’ tab in ‘GL-FAO-Mariculture_v2011-cleanedForStatus_wInland_explore_.xlsx’
sustainability scores for taxonomic groups (averaged across multiple species)

##### Truj_tax_nm_replace
from ‘Tb 3b - FAO sp-taxon list’ tab in ‘GL-FAO-Mariculture_v2011-cleanedForStatus_wInland_explore_.xlsx’
FAO species names and corresponding taxonomic groups

##################################################################################
####################
##### other changes made to match FAO and Trujillo data:

Countries changed from Trujillo original:
FAOcountry	TrujilloCountryIran (Islamic Republic of)	IranFaroe Islands	Faeroe Ils.Korea, Dem. People's Rep	Korea, Dem.Republic of Korea	KoreaRussian Federation	Russian Fed.Taiwan Province of China	TaiwanUnited Kingdom of Great Britain and Northern Ireland	United KingdomUnited States of America	U.S. of AmericaVenezuela, Boliv Rep of	Venezuela

FAO areas replaced from Trujillo original:
FAOarea	TrujilloRegionAtantic	AtlPacific	PacMediterranean and Black Sea	MedIndian Ocean, Eastern	IndiaIndian Ocean, Eastern	IndAsia - Inland waters	EastAfrica - Inland waters	Med

Suffix removed because a unique time series reported for species-country pair:
Suffix	For(Med)	Egypt, Flathead grey mullet(India)	Indonesia, Banana prawn(India)	Indonesia, Giant tiger prawn(India)	Malaysia, Giant tiger prawn (only the "Inland waters" one is in the timeseries)(Med)	Morocco_Pacific cupped oyster (only the Atlantic one appears int eh timeseries)(Ind)	Thailand_Giant tiger prawn (only listed for Asia inland waters)(Pac)	Whiteleg shrimp (br) §(only reported for US and only brackish)(East)	Asia - Inland waters, (deleted just "(East)")(med)	replaced with (Med)