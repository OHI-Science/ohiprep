## checking on FIS data

eez <- read.csv("Global/NCEAS-Fisheries_2014a/tmp/EEZlookup.csv")

eez_old <- read.csv("Global/NCEAS-Fisheries_2014a/data/snk_fis_proparea_saup2rgn_lyr.csv")

setdiff(eez_old$saup_id, eez$SAUP_C_NUM)
setdiff(eez$SAUP_C_NUM, eez_old$saup_id)

eez[eez$OHI_2013_EEZ_ID == 176, ]
eez_old[eez_old$rgn_id == 81,]


eez[eez$SAUP_C_NUM == 274, ] #Gaza strip

eez[eez$SAUP_C_NUM == 197, ] #North Cyprus
eez[eez$SAUP_C_NUM == 198, ] #South Cyprus
eez[eez$OHI_2013_EEZ_ID == 81, ]
eez_old[eez_old$rgn_id == 81,]

eez[eez$SAUP_C_NUM == 360, ] #Indonesia 
eez[eez$OHI_2013_EEZ_ID == 216, ]
eez_old[eez_old$rgn_id == 216,]

eez[eez$SAUP_C_NUM == 392, ] #Japan
eez[eez$OHI_2013_EEZ_ID == 210, ] #Japan
eez_old[eez_old$rgn_id == 210,]

eez[eez$SAUP_C_NUM == 644, ] #Russian Fed (Asia) - no OHI 2013 reporting region


eez[eez$OHI_2013_EEZ_ID == 163,]
eez[eez$SAUP_C_NUM == 847, ] #United States, East Coast and Gulf of Mexico

eez[eez$SAUP_C_NUM == 840, ] #United States, Contiguous states
eez[eez$OHI_2013_EEZ_ID == 163, ] 
eez_old[eez_old$rgn_id == 163,]
