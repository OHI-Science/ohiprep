######  Data prep for Fisheries catch

library(dplyr)
library(tidyr)

# label data:
rgn_labels <- read.csv('Antarctica/Other_v2014/rgn_labels_ccamlr.csv') %>%
  select(ASD = ccamlr_id3, sp_id)

# fishery data:
boat <- read.csv('Antarctica/AQ_FIS/v2015/raw/CCAMLR_2015_Statistical_Bulletin_Volume_27_SBeffort.txt')
catch <- read.csv("Antarctica/AQ_FIS/v2015/raw/CCAMLR_2015_Statistical_Bulletin_Volume_27_SBcatch.txt")

## determine proportion of catch with catch assessments:

## ANI Antarctic Icefish is not included even though there is stock assessment data because the 
## data are from Heard Island and South Georgia, areas we evaluate in the OHI global assessment

### Do not include following regions for ToothFish: 58.5.1, 58.5.2, 58.6, 58.7, 48.4
## This catch is for the island regions that are assessed in OHI global.


data_cut <- catch %>%
  left_join(boat) %>%
  filter(!(SpeciesCode == "ANI" & ASD %in% c('483', '5852'))) %>% #these were caught within island global eez
  filter(!(SpeciesCode %in% c("TOA", "TOP") & ASD %in% c('5851', '5852', '586', '587', '484'))) %>%
  filter(ASD %in% rgn_labels$ASD)  # only include labels included in our data

summary <- data_cut %>%
  mutate(assessed = ifelse(SpeciesCode %in% c("KRI", "TOA", "TOP"), "yes", "no")) %>%
  group_by(assessed) %>%
  summarize(totalCatch = sum(CatchWeight, na.rm=TRUE))
summary
7358173085/(7358173085 + 1675615172) # ~81% of catch is those 4 species.  Pretty good!

## Tooth without Krill
summary_tooth <- data_cut %>%
  filter(SpeciesCode != "KRI") %>%
  mutate(assessed = ifelse(SpeciesCode %in% c("TOA", "TOP"), "yes", "no")) %>%
  group_by(assessed) %>%
  summarize(totalCatch = sum(CatchWeight, na.rm=TRUE))
summary_tooth
151648479/(151648479 + 8882139778) # ~2% of catch is toothfish (with Krill)
151648479/(1675615172+151648479) # ~ 8% of the catch is toothfish (minus Krill)


summary_krill <- data_cut %>%
  mutate(assessed = ifelse(SpeciesCode %in% c("KRI"), "yes", "no")) %>%
  group_by(assessed) %>%
  summarize(totalCatch = sum(CatchWeight, na.rm=TRUE))
summary_krill
7206524606/(7206524606 + 1827263651) # 80% of catch is Krill


data <- data_cut %>%
  filter(SpeciesCode %in% c('KRI', 'TOA', 'TOP')) %>%
  group_by(ASD, SeasonYear, SpeciesCode) %>%
  summarize(catch = sum(CatchWeight, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(catch = catch*0.001) %>%
  left_join(rgn_labels, by="ASD") %>%
  select(sp_id, species_code = SpeciesCode, year=SeasonYear, catch)

write.csv(data, "Antarctica/AQ_FIS/v2015/data/catch.csv", row.names=FALSE)


