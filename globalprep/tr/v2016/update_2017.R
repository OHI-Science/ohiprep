#### 2017 update for new ohi-global framework

library(dplyr)
library(tidyr)

#warnings
warn <- read.csv("globalprep/tr/v2016/output/tr_travelwarnings.csv")
spread(warn, year, multiplier) # really only two years of data

warn <- read.csv("globalprep/tr/v2016/output/tr_travelwarnings.csv") 

warn <- warn %>%
  dplyr::filter(year >= 2015)

write.csv(warn, "globalprep/tr/v2016/output/tr_travelwarnings_updated.csv", row.names=FALSE)

# sustainability

# WEF TTCI data does not appear compatible across years.  May need to rescale.
# Also check for potential differences in gapfilling in 2014 processing. For now will just 
# include the most recent data.

#sus <- read.csv("globalprep/tr/v2015/data/tr_sustainability_2014.csv") %>%
#  mutate(year = 2014)

# sustain <- rbind(sus, sus2) %>%
#   select(rgn_id, year, S_score)

sus2 <- read.csv("globalprep/tr/v2015/data/tr_sustainability.csv") %>%
  mutate(year = 2015) %>%
  select(rgn_id, year, S_score)

write.csv(sus2, "globalprep/tr/v2015/data/tr_sustainability_updated.csv", row.names=FALSE)

