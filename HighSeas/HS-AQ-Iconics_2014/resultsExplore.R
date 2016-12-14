library(plyr)
library(dplyr)
library(ggplot2)

### data summary for iconics

AQ_icons <- read.csv("N:\\model\\GL-HS-AQ-Iconics_v2013\\data\\Antartica_icons.csv")
table(AQ_icons$popn_trend)

HS_icons <- read.csv("N:\\model\\GL-HS-AQ-Iconics_v2013\\data\\HighSeas_icons.csv")
table(HS_icons$popn_trend)

iconics <- read.csv("N:\\model\\GL-HS-AQ-Iconics_v2013\\data\\rgn_ico_status_2013_HS_AQ.csv")
richness <- read.csv("N:\\model\\GL-HS-AQ-SpeciesRichness_v2013\\data\\rgn_spp_score_2013_HS_AQ.csv")

richness <- richness %.%
  select(rgn_id, "rich_status"=score) %.%
  mutate(rich_status=rich_status/100)

allData <- iconics %.%
  rename(c(score="iconics_status"))%.%
  left_join(richness, by="rgn_id")

p <- ggplot(allData, aes(y=iconics_status, x=rich_status, colour=rgn_type, group=rgn_type))
p+geom_point(size=5) + 
  geom_smooth(method="lm") +
  geom_abline(intercept=0, slope=1, size=2)
ggsave("N:\\model\\GL-HS-AQ-Iconics_v2013\\iconicsVSdiversity.png")
