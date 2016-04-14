#########################################
## Gapfilling for resilience SPP
#########################################
library(dplyr)

# No gapfilling for these data layers, but still need to make the _gf data layers

spp <- read.csv('globalprep/SPP_ICO/v2015/data/spp_status_global.csv') %>%
  mutate(score = 0)
write.csv(spp, "globalprep/SPP_ICO/v2015/data/spp_status_global_gf.csv", row.names = FALSE)


spp_3nm <- read.csv('globalprep/SPP_ICO/v2015/data/spp_status_3nm.csv') %>%
  mutate(score = 0)
write.csv(spp_3nm, "globalprep/SPP_ICO/v2015/data/spp_status_3nm_gf.csv", row.names=FALSE)
