##########################################
## organization of ICO data for toolbox
## MRF: June 24 2016
##########################################
library(dplyr)

data <- read.csv("globalprep/spp_ico/v2016/output/ico_status_raw.csv") %>%
  dplyr::select(rgn_id, sciname, year, category=cat)

data[duplicated(data), ]
table(data$category)

write.csv(data, "ico_spp_iucn_status.csv", row.names=FALSE)
