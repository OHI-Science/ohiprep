### checking a few things on lsp scores for 2017
### These two regions had large changes, but they make sense given the data

### Also, putting the resilience into the new format with all years in one file.

library(dplyr)

### decreased -11.26 status
new_lsp_mar <- read.csv("globalprep/lsp/v2017/output/lsp_prot_area_offshore3nm.csv")
new_lsp_mar %>%
  filter(rgn_id == 147)

new_lsp_land <- read.csv("globalprep/lsp/v2017/output/lsp_prot_area_inland1km.csv")
new_lsp_land %>%
  filter(rgn_id == 147)

old_lsp_mar <- read.csv("globalprep/lsp/v2016/output/lsp_protected_offshore3nm.csv")
old_lsp_mar %>%
  filter(rgn_id == 147)

old_lsp_land <- read.csv("globalprep/lsp/v2016/output/lsp_protected_inland1km.csv")
old_lsp_land %>%
  filter(rgn_id == 147)

# increased 65.6 in status
new_lsp_mar <- read.csv("globalprep/lsp/v2017/output/lsp_prot_area_offshore3nm.csv")
new_lsp_mar %>%
  filter(rgn_id == 173)

new_lsp_land <- read.csv("globalprep/lsp/v2017/output/lsp_prot_area_inland1km.csv")
new_lsp_land %>%
  filter(rgn_id == 173)

old_lsp_mar <- read.csv("globalprep/lsp/v2016/output/lsp_protected_offshore3nm.csv")
old_lsp_mar %>%
  filter(rgn_id == 173)

old_lsp_land <- read.csv("globalprep/lsp/v2016/output/lsp_protected_inland1km.csv")
old_lsp_land %>%
  filter(rgn_id == 173)

## 3 nm
data <- data.frame()

for (year in 2011:2017){ # year = 2012
  data_new <- read.csv(sprintf("globalprep/lsp/v2017/output/mpa_3nm_%s.csv", year))
  
  data_new <- data_new %>%
    mutate(year = year) %>%
    select(rgn_id, year, resilience.score)
  
  data <- rbind(data, data_new)
  
}

write.csv(data, "globalprep/lsp/v2017/output/mpa_3nm_updated.csv", row.names=FALSE)


# eez
data <- data.frame()

for (year in 2011:2017){ # year = 2012
  data_new <- read.csv(sprintf("globalprep/lsp/v2017/output/mpa_eez_%s_resilience.csv", year))
  
  data_new <- data_new %>%
    mutate(year = year) %>%
    select(rgn_id, year, resilience.score)
  
  data <- rbind(data, data_new)
  
}

write.csv(data, "globalprep/lsp/v2017/output/mpa_eez_updated_resilience.csv", row.names=FALSE)


