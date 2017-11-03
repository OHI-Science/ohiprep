##############################
## Creating gapfilling files for mpas
## There was no gapfilling for these data

library(dplyr)

res_eez <- read.csv("globalprep/lsp/v2017/output/mpa_eez_updated_resilience.csv")%>%
  mutate(resilience.score = 0) 

write.csv(res_eez, "globalprep/lsp/v2017/output/mpa_eez_updated_resilience_gf.csv", row.names=FALSE)

res_3nm <- read.csv("globalprep/lsp/v2017/output/mpa_3nm_updated.csv")%>%
  mutate(resilience.score = 0) 

write.csv(res_3nm, "globalprep/lsp/v2017/output/mpa_3nm_updated_gf.csv", row.names=FALSE)

inland <- read.csv("globalprep/lsp/v2017/output/lsp_prot_area_inland1km.csv") %>%
  mutate(a_prot_1km = 0)

write.csv(inland, "globalprep/lsp/v2017/output/lsp_prot_area_inland1km_gf.csv", row.names=FALSE)

offshore <- read.csv("globalprep/lsp/v2017/output/lsp_prot_area_offshore3nm.csv") %>%
  mutate(a_prot_3nm = 0)

write.csv(offshore, "globalprep/lsp/v2017/output/lsp_prot_area_offshore3nm_gf.csv", row.names=FALSE)
