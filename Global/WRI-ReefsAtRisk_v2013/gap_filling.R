## create gap-filling dataset for cyanide and blast data
## no gap-filling for these data (so should be easy!)

library(dplyr)

cyanide <- read.csv("Global/WRI-ReefsAtRisk_v2013/data/gl_thr_poison_3nm_rgn2013.csv") %>%
  mutate(gap_fill = ifelse(!is.na(score) & score > 0, 0, NA)) %>%
  select(rgn_id, gap_fill)

write.csv(cyanide, "Global/WRI-ReefsAtRisk_v2013/data/gl_thr_poison_3nm_rgn2013_gapfill.csv", row.names=FALSE)


blast <- read.csv("Global/WRI-ReefsAtRisk_v2013/data/gl_thr_blast_3nm_rgn2013.csv") %>%
  mutate(gap_fill = ifelse(!is.na(score) & score > 0, 0, NA)) %>%
  select(rgn_id, gap_fill)

write.csv(blast, "Global/WRI-ReefsAtRisk_v2013/data/gl_thr_blast_3nm_rgn2013_gapfill.csv", row.names=FALSE)
