### Fisheries data

catch <- read.csv("globalprep/fis/v2016/data/mean_catch.csv")
  
bmsy <- read.csv("globalprep/fis/v2016/data/fis_bbmsy_gf.csv", stringsAsFactors = FALSE) %>%
  mutate(bmsy_data_source = ifelse(is.na(bmsy_data_source), "NONE", bmsy_data_source))

bmsy %>%
  group_by(year, bmsy_data_source) %>%
  summarize(tons=sum(mean_catch, na.rm=TRUE)) %>%
  filter(year==2010)

13779230/(13779230 + 63208853 + 28449717)
13779230/(13779230 + 28449717)

bmsy %>%
  mutate(RAM_gapfilled = ifelse(is.na(RAM_gapfilled), "OTHER", "RAM")) %>%
  group_by(year, RAM_gapfilled) %>%
  summarize(tons=sum(mean_catch, na.rm=TRUE)) %>%
  filter(year==2010)

13786871/sum(bmsy$mean_catch, na.rm=TRUE)
