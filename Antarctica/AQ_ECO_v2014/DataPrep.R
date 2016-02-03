########################################################
##  Antarctica Economies subgoal
##  May 13 2015
##  MRF
#######################################################
library(dplyr)
library(tidyr)

dir_data <- "Antarctica/AQ_ECO_v2014"

# region names
rgns <- read.csv("Antarctica/Other_v2014/rgn_labels_ccamlr.csv", stringsAsFactors=FALSE)

# relevant years
status_year <- 2015 
trend_years <-  (status_year-4):status_year


## Fishing crew ----
f_crew<-read.csv(file.path(dir_data, "raw/fis_crew_2015.csv"))

f_crew_melt <- gather(f_crew, "ccamlr_id3", "value", starts_with("X"))
f_crew_melt <- f_crew_melt %>%
  mutate(ccamlr_id3 = gsub("X", "", ccamlr_id3)) %>%
  filter(value==1) %>%
  left_join(rgns, by="ccamlr_id3") %>%
  select(sp_id, year=SeasonAbbr, crew_f=CREW.N)

f_crew_summary <- f_crew_melt %>%
  group_by(sp_id, year) %>%
  summarise(crew_f=sum(crew_f)) %>%
  data.frame()


# fill in missing values
all.values <- expand.grid(sp_id = unique(f_crew_summary$sp_id),
                          year = unique(f_crew_summary$year))
f_crew_summary <- merge(all.values, f_crew_summary, all=TRUE, by=c("sp_id", "year"))

f_crew_summary$crew_f[is.na(f_crew_summary$crew_f)] <- 0 

ECOdata  <- f_crew_summary %>%
  mutate(sector="cf") %>%
  select(sp_id, sector, year, crew=crew_f)


## Tourism crew ----
## Based on the # of tourists

tour <- read.csv("Antarctica/AQ-Tourism_v2014/data/tr_days.csv")

avg_stay <- 19 # use median duration of Antarctica tours
crew_2_tour <- 0.524666037013717 # median ratio of crew+staff/tourists, 
# from: ANT_TR_crew_ratio.xlsx

# divide tourists by average length of stay, to get tourist numbers & multiply tourist numbers by crew/tourists ratio to get crew
tour <- tour %>%
  mutate(crew_tr = days/avg_stay * crew_2_tour) %>%
  mutate(sector = "tour") %>%
  select(sp_id, sector, year, crew=crew_tr)


## Final organization and save ----
ECOdata <- rbind(ECOdata, tour)

write.csv(ECOdata, file.path(dir_data, "data/ECOdata.csv"), row.names=FALSE, na="")

 