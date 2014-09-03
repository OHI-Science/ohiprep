########################################################
##  Antarctica Livelihoods subgoal
##  May 13 2015
##  MRF
#######################################################
library(dplyr)
library(reshape2)
rm(list = ls())

dir_data <- "Antarctica/AQ_ECO_v2013"

# region names
rgns <- read.csv("Antarctica/Other_v2013/rgn_labels_ccamlr.csv", stringsAsFactors=FALSE)

# relevant years
status_year <- 2013 #2104 data appears incomplete
trend_years <-  (status_year-5):status_year


## Fishing crew ----
f_crew<-read.csv(file.path(dir_data, "raw/fis_crew.csv"))

f_crew_melt <- melt(f_crew, id=c("FLAG_STATE", "CALL_SIGN", "SeasonAbbr", "CREW.N"))
f_crew_melt$variable <- gsub("X","", f_crew_melt$variable)
f_crew_melt <- f_crew_melt %.%
  filter(value == 1,
         SeasonAbbr != 2014) %.%
  select(year=SeasonAbbr, crew_f=CREW.N, ccamlr_id3=variable) %.%
  left_join(rgns, by=c("ccamlr_id3")) %.%
  select(sp_id, year, crew_f) 

f_crew_summary <- f_crew_melt %.%
  group_by(sp_id, year) %.%
  summarise(crew_f=sum(crew_f)) 

# fill in missing values
all.values <- expand.grid(sp_id = unique(f_crew_summary$sp_id),
                          year = unique(f_crew_summary$year))
f_crew_summary <- merge(all.values, f_crew_summary, all=TRUE, by=c("sp_id", "year"))

f_crew_summary$crew_f[is.na(f_crew_summary$crew_f)] <- 0 

ECOdata  <- f_crew_summary %.%
  mutate(sector="cf") %.%
  select(sp_id, sector, year, crew=crew_f)


## Tourism crew ----
tr_n <- read.csv(file.path(dir_data, 'raw/TR_data_ext.csv')) 
site <- read.csv(file.path(dir_data, "tmp/Sites_CCAMLR.csv"))
setdiff(tr_n$Site_name, site$Site_name)
setdiff(site$Site_name, tr_n$Site_name)
# these get cut for not being in the CCAMLR region:
# South Bay, Trinity Island-Mikkelsen Hrbr, Campbell Island
# these get cut for lack of information:
# Unknown Name, FAST ICE
tr_n <- tr_n %.%
  filter(!(Site_name %in% c("South Bay", "Trinity Island-Mikkelsen Hrbr", "Campbell Island",
                            "Unknown Name", "FAST ICE")))
setdiff(tr_n$Site_name, site$Site_name)
setdiff(site$Site_name, tr_n$Site_name)

tourists_summary <- tr_n %.%
  left_join(site, by="Site_name") %.%
  group_by(sp_id, year) %.%
  summarise(Tourist_days_eq=sum(Total))

# fill in missing values
all.values <- expand.grid(sp_id = unique(tourists_summary$sp_id),
                          year = unique(tourists_summary$year))
tourists_summary <- merge(all.values, tourists_summary, all=TRUE, by=c("sp_id", "year"))
tourists_summary$Tourist_days_eq[is.na(tourists_summary$Tourist_days_eq)] <- 0 


avg_stay<-19 # use median duration of Antarctica tours
crew_2_tour<-0.524666037013717 # median ratio of crew+staff/tourists, 
# from: ANT_TR_crew_ratio.xlsx

# divide tourists by average length of stay, to get tourist numbers & multiply tourist numbers by crew/tourists ratio to get crew
tourists_summary$crew_tr<-tourists_summary$Tourist_days_eq/avg_stay*crew_2_tour # use this to weight the contribution of TR to the LIV score in each year of the trend


tourists_summary <- tourists_summary %.%
  mutate(sector="tour") %.%
  select(sp_id, sector, year, crew=crew_tr)


## Final organization and save ----
ECOdata <- rbind(ECOdata, tourists_summary)

write.csv(ECOdata, file.path(dir_data, "data/ECOdata.csv"), row.names=FALSE, na="")

 