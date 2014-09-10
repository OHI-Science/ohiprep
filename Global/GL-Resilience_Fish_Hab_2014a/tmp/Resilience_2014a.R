###################################################################
########## CALCULATE RESILIENCE COMBOS for FISHERIES and HABITATS
###################################################################
# Step 1. # load packages, data, and clean-up

library(gdata)
library(stringr)
library(ohicore)

source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)

dir_d = '../ohiprep/Global/GL-Resilience_Fish_Hab_2014a' # set folder where files are saved
MPA_data = '../ohiprep/Global/WDPA-MPA_v2014/data'
rgn_data = '../ohiprep/tmp/install_local/ohi-global-master/eez2014/layers'

# following files 'as is' from Nature 2012 (disaggregated for 'children' new ohi2013 regions) - no year field bc no updates
r_hab<-read.csv(file.path(dir_d, 'tmp/r_habitat_2013a.csv')) ; head(r_hab)
r_mora<-read.csv(file.path(dir_d, 'tmp/r_mora_2013a.csv'))  ; head(r_mora)
r_mora_s4<-read.csv(file.path(dir_d, 'tmp/r_mora_s4_2013a.csv')) ; head(r_mora_s4) # a table with the list of layers that apply to each resilience layer

r_combos<-read.csv(file.path(dir_d, 'tmp/r_combos.csv')) ; head(r_combos)
r_combos[r_combos==0]<-NA # change 0s to NAs so they can be excluded when calculating the mean (see below)
# remove year identifiers from the layer names
r_combos$Layer<-str_split_fixed(r_combos$Layer,'2013a',2)[,1]

r_join<-read.csv(file.path(dir_d, 'tmp/r_join.csv')) ; head(r_join) #  a table with the complete list of v2013a region ids and a "join" column
r_combo<-join(r_join,r_combos) # create the "weights" matrix, where a 1 means that resilience indicator is included in that resilience layer combo, and 0 means it's omitted
names(r_combo)[5:9]<-paste("w_",names(r_combo)[5:9],sep="")
r_combo<-rename(r_combo,c(rgn_id_2013="rgn_id"))
r_combo<-r_combo %>% select(rgn_id,Layer,w_Mora, w_Mora_s4, w_CBD_hab, w_MPA_coast, w_MPA_eez) 

names(r_hab)[2]<-"hab"
names(r_mora)[2]<-"mora"
names(r_mora_s4)[2]<-"mora_s4"

# MPA area data updated from WDPA (see containing folder ReadMe) - MPAs within 3nm
r_MPA_raw<-read.csv(file.path(MPA_data,'lsp_protarea_offshore3nm.csv') ); head(r_MPA_raw)
rgn<-read.csv(file.path(rgn_data,'rgn_global.csv')) # read in full list of regions... 
r_MPA<-left_join(rgn,r_MPA_raw) ; length(unique(r_MPA$rgn_id)) # ...to fill in missing regions with 0s
r_yr<-min(r_MPA_raw$year[r_MPA_raw$year>0]) # place-holder year for missing records or missing year
r_MPA$year<-replace(r_MPA$year, r_MPA$year==0, r_yr-1) # replace years reported as '0' with place-holder year (min_year-1 = 1882-1 = 1881)
r_MPA$year<-replace(r_MPA$year, is.na(r_MPA$year), r_yr-1) # replace year in empty time-series that were added by joining the regions
r_MPA$area_km2<-ifelse(is.na(r_MPA$area_km2),0,r_MPA$area_km2)

area_3nm<-read.csv(file.path(rgn_data, 'rgn_area_offshore3nm.csv')) ; head(area_3nm) 
names(area_3nm)[2]<-"tot_area_km2"
r_MPA<-join(r_MPA,area_3nm) ; length(unique(r_MPA$rgn_id))# join 3nm coastal area
r_MPA<-arrange(r_MPA,rgn_id,year)

# MPA area data updated from WDPA (see containing folder ReadMe) - MPAs within EEZ
r_MPA_eez<-read.csv(file.path(MPA_data,'lsp_protarea_eez.csv') ); head(r_MPA_eez)
MPA_z<-left_join(rgn,r_MPA_eez) ; length(unique(MPA_z$rgn_id)) # ...to fill in missing regions with 0s
MPA_z$year<-replace(MPA_z$year, MPA_z$year==0, r_yr-1) # replace years reported as '0' with place-holder year (min_year-1 = 1882-1 = 1881)
MPA_z$year<-replace(MPA_z$year, is.na(MPA_z$year), r_yr-1) # replace year in empty time-series that were added by joining the regions
MPA_z$area_km2<-ifelse(is.na(MPA_z$area_km2),0,MPA_z$area_km2)

area_eez<-read.csv(file.path(rgn_data, 'rgn_area.csv')) ; head(area_eez) 
area_eez<-rename(area_eez,c("area_km2"="tot_area_km2"))
MPA_z<-join(MPA_z,area_eez) ; length(unique(MPA_z$rgn_id))# join eez area
MPA_z<-arrange(MPA_z,rgn_id,year)

#### Step 2 ## process MPA data to get a score for each OHI scenario
r_MPAc12<-r_MPA %>% filter(year<2012) %>% group_by(rgn_id,tot_area_km2) %>%  summarise(mpa_area=sum(area_km2)) %>% mutate(scen='2012a')
r_MPAc13<- r_MPA %>% filter(year<2013) %>% group_by(rgn_id,tot_area_km2) %>%  summarise(mpa_area=sum(area_km2)) %>% mutate(scen='2013a')
r_MPAc14<-r_MPA %>% group_by(rgn_id,tot_area_km2) %>%  summarise(mpa_area=sum(area_km2)) %>% mutate(scen='2014a')
r_MPAc<-rbind(r_MPAc12,r_MPAc13,r_MPAc14)

# calculate MPA score (where MPA areas occupying 30% of the coastal area within 3nm gets a score of 1)
r_MPAc<-r_MPAc %>% mutate(score=ifelse(mpa_area/tot_area_km2*1/0.3>1,1,mpa_area/tot_area_km2*1/0.3))
r_MPA3nm<-r_MPAc %>% select(rgn_id, scen, 'MPA_coast'=score)

# same for MPAs within EEZ area
r_MPAe12<-r_MPA %>% filter(year<2012) %>% group_by(rgn_id,tot_area_km2) %>%  summarise(mpa_area=sum(area_km2)) %>% mutate(scen='2012a')
r_MPAe13<- r_MPA %>% filter(year<2013) %>% group_by(rgn_id,tot_area_km2) %>%  summarise(mpa_area=sum(area_km2)) %>% mutate(scen='2013a')
r_MPAe14<-r_MPA %>% group_by(rgn_id,tot_area_km2) %>%  summarise(mpa_area=sum(area_km2)) %>% mutate(scen='2014a')
r_MPAe<-rbind(r_MPAe12,r_MPAe13,r_MPAe14)

# calculate MPA score (where MPA areas occupying 30% of the coastal area within 3nm gets a score of 1)
r_MPAe<-r_MPAe %>% mutate(score=ifelse(mpa_area/tot_area_km2*1/0.3>1,1,mpa_area/tot_area_km2*1/0.3))
r_MPAeez<-r_MPAe %>% select(rgn_id, scen, 'MPA_eez'=score)


## merge all resilience data and weights into a single dataframe
# sv_combo<-r_combo
# r_combo<-sv_combo
# r_combo<- inner_join(sv_combo,r_mora)
r_combo<- left_join(r_combo,r_mora) # Joining by: rgn_id
r_combo<- left_join(r_combo,r_mora_s4) # Joining by: rgn_id
r_combo<- left_join(r_combo,r_hab); dim(r_combo) # Joining by: rgn_id
r_combo<- join(r_MPA3nm,r_combo) # Joining by: rgn_id
r_combo<-r_combo[,c(1:2,4:12,3)]
r_combo<- left_join(r_combo,r_MPAeez); dim(r_combo) # Joining by: c("rgn_id", "scen")

# multiply each layer by its weight, and sum to obtain a resilience score per layer, region, per scenario
r_scores <-r_combo %>% group_by(rgn_id, scen, Layer) %>% mutate(score=sum(w_Mora*mora, w_Mora_s4*mora_s4, w_CBD_hab*hab, MPA_coast*w_MPA_coast, w_MPA_eez*MPA_eez,
                                                                        na.rm=T)/sum(w_Mora, w_Mora_s4, w_CBD_hab, w_MPA_coast, w_MPA_eez, na.rm=T)
                                                                        ) ; head(r_scores)
                                                                        
r_scores<-r_scores %>% select(rgn_id,  scen, score) # Layer is also included because they're all the variables in the grouping factor

# save as separate files for each layer-scenario combination

d<-r_scores
d<-rename(d,c('score'='resilience.score'))
d<-d[!is.na(d$Layer),]
d$Layer<-paste(d$Layer,d$scen,sep='')
d<-d[,-3]
d<-ungroup(d)
labels<-unique(d$Layer)

            for (i in labels) {
                              csv = sprintf('%s.csv', i)   
                              d2 = d %>% filter(Layer==i) %>% select (rgn_id, resilience.score)
                              write.csv(d2, file.path(dir_d, 'data', csv), row.names=F)
                             
                              stopifnot(anyDuplicated(d2[,c('rgn_id', 'year')]) == 0)
                              
                              }

