##################################################################
########## CALCULATE RESILIENCE COMBOS for FISHERIES and HABITATS
##################################################################
# Step 1. # load packages, data, and clean-up

library(gdata)
library(stringr)
library(ohicore)

source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)

# set directory paths
dir_d = '../ohiprep/Global/GL-Resilience_Fish_Hab_2014a' # set folder where files are saved
MPA_data = '../ohiprep/Israel/MPAs' # change to appropriate path on your file system

# load resilience data used in Nature 2012 - no year field because data were tken from a publication and not updated since
r_hab<-read.csv(file.path(dir_d, 'tmp/r_habitat_2013a.csv')) ; head(r_hab)
r_mora<-read.csv(file.path(dir_d, 'tmp/r_mora_2013a.csv'))  ; head(r_mora)
r_mora_s4<-read.csv(file.path(dir_d, 'tmp/r_mora_s4_2013a.csv')) ; head(r_mora_s4) # a table with the list of layers that apply to each resilience layer
names(r_hab)[2]<-"hab"
names(r_mora)[2]<-"mora"
names(r_mora_s4)[2]<-"mora_s4"
# load the matrix for how the data are combined in each combo layer
r_combos<-read.csv(file.path(dir_d, 'tmp/r_combos.csv')) ; head(r_combos)
r_combos[r_combos==0]<-NA # change 0s to NAs so they can be excluded when calculating the mean (see below)
r_combos$Join <- 79 # change the Join field to the Israel global rgn_id_2013 identifier (will be useful later)
r_combos$Layer<-str_split_fixed(r_combos$Layer,'_2013a',2)[,1] # remove year identifiers from the layer names

# load Israel-specific data for MPAs:
MPA_raw<-read.csv(file.path(MPA_data,'MPA_subregions.csv') ); head(MPA_raw)
names(MPA_raw) <- c('Region_Code', 'Name', 'MPA_area_km2', 'prop_MPA_coast', 'prop_MPA_eez')

# create a table with the complete list of sub-regions and a "join" field... 
r_join<-data.frame( cbind(MPA_raw[,1:2],'Join' = 79) )
# ...use it to create a list of resilience combo layers for each sub-region
r_combo<-join(r_join, r_combos) 
# and create the "weights" matrix, where a 1 means that resilience indicator is included in that resilience layer combo, and NA means it's omitted
names(r_combo)[5:9]<-paste("w_",names(r_combo)[5:9],sep="")

# select region 79 (=Israel) from the global layers (Mora, Mora_s4, CBD_hab), and rename the region id as 'Join':
i_mora <- filter(r_mora, rgn_id == 79) %>% rename( c('rgn_id' = 'Join'))
i_mora_s4 <- filter(r_mora_s4, rgn_id == 79) %>% rename( c('rgn_id' = 'Join'))
i_hab <- filter(r_hab, rgn_id == 79) %>% rename( c('rgn_id' = 'Join'))

## merge all resilience data and weights into a single dataframe
# use the 'Join' field to join these files to r_combo
r_combo <- left_join(r_combo, i_mora) # Joining by: Join
r_combo <- left_join(r_combo, i_mora_s4) # Joining by: Join
r_combo <- left_join(r_combo, i_hab); dim(r_combo) # Joining by: Join

r_combo <- r_combo %>% select(-(Join)) # remove the 'Join' field now that it's no longer needed

# calcualte the MPA scores per region, setting the maximum value at 30% of the total area 
# i.e. MPAs occupying 30% of water within 3nm score 1 for the MPA_coast indicator, MPAs occupying 30% of the EEZ score 1 for the MPA_eez indicator
# (the 'ifelse' statement ensures that if the MPA area exceeds 30%, the value is capped at 1 - in this case the MPAs are so small that it's not an issue)
MPA_raw <- MPA_raw %>% mutate (MPA_coast = ifelse( prop_MPA_coast/30>1, 1, prop_MPA_coast/30 ), MPA_eez = ifelse( prop_MPA_eez/30>1, 1, prop_MPA_eez/30 ))
MPA_scores <- select(MPA_raw, Region_Code, Name, MPA_coast, MPA_eez) # select only the layers needed for the calculation

# Join the MPA scores to r_combo by the Region_code 
r_combo <- left_join(r_combo, MPA_scores) # Joining by: c("Region_Code", "Name")

# calculate scores
r_scores <- r_combo %>% group_by(Region_Code, Name, Layer) %>% mutate(score = sum (w_Mora*mora, w_Mora_s4*mora_s4, w_CBD_hab*hab, MPA_coast*w_MPA_coast, w_MPA_eez*MPA_eez,
                                                                          na.rm=T)/sum(w_Mora, w_Mora_s4, w_CBD_hab, w_MPA_coast, w_MPA_eez, na.rm=T)
) ; head(r_scores)

## save as separate files for each layer

d <- r_scores
d <- rename(d,c('score'='resilience.score'))
# d<-d[!is.na(d$Layer),]
# d$Layer<-paste(d$Layer,d$scen,sep='')
# d<-d[,-3]
d <- ungroup(d)
d$Layer <- str_split_fixed(d$Layer,'r_',2)[,2]
labels <- unique(d$Layer)

            for (i in labels) {
                              csv = sprintf('%s_israel2014.csv', i)   
                              d2 = d %>% 
                                filter(Layer == i) %>% 
                                select (rgn_id = Region_Code, 
                                        resilience.score)
                              write.csv(d2, file.path(MPA_data, csv), row.names=F)
                             
                              #stopifnot(anyDuplicated(d2[,c('Region_Code')]) != 0)
                              
                              }

