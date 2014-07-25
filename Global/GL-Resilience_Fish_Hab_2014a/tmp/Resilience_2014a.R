##################################################################
########## CALCULATE RESILIENCE COMBOS for FISHERIES and HABITATS
##################################################################
# setup
library(gdata)
library(stringr)
library(ohicore)

source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)

dir_d = '../ohiprep/Global/GL-Resilience_Fish_Hab_2014a/tmp' # set folder where files are saved

MPA_data = '../ohiprep/Global/WDPA-MPA_v2014/data'
rgn_data = '../ohiprep/tmp/install_local/ohi-global-master/eez2014/layers'
#
r_hab<-read.csv(file.path(dir_d, 'r_habitat_2013a.csv')) ; head(r_hab)
r_mora<-read.csv(file.path(dir_d, 'r_mora_2013a.csv'))  ; head(r_mora)
r_mora_s4<-read.csv(file.path(dir_d, 'r_mora_s4_2013a.csv')) ; head(r_mora_s4) # a table with the list of layers that apply to each resilience layer
r_MPA_raw<-read.csv(file.path(MPA_data,'lsp_protarea_offshore3nm.csv') ); head(r_MPA_raw)
# r_MPAe12<-read.csv(file.path(dir_d, 'r_MPA_eez_2012.csv') ) ; head(r_MPAe12)
# r_MPAe13<-read.csv(file.path(dir_d, 'r_MPA_eez_2013.csv') ) ; head(r_MPAe13)
# r_MPAe14<-  data not found on Github                  
r_combos<-read.csv(file.path(dir_d, 'r_combos.csv')) ; head(r_combos)
r_combos[r_combos==0]<-NA # change 0s to NAs so they can be excluded from the averaging, as opposed to real 0s in the data
r_join<-read.csv(file.path(dir_d, 'r_join.csv')) ; head(r_join) #  a table with the complete list of v2013a region ids and a "join" column
r_combo<-join(r_join,r_combos) # create the "weights" matrix, where a 1 means that resilience indicator is included in that resilience layer combo, and 0 means it's omitted
names(r_combo)[5:9]<-paste("w_",names(r_combo)[5:9],sep="")
names(r_combo)[2]<-"rgn_id"
r_combo<-r_combo[,-1]

names(r_hab)[2]<-"hab"
names(r_mora)[2]<-"mora"
names(r_mora_s4)[2]<-"mora_s4"

area_3nm<-read.csv(file.path(rgn_data, 'rgn_area_offshore3nm.csv')) ; head(area_3nm) 
names(area_3nm)[2]<-"tot_area_km2"
r_MPA<-join(r_MPA_raw,area_3nm)# join 3nm coastal area
# get total MPA area for each scenario
r_MPAc12<-summarise(group_by(r_MPA[r_MPA$year<2012,],rgn_id,tot_area_km2),mpa_area=sum(area_km2))
r_MPAc13<-summarise(group_by(r_MPA[r_MPA$year<2013,],rgn_id,tot_area_km2),mpa_area=sum(area_km2))
r_MPAc14<-summarise(group_by(r_MPA,rgn_id, tot_area_km2),mpa_area=sum(area_km2))

# calculate MPA score (where MPA areas occupying 30% of the coastal area within 3nm gets a score of 1)
r_MPAc12$score<-ifelse(r_MPAc12$mpa_area/r_MPAc12$tot_area_km2*1/0.3>1,1,r_MPAc12$mpa_area/r_MPAc12$tot_area_km2*1/0.3) # calculate MPA score
r_MPAc13$score<-ifelse(r_MPAc13$mpa_area/r_MPAc13$tot_area_km2*1/0.3>1,1,r_MPAc13$mpa_area/r_MPAc13$tot_area_km2*1/0.3) 
r_MPAc14$score<-ifelse(r_MPAc14$mpa_area/r_MPAc14$tot_area_km2*1/0.3>1,1,r_MPAc14$mpa_area/r_MPAc14$tot_area_km2*1/0.3) 

names(r_MPAc14)[4]<-"MPA3nm_43"
names(r_MPAc13)[4]<-"MPA3nm_13"
names(r_MPAc12)[4]<-"MPA3nm_12"
# names(r_MPAe13)[2]<-"MPA3eez_13"
# names(r_MPAe12)[2]<-"MPA3eez_12"

############################################
##### need to finish updating script #####
install.packages("reshape")
library(reshape) # this library has fun "merge_recurse", which is missing in "reshape2"
r_combo2<-merge_recurse(list(r_combo,r_mora,r_mora_s4,r_hab,r_MPAc12,r_MPAe12,r_MPAc13,r_MPAe13),by="rgn_id")
r_combo2<-r_combo2[!is.na(r_combo2$Layer),] 

r_combo2[,9:15][is.na(r_combo2[,9:15])]<-0 # convert missing MPA values to real 0s
# multiply each layer by its weight
r_combo12<-data.frame(r_combo2$rgn_id,as.character(r_combo2$Layer),as.numeric(r_combo2$w_Mora*r_combo2$mora),r_combo2$w_Mora_s4*r_combo2$mora_s4,r_combo2$w_CBD_hab*r_combo2$hab,r_combo2$w_MPA_coast*r_combo2$MPA3nm_12,r_combo2$w_MPA_eez*r_combo2$MPA3eez_12)
names(r_combo12)<-c("rgn_id","Layer","r_Mora","r_Mora_s4","r_hab","r_MPAc","r_MPAeez")
r_combo13<-data.frame(r_combo2$rgn_id,as.character(r_combo2$Layer),r_combo2$w_Mora*r_combo2$mora,r_combo2$w_Mora_s4*r_combo2$mora_s4,r_combo2$w_CBD_hab*r_combo2$hab,r_combo2$w_MPA_coast*r_combo2$MPA3nm_13,r_combo2$w_MPA_eez*r_combo2$MPA3eez_13)
names(r_combo13)<-c("rgn_id","Layer","r_Mora","r_Mora_s4","r_hab","r_MPAc","r_MPAeez")
# average by row 
r_combo12_means<-data.frame(r_combo12[,1:2],"value"=rowMeans(r_combo12[,3:7],na.rm = T))
r_combo13_means<-data.frame(r_combo13[,1:2],"value"=rowMeans(r_combo13[,3:7],na.rm = T))

# get each layer to be a separate column, keeping rgn_id as the id column
r_combo12_final<-dcast(data=r_combo12_means, rgn_id  ~ Layer)
r_combo13_final<-dcast(data=r_combo13_means, rgn_id  ~ Layer)

# generate separate files for each layer, for the 2 years
write.csv(data.frame("rgn_id"=r_combo12_final[,1],"resilience.score"=r_combo12_final[,2]),"Outputs/Resil_newfiles/r_fishing_v1_2012a.csv", row.names = F)
write.csv(data.frame("rgn_id"=r_combo12_final[,1],"resilience.score"=r_combo12_final[,3]),"Outputs/Resil_newfiles/r_fishing_v1_eez_2012a.csv", row.names = F)
write.csv(data.frame("rgn_id"=r_combo12_final[,1],"resilience.score"=r_combo12_final[,4]),"Outputs/Resil_newfiles/r_fishing_v2_eez_2012a.csv", row.names = F)
write.csv(data.frame("rgn_id"=r_combo12_final[,1],"resilience.score"=r_combo12_final[,5]),"Outputs/Resil_newfiles/r_fishing_v3_2012a.csv", row.names = F)
write.csv(data.frame("rgn_id"=r_combo12_final[,1],"resilience.score"=r_combo12_final[,6]),"Outputs/Resil_newfiles/r_fishing_v3_eez_2012a.csv", row.names = F)
write.csv(data.frame("rgn_id"=r_combo12_final[,1],"resilience.score"=r_combo12_final[,7]),"Outputs/Resil_newfiles/r_habitat_combo_2012a.csv", row.names = F)
write.csv(data.frame("rgn_id"=r_combo12_final[,1],"resilience.score"=r_combo12_final[,8]),"Outputs/Resil_newfiles/r_habitat_combo_eez_2012a.csv", row.names = F)

write.csv(data.frame("rgn_id"=r_combo13_final[,1],"resilience.score"=r_combo13_final[,2]),"Outputs/Resil_newfiles/r_fishing_v1_2013a.csv", row.names = F)
write.csv(data.frame("rgn_id"=r_combo13_final[,1],"resilience.score"=r_combo13_final[,3]),"Outputs/Resil_newfiles/r_fishing_v1_eez_2013a.csv", row.names = F)
write.csv(data.frame("rgn_id"=r_combo13_final[,1],"resilience.score"=r_combo13_final[,4]),"Outputs/Resil_newfiles/r_fishing_v2_eez_2013a.csv", row.names = F)
write.csv(data.frame("rgn_id"=r_combo13_final[,1],"resilience.score"=r_combo13_final[,5]),"Outputs/Resil_newfiles/r_fishing_v3_2013a.csv", row.names = F)
write.csv(data.frame("rgn_id"=r_combo13_final[,1],"resilience.score"=r_combo13_final[,6]),"Outputs/Resil_newfiles/r_fishing_v3_eez_2013a.csv", row.names = F)
write.csv(data.frame("rgn_id"=r_combo13_final[,1],"resilience.score"=r_combo13_final[,7]),"Outputs/Resil_newfiles/r_habitat_combo_2013a.csv", row.names = F)
write.csv(data.frame("rgn_id"=r_combo13_final[,1],"resilience.score"=r_combo13_final[,8]),"Outputs/Resil_newfiles/r_habitat_combo_eez_2013a.csv", row.names = F)

