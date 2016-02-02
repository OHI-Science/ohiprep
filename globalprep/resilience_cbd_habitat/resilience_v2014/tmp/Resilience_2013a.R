r_hab<-read.csv("Data/Resilience/r_habitat_2013a.csv")
r_mora<-read.csv("Data/Resilience/r_mora_2013a.csv")
r_mora_s4<-read.csv("Data/Resilience/r_mora_s4_2013a.csv") # a table with the list of layers that apply to each resilience layer
r_MPAc12<-read.csv("Data/Resilience/r_MPA_coast_2012.csv")
r_MPAe12<-read.csv("Data/Resilience/r_MPA_eez_2012.csv")
r_MPAc13<-read.csv("Data/Resilience/r_MPA_coast_2013.csv")
r_MPAe13<-read.csv("Data/Resilience/r_MPA_eez_2013.csv")
r_combos<-read.csv("Data/Resilience/r_combos.csv")
r_combos[r_combos==0]<-NA # change 0s to NAs so they can be excluded from the averaging, as opposed to real 0s in the data
r_join<-read.csv("Data/Resilience/r_join.csv") #  a table with the complete list of v2013a region ids and a "join" column
r_combo<-merge(r_join,r_combos,by="Join",all.x=T) # create the "weights" matrix, where a 1 means that resilience indicator is included in that resilience layer combo, and 0 means it's omitted
names(r_combo)[5:9]<-paste("w_",names(r_combo)[5:9],sep="")
names(r_combo)[2]<-"rgn_id"
r_combo<-r_combo[,-1]

names(r_hab)[2]<-"hab"
names(r_mora)[2]<-"mora"
names(r_mora_s4)[2]<-"mora_s4"
names(r_MPAc13)[2]<-"MPA3nm_13"
names(r_MPAc12)[2]<-"MPA3nm_12"
names(r_MPAe13)[2]<-"MPA3eez_13"
names(r_MPAe12)[2]<-"MPA3eez_12"

r_MPAc13[2]<-r_MPAc13[2]/100 # all must scale 0 to 1
r_MPAc12[2]<-r_MPAc12[2]/100
r_MPAe13[2]<-r_MPAe13[2]/100
r_MPAe12[2]<-r_MPAe12[2]/100


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

