#Create unique list of FAO reported species at all taxonomic levels
#
#Created by JAfflerbach 3.3.2014
#
#

library(stringr)

dir1 = 'C:/Users/Afflerbach/ohi_spp_data'
setwd(dir1)

db = read.csv("J:\\SpatialFishCatch\\species\\FAO_raw_clean.csv")

#ADD a column that counts how many "X's" are in the species ID
db$X.count<-str_count(db$Taxon_ID,"X")

#Subset FAO data according to unique Taxon ID - (Remove duplicates)
db2 <- db[!duplicated(db[,5]),]
nrow(db2) #should equal length(unique(db$Taxon_ID))

#Get discrete counts of unique species and taxon levels
sp_id = unique(db2$Taxon_ID)
X.count = str_count(sp_id, "X")
Species.count = sum(X.count==0)
Genus.count = sum(X.count==2)
Family.count = sum(X.count==5)
Order.count = sum(X.count==7) #This is order and higher

#Get a list of each taxon group's species
#List of unique species
sp.list = subset(db2,X.count==0)
sp.list2 = sp.list[,2:7]
#write.csv(sp.list2, file="FAO_species")
#List of unique genus
ge.list = subset(db2,X.count==2)
ge.list2 = ge.list[,2:7]
#write.csv(ge.list2, file = "FAO_genus")
#list of unique famiy
fa.list = subset(db2,X.count==5)
fa.list2 = fa.list[,2:7]
#write.csv(fa.list2, file = "FAO_family")
#list of unique order
or.list = subset(db2,X.count==7)
or.list2=or.list[,2:7]
#write.csv(or.list2, file = "FAO_order")

#Create full list of FAO unique species
fao_species = rbind(sp.list2, ge.list2, fa.list2, or.list2)
write.csv(fao_species, file="fao_full_specieslist")

##################################################
#Calculating the breakdown (% of total) of reported catch for each taxonomic level 

#subset for only year 2011 catches for each taxonomic level reported
yr.2011 = subset(db, db$Year==2011)
catch.total = sum(yr.2011$Catch)

#species-taxon-level
sp.yr.2011 = subset(yr.2011, Scientific_Name %in% sp.list2$Scientific_Name)
sp.catch.tot = sum(sp.yr.2011$Catch)
pcnt.sp = (sp.catch.tot/catch.total) 
#pcnt.sp = 65% of total global catch is reported at species level

#genus-taxon-level
ge.yr.2011 = subset(yr.2011, Scientific_Name %in% ge.list2$Scientific_Name)
ge.catch.tot = sum(ge.yr.2011$Catch)
pcnt.ge = (ge.catch.tot/catch.total)
#10.6% reported at Genus level

#family-taxon-level
fa.yr.2011 = subset(yr.2011, Scientific_Name %in% fa.list2$Scientific_Name)
fa.catch.tot = sum(fa.yr.2011$Catch)
pcnt.fa = (fa.catch.tot/catch.total) 
#6.5% of 2011 catch reported at family level

#order-taxon-level
or.yr.2011 = subset(yr.2011, Scientific_Name %in% or.list2$Scientific_Name) 
or.catch.tot = sum(or.yr.2011$Catch)
pcnt.or = (or.catch.tot/catch.total) 
#18% of global 2011 catch reported at the order taxon level or higher



