
#Now that we have lists of unique species reported to FAO, find out how many of these species ranges we already have from AquaMaps.
#Using data from OHI Global Species Diversity Model which used both IUCN and AquaMaps maps

#Looking at AquaMaps data from raw data folder in: GL-NCEAS-SpeciesDiversity_v2013a\\raw\\AquaMaps_OHI_082013\ 
samples_names <- read.csv("C:\\Users\\Afflerbach\\GL-NCEAS-SpeciesDiversity_v2013a\\raw\\AquaMaps_OHI_082013\\hdr_hcaf_species_native.csv", header=FALSE)
samples_names <- samples_names$V1
samples <- read.csv("C:\\Users\\Afflerbach\\GL-NCEAS-SpeciesDiversity_v2013a\\raw\\AquaMaps_OHI_082013\\tbl_hcaf_species_native.csv", header = F,
                    col.names=samples_names)
length(unique(samples$SpeciesID)) #Gives us number of AquaMap species

species_names <- read.csv("C:\\Users\\Afflerbach\\GL-NCEAS-SpeciesDiversity_v2013a\\raw\\AquaMaps_OHI_082013\\hdr_speciesoccursum.csv", header=FALSE)
species_names <- species_names$V1
species <- read.csv("C:\\Users\\Afflerbach\\GL-NCEAS-SpeciesDiversity_v2013a\\raw\\AquaMaps_OHI_082013\\tbl_speciesoccursum.csv", header=F,
                    col.names=species_names)
species$name <- paste(species$Genus, species$Species, sep=" ")
dim(species)
am_spp = species$name #list of all AquaMaps species names
length(am_spp) #should be same as length of samples (3,332)


sites_names <-read.csv("C:\\Users\\Afflerbach\\GL-NCEAS-SpeciesDiversity_v2013a\\raw\\AquaMaps_OHI_082013\\hdr_hcaf.csv", header=F)
sites_names <- sites_names$V1
sites <- read.csv("C:\\Users\\Afflerbach\\GL-NCEAS-SpeciesDiversity_v2013a\\raw\\AquaMaps_OHI_082013\\tbl_hcaf.csv", header = F, col.names = sites_names)

#read in FAO species list -Important - this is only species level information reported to FAO! (1,366 out of 1,700 distinct species reported)

FAO_spp = read.csv("J:\\ohi_spp\\FAO\\FAO_Species.csv")

sp.have = intersect(species$name,FAO_spp$Scientific_Name) #list of species we already have
length(sp.have) #number of species maps we have (457)
#write.csv(sp.have, file="Species with AquaMap Data")

sp.need = setdiff(FAO_spp$Scientific_Name, species$name) #list of species we need to get maps for
length(sp.need) #number of species we need maps for
#write.csv(sp.need, file="Species without AquaMap Data")

#Find the total percentage of global catch that the overlapping species make up!

#subset FAO_raw_clean to get only year 2011 catches for the overlapping species (457 species)
yr.2011 = subset(db, db$Year==2011)
catch.total = sum(yr.2011$Catch)
am.yr.2011 = subset(yr.2011, Scientific_Name %in% am_spp) # create dataframe with just 2011 catches of fish that are reported to FAO and that we also have aquamaps for
am.catch.tot = sum(am.yr.2011$Catch)
pcnt = (am.catch.tot/catch.total) #This gives us the percent (%) of catch we have aquamaps data for already. Based on 2011 reported catches.


########################################################
#Percent of total annual catch that species w/o aquamaps makes up
no.am.yr.2011 = subset(yr.2011, Scientific_Name %in% sp.need)
no.am.catch.tot = sum(no.am.yr.2011$Catch)
pcnt.no.am.2011 = (no.am.catch.tot/catch.total)


###########################
#Trying to figure out why spp.csv has 10,000 aquamaps species reported while the raw aquamaps data only shows #3,332 species
  #df = subset(spp, spp$src_distn=="SAUP")
  #df2 = subset(spp, spp$src_distn=="IUCN")

  #a = unique(df$scientific)
  #b = unique(df2$scientific)

  #sp.have = intersect(FAO.Species$Scientific_Name, df$scientific)

  #c = am_spp_data$SPECIESID
  #d = spp$saup_sid  


#spp <- read.csv("J:\\ohi_spp\\data\\spp.csv")
  #table(spp$src_distn)
  #df2 = subset(spp, spp$src_distn=="SAUP")
  #length(unique(df2$scientific))

  #dim(df2)
  #dim(species)
  #length(setdiff(df2$scientific, species$name))

#####################
