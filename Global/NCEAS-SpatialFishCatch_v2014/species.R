# Determining what AquaMaps we do and do not have for FAO reported species
#
# Created 3.3.2014 by JAfflerbach

source('./Global/NCEAS-SpatialFishCatch_v2014/common.R')

fao.raw = read.csv(file.path(dd, 'data/FAO_raw_clean.csv')) #Raw FAO data
fao.sp = read.csv(file.path(dd, 'data/FAO_species.csv')) #FAO species list - created in "FAO_species.R"

#AquaMaps data from OHI_2012
am.2012 = read.csv(file.path(nd, 'model/GL-NCEAS-SpeciesDiversity/ohi_spp/data/spp.csv'))
am.2012 = subset(am, src_distn=="SAUP") #10,554 AquaMaps species
am.2012$datayear = "2012" #"datayear" column identifies what year the data comes from (2012 from spp.csv, 2013 for am_spp_data.csv) - only using this for reference

#AquaMaps data from OHI_2013
am.2013 = read.csv(file.path(nd, 'model\GL-NCEAS-SpeciesDiversity_v2013a/tmp/am_spp_data.csv'))
am.2013$scientific <- paste(am.2013$Genus, am.2013$Species, sep=" ")
am.2013$datayear = "2013"

#List of species found in both AquaMaps datasets
both = intersect(am.2012$scientific, am.2013$scientific) #1868 species overlap

#create full list of unique Aquamap species and what dataset they came from (2012 or 2013)
a = am.2012[,c("scientific","datayear")]
b = am.2013[,c("scientific","datayear")]
list = rbind(a,b)
am.spp = unique(list$scientific)

#List of FAO species that we have aquamaps for
species = intersect(fao.sp$Scientific_Name,am.spp) #1066 FAO species maps

#Find list of species that are not in either 2012 or 2013 AquaMaps data
fao.missing = setdiff(fao.sp$Scientific_Name, am.spp)

#calculate what % of global catch we have maps for
yr.2011 = subset(fao.raw, fao.raw$Year==2011)
catch.total = sum(yr.2011$Catch) #Total catch for FAO species in 2011
am.yr.2011 = subset(yr.2011, Scientific_Name %in% species) # create dataframe with just 2011 catches of fish that are reported to FAO and that we also have aquamaps for
am.catch.tot = sum(am.yr.2011$Catch)
pcnt = (am.catch.tot/catch.total) #The maps we have account for 60% of global catch reported in 2011

sp.yr.2011 = subset(yr.2011, Scientific_Name %in% fao.sp$Scientific_Name)
sp.catch.tot = sum(sp.yr.2011$Catch) #Total catch reported at the species level
pcnt.sp = (sp.catch.tot/catch.total) #65% of total FAO catch was reported at the species level
am.sp.catch = (am.catch.tot/sp.catch.tot)#91% of FAO catch reported at the species level is represented in aquamaps

#For the 300 species we don't have, what is the total % of global catch they make up?
sp.missing = subset(yr.2011, Scientific_Name %in% fao.missing)
sp.missing.catch = sum(sp.missing$Catch)
pcnt.sp.missing = (sp.missing.catch/catch.total) 
#The FAO species we are missing maps for make up only 6% of total global catch

