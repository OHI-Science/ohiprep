# clean_UNDP.r: (by JStewart Jul2013)


## sovereignty (parent-children) gapfilling with add_gapfill_sov.r

#setwd('N:/git-annex/Global/FigurePrep_2013OHI/GL-UNDP-HDI_v2012_continuousVals_mrf/raw')

library(reshape2)
library(gdata)
library(plyr)
require(sqldf)
options(max.print=5E6)
options(gsubfn.engine = "R") # otherwise, get X11 launching for sqldf pa

#This is the old version:
#source('C:/Users/Melanie/Desktop/NCEAS/Projects/OHI 2013/GL-UNDP-HDI_v2012/ohi_clean_fxns.r') 
source('')
d.all =  matrix(nrow=0, ncol=0)
for (f in list.files(pattern=glob2rx('*Table1.csv'))){
  #f <- list.files(pattern=glob2rx('*Table1.csv'))[[1]]
  d = read.csv(f, header=F) 
    names(d)[1] = 'Country'
  
  # add identifier column
  a = strsplit(f, '_', fixed=FALSE)
  d$Developed_Developing = rep.int(unlist(a)[2], dim(d)[1])  

  d.all = rbind(d.all, d)
}

d.all[, "Developed_Developing"] = gsub('.*ed', '1', d.all[, "Developed_Developing"]) 
d.all[, "Developed_Developing"] = gsub('.*ing', '0', d.all[, "Developed_Developing"]) 
d.all[, "Developed_Developing"] = as.numeric(d.all[, "Developed_Developing"])
  
d.all$year = rep.int(2013, dim(d.all)[1])

d.all <- rename(d.all, c(V2="HDI", V3="LifeExp", V4="Schooling", 
                         V5="ExpectedSchooling", V6="GNI", V7="GNIrankHDIrank",
                         V8= "NonIncomeHDI"))

d.all$GNIrankHDIrank <- as.numeric(gsub("â???", "-", d.all$GNIrankHDIrank))

###############################################################
# this assigns the region name and id
  # example: uifileread = '/Volumes/ohi/Work/2013Update/data/GL-FAO-Commodities/data/cleaned/GL-FAO-Commodities.csv'
  #          uifilesave = '/Volumes/ohi/Work/2013Update/data/GL-FAO-Commodities/data/cleaned/GL-FAO-Commodities-cleaned.csv'
  
  # add_ISO.r: use SQLite to add OHI region codes and save as new file (J. Stewart, B. Best Apr 2013) 
  #   read in user-specified data file that needs ISO codes (tbd)
  #   rename anything with accents (Cote d'Ivoire->Ivory Coast, Reunion, Republique)
  #   read in official list of OHI regions (countries) and ISO codes: eez_rgn_2013master.csv (this is BB's file based on BH's xls for Radical)
  #   read in supplementary list of OHI regions/ISO codes: rgn_eez_v2013a_synonyms.csv (this is JS's file based on BB's file with 2-letter OHI regionkeys. Also exists one saved from BH's Radical file with 3-digit codes)
  #   query user-specified file to the ISO codes and save as a new file
  #   
  #   Note: the beginnings of regions_eezSynonyms.xlsx came from here, included at the bottom
uidata <- d.all
dir1 <- "C:/Users/Melanie/Desktop/NCEAS/Projects/OHI 2013/GL-UNDP-HDI_v2012"
uifilesave = paste(dir1,'/data/', 'GL-UNDP-HDI_v2012-cleaned_mrf2.csv', sep='') 

  print('-->>> add_rgn_id.r expects that the first two columns of the matrix will be country_name, value_units ')
  print('.')
  print('..')
  
  
  
  # remove accents
  col_num = grep('country', names(uidata), ignore.case = TRUE)
  names(uidata)[col_num] = 'country_id'
  uidata[,col_num] = gsub('.+voire', 'Ivory Coast', uidata[,col_num]) # Ivory Coast
  uidata[,col_num] = gsub('.+union', 'Reunion', uidata[,col_num]) # Reunion
  uidata[,col_num] = gsub('.+publique du', 'Republic of', uidata[,col_num]) # Congo
  uidata[,col_num] = gsub('Cura.+', 'Curacao', uidata[,col_num]) # Curacao 
  uidata[,col_num] = gsub('Saint Barth.+', 'Saint Barthelemy', uidata[,col_num]) # Saint Barthelemy 
  uidata[,col_num] = gsub('.+Principe', 'Sao Tome and Principe', uidata[,col_num]) # Sao Tome and Principe
  uidata = uidata[-grep("Lao", uidata[,col_num]), ]


  
  ## read in more offical (by BB) and redundant (by JS) lists with 2-letter OHI region codes, combine into one data.frame 
  #rk = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/eez_rgn_2013master.csv')
  rk <- read.csv("C:/Users/Melanie/Desktop/NCEAS/data/eez_rgn_2013master.csv")
  #rk2 = read.csv('/Volumes/ohi/Work/2013Update/regioncodes_iso_ohi_saup_etc/rgn_eez_v2013a_synonyms.csv')
  rk2 <- read.csv("C:/Users/Melanie/Desktop/NCEAS/data/rgn_eez_v2013a_synonyms.csv")
  
# manage official region_id data
  rk = rk[rk$rgn_id_2013 < 255,]# remove high seas and non-regions
  rk$rgn_typ = rep(NA, length(rk[,1]))
  rk$rgn_typ[!is.na(rk$rgn_id_2013)] = 'ohi_region'
  
  # manage synonym region_id data
  rkb = data.frame(rk$rgn_id_2013, rk$rgn_key_2013, rk$rgn_nam_2013, rk$region_id_2012, rk$rgn_typ) 
  rk2b = data.frame(rk2$rgn_id_2013, rk2$rgn_key_2013, rk2$rgn_nam_2013, rk2$region_id_2012, rk2$rgn_typ)
  
  # combine official and synonym region_id data
  names(rkb) = c('rgn_id_2013', 'rgn_key_2013', 'rgn_nam_2013', 'region_id_2012', 'rgn_typ'); names(rk2b) = c('rgn_id_2013', 'rgn_key_2013', 'rgn_nam_2013', 'region_id_2012', 'rgn_typ')
  regionkey = rbind(rkb, rk2b)
  
  ## sqlite: it isn't case sensitive so this should be fine if uidata is labeled Country or country. 
  uidata2 = sqldf("SELECT b.rgn_id_2013, a.*, b.rgn_typ
                  FROM uidata AS a
                  LEFT OUTER JOIN (
                  SELECT DISTINCT rgn_nam_2013, rgn_key_2013, rgn_id_2013, rgn_typ  
                  FROM regionkey 
                  ) AS b ON b.rgn_nam_2013 = a.country_id") 
  
  ## remove landlocked rows from datafiles, leaving only ohi regions and NA regions: need to assign those later.  
  unique(uidata2$rgn_typ)
  uidata3 = subset(uidata2, (rgn_typ == 'ohi_region' | is.na(rgn_typ))) # keep NAs because they need to be assigned later on
  #uidata3 = uidata3[uidata3$rgn_id < 255,] # Also removed disputed. for high-seas stuff, would want this to just say !=255 to exclude disputed areas
  
  # indicate which were removed
  print('These landlocked/largescale countries were removed:')
  RemovedMatrix = subset(uidata2, (rgn_typ == 'landlocked' | rgn_typ == 'largescale' | rgn_typ == 'disputed')) 
  RemovedCountry = data.frame(RemovedMatrix$country_id)
  print(unique(RemovedCountry))
  print('')
  
  # indicate which still need to be assigned:
  print('These non-landlocked countries were not matched with OHI rgn_id codes:')
  uidata3.na = subset(uidata3,  is.na(rgn_typ)) 
  print(unique(data.frame(uidata3.na$country_id)))
  
  print('TRUE if everything is working properly: ')
  print(dim(uidata3)[1] + dim(RemovedMatrix)[1] == dim(uidata2)[1]) # make sure this is TRUE
  
  uidata4 = uidata3
  
  ## save
  uidata4$rgn_typ <- NULL
  # uidata4$country_id <- NULL # we need this in there to ID which countries aren't matched. remove in add_gapfill.r
  names(uidata4)[c(1,2)] = c('rgn_id', 'rgn_nam')
  uidata4 = uidata4[order(uidata4$rgn_id),]
  
  
  print('Be sure to inspect saved .csv file for additional or missing ISO codes.')
  write.csv(uidata4, uifilesave, na = '', row.names=FALSE)
  
  write.table(regionkey, 'regionkeytest_mrf.txt', sep='\t', row.names=FALSE)




###################################################################################
## sovereignty (parent-children) gapfilling with add_gapfill_sov.r
cleaned_data = read.csv(uifilesave)
#cleaned_data_sov =  add_gapfill_sov(cleaned_data) 

  # sovereignty
  # use SQLite to add UN gapfilling regions and save as new file (J. Stewart, Aug 2013) 
  
  
  print('-->>> add_gapfill_sov.r gives children the parent values based on sovereignty')
  print('.')
  print('..')
  
  options(gsubfn.engine = "R") # otherwise, get X11 launching for sqldf package
  
  cleandata = cleaned_data
  n = names(cleaned_data)
  
  # SECOND, use the master OHI region list (to identify which rgn_ids are missing) and join to the sovereignty so they can then be joined to cleandata to see how to gapfill. 
  
  # read in master OHI list with 2letter code; region ids
  rk = rk[order(rk$rgn_id_2013),]
  rkx = rk[rk$rgn_id_2013 < 255,]# remove high seas and non-regions
  rk_uni = unique(rkx)
  rgn_tofill = sqldf("SELECT a.rgn_id_2013, a.rgn_nam_2013, a.sov_id, a.sov_nam
                     FROM rk_uni AS a
                     LEFT OUTER JOIN (
                     SELECT DISTINCT rgn_id
                     FROM cleandata
                     ) AS b ON b.rgn_id = a.rgn_id_2013
                     WHERE b.rgn_id IS null") 
  
  # THIRD, join and gapfill
  # do this for every year: 
  
  yrtrix = unique(cleandata$year)
  rgn_gf_sov_all = matrix(nrow=0, ncol=0)
  for(yr in yrtrix) {
    trixtmp = cleandata[cleandata$year == yr,]
    
    rgn_gf_sov = sqldf("SELECT a.*, b.HDI, b.year 
                      FROM rgn_tofill AS a
                       LEFT OUTER JOIN (
                       SELECT HDI, rgn_id, year
                       FROM trixtmp
                       ) AS b ON b.rgn_id = a.sov_id") 
    
    
    rgn_gf_sov_all = rbind(rgn_gf_sov_all, rgn_gf_sov)
    
  }
  
  rgn_gf_sov_all = rgn_gf_sov_all[order(rgn_gf_sov_all$rgn_id, rgn_gf_sov_all$year),]
  
  cleaned_data_sov = rgn_gf_sov_all[,c(1,2,dim(rgn_gf_sov_all)[2]-1, dim(rgn_gf_sov_all)[2])]
  


# combine
cleaned_data <- subset(cleaned_data, select=c("rgn_id", "rgn_nam", "HDI", "year"))
cleaned_data <- rename(cleaned_data, c(rgn_id="rgn_id_2013", rgn_nam="rgn_nam_2013"))
cleaned_data2 = rbind(cleaned_data, cleaned_data_sov)
cleaned_data2 = cleaned_data2[order(cleaned_data2$rgn_id_2013),]

# Weighted average of China data based on population
(0.906*7.859 + 0.699*1367)/(1367+7.859) # 0.700
cleaned_data2 <- subset(cleaned_data2, rgn_nam_2013!="Hong Kong China (SAR)")
cleaned_data2[cleaned_data2$rgn_id_2013 == 209, ]
cleaned_data2$HDI[cleaned_data2$rgn_nam_2013=="China"] <- 0.700

write.csv(cleaned_data2, "C:/Users/Melanie/Desktop/NCEAS/Projects/OHI 2013/GL-UNDP-HDI_v2012/data/HDI_mrf_2.csv", na = '', row.names=FALSE)
# SAR population 7858800  
# China population 1367030000
# 7858800/(1367030000+23315822)




