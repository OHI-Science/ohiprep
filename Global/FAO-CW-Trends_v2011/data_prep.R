# data_prep.R

# Prepare FAO fertilizer/pesticides data for CW trends. 
# By JSLowndes Apr2014; File was originally clean_FAOtrends.r:(by JStewart Jul2013)

# for trends in pesticides and fertilizers. Note that for 2014a, only pesticides are updated (fertilizer 2011 data are all 0's)
# Data are in a new format since 2013a. Available from http://faostat3.fao.org/faostat-gateway/go/to/download/R/*/E

#   read in individual files
#   remove Totals row
#   remove/translate FAO data codes (F, ..., -, 0 0)
#   add identifier column
#   concatenate data from each file into a single file
#   run add_rgn_id.r (function by J. Stewart, B. Best)
#   save single file
  

# setup ----

# load libraries
library(reshape2)
library(gdata)
library(dplyr)

# from get paths configuration based on host machine name
source('src/R/common.R') # set dir_neptune_data
# Otherwise, presume that scripts are always working from your default ohiprep folder
dir_d = 'Global/FAO-CW-Trends_v2011'

# get functions
source('src/R/ohi_clean_fxns.R')


## read in and process files ----

for (f in list.files(path = file.path(dir_d, 'raw'), pattern=glob2rx('*csv'), full.names=T)) {  # f = "Global/FAO-CW-Trends_v2011/raw/FAO_fertilizers_thru2011.csv"
  
  d.fao = read.csv(f, header=F); head(d.fao)
  
  v = strsplit(as.character(f), '_') 
  names(d.fao) = c(unlist(v)[3], 'country', 'category', 'unit', 'year', 'tonnes'); head(d.fao)
  
  # clean up
  d = d.fao %.%
    select(country, category, year, tonnes) %.%
    group_by(country, year) %.%
    summarise(tonnes = sum(tonnes)); head(d)
  
  ## run add_rgn_id and save ----
  uifilesave = file.path(dir_d, 'raw', paste('FAO-', unlist(v)[3], '-trends_v2011-cleaned.csv', sep=''))
  add_rgn_id(d, uifilesave)
  
}

## Further processing ----
## Treat Pesticides and Fertilizer differently because Fertilizers have weird 0's and Pesticides don't 

## Pesticides ----
g = "Global/FAO-CW-Trends_v2011/raw/FAO-pesticides-trends_v2011-cleaned.csv"
pest = read.csv(g); head(pest,30)

## clean up data: described in Global SOM 2013: section 5.19 ----

# see if there are a lot of 0's
explore = pest %.%
  filter(tonnes == 0); head(explore,30) # no there aren't

# see if there are countries with only 1 year of data 
explore2 = pest %.%
  group_by(rgn_id, rgn_nam) %.% 
  summarise(count = n()) %.%
  filter(count == 1); explore2 # yes there are

pest2 = pest %.% # remove countries with only 1 year of data 
  filter(!rgn_id %in% explore2$rgn_id)
  
# see if there are countries no data after 2005 
explore3 = pest2 %.%
  group_by(rgn_id, rgn_nam) %.% 
  summarise(max_year = max(year)) %.%
  filter(max_year < 2005); explore3 # yes there are

pest3 = pest2 %.% # remove countries with no data after 2005 
  filter(!rgn_id %in% explore3$rgn_id)

# gapfilling follows, both for pesticides and fertilizers together. 


## Fertilizers ----
g = "Global/FAO-CW-Trends_v2011/raw/FAO-fertilizers-trends_v2011-cleaned.csv"
fert = read.csv(g); head(fert,30)

# clean up data: described in Global SOM 2013: section 5.19
# see if there are a lot of 0's
explore = fert %.%
  filter(tonnes == 0); head(explore,30) # yes they are

fert2 = fert %.% 
  filter(tonnes !=0); head(fert2,30) # remove 0's; don't replace with NA because lm() below will need NAs removed 

# see if there are countries with only 1 year of data 
explore2 = fert2 %.%
  group_by(rgn_id, rgn_nam) %.%
  summarise(count = n()) %.%
  filter(count == 1); explore2 # yes there are 

fert3 = fert2 %.% # remove countries with only 1 year of data 
  filter(!rgn_id %in% explore2$rgn_id)

# see if there are countries no data after 2005 
explore3 = fert3 %.%
  group_by(rgn_id, rgn_nam) %.% 
  summarise(max_year = max(year)) %.%
  filter(max_year < 2005); explore3 # no there aren't



## calculate trend and gapfill for both fertilizers and pesticides:: KLo style. ----
# See readme.md and Global SOM 2013 section 5.19. Approach by Katie Longo, September 2013: github/ohiprep/Global/FAO-CW-Trends_v2011/raw/Fertilizer_Pesticide_trend_KLongo2013.R
# trend years: 2012a (2005:2009) and 2013a (2006:2010). Note:: error in KLo 2013 approach: trend was created through multiplying slope by 4 instead of 5

# 1) calculate fert and pest trend, excluding NAs, for 2014a. fert = 2006:2010 (data not actually updated); pest = 2007:2011
## this is the logic for the calc_trend function below using ddply and summarize. JSL April 2014: couldn't get dplyr to work with lm(). 
# slope = lm(fert$tonnes ~ fert$year)$coefficients[[2]]
# intercept = lm(fert$tonnes ~ fert$year)$coefficients[[1]]
# x1 = 2005
# y1 = x1*slope+intercept
# trend = max(min(slope/(y1) * 5, 1), -1) # normalize slope by y in a given year (we use x1, but could be any year), multiple by 5 for the trend and bound it between -1 and 1. 

# ---------
# function that calculates trend
  calc_trend = function(data, x1) {
    library('plyr')
    trend2014 = plyr::ddply(
      data, .(rgn_id), summarize,
      trend = max(min(lm(tonnes ~ year)$coefficients[[2]] /
                        (lm(tonnes ~ year)$coefficients[[2]]*x1 +
                           lm(tonnes ~ year)$coefficients[[1]]) * 5, 1), -1))
    return(trend2014)
  }
# --------

## calculate fertilizer trends: 
data = na.omit(fert3)
x1 = 2006
trend_pest = calc_trend(data, x1) 

## calculate pesticide trends: 
data = na.omit(pest3)
x1 = 2007
trend_fert = calc_trend(data, x1) 


# 2) calculate pop trend for 2012a and 2013a (years 2005:2009, 2006:2011, respectively)

## JSL come back::: read in most recent pop data (so can work with it and reinsert more later). 
# model/GL-NCEAS-CoastalPopulation_v2013/data -- maybe even rgn_popn5yrtrend_inland25mi_2008to2013.csv



#load pop data
pop05<-read.csv("Data/MARdata/rgn_popsum2005_inland25mi.csv")
pop06<-read.csv("Data/MARdata/rgn_popsum2006_inland25mi.csv")
pop07<-read.csv("Data/MARdata/rgn_popsum2007_inland25mi.csv")
pop08<-read.csv("Data/MARdata/rgn_popsum2008_inland25mi.csv")
pop09<-read.csv("Data/MARdata/rgn_popsum2009_inland25mi.csv")
pop10<-read.csv("Data/MARdata/rgn_popsum2010_inland25mi.csv")

#names(pop11)<-c("rgn_id","coast_11")
names(pop10)<-c("rgn_id","coast_10")
names(pop09)<-c("rgn_id","coast_09")
names(pop08)<-c("rgn_id","coast_08")
names(pop07)<-c("rgn_id","coast_07")
names(pop06)<-c("rgn_id","coast_06")
names(pop05)<-c("rgn_id","coast_05")

# merge into single dataframe
POPdata<-merge(pop05[,1:2],pop06[,1:2],by="rgn_id")
POPdata<-merge(POPdata,pop07[,1:2],by="rgn_id")
POPdata<-merge(POPdata,pop08[,1:2],by="rgn_id")
POPdata<-merge(POPdata,pop09[,1:2],by="rgn_id")
POPdata<-merge(POPdata,pop10[,1:2],by="rgn_id")
# POPdata<-merge(POPdata,pop11[,1:2],by="rgn_id") # not using this year
names(POPdata)<-c("rgn_id",2005:2010) # rename to transform to long format 

POPdata2<-melt(POPdata,id="rgn_id") # long format
names(POPdata2)[2:3]<-c("year","Coast_POP")
POPdata2$year<-as.numeric(as.character(POPdata2$year)) 

# calculate trend
Pop_Delta12 = ddply(
  POPdata2[POPdata2$year%in%c(2005:2009),], .(rgn_id), summarize,
  trend = min(lm(Coast_POP ~ year)$coefficients[[2]]/(2005*lm(Coast_POP ~ year)$coefficients[[2]]+lm(Coast_POP ~ year)$coefficients[['(Intercept)']]) * 4,1)) # normalize trend by dividing by fitted value of first year

Pop_Delta13 = ddply(
    POPdata2[POPdata2$year%in%c(2006:2010),], .(rgn_id), summarize,
    trend = min(lm(Coast_POP ~ year)$coefficients[[2]]/(2006*lm(Coast_POP ~ year)$coefficients[[2]]+lm(Coast_POP ~ year)$coefficients[['(Intercept)']]) * 4,1)) # normalize trend by dividing by fitted value of first year
    
# # test formula for region 1 and for region 216
# POPdata2.1<-POPdata2[POPdata2$rgn_id==1,]
# POPdata2.216<-POPdata2[POPdata2$rgn_id==216,]
# get the delta and the value for 2005
# lm(POPdata2.1$Coast_POP ~ POPdata2.1$year)$coefficients[[2]]*4 #  -120.7378
# summary(lm(POPdata2.1$Coast_POP ~ POPdata2.1$year))
# POP09.1<- -3.018e+01*2009 + 6.133e+04
# POP05.1<- -3.018e+01*2005 + 6.133e+04 
# deltaPOP.1<- POP09.1-POP05.1 # -120.72
# deltaPOP.1/POP05.1 #  -0.1473813
# #
# summary(lm(POPdata2.216$Coast_POP ~ POPdata2.216$year))POP09.216<- -3.052e+09 + 1.601e+06*2009 
# POP05.216<- -3.052e+09 + 1.601e+06*2005
# deltaPOP.216<- POP09.216-POP05.216 #
# deltaPOP.216/POP05.216 #  0.04053036
# # compare to:
# lm(POPdata2.1$Coast_POP ~ POPdata2.1$year)$coefficients[[2]]*4/(2005*lm(POPdata2.1$Coast_POP ~ POPdata2.1$year)$coefficients[[2]]+lm(POPdata2.1$Coast_POP ~ POPdata2.1$year)$coefficients[['(Intercept)']])
# # -0.1492134
# lm(POPdata2.216$Coast_POP ~ POPdata2.216$year)$coefficients[[2]]/(2005*lm(POPdata2.216$Coast_POP ~ POPdata2.216$year)$coefficients[[2]]+lm(POPdata2.216$Coast_POP ~ POPdata2.216$year)$coefficients[['(Intercept)']]) * 4
# # 0.04073981

# 3) merge fert and pesticide trend with complete list of 2013 region ids to append NAs
Pest_r2_12 = sqldf("SELECT a.*, b.*
                   FROM gf AS a
                   LEFT OUTER JOIN 
                 Pest_Delta12
                 AS b ON b.rgn_id = a.rgn_id") 

Fert_r2_12 = sqldf("SELECT a.*, b.*
                FROM gf AS a
                LEFT OUTER JOIN 
                Fert_Delta12
                AS b ON b.rgn_id = a.rgn_id") 

Pest_r2_13 = sqldf("SELECT a.*, b.*
                   FROM gf AS a
                   LEFT OUTER JOIN 
                   Pest_Delta13
                   AS b ON b.rgn_id = a.rgn_id") 

Fert_r2_13 = sqldf("SELECT a.*, b.*
                   FROM gf AS a
                   LEFT OUTER JOIN 
                   Fert_Delta13
                   AS b ON b.rgn_id = a.rgn_id") 
# clean up duplicate fields
Pest_r2_12<-Pest_r2_12[,-6]
Pest_r2_13<-Pest_r2_13[,-6]
Fert_r2_12<-Fert_r2_12[,-6]
Fert_r2_13<-Fert_r2_13[,-6]
# 4) merge pop trend with list of region ids and region groups (new file with b suffix)
# read in region v2013a list with corresponding georegional grouping factors (from fine to coarse, respectively: r2, r1, r0)
gf = read.csv("Data/rgn_georegions_2013b.csv")
gf<-gf[,c(1,2,4,6,8)] # check: tail(gf[order(gf$rgn_id_2013),])
gf<-gf[gf[,1]!=213,] # dim(gf)

POP_r2_12 = sqldf("SELECT a.*, b.*
                FROM gf AS a
                LEFT OUTER JOIN 
                   Pop_Delta12
                   AS b ON b.rgn_id = a.rgn_id") 

POP_r2_13 = sqldf("SELECT a.*, b.*
                   FROM gf AS a
                   LEFT OUTER JOIN 
                   Pop_Delta13
                   AS b ON b.rgn_id = a.rgn_id") 

names(POP_r2_12)[6]<-"POPtrend"
names(POP_r2_13)[6]<-"POPtrend"
# 5) calculate mean pop trend per r2 group
POP_r2_12<-POP_r2_12[,-6] # rm the extra rgn_id filed containing NAs
r2meanPOP12=aggregate(POP_r2_12$POPtrend,by=list(POP_r2_12$r2),FUN="mean",na.rm=T) # sub-regional level georegional aggregation
names(r2meanPOP12)<-c("r2","POP_r2_12")

POP_r2_13<-POP_r2_13[,-6] # rm the extra rgn_id filed containing NAs
r2meanPOP13=aggregate(POP_r2_13$POPtrend,by=list(POP_r2_13$r2),FUN="mean",na.rm=T) # sub-regional level georegional aggregation
names(r2meanPOP13)<-c("r2","POP_r2_13")

# 7) replace remaining NAs in fert and pesticide trends with pop trends 
Fert_gap12$Fert_gapfill<-ifelse(is.na(Fert_gap12$trend),Fert_gap12$POPtrend,Fert_gap12$trend)
Fert_gap12$Fert_gapfill<-ifelse(Fert_gap12$Fert_gapfill< -1,-1,Fert_gap12$Fert_gapfill) # cap to -1

Fert_gap13$Fert_gapfill<-ifelse(is.na(Fert_gap13$trend),Fert_gap13$POPtrend,Fert_gap13$trend)
Fert_gap13$Fert_gapfill<-ifelse(Fert_gap13$Fert_gapfill< -1,-1,Fert_gap13$Fert_gapfill) # cap to -1

Pest_gap12$Pest_gapfill<-ifelse(is.na(Pest_gap12$trend),Pest_gap12$POPtrend,Pest_gap12$trend)
Pest_gap12$Pest_gapfill<-ifelse(Pest_gap12$Pest_gapfill< -1,-1,Pest_gap12$Pest_gapfill) # cap to -1

Pest_gap13$Pest_gapfill<-ifelse(is.na(Pest_gap13$trend),Pest_gap13$POPtrend,Pest_gap13$trend)
Pest_gap13$Pest_gapfill<-ifelse(Pest_gap13$Pest_gapfill< -1,-1,Pest_gap13$Pest_gapfill) # cap to -1

names(Fert_gap13)[5]<-"trend.score"
names(Fert_gap12)[5]<-"trend.score"
names(Pest_gap13)[5]<-"trend.score"
names(Pest_gap12)[5]<-"trend.score"
# 8) clean waters scores are the inverse of pressures, so the delta in score is the inverse of the pressure trend
Fert_gap13[,5]<-Fert_gap13[,5]*-1 
Fert_gap12[,5]<-Fert_gap12[,5]*-1
Pest_gap13[,5]<-Pest_gap13[,5]*-1
Pest_gap12[,5]<-Pest_gap12[,5]*-1

# 9) remove uninhabited islands
uninhabited<-read.csv("Data/uninhabited_ohi2013.csv")
Fert_gap13<-Fert_gap13[!(Fert_gap13$rgn_id%in%uninhabited[,1]),]
Fert_gap12<-Fert_gap12[!(Fert_gap12$rgn_id%in%uninhabited[,1]),]
Pest_gap13<-Pest_gap13[!(Pest_gap13$rgn_id%in%uninhabited[,1]),]
Pest_gap12<-Pest_gap12[!(Pest_gap12$rgn_id%in%uninhabited[,1]),]

missing_Fert13<-Fert_gap13[is.na(Fert_gap13$trend.score),] # a few inhabited regions had no population data for Fert2013a
missing_Fert12<-Fert_gap12[is.na(Fert_gap12$trend.score),] # Fert 2012a same as missing_Fert13
missing_Pest13<-Pest_gap13[is.na(Pest_gap13$trend.score),] # Pest 2013a same as missing_Fert13
missing_Pest12<-Pest_gap12[is.na(Pest_gap12$trend.score),] # Pest 2012a same as missing_Fert13

# exported for inspection and decision-making on how to gap-fill (is it sensible to use regional fertilizer/pesticide means if no or very small pop?)
write.csv(missing_Fert[!(missing_Fert$rgn_id%in%uninhabited[,1]),c(1,4,5)],"Outputs/inhabited_missing_Fert.csv")

# decision made: only gapfill with regional mean for regions 33 and 30 (Glorioso Islands and Juan de Nova island)
# find what r2 gapfilling region they're from 
Fert_r2_12[Fert_r2_12$rgn_id==33,] # r2 for region 33 is group 14
Fert_r2_12[Fert_r2_12$rgn_id==30,] # r2 for region 30 is group 14 as well
# get the mean values for region r2 "14":
Fert12_r2mean14<-mean(Fert_r2_12$trend[Fert_r2_12$r2==14],na.rm=T) # gap-fill value for Fert 2012a: 0.5239608
Fert13_r2mean14<-mean(Fert_r2_13$trend[Fert_r2_13$r2==14],na.rm=T) # gap-fill value for Fert 2013a: 0.3158611
Pest12_r2mean14<-mean(Pest_r2_12$trend[Pest_r2_12$r2==14],na.rm=T) # gap-fill value for Pest 2012a: -1.32918
Pest13_r2mean14<-mean(Pest_r2_13$trend[Pest_r2_13$r2==14],na.rm=T) # gap-fill value for Pest 2013a: -1.524768
# use their inverse to gapfill (i.e. the steeper the pesticide or fertilizer increase, the steeper the clean waters status decline)
Fert_gap13$trend.score[Fert_gap13$rgn_id%in%c(30,33)]<-Fert13_r2mean14*-1
Fert_gap13$trend.score[is.na(Fert_gap13$trend.score)]<-0
Fert_gap12$trend.score[Fert_gap12$rgn_id%in%c(30,33)]<-Fert12_r2mean14*-1
Fert_gap12$trend.score[is.na(Fert_gap12$trend.score)]<-0
Pest_gap13$trend.score[Pest_gap13$rgn_id%in%c(30,33)]<-Pest13_r2mean14*-1
Pest_gap13$trend.score[is.na(Pest_gap13$trend.score)]<-0
Pest_gap12$trend.score[Pest_gap12$rgn_id%in%c(30,33)]<-Pest12_r2mean14*-1
Pest_gap12$trend.score[is.na(Pest_gap12$trend.score)]<-0


write.csv(Fert_gap13[,c(1,5)],"Outputs/GL-NCEAS_CW_Fertilizers_trend_v2013a.csv",row.names = F)
write.csv(Fert_gap12[,c(1,5)],"Outputs/GL-NCEAS_CW_Fertilizers_trend_v2012a.csv",row.names = F)
write.csv(Pest_gap13[,c(1,5)],"Outputs/GL-NCEAS_CW_Pesticides_trend_v2013a.csv",row.names = F)
write.csv(Pest_gap12[,c(1,5)],"Outputs/GL-NCEAS_CW_Pesticides_trend_v2012a.csv",row.names = F)

###########################
###### option B for gapfilling is to use regional population trends
# 10) re-join mean pop trend to the corresponding unique region 2013 ids per r2 group

Fert_r2POP_13 <-   sqldf("SELECT a.*, b.*
                         FROM Fert_r2_13 AS a
                         LEFT OUTER JOIN 
                         r2meanPOP13
                         AS b ON b.r2 = a.r2") 

Fert_r2POP_12 <-  sqldf("SELECT a.*, b.*
                        FROM Fert_r2_12 AS a
                        LEFT OUTER JOIN 
                        r2meanPOP12
                        AS b ON b.r2 = a.r2") 

Pest_r2POP_13 <-   sqldf("SELECT a.*, b.*
                         FROM Pest_r2_13 AS a
                         LEFT OUTER JOIN 
                         r2meanPOP13
                         AS b ON b.r2 = a.r2") 

Pest_r2POP_12 <-  sqldf("SELECT a.*, b.*
                        FROM Pest_r2_12 AS a
                        LEFT OUTER JOIN 
                        r2meanPOP12
                        AS b ON b.r2 = a.r2") 

write.csv(Fert_r2POP_13[,c(1,6,7,8)],"Outputs/Fertilizers_r2POPtrend_v2013a.csv",row.names = F)
write.csv(Fert_r2POP_13[,c(1,6,7,8)],"Outputs/Fertilizers_r2POPtrend_v2012a.csv",row.names = F)
write.csv(Pest_r2POP_13[,c(1,6,7,8)],"Outputs/Pesticides_r2POPtrend_v2013a.csv",row.names = F)
write.csv(Pest_r2POP_13[,c(1,6,7,8)],"Outputs/Pesticides_r2POPtrend_v2012a.csv",row.names = F)

###########################
###########################


