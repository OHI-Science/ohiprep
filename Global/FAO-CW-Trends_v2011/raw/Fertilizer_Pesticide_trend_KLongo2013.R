## by Katie Longo, September 2013. 
# Copied here from /Volumes/data_edit/model/GL-NCEAS_CW_trends_v2013a/tmp/Fertilizer_Pesticide_trend.R

# upload libraries needed for Julie's gap-filling scripts
library(reshape2)
library(gdata)
library(plyr)
options(gsubfn.engine = "R") # otherwise, get X11 launching for sqldf package
require(sqldf)

fert_pest<-read.csv("Data/GL-FAO-Trends_v2010-cleaned.csv")

#######################################################################
# clean and check the fertilizer and pesticides data

 head(fert_pest)
fert_pest[fert_pest$tonnes==0,]<-NA # 0s are actually NAs
fert_pest2<-na.omit(fert_pest) # remove NAs
names(fert_pest2)[1]<-"rgn_id"

# count at least 2 datapoint per country time-series for fertilizers
yearcheck<-aggregate(fert_pest2[fert_pest2$layer=="Fertilizers",4],by=list(fert_pest2[fert_pest2$layer=="Fertilizers",1]),FUN=length)
yearcheck[yearcheck$x<2,] # Qatar only has a value in 2010, rgn_id=190
# count there are at least 2 datapoints per country time-series for fertilizers when removing year 2010
yearcheck<-aggregate(fert_pest2[fert_pest2$year<2010 & fert_pest2$layer=="Fertilizers",4],by=list(fert_pest2[fert_pest2$year<2010 & fert_pest2$layer=="Fertilizers",1]),FUN=length)
yearcheck[yearcheck$x<2,]
fert_pest2[fert_pest2$rgn_id==179 & fert_pest2$layer=="Fertilizers",] # France doesn't have 2 datapoints if I limit the trend calculation for 2012 to values up to 2009
# same for pesticides
yearcheck<-aggregate(fert_pest2[fert_pest2$layer=="Pesticides",4],by=list(fert_pest2[fert_pest2$layer=="Pesticides",1]),FUN=length)
yearcheck[yearcheck$x<2,] # rgn_id 120 only has one datapoint in 2010
yearcheck<-aggregate(fert_pest2[fert_pest2$year<2010 & fert_pest2$layer=="Pesticides",4],by=list(fert_pest2[fert_pest2$year<2010 & fert_pest2$layer=="Pesticides",1]),FUN=length)
yearcheck[yearcheck$x<2,]
fert_pest2[fert_pest2$rgn_id%in%yearcheck[yearcheck$x<2,1] & fert_pest2$layer=="Pesticides",] # Mauritius, Tunisia, Bulgaria, and Israel only have one data point and all in the 90s
# check all countries have a time-series ending no earlier than 2005
Fert_max = ddply(
  fert_pest2[fert_pest2$layer=="Fertilizers",], .(rgn_id), summarize,
  max = max(year)) # all years have a data point later than 2004 for Fertilizer data
Pest_max = ddply(
  fert_pest2[fert_pest2$layer=="Pesticides",], .(rgn_id), summarize,
  max = max(year)) 
summary(Pest_max$max) # some years have the most recent datapoint in 1990 for Pesticide data
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1990    2000    2008    2005    2010    2010 
## Remove countries where the most recent value is prior to 2005 (i.e. the first year in our trend)
Pest_old<-Pest_max[Pest_max$max<2005,1] # these are the rgn_ids to exclude
fert_pest2<-fert_pest2[!(fert_pest2$layer=="Pesticides" & fert_pest2$rgn_id%in%Pest_old),] # exclude those timeseries for pesticides
fert_pest2<-fert_pest2[!(fert_pest2$layer=="Fertilizers" & fert_pest2$rgn_id==190),] # exclude Qatar from fertilizers timeseries bc it only has 1 datapoint
fert_pest2<-fert_pest2[!(fert_pest2$layer=="Pesticides" & fert_pest2$rgn_id==120),] # exclude Antigua and Barbuda from Pesticides because it only has 1 datapoint, rgn_id=120
# now check if there are still places with Pesticides time-series of only 1 datapoint
yearcheck<-aggregate(fert_pest2[fert_pest2$year<2010 & fert_pest2$layer=="Pesticides",4],by=list(fert_pest2[fert_pest2$year<2010 & fert_pest2$layer=="Pesticides",1]),FUN=length)
yearcheck[yearcheck$x<2,] # none

# Pest_max = ddply(
#   fert_pest2[fert_pest2$layer=="Pesticides",], .(rgn_id), summarize,
#   max = max(year)) 
# summary(Pest_max$max) 

#########################################################################

# 1) calculate fert and pest trend, excluding NAs, for 2012a and 2013a (years 2005:2009, 2006:2010, respectively)
# normalize trend by dividing by the fitted value for the first year of the series
##################### testing the function works
# fert.11<-fert_pest2[fert_pest2$rgn_id==11 & fert_pest2$layer=="Fertilizers",]
# lm(fert.11$tonnes ~ fert.11$year)$coefficients[[2]]*4 #   1.6
# summary(lm(fert.11$tonnes ~ fert.11$year))
# fert09.11<- 0.4000*2009 -796.9000
# fert05.11<- 0.4000*2005 -796.9000
# deltafert.11<- fert09.11-fert05.11 # 1.6
# deltafert.11/fert05.11 #   0.3137255

Fert_Delta12 = ddply(
  fert_pest2[fert_pest2$year<2010 & fert_pest2$layer=="Fertilizers",], .(rgn_id), summarize,
  trend = min(lm(tonnes ~ year)$coefficients[[2]]/(2005*lm(tonnes ~ year)$coefficients[[2]]+lm(tonnes ~ year)$coefficients[['(Intercept)']]) * 4,1)) # normalize trend by dividing by fitted value of first year
# fix missing value for France
Fert.Fr<-fert_pest2[fert_pest2$rgn_id==179 & fert_pest2$layer=="Fertilizers",] 
Fert_Delta12$trend[Fert_Delta12$rgn_id==179] <-min(lm(Fert.Fr$tonnes ~ Fert.Fr$year)$coefficients[[2]]/(2005*lm(Fert.Fr$tonnes ~ Fert.Fr$year)$coefficients[[2]]+lm(Fert.Fr$tonnes ~ Fert.Fr$year)$coefficients[['(Intercept)']]) * 4,1) # fitted value for France when both 2010 and 2009 points are used

Fert_Delta13 = ddply(
  fert_pest2[fert_pest2$layer=="Fertilizers",], .(rgn_id), summarize,
  trend = min(lm(tonnes ~ year)$coefficients[[2]]/(2006*lm(tonnes ~ year)$coefficients[[2]]+lm(tonnes ~ year)$coefficients[['(Intercept)']]) * 4,1)) # normalize trend by dividing by fitted value of first year

Pest_Delta12 = ddply(
  fert_pest2[fert_pest2$year<2010 & fert_pest2$layer=="Pesticides",], .(rgn_id), summarize,
  trend = min(lm(tonnes ~ year)$coefficients[[2]]/(2005*lm(tonnes ~ year)$coefficients[[2]]+lm(tonnes ~ year)$coefficients[['(Intercept)']]) * 4,1)) # normalize trend by dividing by fitted value of first year

Pest_Delta13 = ddply(
  fert_pest2[fert_pest2$layer=="Pesticides",], .(rgn_id), summarize,
  trend = min(lm(tonnes ~ year)$coefficients[[2]]/(2006*lm(tonnes ~ year)$coefficients[[2]]+lm(tonnes ~ year)$coefficients[['(Intercept)']]) * 4,1)) # normalize trend by dividing by fitted value of first year

# 2) calculate pop trend for 2012a and 2013a (years 2005:2009, 2006:2011, respectively)
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
