####################################################################
## Calculate Status/Trend for High Seas
## April 1 2014 
####################################################################
library(dplyr)
library(plyr)

rm(list=ls())

status.year=2011
trend.year=status.year:(status.year-4)

setwd("N:\\model\\GL-AQ-NaturalProducts_v2013")


cmsy <- read.csv("tmp/fnk_np_ccmsy.csv")
regions <- read.csv("N:/git-annex/Global/NCEAS-Antarctica-Other_v2014/rgn_labels_ccamlr.csv")

eps <- .25 
score_range  <- 1-0.25
value_range <- 0.90-0

cmsy <- cmsy %.%
  mutate(score = ifelse(C_msy_assmt > 1.0, 2.0-C_msy_assmt, 
                        ifelse(C_msy_assmt < 0.9, eps + score_range/value_range * C_msy_assmt,
                               1)))

# # ### experimenting with c/cmsy to score conversion
# eps <- .25 
# score_range  <- 1-0.25
# value_range <- 0.90-0
# tmp <- data.frame(C_msy_assmt= seq(0,2, by=0.01))
# 
# tmp <- tmp %.%
#   mutate(score = ifelse(C_msy_assmt > 1.0, 2.0-C_msy_assmt, 
#                         ifelse(C_msy_assmt < 0.9, eps + score_range/value_range * C_msy_assmt,
#                         1)))
# plot(score ~ C_msy_assmt, data=tmp)



## Status ----
# 2013 status is based on 2011 data (most recent data)

regions <- regions %.%
  select(sp_id)

Status <- cmsy %.%
  filter(year==status.year) %.%
  select(sp_id, status = score) %.%
  mutate(status = round(status*100, 2))

Status <- merge(regions, Status, by="sp_id", all.x=TRUE)

write.csv(Status, "data\\status.csv", row.names=F, na="")


#Trend ----

cmsy <- cmsy[cmsy$year %in% trend.year,]

lm = dlply(
  cmsy, .(sp_id),
  function(x) lm(score ~ year, x))
  
Trend <- ldply(lm, coef)

Trend <- Trend %.%
  select(sp_id, trend=year) %.%
  mutate(trend = round(trend*5, 4))

  
Trend <- merge(regions, Trend, by="sp_id", all.x=TRUE)

write.csv(Trend, "data\\trend.csv", row.names=F, na='')

lm(score ~ year, data=subset(cmsy, sp_id==248200)) #data check....




