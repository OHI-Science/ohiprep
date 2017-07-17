# Prep raw sea surface temperature (CoRTAD version 5) for OHI 2015

library(raster)
library(RColorBrewer)
library(dplyr)

#paths
source('src/R/common.R')

#set working directory
setwd(file.path(dir_M,'git-annex/globalprep/prs_sst'))

cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme for plotting

# set tmp directory

tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)

#-----------------------------------------------------

# data

# Bring in SSTA

ssta         = stack('data/cortadv5_SSTA.nc',varname='SSTA')
weekly_sst   = stack('data/cortadv5_WeeklySST.nc',varname='WeeklySST')

names_ssta   = names(ssta)
names_weekly = names(weekly_sst)

#Create weekly standard deviations across all years
for(i in 1:53){
  
  s = stack()
  
  for (j in 1982:2012){
    
    print(j)
    w = which(substr(names_weekly,2,5)==j)[i] 
    if(is.na(w))next()
    
    w_week = weekly_sst[[w]]
    
    s = stack(s,w_week)
    
  }
  
  sd = calc(s,fun=function(x){sd(x,na.rm=T)},progress='text',filename=paste('sd_sst_week_',i,'.tif'))
  
}



# Second Loop to calculate annual positive anomalies


for (i in 1982:2012){
  
  print(i)
  
  s = stack()
  
  for (j in 1:53){
    
    print(j)
    
    sd = raster(paste0('tmp/sd_sst_week_',j,'.tif')) #sd for week
    w = which(substr(names_ssta,2,5)==i)[j]
    if(is.na(w))next()
    
    w_ssta = ssta[[w]] #subset the week/year anomaly
    
    count = overlay(w_ssta,sd,fun=function(x,y){ifelse(x>y,1,0)},progress='text') #compare to average anomaly for that week 
    
    s = stack(s,count)
    
  }
  
  year = calc(s,fun=function(x){sum(x,na.rm=T)},progress='text',filename=paste0('tmp/annual_pos_anomalies_sd_',i,'.tif'),overwrite=T)
}

