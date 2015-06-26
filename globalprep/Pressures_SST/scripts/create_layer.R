# Create SST layers

library(raster)
library(RColorBrewer)
library(dplyr)

#paths

dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_N,'git-annex/globalprep/Pressures_SST'))

cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme

# set tmp directory

	tmpdir='~/big/R_raster_tmp'
	dir.create(tmpdir, showWarnings=F)
	rasterOptions(tmpdir=tmpdir)

# data

# Bring in SSTA

ssta = stack('data/cortadv5_SSTA.nc',varname='SSTA')
weekly_sst = stack('data/cortadv5_WeeklySST.nc',varname='WeeklySST')

names_ssta = names(ssta)
names_weekly = names(weekly_sst)


#TODO REWRITE FIRST LOOP


for(i in 1:53){
  
  s = stack()
  
  for (j in 1982:2012){
    
    w = which(substr(names_weekly,2,5)==j)[i] 
    if(is.na(w))next()
    
    w_week = weekly_sst[[w]]
    
    s = stack(s,w_week)
    
  }
  
  sd = calc(s,fun=function(x){sd(x,na.rm=T)},progress='text',filename=paste('sd_sst_week_',i,'.tif'))
}

# Second Loop to calculate annual positive anomalies


for (i in 1983:2012){
  
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




# Look at all years

l = list.files('tmp',pattern='annual_pos_anomalies',full.names=TRUE)

sta = stack(l)

plot(sta,col=cols)

# Get 5 year aggregates

yrs_08_12 = stack(l[27:31])%>%sum(.)

yrs_00_05 = stack(l[19:24])%>%sum(.)

yrs_05_10 = stack(l[24:29])%>%sum(.)

yrs_1982_86 = stack(l[1:5])%>%sum(.)

yrs_1985_90 = stack(l[4:9])%>%sum(.)

# Look at difference between

yrs_00_05_85_90 = yrs_00_05 - yrs_1985_90






