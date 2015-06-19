# Create SST layers

library(raster)

#paths

dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_N,'git-annex/globalprep/Pressures_SST'))

# set tmp directory

	tmpdir='~/big/R_raster_tmp'
	dir.create(tmpdir, showWarnings=F)
	rasterOptions(tmpdir=tmpdir)

# data

# Bring in SSTA

ssta = stack('data/cortadv5_SSTA.nc',varname='SSTA')
weekly_sst = stack('data/cortadv5_WeeklySST.nc',varname='WeeklySST')

names= names(ssta)

# Second Loop to calculate annual positive anomalies

# Bring in 

for (i in 1992:2012){
  
  print(i)
  
  s = stack()
  
  for (j in 1:53){
    
    print(j)
    
    sd = raster(paste0('tmp/sd_sst_week_',j,'.tif')) #sd for week
    w = which(substr(names,2,5)==i)[j]
    
    w_ssta = ssta[[w]] #subset the week/year anomaly
    
    count = overlay(w_ssta,sd,fun=function(x,y){ifelse(x>y,1,0)},progress='text') #compare to average anomaly for that week 
    
    s = stack(s,count)
    
  }
  
  year = calc(s,fun=function(x){sum(x)},progress='text',filename=paste0('tmp/annual_pos_anomalies_sd_',i,'.tif'))
}