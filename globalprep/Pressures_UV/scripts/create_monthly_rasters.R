# Creating monthly uv rasters

# Take the new data (from 2013, 2014) and create monthly rasters in the same way as John Potapenko did using the 'create_monthly_rasters.py'

#filepaths

dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]


setwd(file.path(dir_N,'git-annex/globalprep/Pressures_UV/'))

#libraries

library(raster)


# read in tif files

daily_tifs = list.files('data/uv_omi_aura_2013_2014/tif_from_nc',full.names=T)



# create function to make monthly rasters



for(i in 2013:2014){
  
  for (for j in 1:12){
    
    #paste i and j to get year and month
    date = paste0(i,'m',j,sep='')
    
    #select all rasters in the month and year
    daily_files = filter(daily_tifs,substr(58,65)==date) #idk if filter will work here??

    s = stack(daily_files)
    
    mean = calc(s,fun=function(x){mean(x,na.rm=T)})
    
    std = calc(s,fun=function(x){standarddeviation(x)}) # try fun=function(x){sqrt(var(x))}
    
    
  }
  
  
}