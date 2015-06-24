# Creating annual averages for each year

library(dplyr)

#paths
dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

# set tmp directory
tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)


setwd(file.path(dir_N,'git-annex/globalprep/VGPM_primary_productivity/v_2015'))



  
  l = list.files('working/rasterized_rawdata',pattern=as.character(i),full.names=T)
  
  s = stack(l)%>%calc(.,fun=function(x){mean(x,na.rm=T)},progress='text',filename='output/annual_mean_npp_',i,'.tif')
  


for (i in 2004:2014){
  print(i)
  
  l = list.files('working/rasterized_rawdata',pattern=as.character(i),full.names=T)
  
  s = stack(l)
  
  c = calc(s,fun=function(x){mean(x,na.rm=T)},progress='text',filename=paste0('output/annual_mean_npp_',i,'.tif',sep=''),overwrite=T)
  
}


