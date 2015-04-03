# Looking at ocean acidification data to make transformation and rescaling decisions

# Bring in final layer (before transforming or rescaling)

#temp directory for rasters
tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)

#libraries
library(raster)
library(rasterVis)

# paths

dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]


# data

#need to pull in data from native resolution

hist_mean = raster('working/global_oa_1880_1889_arag_mean_moll.tif') # historical decadal mean of aragonite saturation state from 1880-1889
files = list.files('working/annualmean_2005-2014/moll',full.names=TRUE,recursive=TRUE) # list the annual mean raster files for each year in 2005-2014


# (Step 3): function that subtracts annual mean from historical decadal mean and outputs raster to specified folder

newfiles = list.files('working/annualchange_2005-2014',full.names=TRUE,recursive=TRUE) #list the new raster files (difference from historical mean)



#### look at just raw data for given year and rescale so that all values less than 1 are 1, and then
###  rescale all values greater than 1 based on the change

currentMeans = list.files('working/annualmean_2005-2014/moll',full.names=TRUE)
currentDiff = list.files('working/annualchange_2005-2014',full.names=TRUE,recursive=TRUE) #list the new raster files (difference from historical mean)


mean_2005 = raster(currentMeans[1])
diff_2005 = raster(currentDiff[1])


#all values less than 1 = 1

mean_2005[mean_2005<1]=1

# get the change from oa_2005

# where values are >1, swap in diff_2005

update = mask(mean_2005,diff_2005,updatevalue = )

mean_2005[mean_2005>1] = diff_2005[mean_2005>1]

mean_2005[mean_2005<0]<-0


#compare this with change in historical then rescaled by the max

diff = diff_2005

max = quantile(diff,0.9999)

out = calc(diff,fun=function(x){x/max})

out[out<0]<-0
out[out>1]<-1

