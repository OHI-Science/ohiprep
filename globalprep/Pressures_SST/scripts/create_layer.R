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

#ocean raster at 1km
	
ocean = raster(file.path(dir_N, 'model/GL-NCEAS-Halpern2008/tmp/ocean.tif'))

mollCRS=crs('+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs')
	


# Look at all years

  l   <- list.files('tmp',pattern='annual_pos_anomalies',full.names=TRUE)
  sta <- stack(l)
  plot(sta,col=cols)

  
# Get 5 year aggregates

  yrs_08_12   <- stack(l[27:31])%>%sum(.)
  yrs_00_05   <- stack(l[19:24])%>%sum(.)
  yrs_05_10   <- stack(l[24:29])%>%sum(.)
  yrs_1982_86 <- stack(l[1:5])%>%sum(.)
  yrs_1985_90 <- stack(l[4:9])%>%sum(.)

# Look at difference between

# (1) used in original CHI
yrs_00_05_85_90 = yrs_00_05 - yrs_1985_90

# (2) used in updated OHI/CHI
yrs_05_10_85_90 = yrs_05_10 - yrs_1985_90

# (3) new
yrs_08_12_85_90 = yrs_08_12 - yrs_1985_90

projection(yrs_08_12_85_90) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 

sst_2015 = projectRaster(yrs_08_12_85_90,crs=mollCRS,progress='text',over=T)%>%
            resample(.,ocean,method='ngb',progress='text')%>%
              mask(.,ocean,filename='sst_08_12-85_90.tif')




#compare 2 and 3

com = yrs_08_12_85_90 - yrs_05_10_85_90
