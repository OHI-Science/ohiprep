# Understanding the global UV pressures data 

library(raster)


dir_halpern2008 = c('Windows' = '//neptune.nceas.ucsb.edu/halpern2008_edit',
                    'Darwin'  = '/Volumes/halpern2008_edit',
                    'Linux'   = '/var/cache/halpern-et-al')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_halpern2008,'mnt/storage/marine_threats/impact_layers_2013_redo/impact_layers/work/uv/uv_baseline_anomaly'))

omi <- raster('omi_aura_uv_anomaly_2008m01-2012m12_raw.tif')
rng <- range(omi[])
arg <- list(at=rng, labels=rng)
toms <- raster('toms_ep_uv_anomaly_1997m01-2001m12_raw.tif')
rng2 <- range(toms[])
arg2 <- list(at=rng2, labels=rng)

par(mfrow=c(2,1))
plot(omi_raw,main='Number of anomalous values from\nOMI AURA data 2008-2012',axis.args=arg)
plot(toms_raw,main='Number of anomalous values from\nTOMS data 1997-2001',axis.args=arg2)

diff = raster('uv_anomaly_difference_2008m01-2012m12_minus_1997m01-2001m12_raw.tif')
rng <- range(diff[])
arg <- list(at=rng,labels=rng)
par(mfrow=c(1,1))
par(mar=c(6,5,5,4)+0.1)
plot(diff,main = 'Difference in number of UV anomalies \n recent (2008-2012) - historical (1997-2001)',axis.args=arg)


trans = raster('uv_anomaly_difference_2008m01-2012m12_minus_1997m01-2001m12_trans.tif')
rng <- range(trans[])
arg <- list(at=rng,labels=round(rng,4))
plot(trans,main='Difference in number of UV anomalies\n log10 transformed',axis.args = arg)


abs = calc(diff,fun=abs)
max = cellStats(abs,stat='max')
out = calc(diff,fun=function(x){ifelse(x>0,1,-1)*log10(abs(x)+1)/log10(max+1)})
rng <- range(out[])
arg <- list(at=rng,labels=rng)
plot(out,main = 'Difference in number of UV anomalies ')