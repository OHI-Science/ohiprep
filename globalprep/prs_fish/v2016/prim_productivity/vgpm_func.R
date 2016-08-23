# Function to create rasters from VGPM .xyz files


# This code was adapted from Luke Miller: http://lukemiller.org/index.php/2011/12/loading-osus-vgpm-ocean-productivity-data-in-r/

#libraries

# set tmp directory
tmpdir='~/big/R_raster_tmp'
dir.create(tmpdir, showWarnings=F)
rasterOptions(tmpdir=tmpdir)


#create empty raster

e <- extent(c(-180,180,-90,90))
r <- raster(e,ncol=2160,nrow=1080)

#The options supplied to vgpm.raster() are as follows:
# file = file name (or substitute file.choose() to pick file interactively)
# w.lon = western longitude limit for region of interest (-180 to +180)
# e.lon = eastern longitude limit for region of interest (-180 to +180)
# n.lat = northern latitude limit for region of interest (+90 to -90)
# s.lat = southern latitude limit for region of interest (+90 to -90)
# log = TRUE - log10 transform productivity data before plotting
# color = specify color set to plot productivity data
# Function returns a matrix of productivity values for the specified region of
# interest with lat/lon listed in the row and column names.

# I (jamie) added to this function to create an output raster rather than matrix of values

w.lon = -180
e.lon = 180
n.lat = 90
s.lat = -90

vgpm.raster = function(file, w.lon, e.lon, n.lat, s.lat, log = TRUE, 
                       color = tim.colors(30)){
  
  #Extract date from file title
  fname = basename(file)
  print(fname)
  dots  = gregexpr('\\.',fname) #find locations of . in file name
  yrday = substr(fname,dots[[1]][1]+1,dots[[1]][2]-1) #extract yearday combo
  yr    = substr(yrday,1,4) #extract year
  doy   = substr(yrday,5,7) #extract day of year
  day1  = as.Date(paste(yr,doy,sep = '-'), format = '%Y-%j') #convert to Date
  
  #Read data from input file
  x = read.table(file, sep = ' ', skip = 1, na.strings = '-9999')
  
  names(x) = c('lon','lat','values') #Rename input columns
  
  if (nrow(x) == 2332800) { f.size = '1080'
  } else if (nrow(x) == 9331200) { f.size = '2160'
  } else {
    warning('Unknown file type\n', immediate. = TRUE)
  }
  
  if (f.size == '1080') {
    lons = x$lon[1:2160] #get set of longitude values
    lats = x$lat[seq(1,2332800,by = 2160)] #get latitude values
    values = matrix(x$values, nrow = 1080, ncol = 2160, byrow = TRUE)
  } else if (f.size == '2160') {
    lons = x$lon[1:4320] #get set of longitude values
    lats = x$lat[seq(1,9331200,by = 4320)] #get latitude values
    values = matrix(x$values, nrow = 2160, ncol = 4320, byrow = TRUE)
  }
  #Insert the lat/lon values as the 'values' matrix dimension names
  dimnames(values) = list(Latitude = lats, Longitude = lons)
  
  # Specify the boundaries of your lat/lon of interest. Recall that
  # longitude values run from -180E (international date line in the Pacific)
  # to +180E, where Greenwich,England is at 0E. Latitude values range from
  # +90N (north pole) to -90 (south pole). The first value for longitude must be
  # the western-most edge of your region of interest, and the first value for the
  # latitude must be the northern-most edge of the region of interest.
  lonlim = c(w.lon,e.lon) # c(western edge, eastern edge)
  latlim = c(n.lat,s.lat)	# c(northern edge, southern edge)
  
  # Create vectors of lat/lon indices
  lonindx = 1:length(lons) #make vector of longitude cell indices
  latindx = 1:length(lats) #make vector of latitude cell indices
  
  # Pull out 2 vectors that contain the indices of the lat/lon coordinates
  # of interest. We search for longitudes that are greater than the 1st value
  # in lonlim, and longitudes that are less than the 2nd value in lonlim, and
  # then grab the corresponding indices from lonindx to store in goodlons. The
  # same is done for the latitudes
  goodlons = lonindx[lons >= lonlim[1] & lons <= lonlim[2]]
  goodlats = latindx[lats >= latlim[2] & lats <= latlim[1]]
  
  # Extract a subset of the matrix 'values', call it the Region of Interest (ROI) 
  ROI = values[goodlats[1]:goodlats[length(goodlats)],
               goodlons[1]:goodlons[length(goodlons)]]
  # Add the latitudes and longitudes to the ROI matrix as dimension names
  dimnames(ROI) = list(Latitude = lats[goodlats], Longitude = lons[goodlons])
  n.lats = as.numeric(rownames(ROI))
  n.lons = as.numeric(colnames(ROI))
  
  # Generate a new set of lats and longs on a regular grid spacing for plot.
  if (f.size == '1080') {
    lats2 = seq(n.lats[1],(n.lats[length(n.lats)]-0.1666667),by=-0.1666667)
    lons2 = seq(n.lons[1],(n.lons[length(n.lons)]+0.1666667),by=0.1666667)
  } else if (f.size == '2160') {
    lats2 = seq(n.lats[1],(n.lats[length(n.lats)]-0.0833333),by=-0.0833333)
    lons2 = seq(n.lons[1],(n.lons[length(n.lons)]+0.0833333),by=0.0833333)
  }
  if(length(lats2) > length(n.lats)) lats2 = lats2[1:length(n.lats)]
  if(length(lons2) > length(n.lons)) lons2 = lons2[1:length(n.lons)]
  ROI.plot = t(ROI) # swap longs and lats in 'ROI', so lats are in columns
  ROI.plot = ROI.plot[,rev(1:length(lats2))] # reverse latitudes so that 
  # southern lats are listed first
  if (log) {
    image.plot(lons2, rev(lats2), log10(ROI.plot), useRaster = TRUE, 
               col = color,
               xlab = 'Longitude', ylab = 'Latitude', 
               main = paste('Net Primary Production', strftime(day1,'%B %Y')), 
               legend.lab = expression(paste(log[10],'(mg C /', m^2,'/ day)')),
               legend.mar = 4.1)
  } else if (!log){
    image.plot(lons2, rev(lats2), ROI.plot, useRaster = TRUE, 
               col = color,
               xlab = 'Longitude', ylab = 'Latitude', 
               main = paste('Net Primary Production', strftime(day1,'%B %Y')), 
               legend.lab = expression(paste('mg C /', m^2,'/ day')),
               legend.mar = 4.3)
  }
  ROI # return region of interest data to workspace
  
  # From here down, added code to the function to rasterize the matrix
  
  log = log10(ROI)
  r = raster(log)
  extent(r)<-e
  projection(r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  plot(r,col=color,
       xlab='Longitude',
       ylab='Latitude',
       main=paste('Net Primary Production',strftime(day1,'%B %Y')),
       legend.lab=expression(paste('mg C /', m^2,'/ day')),
       legend.mar=4.3)
  
  writeRaster(r,filename=paste0(file.path(dir_M,'git-annex/globalprep/prs_fish/v2016/VGPM_primary_productivity/int/rasterized_rawdata/'),'npp',sep='_',strftime(day1,'%B %Y')),
                                          format='GTiff',overwrite=T)
  
}  # end of vgpm.raster() function


