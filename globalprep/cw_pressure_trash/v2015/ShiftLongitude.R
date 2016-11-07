## shifting a rasters longitude


library(raster)

dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

data_wd = file.path(dir_N,'git-annex/globalprep/CW_pressure_trash/v2015')

setwd(data_wd)

x <- raster('globalplastic_wd-cd_rasters/count_density_size1.tif')

trim <- trim(x,values=0.2309009)

#set extent for cropping raster 
ext1 <- c(20, 360, -90, 90)
r1 <- crop(x, ext1)
#for some reason this crops to extent of 20,360.0111,-90,90 which gives a weird line. Need to reassign extent
#extent(r1)<-extent(20,360,-90,90)
r1 <- shift(r1, x=(360-extent(r1)[2])) #this is 0?

#set extent for piece of original raster that goes from 360-380. this is going to be chopped off, then moved to the correct side of the raster
ext2 <- c(360, 380, -90, 90)
r2 <- crop(x, ext2)
#extent(r2)<-extent(360,380,-90,90)
r2 <- shift(r2, x=-extent(r2)[1])
out <- merge(r1, r2,overlap=FALSE)

out2 <- rotate(out)  #weird line...not sure where that is coming from.




### Used the raster "rotate" function as the general framework
### Here is how you can find the code for a function in a package:
> showMethods(rotate)
# Function: rotate (package raster)
# x="Raster"


> getMethod('rotate', 'Raster')
# Method Definition:
  
  function (x, ...) 
  {
    .local <- function (x, filename = "", ...) 
    {
      e <- extent(x)
      xrange <- e@xmax - e@xmin
      if (xrange < 350 | xrange > 370 | e@xmin < -10 | e@xmax > 
            370) {
        if (xrange < 350 | xrange > 370 | e@xmin < -190 | 
              e@xmax > 190) {
          warning("this does not look like an appropriate object for this function")
        }
      }
      ext1 <- extent(0, 180, -90, 90)
      if (is.null(intersect(e, ext1))) {
        r1 <- NULL
      }
      else {
        r1 <- crop(x, ext1)
      }
      ext2 <- extent(180, 360 + res(x)[1], -90, 90)
      if (is.null(intersect(e, ext2))) {
        r2 <- NULL
      }
      else {
        r2 <- crop(x, ext2)
        r2 <- shift(r2, -360)
      }
      ln <- names(x)
      if (is.null(r1)) {
        out <- r2
      }
      else if (is.null(r2)) {
        out <- r1
      }
      else {
        out <- merge(r1, r2, overlap = FALSE)
      }
      names(out) <- names(x)
      out@z <- x@z
      p <- projection(out)
      if (length(grep("\\+over", p)) > 0) {
        projection(out) <- gsub("[[:space:]]\\+over", "", 
                                p)
      }
      if (filename != "") {
        out <- writeRaster(out, filename, ...)
      }
      return(out)
    }
    .local(x, ...)
  }
<bytecode: 0x000000000f617320>
  <environment: namespace:raster>
  
  Signatures:
  x       
target  "Raster"
defined "Raster"