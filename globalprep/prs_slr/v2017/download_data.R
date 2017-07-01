#download newest data

#as of 6.30.17 there is only data through August of 2016. This code is close to working but not there yet. The raw data on mazu was downloaded by hand.

library(RCurl)
library(stringr)
url <- "ftp://ftp.aviso.altimetry.fr/global/delayed-time/grids/climatology/monthly_mean/"
userpwd <- "nceas_ohara:diors54jh"
filenames <- getURL(url, userpwd = userpwd,
                    ftp.use.epsv = FALSE,dirlistonly = TRUE) 

filenames = paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")
filenames = filenames[str_detect(filenames,"2016")]
filenames = filenames[str_detect(filenames, ".nc.gz")]
con = getCurlHandle( ftp.use.epsv = FALSE)
contents = sapply(filenames, function(x) try(getURL(x, curl = con)))
